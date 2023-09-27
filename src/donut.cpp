// Donut compressor for NES tile data by JRoatch

#include <cstdio>   /* I/O */
#include <cstdlib>  /* exit(), strtol() */
#include <cstdint>  /* uint8_t */
#include <cstring>  /* memcpy() */

#include "builtin.hpp"
#include "convert.hpp"

namespace // anonymous
{

/* According to a strace of cat on my system, and a quick dd of dev/zero:
   131072 is the optimal block size,
   but that's 2 times the size of the entire 6502 address space!
   The usual data input is going to be 512 tiles of NES gfx data. */
#define BUF_IO_SIZE 8192
#define BUF_GAP_SIZE 512
#define BUF_TOTAL_SIZE ((BUF_IO_SIZE+BUF_GAP_SIZE)*2)

static uint8_t byte_buffer[BUF_TOTAL_SIZE];

#define OUTPUT_BEGIN (byte_buffer)
#define INPUT_BEGIN (byte_buffer + BUF_TOTAL_SIZE - BUF_IO_SIZE)

int popcount8(uint8_t x)
{
    return builtin::popcount(x);
}

struct buffer_pointers_t
{
    std::uint8_t* dest_begin; /* first valid byte */
    std::uint8_t* dest_end;   /* one past the last valid byte */
                         /* length is infered as end - begin. */
    std::uint8_t* src_begin;
    std::uint8_t* src_end;
};

enum 
{
    DESTINATION_FULL,
    SOURCE_EMPTY,
    SOURCE_IS_PARTIAL,
    ENCOUNTERED_UNDEFINED_BLOCK
};

std::uint64_t flip_plane_bits_135(std::uint64_t plane)
{
    std::uint64_t result = 0;
    std::uint64_t t;
    if(plane == 0xffffffffffffffff)
        return plane;
    if(plane == 0x0000000000000000)
        return plane;
    for(unsigned i = 0; i < 8; ++i) 
    {
        t = plane >> i;
        t &= 0x0101010101010101;
        t *= 0x0102040810204080;
        t >>= 56;
        t &= 0xff;
        result |= t << (i*8);
    }
    return result;
}

int pack_pb8(std::uint8_t *buffer_ptr, std::uint64_t plane, std::uint8_t top_value)
{
    std::uint8_t pb8_ctrl;
    std::uint8_t pb8_byte;
    std::uint8_t c;
    std::uint8_t* p;
    p = buffer_ptr;
    ++p;
    pb8_ctrl = 0;
    pb8_byte = top_value;
    for(unsigned i = 0; i < 8; ++i) 
    {
        c = plane >> (8*(7-i));
        if(c != pb8_byte) 
        {
            *p = c;
            ++p;
            pb8_byte = c;
            pb8_ctrl |= 0x80>>i;
        }
    }
    *buffer_ptr = pb8_ctrl;
    return p - buffer_ptr;
}

std::uint64_t read_plane(std::uint8_t* p)
{
    return (
        (std::uint64_t(p[0]) << (8*0)) |
        (std::uint64_t(p[1]) << (8*1)) |
        (std::uint64_t(p[2]) << (8*2)) |
        (std::uint64_t(p[3]) << (8*3)) |
        (std::uint64_t(p[4]) << (8*4)) |
        (std::uint64_t(p[5]) << (8*5)) |
        (std::uint64_t(p[6]) << (8*6)) |
        (std::uint64_t(p[7]) << (8*7))
    );
}

int cblock_cost(std::uint8_t* p, int l)
{
    int cycles;
    std::uint8_t block_header;
    std::uint8_t plane_def;
    int pb8_count;
    bool decode_only_1_pb8_plane;
    std::uint8_t short_defs[4] = {0x00, 0x55, 0xaa, 0xff};
    if(l < 1)
        return 0;
    block_header = *p;
    --l;
    if(block_header >= 0xc0)
        return 0;
    if(block_header == 0x2a)
        return 1268;
    cycles = 1298;
    if(block_header & 0xc0)
        cycles += 640;
    if(block_header & 0x20)
        cycles += 4;
    if(block_header & 0x10)
        cycles += 4;
    if(block_header & 0x02) 
    {
        if(l < 1)
            return 0;
        plane_def = *(p+1);
        --l;
        cycles += 5;
        decode_only_1_pb8_plane = ((block_header & 0x04) && (plane_def != 0x00));
    } 
    else 
    {
        plane_def = short_defs[(block_header & 0x0c) >> 2];
        decode_only_1_pb8_plane = false;
    }
    pb8_count = popcount8(plane_def);
    cycles += (block_header & 0x01) ? (pb8_count * 614) : (pb8_count * 75);
    if(!decode_only_1_pb8_plane) 
    {
        l -= pb8_count;
        cycles += l * 6;
    } 
    else 
    {
        --l;
        cycles += 1 * pb8_count;
        cycles += (l * 6 * pb8_count);
    }
    return cycles;
}

bool all_pb8_planes_match(uint8_t *p, int pb8_length, int number_of_pb8_planes)
{
    int i, c, l;
    if(number_of_pb8_planes <= 1) 
    {
        // a block of 0 duplicate pb8 planes is 1 byte more then normal,
        // and a normal block of 1 plane is 5 cycles less to decode 
        return false;
    }
    l = number_of_pb8_planes*pb8_length;
    for(c = 0, i = pb8_length; i < l; ++i, ++c) 
    {
        if(c >= pb8_length) 
            c = 0;
        if(*(p + c) != *(p + i))
            return false;
    }
    return true;
}

int compress_blocks(buffer_pointers_t* result_p, bool allow_partial, bool use_bit_flip, int cycle_limit)
{
    buffer_pointers_t p;
    std::uint64_t block[8];
    std::uint64_t plane;
    std::uint64_t plane_predict;
    int shortest_length;
    int least_cost;
    int a, i, r, l;
    std::uint8_t temp_cblock[74];
    std::uint8_t *temp_p;
    std::uint8_t plane_def;
    std::uint8_t short_defs[4] = {0x00, 0x55, 0xaa, 0xff};
    bool planes_match;
    std::uint64_t first_non_zero_plane;
    std::uint64_t first_non_zero_plane_predict;
    int number_of_pb8_planes;
    int first_pb8_length;
    p = *(result_p);
    while (p.src_begin - p.dest_end >= 65) 
    {
        l = p.src_end - p.src_begin;
        if(l <= 0)
            return SOURCE_EMPTY;
        else if(l < 64) 
        {
            if(!allow_partial)
                return SOURCE_IS_PARTIAL;
            std::memset(p.dest_end + 1, 0x00, 64);
        } 
        else
            l = 64;
        *(p.dest_end) = 0x2a;
        std::memmove(p.dest_end + 1, p.src_begin, l);
        p.src_begin += l;
        shortest_length = 65;
        least_cost = 1268;
        for(i = 0; i < 8; ++i) 
            block[i] = read_plane((p.dest_end + 1) + (i*8));
        for(r = 0; r < 2; ++r) 
        {
            if(r == 1) 
            {
                if(use_bit_flip) 
                {
                    for(i = 0; i < 8; ++i)
                        block[i] = flip_plane_bits_135(block[i]);
                }
                else
                    break;
            }
            for(a = 0; a < 0xc; ++a) 
            {
                temp_p = temp_cblock + 2;
                plane_def = 0x00;
                number_of_pb8_planes = 0;
                planes_match = true;
                first_pb8_length = 0;
                first_non_zero_plane = 0;
                first_non_zero_plane_predict = 0;
                for(i = 0; i < 8; ++i) 
                {
                    plane = block[i];
                    if(i & 1) 
                    {
                        plane_predict = (a & 0x1) ? 0xffffffffffffffff : 0x0000000000000000;
                        if(a & 0x4) 
                            plane ^= block[i-1];
                    } 
                    else 
                    {
                        plane_predict = (a & 0x2) ? 0xffffffffffffffff : 0x0000000000000000;
                        if(a & 0x8) 
                            plane ^= block[i+1];
                    }
                    plane_def <<= 1;
                    if(plane != plane_predict) 
                    {
                        l = pack_pb8(temp_p, plane, (uint8_t)plane_predict);
                        temp_p += l;
                        plane_def |= 1;
                        if(number_of_pb8_planes == 0) 
                        {
                            first_non_zero_plane_predict = plane_predict;
                            first_non_zero_plane = plane;
                            first_pb8_length = l;
                        } 
                        else if(first_non_zero_plane != plane)
                            planes_match = false;
                        else if(first_non_zero_plane_predict != plane_predict)
                            planes_match = false;
                        ++number_of_pb8_planes;
                    }
                }
                if(number_of_pb8_planes <= 1) 
                {
                    planes_match = false;
                    /* a normal block of 1 plane is cheaper to decode,
                       and may even be smaller. */
                }
                temp_cblock[0] = r | (a<<4) | 0x02;
                temp_cblock[1] = plane_def;
                l = temp_p - temp_cblock;
                temp_p = temp_cblock;
                if(all_pb8_planes_match(temp_p+2, first_pb8_length, number_of_pb8_planes)) 
                {
                    *(temp_p + 0) = r | (a<<4) | 0x06;
                    l = 2 + first_pb8_length;
                } 
                else if(planes_match) 
                {
                    *(temp_p + 0) = r | (a<<4) | 0x06;
                    l = 2 + pack_pb8(temp_p+2, first_non_zero_plane, ~(uint8_t)first_non_zero_plane);
                } 
                else 
                {
                    for(i = 0; i < 4; ++i) 
                    {
                        if(plane_def == short_defs[i]) 
                        {
                            ++temp_p;
                            *(temp_p + 0) = r | (a<<4) | (i << 2);
                            --l;
                            break;
                        }
                    }
                }
                if(l <= shortest_length) 
                {
                    i = cblock_cost(temp_p, l);
                    if((i <= cycle_limit) && ((l < shortest_length) || (i < least_cost))) 
                    {
                        std::memmove(p.dest_end, temp_p, l);
                        shortest_length = l;
                        least_cost = i;
                    }
                }
            }
        }
        p.dest_end += shortest_length;
        *(result_p) = p;
    }
    return DESTINATION_FULL;
}

} // end anonymous namespace

std::vector<std::uint8_t> compress_donut(std::uint8_t* begin, std::uint8_t* end)
{
    std::size_t const span = end - begin;
    if((span % 64) != 0)
        throw convert_error_t("Donut conversion error: Expecting size to be a multiple of 64.");

    std::vector<std::uint8_t> result;

    buffer_pointers_t p = {};
    p.src_begin = INPUT_BEGIN;
    p.src_end = INPUT_BEGIN;
    p.dest_begin = OUTPUT_BEGIN;
    p.dest_end = OUTPUT_BEGIN;
    int status = SOURCE_EMPTY;

    while(true) 
    {
        std::size_t l = p.src_end - p.src_begin;
        if((l <= BUF_GAP_SIZE) && end != begin) 
        {
            if(l > 0)
                std::memmove(INPUT_BEGIN - l, p.src_begin, l);
            p.src_begin = INPUT_BEGIN - l;
            p.src_end = INPUT_BEGIN;

            l = std::min<std::size_t>(end - begin, BUF_IO_SIZE);
            std::copy(begin, begin + l, INPUT_BEGIN);
            begin += l;
            p.src_end += l;
        }

        status = compress_blocks(&p, begin == end, true, 100000);

        l = (size_t)(p.dest_end - p.dest_begin);
        if(l >= BUF_IO_SIZE) {
            for(unsigned i = 0; i < BUF_IO_SIZE; ++i)
                result.push_back(p.dest_begin[i]);

            p.dest_begin += l;

            l = (size_t)(p.dest_end - p.dest_begin);
            if (l > 0)
                memmove(OUTPUT_BEGIN, p.dest_begin, l);
            p.dest_begin = OUTPUT_BEGIN;
            p.dest_end = OUTPUT_BEGIN + l;
        }

        if((begin == end && (status == SOURCE_EMPTY)) || (status == ENCOUNTERED_UNDEFINED_BLOCK)) 
        {
            l = (size_t)(p.dest_end - p.dest_begin);
            if(l > 0) 
            {
                for(unsigned i = 0; i < l; ++i)
                    result.push_back(p.dest_begin[i]);
                p.dest_begin += l;
            }
            if(status == ENCOUNTERED_UNDEFINED_BLOCK)
                throw convert_error_t("Donut conversion error: Unhandled block header.");
            break;
        }
    }

    return result;
}
