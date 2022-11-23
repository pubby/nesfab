SSA_DEF(null, 0, INPUT_NONE, 0) // Keep as first!
SSA_DEF(gen_load, 0, INPUT_NONE, 0) // Fake. Used by cg_isel for debugging purposes.

SSA_DEF(if,      1, INPUT_VALUE, SSAF_NO_GVN | SSAF_CONDITIONAL)

// (condition, cases...)
SSA_DEF(switch_full, -1, INPUT_VALUE, SSAF_NO_GVN | SSAF_CONDITIONAL) // has no default case
SSA_DEF(switch_partial, -1, INPUT_VALUE, SSAF_NO_GVN | SSAF_CONDITIONAL) // has a default case

// (condition, UU offset)
SSA_DEF(switch_table, 2, INPUT_VALUE, SSAF_NO_GVN | SSAF_CONDITIONAL) // has a default case

// ((arg, locator)...)
SSA_DEF(return, -1, INPUT_VALUE, SSAF_WRITE_GLOBALS | SSAF_NO_GVN)

// (defs...)
SSA_DEF(phi,   -1, INPUT_VALUE, SSAF_NO_GVN | SSAF_FREE)

// ()
SSA_DEF(entry, 0, INPUT_NONE, SSAF_NO_GVN)

// ()
SSA_DEF(uninitialized, 0, INPUT_NONE, SSAF_CG_UNLIVE | SSAF_NO_GVN | SSAF_FREE) // for new vars

// ()
SSA_DEF(ready, 0, INPUT_NONE, SSAF_NO_GVN)

// (op)
SSA_DEF(carry, 1, INPUT_LINK, SSAF_TRACE_INPUTS)

SSA_DEF(trace, -1, INPUT_VALUE, 0)

// (value)
SSA_DEF(cast,   1, INPUT_VALUE, SSAF_FREE)

// (value) - 'SSA_cast' can't convert to bool, but this NOP can. Just make sure values are in range [0, 1].
SSA_DEF(as_bool,1, INPUT_VALUE, SSAF_FREE)

// (value) - Creates all zeroes or all ones, depending on input.
SSA_DEF(sign_extend, 1, INPUT_VALUE, SSAF_TRACE_INPUTS | SSAF_BRANCHY_CG)

// TODO: remove?
// Same as above, but outputs a carry too.
//SSA_DEF(sign_extend_carry, 1, INPUT_VALUE, SSAF_TRACE_INPUTS | SSAF_BRANCHY_CG)

// (lhs, rhs, carry)
SSA_DEF(add,    3, INPUT_VALUE, SSAF_TRACE_INPUTS | SSAF_CLOBBERS_CARRY | SSAF_COMMUTATIVE)
SSA_DEF(sub,    3, INPUT_VALUE, SSAF_TRACE_INPUTS | SSAF_CLOBBERS_CARRY)

// (lhs, rhs)
SSA_DEF(mul,    2, INPUT_VALUE, SSAF_TRACE_INPUTS | SSAF_CLOBBERS_CARRY | SSAF_COMMUTATIVE | SSAF_EXPENSIVE)

// (lhs, rhs)
SSA_DEF(mul8_lo, 2, INPUT_VALUE, SSAF_EXPENSIVE | SSAF_COMMUTATIVE | SSAF_CLOBBERS_CARRY)
// (mul8_lo)
SSA_DEF(mul8_hi, 1, INPUT_LINK, SSAF_FREE | SSAF_CLOBBERS_CARRY)

//SSA_DEF(div,    2, INPUT_VALUE, SSAF_TRACE_INPUTS | SSAF_CLOBBERS_CARRY) // TODO

// (value, shift)
SSA_DEF(shl,    2, INPUT_VALUE, SSAF_TRACE_INPUTS | SSAF_CLOBBERS_CARRY)
SSA_DEF(shr,    2, INPUT_VALUE, SSAF_TRACE_INPUTS | SSAF_CLOBBERS_CARRY)

// (value, atom)
SSA_DEF(get_byte,        2, INPUT_VALUE, SSAF_TRACE_INPUTS | SSAF_FREE)
SSA_DEF(array_get_byte,  2, INPUT_VALUE, SSAF_TRACE_INPUTS | SSAF_FREE)
// (value, atom, replace)
SSA_DEF(replace_byte,    3, INPUT_VALUE, SSAF_TRACE_INPUTS | SSAF_FREE)
SSA_DEF(array_replace_byte,  2, INPUT_VALUE, SSAF_TRACE_INPUTS | SSAF_FREE)

// (value, carry) - This only supports unsigned inputs.
SSA_DEF(rol,    2, INPUT_VALUE, SSAF_CLOBBERS_CARRY | SSAF_TRACE_INPUTS)
SSA_DEF(ror,    2, INPUT_VALUE, SSAF_CLOBBERS_CARRY | SSAF_TRACE_INPUTS)

// (value) - Converts high bit to bool
SSA_DEF(sign, 1, INPUT_VALUE, SSAF_CLOBBERS_CARRY | SSAF_TRACE_INPUTS)

// (lhs, rhs)
SSA_DEF(and,    2, INPUT_VALUE, SSAF_TRACE_INPUTS | SSAF_COMMUTATIVE)
SSA_DEF(or,     2, INPUT_VALUE, SSAF_TRACE_INPUTS | SSAF_COMMUTATIVE)
SSA_DEF(xor,    2, INPUT_VALUE, SSAF_TRACE_INPUTS | SSAF_COMMUTATIVE)

// (lhs, rhs) - eq and not_eq expect their inputs to be the same size.
SSA_DEF(eq,     2, INPUT_VALUE, SSAF_TRACE_INPUTS | SSAF_CLOBBERS_CARRY | SSAF_COMMUTATIVE)
SSA_DEF(not_eq, 2, INPUT_VALUE, SSAF_TRACE_INPUTS | SSAF_CLOBBERS_CARRY | SSAF_COMMUTATIVE)

// (lhs, rhs) - lt and lte work with inputs of different size
SSA_DEF(lt,     2, INPUT_VALUE, SSAF_TRACE_INPUTS | SSAF_CLOBBERS_CARRY)
SSA_DEF(lte,    2, INPUT_VALUE, SSAF_TRACE_INPUTS | SSAF_CLOBBERS_CARRY)

// Multi-byte comparisons.
// Last two arguments are compared as if the first is unsigned, the second is signed.
// Other arguments are compared as if both are unsigned.
SSA_DEF(multi_eq,     -1, INPUT_VALUE, SSAF_TRACE_INPUTS | SSAF_CLOBBERS_CARRY | SSAF_BRANCHY_CG)
SSA_DEF(multi_not_eq, -1, INPUT_VALUE, SSAF_TRACE_INPUTS | SSAF_CLOBBERS_CARRY | SSAF_BRANCHY_CG)

// The first two arguments are the types being compared, as constant ints.
// Next, the sequential bytes of LHS, followed by the sequential bytes of RHS,
// from least significant bytes to most significant.
// If both types are signed, both types must have the same number of whole bytes.
SSA_DEF(multi_lt,  -1, INPUT_VALUE, SSAF_TRACE_INPUTS | SSAF_CLOBBERS_CARRY | SSAF_BRANCHY_CG)
SSA_DEF(multi_lte, -1, INPUT_VALUE, SSAF_TRACE_INPUTS | SSAF_CLOBBERS_CARRY | SSAF_BRANCHY_CG)

//SSA_DEF(byte_lt,     -1, INPUT_VALUE, SSAF_TRACE_INPUTS | SSAF_CLOBBERS_CARRY)
//SSA_DEF(byte_lte,    -1, INPUT_VALUE, SSAF_TRACE_INPUTS | SSAF_CLOBBERS_CARRY)

SSA_DEF(jump,           0, INPUT_VALUE, 0)
SSA_DEF(branch_eq,     -1, INPUT_VALUE, SSAF_CLOBBERS_CARRY | SSAF_BRANCHY_CG)
SSA_DEF(branch_not_eq, -1, INPUT_VALUE, SSAF_CLOBBERS_CARRY | SSAF_BRANCHY_CG)
SSA_DEF(branch_lt,     -1, INPUT_VALUE, SSAF_CLOBBERS_CARRY | SSAF_BRANCHY_CG)
SSA_DEF(branch_lte,    -1, INPUT_VALUE, SSAF_CLOBBERS_CARRY | SSAF_BRANCHY_CG)
SSA_DEF(branch_sign,   -1, INPUT_VALUE, SSAF_CLOBBERS_CARRY | SSAF_BRANCHY_CG)

SSA_DEF(wait_nmi,      -1, INPUT_VALUE, SSAF_IO_IMPURE | SSAF_WRITE_GLOBALS | SSAF_FENCE | SSAF_PRIO_SCHEDULE)
SSA_DEF(fence,         -1, INPUT_VALUE, SSAF_WRITE_GLOBALS | SSAF_FENCE | SSAF_PRIO_SCHEDULE)

///////////////
// FUNCTIONS //
///////////////

// (fn, bank (optional), (arg, locator)...)
SSA_DEF(fn_call, -1, INPUT_VALUE, SSAF_WRITE_GLOBALS | SSAF_CLOBBERS_CARRY | SSAF_PRIO_SCHEDULE | SSAF_NULL_INPUT_VALID)

// (fn, stmt, (arg, locator)...)
SSA_DEF(goto_mode, -1, INPUT_VALUE, SSAF_WRITE_GLOBALS | SSAF_IO_IMPURE | SSAF_NO_GVN | SSAF_NULL_INPUT_VALID)

////////////
// ARRAYS //
////////////

// (array, offset, index)
SSA_DEF(read_array8,  3, INPUT_VALUE, SSAF_INDEXES_ARRAY8 | SSAF_READ_ARRAY | SSAF_ARRAY_OFFSET)

// (array, offset, index)
SSA_DEF(read_array16, 3, INPUT_VALUE, SSAF_INDEXES_ARRAY16 | SSAF_READ_ARRAY | SSAF_ARRAY_OFFSET)
// Same as above, but more efficient in code gen.
SSA_DEF(cg_read_array8_direct, 3, INPUT_VALUE, SSAF_INDEXES_ARRAY8 | SSAF_CG_NEVER_STORE | SSAF_READ_ARRAY | SSAF_ARRAY_OFFSET)

// (array, index_hi, index)
SSA_DEF(read_array16_b, 3, INPUT_VALUE, SSAF_INDEXES_ARRAY16 | SSAF_READ_ARRAY)

// (array, offset, index, value)
SSA_DEF(write_array8,  4, INPUT_VALUE, SSAF_WRITE_ARRAY | SSAF_INDEXES_ARRAY8 | SSAF_ARRAY_OFFSET | SSAF_CHEAP_SCHEDULE)
SSA_DEF(write_array16, 4, INPUT_VALUE, SSAF_WRITE_ARRAY | SSAF_INDEXES_ARRAY16 | SSAF_ARRAY_OFFSET | SSAF_CHEAP_SCHEDULE)

// (array, index_hi, index, value)
SSA_DEF(write_array16_b, 4, INPUT_VALUE, SSAF_INDEXES_ARRAY16 | SSAF_WRITE_ARRAY | SSAF_CHEAP_SCHEDULE)

// (array) - uses type as resize target
SSA_DEF(resize_array, 1, INPUT_VALUE, SSAF_WRITE_ARRAY | SSAF_READ_ARRAY)

// (array dest, new locator, array src) TODO
// SSA_DEF(copy_array, 2, INPUT_ORDER, SSAF_WRITE_ARRAY | SSAF_READ_ARRAY) TODO

// (value) - inits all elements of an array with 'value'
SSA_DEF(fill_array, 1, INPUT_VALUE, 0)

// (values...)
SSA_DEF(init_array, -1, INPUT_VALUE, 0)


//////////////
// POINTERS //
//////////////

// (hi, lo) - combines two bytes into a pointer
SSA_DEF(make_ptr_lo, 2, INPUT_NONE, SSAF_COPY | SSAF_CHEAP_SCHEDULE)
// (lo, hi) - combines two bytes into a pointer
SSA_DEF(make_ptr_hi, 2, INPUT_NONE, SSAF_COPY | SSAF_CHEAP_SCHEDULE)

// (ptr, ptr_hi, bank, index)
SSA_DEF(read_ptr,    4, INPUT_VALUE, SSAF_INDEXES_PTR | SSAF_NULL_INPUT_VALID | SSAF_BANK_INPUT)
SSA_DEF(read_ptr_hw, 4, INPUT_VALUE, SSAF_INDEXES_PTR | SSAF_NULL_INPUT_VALID | SSAF_BANK_INPUT | SSAF_IO_IMPURE)

// (ptr, ptr_hi, bank, index, value)
SSA_DEF(write_ptr,    5, INPUT_VALUE, SSAF_INDEXES_PTR | SSAF_NULL_INPUT_VALID | SSAF_BANK_INPUT | SSAF_IO_IMPURE | SSAF_CHEAP_SCHEDULE)
SSA_DEF(write_ptr_hw, 5, INPUT_VALUE, SSAF_INDEXES_PTR | SSAF_NULL_INPUT_VALID | SSAF_BANK_INPUT | SSAF_IO_IMPURE | SSAF_CHEAP_SCHEDULE)

/////////////
// GLOBALS //
/////////////

// (link, locator)
SSA_DEF(read_global, 2, INPUT_LINK, 0)

//////////////
// CODE-GEN //
//////////////

// IDEAS:
// - inc_array
// - dec_array
// - lshift_array
// - rshift_array
// - sax_array
// (these help codegen)

// Used to implement out-of-SSA for phi node inputs.
// (node)
SSA_DEF(phi_copy,      1, INPUT_VALUE, SSAF_COPY | SSAF_CHEAP_SCHEDULE)

// Stores into a locator at some point earlier in the program than needed.
// Used in out-of-SSA as an optimization.
// (node)
SSA_DEF(early_store, 1, INPUT_VALUE,  SSAF_COPY | SSAF_PRIO_SCHEDULE | SSAF_CHEAP_SCHEDULE)
SSA_DEF(aliased_store, 1, INPUT_VALUE,  SSAF_COPY | SSAF_CHEAP_SCHEDULE) // TODO: is this really necessary?

#undef SSA_DEF
