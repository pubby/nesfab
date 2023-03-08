#include "unroll_divisor.hpp"

#include <algorithm>
#include <bit>
#include <cstdint>

#include "builtin.hpp"

static std::uint8_t divisors_1[] = { 1 };
static std::uint8_t divisors_2[] = { 1 };
static std::uint8_t divisors_3[] = { 1 };
static std::uint8_t divisors_4[] = { 2, 1 };
static std::uint8_t divisors_5[] = { 1 };
static std::uint8_t divisors_6[] = { 3, 2, 1 };
static std::uint8_t divisors_7[] = { 1 };
static std::uint8_t divisors_8[] = { 4, 2, 1 };
static std::uint8_t divisors_9[] = { 3, 1 };
static std::uint8_t divisors_10[] = { 5, 2, 1 };
static std::uint8_t divisors_11[] = { 1 };
static std::uint8_t divisors_12[] = { 6, 4, 3, 2, 1 };
static std::uint8_t divisors_13[] = { 1 };
static std::uint8_t divisors_14[] = { 7, 2, 1 };
static std::uint8_t divisors_15[] = { 5, 3, 1 };
static std::uint8_t divisors_16[] = { 8, 4, 2, 1 };
static std::uint8_t divisors_17[] = { 1 };
static std::uint8_t divisors_18[] = { 9, 6, 3, 2, 1 };
static std::uint8_t divisors_19[] = { 1 };
static std::uint8_t divisors_20[] = { 10, 5, 4, 2, 1 };
static std::uint8_t divisors_21[] = { 7, 3, 1 };
static std::uint8_t divisors_22[] = { 11, 2, 1 };
static std::uint8_t divisors_23[] = { 1 };
static std::uint8_t divisors_24[] = { 12, 8, 6, 4, 3, 2, 1 };
static std::uint8_t divisors_25[] = { 5, 1 };
static std::uint8_t divisors_26[] = { 13, 2, 1 };
static std::uint8_t divisors_27[] = { 9, 3, 1 };
static std::uint8_t divisors_28[] = { 14, 7, 4, 2, 1 };
static std::uint8_t divisors_29[] = { 1 };
static std::uint8_t divisors_30[] = { 15, 10, 6, 5, 3, 2, 1 };
static std::uint8_t divisors_31[] = { 1 };
static std::uint8_t divisors_32[] = { 16, 8, 4, 2, 1 };
static std::uint8_t divisors_33[] = { 11, 3, 1 };
static std::uint8_t divisors_34[] = { 17, 2, 1 };
static std::uint8_t divisors_35[] = { 7, 5, 1 };
static std::uint8_t divisors_36[] = { 18, 12, 9, 6, 4, 3, 2, 1 };
static std::uint8_t divisors_37[] = { 1 };
static std::uint8_t divisors_38[] = { 19, 2, 1 };
static std::uint8_t divisors_39[] = { 13, 3, 1 };
static std::uint8_t divisors_40[] = { 20, 10, 8, 5, 4, 2, 1 };
static std::uint8_t divisors_41[] = { 1 };
static std::uint8_t divisors_42[] = { 21, 14, 7, 6, 3, 2, 1 };
static std::uint8_t divisors_43[] = { 1 };
static std::uint8_t divisors_44[] = { 22, 11, 4, 2, 1 };
static std::uint8_t divisors_45[] = { 15, 9, 5, 3, 1 };
static std::uint8_t divisors_46[] = { 23, 2, 1 };
static std::uint8_t divisors_47[] = { 1 };
static std::uint8_t divisors_48[] = { 24, 16, 12, 8, 6, 4, 3, 2, 1 };
static std::uint8_t divisors_49[] = { 7, 1 };
static std::uint8_t divisors_50[] = { 25, 10, 5, 2, 1 };
static std::uint8_t divisors_51[] = { 17, 3, 1 };
static std::uint8_t divisors_52[] = { 26, 13, 4, 2, 1 };
static std::uint8_t divisors_53[] = { 1 };
static std::uint8_t divisors_54[] = { 27, 18, 9, 6, 3, 2, 1 };
static std::uint8_t divisors_55[] = { 11, 5, 1 };
static std::uint8_t divisors_56[] = { 28, 14, 8, 7, 4, 2, 1 };
static std::uint8_t divisors_57[] = { 19, 3, 1 };
static std::uint8_t divisors_58[] = { 29, 2, 1 };
static std::uint8_t divisors_59[] = { 1 };
static std::uint8_t divisors_60[] = { 30, 20, 15, 12, 10, 6, 5, 4, 3, 2, 1 };
static std::uint8_t divisors_61[] = { 1 };
static std::uint8_t divisors_62[] = { 31, 2, 1 };
static std::uint8_t divisors_63[] = { 21, 9, 7, 3, 1 };
static std::uint8_t divisors_64[] = { 32, 16, 8, 4, 2, 1 };
static std::uint8_t divisors_65[] = { 13, 5, 1 };
static std::uint8_t divisors_66[] = { 33, 22, 11, 6, 3, 2, 1 };
static std::uint8_t divisors_67[] = { 1 };
static std::uint8_t divisors_68[] = { 34, 17, 4, 2, 1 };
static std::uint8_t divisors_69[] = { 23, 3, 1 };
static std::uint8_t divisors_70[] = { 35, 14, 10, 7, 5, 2, 1 };
static std::uint8_t divisors_71[] = { 1 };
static std::uint8_t divisors_72[] = { 36, 24, 18, 12, 9, 8, 6, 4, 3, 2, 1 };
static std::uint8_t divisors_73[] = { 1 };
static std::uint8_t divisors_74[] = { 37, 2, 1 };
static std::uint8_t divisors_75[] = { 25, 15, 5, 3, 1 };
static std::uint8_t divisors_76[] = { 38, 19, 4, 2, 1 };
static std::uint8_t divisors_77[] = { 11, 7, 1 };
static std::uint8_t divisors_78[] = { 39, 26, 13, 6, 3, 2, 1 };
static std::uint8_t divisors_79[] = { 1 };
static std::uint8_t divisors_80[] = { 40, 20, 16, 10, 8, 5, 4, 2, 1 };
static std::uint8_t divisors_81[] = { 27, 9, 3, 1 };
static std::uint8_t divisors_82[] = { 41, 2, 1 };
static std::uint8_t divisors_83[] = { 1 };
static std::uint8_t divisors_84[] = { 42, 28, 21, 14, 12, 7, 6, 4, 3, 2, 1 };
static std::uint8_t divisors_85[] = { 17, 5, 1 };
static std::uint8_t divisors_86[] = { 43, 2, 1 };
static std::uint8_t divisors_87[] = { 29, 3, 1 };
static std::uint8_t divisors_88[] = { 44, 22, 11, 8, 4, 2, 1 };
static std::uint8_t divisors_89[] = { 1 };
static std::uint8_t divisors_90[] = { 45, 30, 18, 15, 10, 9, 6, 5, 3, 2, 1 };
static std::uint8_t divisors_91[] = { 13, 7, 1 };
static std::uint8_t divisors_92[] = { 46, 23, 4, 2, 1 };
static std::uint8_t divisors_93[] = { 31, 3, 1 };
static std::uint8_t divisors_94[] = { 47, 2, 1 };
static std::uint8_t divisors_95[] = { 19, 5, 1 };
static std::uint8_t divisors_96[] = { 48, 32, 24, 16, 12, 8, 6, 4, 3, 2, 1 };
static std::uint8_t divisors_97[] = { 1 };
static std::uint8_t divisors_98[] = { 49, 14, 7, 2, 1 };
static std::uint8_t divisors_99[] = { 33, 11, 9, 3, 1 };
static std::uint8_t divisors_100[] = { 50, 25, 20, 10, 5, 4, 2, 1 };
static std::uint8_t divisors_101[] = { 1 };
static std::uint8_t divisors_102[] = { 51, 34, 17, 6, 3, 2, 1 };
static std::uint8_t divisors_103[] = { 1 };
static std::uint8_t divisors_104[] = { 52, 26, 13, 8, 4, 2, 1 };
static std::uint8_t divisors_105[] = { 35, 21, 15, 7, 5, 3, 1 };
static std::uint8_t divisors_106[] = { 53, 2, 1 };
static std::uint8_t divisors_107[] = { 1 };
static std::uint8_t divisors_108[] = { 54, 36, 27, 18, 12, 9, 6, 4, 3, 2, 1 };
static std::uint8_t divisors_109[] = { 1 };
static std::uint8_t divisors_110[] = { 55, 22, 11, 10, 5, 2, 1 };
static std::uint8_t divisors_111[] = { 37, 3, 1 };
static std::uint8_t divisors_112[] = { 56, 28, 16, 14, 8, 7, 4, 2, 1 };
static std::uint8_t divisors_113[] = { 1 };
static std::uint8_t divisors_114[] = { 57, 38, 19, 6, 3, 2, 1 };
static std::uint8_t divisors_115[] = { 23, 5, 1 };
static std::uint8_t divisors_116[] = { 58, 29, 4, 2, 1 };
static std::uint8_t divisors_117[] = { 39, 13, 9, 3, 1 };
static std::uint8_t divisors_118[] = { 59, 2, 1 };
static std::uint8_t divisors_119[] = { 17, 7, 1 };
static std::uint8_t divisors_120[] = { 60, 40, 30, 24, 20, 15, 12, 10, 8, 6, 5, 4, 3, 2, 1 };
static std::uint8_t divisors_121[] = { 11, 1 };
static std::uint8_t divisors_122[] = { 61, 2, 1 };
static std::uint8_t divisors_123[] = { 41, 3, 1 };
static std::uint8_t divisors_124[] = { 62, 31, 4, 2, 1 };
static std::uint8_t divisors_125[] = { 25, 5, 1 };
static std::uint8_t divisors_126[] = { 63, 42, 21, 18, 14, 9, 7, 6, 3, 2, 1 };
static std::uint8_t divisors_127[] = { 1 };
static std::uint8_t divisors_128[] = { 64, 32, 16, 8, 4, 2, 1 };
static std::uint8_t divisors_129[] = { 43, 3, 1 };
static std::uint8_t divisors_130[] = { 65, 26, 13, 10, 5, 2, 1 };
static std::uint8_t divisors_131[] = { 1 };
static std::uint8_t divisors_132[] = { 66, 44, 33, 22, 12, 11, 6, 4, 3, 2, 1 };
static std::uint8_t divisors_133[] = { 19, 7, 1 };
static std::uint8_t divisors_134[] = { 67, 2, 1 };
static std::uint8_t divisors_135[] = { 45, 27, 15, 9, 5, 3, 1 };
static std::uint8_t divisors_136[] = { 68, 34, 17, 8, 4, 2, 1 };
static std::uint8_t divisors_137[] = { 1 };
static std::uint8_t divisors_138[] = { 69, 46, 23, 6, 3, 2, 1 };
static std::uint8_t divisors_139[] = { 1 };
static std::uint8_t divisors_140[] = { 70, 35, 28, 20, 14, 10, 7, 5, 4, 2, 1 };
static std::uint8_t divisors_141[] = { 47, 3, 1 };
static std::uint8_t divisors_142[] = { 71, 2, 1 };
static std::uint8_t divisors_143[] = { 13, 11, 1 };
static std::uint8_t divisors_144[] = { 72, 48, 36, 24, 18, 16, 12, 9, 8, 6, 4, 3, 2, 1 };
static std::uint8_t divisors_145[] = { 29, 5, 1 };
static std::uint8_t divisors_146[] = { 73, 2, 1 };
static std::uint8_t divisors_147[] = { 49, 21, 7, 3, 1 };
static std::uint8_t divisors_148[] = { 74, 37, 4, 2, 1 };
static std::uint8_t divisors_149[] = { 1 };
static std::uint8_t divisors_150[] = { 75, 50, 30, 25, 15, 10, 6, 5, 3, 2, 1 };
static std::uint8_t divisors_151[] = { 1 };
static std::uint8_t divisors_152[] = { 76, 38, 19, 8, 4, 2, 1 };
static std::uint8_t divisors_153[] = { 51, 17, 9, 3, 1 };
static std::uint8_t divisors_154[] = { 77, 22, 14, 11, 7, 2, 1 };
static std::uint8_t divisors_155[] = { 31, 5, 1 };
static std::uint8_t divisors_156[] = { 78, 52, 39, 26, 13, 12, 6, 4, 3, 2, 1 };
static std::uint8_t divisors_157[] = { 1 };
static std::uint8_t divisors_158[] = { 79, 2, 1 };
static std::uint8_t divisors_159[] = { 53, 3, 1 };
static std::uint8_t divisors_160[] = { 80, 40, 32, 20, 16, 10, 8, 5, 4, 2, 1 };
static std::uint8_t divisors_161[] = { 23, 7, 1 };
static std::uint8_t divisors_162[] = { 81, 54, 27, 18, 9, 6, 3, 2, 1 };
static std::uint8_t divisors_163[] = { 1 };
static std::uint8_t divisors_164[] = { 82, 41, 4, 2, 1 };
static std::uint8_t divisors_165[] = { 55, 33, 15, 11, 5, 3, 1 };
static std::uint8_t divisors_166[] = { 83, 2, 1 };
static std::uint8_t divisors_167[] = { 1 };
static std::uint8_t divisors_168[] = { 84, 56, 42, 28, 24, 21, 14, 12, 8, 7, 6, 4, 3, 2, 1 };
static std::uint8_t divisors_169[] = { 13, 1 };
static std::uint8_t divisors_170[] = { 85, 34, 17, 10, 5, 2, 1 };
static std::uint8_t divisors_171[] = { 57, 19, 9, 3, 1 };
static std::uint8_t divisors_172[] = { 86, 43, 4, 2, 1 };
static std::uint8_t divisors_173[] = { 1 };
static std::uint8_t divisors_174[] = { 87, 58, 29, 6, 3, 2, 1 };
static std::uint8_t divisors_175[] = { 35, 25, 7, 5, 1 };
static std::uint8_t divisors_176[] = { 88, 44, 22, 16, 11, 8, 4, 2, 1 };
static std::uint8_t divisors_177[] = { 59, 3, 1 };
static std::uint8_t divisors_178[] = { 89, 2, 1 };
static std::uint8_t divisors_179[] = { 1 };
static std::uint8_t divisors_180[] = { 90, 60, 45, 36, 30, 20, 18, 15, 12, 10, 9, 6, 5, 4, 3, 2, 1 };
static std::uint8_t divisors_181[] = { 1 };
static std::uint8_t divisors_182[] = { 91, 26, 14, 13, 7, 2, 1 };
static std::uint8_t divisors_183[] = { 61, 3, 1 };
static std::uint8_t divisors_184[] = { 92, 46, 23, 8, 4, 2, 1 };
static std::uint8_t divisors_185[] = { 37, 5, 1 };
static std::uint8_t divisors_186[] = { 93, 62, 31, 6, 3, 2, 1 };
static std::uint8_t divisors_187[] = { 17, 11, 1 };
static std::uint8_t divisors_188[] = { 94, 47, 4, 2, 1 };
static std::uint8_t divisors_189[] = { 63, 27, 21, 9, 7, 3, 1 };
static std::uint8_t divisors_190[] = { 95, 38, 19, 10, 5, 2, 1 };
static std::uint8_t divisors_191[] = { 1 };
static std::uint8_t divisors_192[] = { 96, 64, 48, 32, 24, 16, 12, 8, 6, 4, 3, 2, 1 };
static std::uint8_t divisors_193[] = { 1 };
static std::uint8_t divisors_194[] = { 97, 2, 1 };
static std::uint8_t divisors_195[] = { 65, 39, 15, 13, 5, 3, 1 };
static std::uint8_t divisors_196[] = { 98, 49, 28, 14, 7, 4, 2, 1 };
static std::uint8_t divisors_197[] = { 1 };
static std::uint8_t divisors_198[] = { 99, 66, 33, 22, 18, 11, 9, 6, 3, 2, 1 };
static std::uint8_t divisors_199[] = { 1 };
static std::uint8_t divisors_200[] = { 100, 50, 40, 25, 20, 10, 8, 5, 4, 2, 1 };
static std::uint8_t divisors_201[] = { 67, 3, 1 };
static std::uint8_t divisors_202[] = { 101, 2, 1 };
static std::uint8_t divisors_203[] = { 29, 7, 1 };
static std::uint8_t divisors_204[] = { 102, 68, 51, 34, 17, 12, 6, 4, 3, 2, 1 };
static std::uint8_t divisors_205[] = { 41, 5, 1 };
static std::uint8_t divisors_206[] = { 103, 2, 1 };
static std::uint8_t divisors_207[] = { 69, 23, 9, 3, 1 };
static std::uint8_t divisors_208[] = { 104, 52, 26, 16, 13, 8, 4, 2, 1 };
static std::uint8_t divisors_209[] = { 19, 11, 1 };
static std::uint8_t divisors_210[] = { 105, 70, 42, 35, 30, 21, 15, 14, 10, 7, 6, 5, 3, 2, 1 };
static std::uint8_t divisors_211[] = { 1 };
static std::uint8_t divisors_212[] = { 106, 53, 4, 2, 1 };
static std::uint8_t divisors_213[] = { 71, 3, 1 };
static std::uint8_t divisors_214[] = { 107, 2, 1 };
static std::uint8_t divisors_215[] = { 43, 5, 1 };
static std::uint8_t divisors_216[] = { 108, 72, 54, 36, 27, 24, 18, 12, 9, 8, 6, 4, 3, 2, 1 };
static std::uint8_t divisors_217[] = { 31, 7, 1 };
static std::uint8_t divisors_218[] = { 109, 2, 1 };
static std::uint8_t divisors_219[] = { 73, 3, 1 };
static std::uint8_t divisors_220[] = { 110, 55, 44, 22, 20, 11, 10, 5, 4, 2, 1 };
static std::uint8_t divisors_221[] = { 17, 13, 1 };
static std::uint8_t divisors_222[] = { 111, 74, 37, 6, 3, 2, 1 };
static std::uint8_t divisors_223[] = { 1 };
static std::uint8_t divisors_224[] = { 112, 56, 32, 28, 16, 14, 8, 7, 4, 2, 1 };
static std::uint8_t divisors_225[] = { 75, 45, 25, 15, 9, 5, 3, 1 };
static std::uint8_t divisors_226[] = { 113, 2, 1 };
static std::uint8_t divisors_227[] = { 1 };
static std::uint8_t divisors_228[] = { 114, 76, 57, 38, 19, 12, 6, 4, 3, 2, 1 };
static std::uint8_t divisors_229[] = { 1 };
static std::uint8_t divisors_230[] = { 115, 46, 23, 10, 5, 2, 1 };
static std::uint8_t divisors_231[] = { 77, 33, 21, 11, 7, 3, 1 };
static std::uint8_t divisors_232[] = { 116, 58, 29, 8, 4, 2, 1 };
static std::uint8_t divisors_233[] = { 1 };
static std::uint8_t divisors_234[] = { 117, 78, 39, 26, 18, 13, 9, 6, 3, 2, 1 };
static std::uint8_t divisors_235[] = { 47, 5, 1 };
static std::uint8_t divisors_236[] = { 118, 59, 4, 2, 1 };
static std::uint8_t divisors_237[] = { 79, 3, 1 };
static std::uint8_t divisors_238[] = { 119, 34, 17, 14, 7, 2, 1 };
static std::uint8_t divisors_239[] = { 1 };
static std::uint8_t divisors_240[] = { 120, 80, 60, 48, 40, 30, 24, 20, 16, 15, 12, 10, 8, 6, 5, 4, 3, 2, 1 };
static std::uint8_t divisors_241[] = { 1 };
static std::uint8_t divisors_242[] = { 121, 22, 11, 2, 1 };
static std::uint8_t divisors_243[] = { 81, 27, 9, 3, 1 };
static std::uint8_t divisors_244[] = { 122, 61, 4, 2, 1 };
static std::uint8_t divisors_245[] = { 49, 35, 7, 5, 1 };
static std::uint8_t divisors_246[] = { 123, 82, 41, 6, 3, 2, 1 };
static std::uint8_t divisors_247[] = { 19, 13, 1 };
static std::uint8_t divisors_248[] = { 124, 62, 31, 8, 4, 2, 1 };
static std::uint8_t divisors_249[] = { 83, 3, 1 };
static std::uint8_t divisors_250[] = { 125, 50, 25, 10, 5, 2, 1 };
static std::uint8_t divisors_251[] = { 1 };
static std::uint8_t divisors_252[] = { 126, 84, 63, 42, 36, 28, 21, 18, 14, 12, 9, 7, 6, 4, 3, 2, 1 };
static std::uint8_t divisors_253[] = { 23, 11, 1 };
static std::uint8_t divisors_254[] = { 127, 2, 1 };
static std::uint8_t divisors_255[] = { 85, 51, 17, 15, 5, 3, 1 };
static std::uint8_t divisors_256[] = { 128, 64, 32, 16, 8, 4, 2, 1 };

static std::uint8_t const* divisors[] =
{
    divisors_1,
    divisors_2,
    divisors_3,
    divisors_4,
    divisors_5,
    divisors_6,
    divisors_7,
    divisors_8,
    divisors_9,
    divisors_10,
    divisors_11,
    divisors_12,
    divisors_13,
    divisors_14,
    divisors_15,
    divisors_16,
    divisors_17,
    divisors_18,
    divisors_19,
    divisors_20,
    divisors_21,
    divisors_22,
    divisors_23,
    divisors_24,
    divisors_25,
    divisors_26,
    divisors_27,
    divisors_28,
    divisors_29,
    divisors_30,
    divisors_31,
    divisors_32,
    divisors_33,
    divisors_34,
    divisors_35,
    divisors_36,
    divisors_37,
    divisors_38,
    divisors_39,
    divisors_40,
    divisors_41,
    divisors_42,
    divisors_43,
    divisors_44,
    divisors_45,
    divisors_46,
    divisors_47,
    divisors_48,
    divisors_49,
    divisors_50,
    divisors_51,
    divisors_52,
    divisors_53,
    divisors_54,
    divisors_55,
    divisors_56,
    divisors_57,
    divisors_58,
    divisors_59,
    divisors_60,
    divisors_61,
    divisors_62,
    divisors_63,
    divisors_64,
    divisors_65,
    divisors_66,
    divisors_67,
    divisors_68,
    divisors_69,
    divisors_70,
    divisors_71,
    divisors_72,
    divisors_73,
    divisors_74,
    divisors_75,
    divisors_76,
    divisors_77,
    divisors_78,
    divisors_79,
    divisors_80,
    divisors_81,
    divisors_82,
    divisors_83,
    divisors_84,
    divisors_85,
    divisors_86,
    divisors_87,
    divisors_88,
    divisors_89,
    divisors_90,
    divisors_91,
    divisors_92,
    divisors_93,
    divisors_94,
    divisors_95,
    divisors_96,
    divisors_97,
    divisors_98,
    divisors_99,
    divisors_100,
    divisors_101,
    divisors_102,
    divisors_103,
    divisors_104,
    divisors_105,
    divisors_106,
    divisors_107,
    divisors_108,
    divisors_109,
    divisors_110,
    divisors_111,
    divisors_112,
    divisors_113,
    divisors_114,
    divisors_115,
    divisors_116,
    divisors_117,
    divisors_118,
    divisors_119,
    divisors_120,
    divisors_121,
    divisors_122,
    divisors_123,
    divisors_124,
    divisors_125,
    divisors_126,
    divisors_127,
    divisors_128,
    divisors_129,
    divisors_130,
    divisors_131,
    divisors_132,
    divisors_133,
    divisors_134,
    divisors_135,
    divisors_136,
    divisors_137,
    divisors_138,
    divisors_139,
    divisors_140,
    divisors_141,
    divisors_142,
    divisors_143,
    divisors_144,
    divisors_145,
    divisors_146,
    divisors_147,
    divisors_148,
    divisors_149,
    divisors_150,
    divisors_151,
    divisors_152,
    divisors_153,
    divisors_154,
    divisors_155,
    divisors_156,
    divisors_157,
    divisors_158,
    divisors_159,
    divisors_160,
    divisors_161,
    divisors_162,
    divisors_163,
    divisors_164,
    divisors_165,
    divisors_166,
    divisors_167,
    divisors_168,
    divisors_169,
    divisors_170,
    divisors_171,
    divisors_172,
    divisors_173,
    divisors_174,
    divisors_175,
    divisors_176,
    divisors_177,
    divisors_178,
    divisors_179,
    divisors_180,
    divisors_181,
    divisors_182,
    divisors_183,
    divisors_184,
    divisors_185,
    divisors_186,
    divisors_187,
    divisors_188,
    divisors_189,
    divisors_190,
    divisors_191,
    divisors_192,
    divisors_193,
    divisors_194,
    divisors_195,
    divisors_196,
    divisors_197,
    divisors_198,
    divisors_199,
    divisors_200,
    divisors_201,
    divisors_202,
    divisors_203,
    divisors_204,
    divisors_205,
    divisors_206,
    divisors_207,
    divisors_208,
    divisors_209,
    divisors_210,
    divisors_211,
    divisors_212,
    divisors_213,
    divisors_214,
    divisors_215,
    divisors_216,
    divisors_217,
    divisors_218,
    divisors_219,
    divisors_220,
    divisors_221,
    divisors_222,
    divisors_223,
    divisors_224,
    divisors_225,
    divisors_226,
    divisors_227,
    divisors_228,
    divisors_229,
    divisors_230,
    divisors_231,
    divisors_232,
    divisors_233,
    divisors_234,
    divisors_235,
    divisors_236,
    divisors_237,
    divisors_238,
    divisors_239,
    divisors_240,
    divisors_241,
    divisors_242,
    divisors_243,
    divisors_244,
    divisors_245,
    divisors_246,
    divisors_247,
    divisors_248,
    divisors_249,
    divisors_250,
    divisors_251,
    divisors_252,
    divisors_253,
    divisors_254,
    divisors_255,
    divisors_256,
};

unsigned estimate_unroll_divisor(unsigned n, unsigned d)
{
    assert(d);

    if(d >= n)
        return n;

    if(n > 0 && n <= 256)
    {
        // Use lookup tables to quickly find a divisor:
        auto const* table = divisors[n-1];
        while(*table > d)
            ++table;
        assert(n % *table == 0);
        return *table;
    }

    if(n % d == 0)
        return d;

    // It's fast to find a power of 2 divisor:
    unsigned const pow2 = std::min<unsigned>(1 << builtin::ctz(n), std::bit_floor(d));
    assert(n % pow2 == 0);

    // But first we'll check a few odd divisors:
    if(5 < d && 5 > pow2 && 5 % n == 0)
        return 5;
    if(3 < d && 3 > pow2 && 3 % n == 0)
        return 3;
    
    return pow2;
}
