SSA_DEF(null, 0, INPUT_NONE, 0) // Keep as first!

SSA_DEF(if,     1, INPUT_VALUE, SSAF_NO_GVN)

// ((arg, locator)...)
SSA_DEF(return, -1, INPUT_VALUE, SSAF_WRITE_GLOBALS | SSAF_NO_GVN)

// (defs...)
SSA_DEF(phi,   -1, INPUT_VALUE, SSAF_NO_GVN)

// ()
SSA_DEF(entry, 0, INPUT_NONE, SSAF_NO_GVN)

// ()
SSA_DEF(uninitialized, 0, INPUT_NONE, 0) // for new vars

// (op)
SSA_DEF(carry, 1, INPUT_LINK, SSAF_TRACE_INPUTS)

SSA_DEF(trace, -1, INPUT_VALUE, 0)
SSA_DEF(cast,   1, INPUT_VALUE, SSAF_TRACE_INPUTS)
SSA_DEF(sign_extend, 1, INPUT_VALUE, SSAF_TRACE_INPUTS | SSAF_BRANCHY_CG)

// (lhs, rhs, carry)
SSA_DEF(add,    3, INPUT_VALUE, SSAF_TRACE_INPUTS | SSAF_CLOBBERS_CARRY | SSAF_COMMUTATIVE)
SSA_DEF(sub,    3, INPUT_VALUE, SSAF_TRACE_INPUTS | SSAF_CLOBBERS_CARRY)
SSA_DEF(mul,    3, INPUT_VALUE, SSAF_TRACE_INPUTS | SSAF_CLOBBERS_CARRY | SSAF_COMMUTATIVE)
SSA_DEF(div,    3, INPUT_VALUE, SSAF_TRACE_INPUTS | SSAF_CLOBBERS_CARRY)

// (value, shift)
SSA_DEF(shl,    2, INPUT_VALUE, SSAF_TRACE_INPUTS | SSAF_CLOBBERS_CARRY)
SSA_DEF(shr,    2, INPUT_VALUE, SSAF_TRACE_INPUTS | SSAF_CLOBBERS_CARRY)

// (value, carry) - This only supports unsigned inputs.
SSA_DEF(rol,    2, INPUT_VALUE, SSAF_CLOBBERS_CARRY | SSAF_TRACE_INPUTS)
SSA_DEF(ror,    2, INPUT_VALUE, SSAF_CLOBBERS_CARRY | SSAF_TRACE_INPUTS)

// (value) - Converts high bit to the carry.
SSA_DEF(sign_to_carry, 1, INPUT_VALUE, SSAF_CLOBBERS_CARRY | SSAF_TRACE_INPUTS)

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

// Last two arguments are compared as if the first is unsigned, the second is signed.
// Other arguments are compared as if both are unsigned.
SSA_DEF(multi_eq,     -1, INPUT_VALUE, SSAF_TRACE_INPUTS | SSAF_CLOBBERS_CARRY | SSAF_BRANCHY_CG)
SSA_DEF(multi_not_eq, -1, INPUT_VALUE, SSAF_TRACE_INPUTS | SSAF_CLOBBERS_CARRY | SSAF_BRANCHY_CG)

// The first two arguments are the types being compared, as constant ints.
// Next, the sequential bytes of LHS, followed by the sequential bytes of RHS.
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

///////////////
// FUNCTIONS //
///////////////

// (fn, (arg, locator)...)
SSA_DEF(fn_call, -1, INPUT_VALUE, SSAF_WRITE_GLOBALS | SSAF_CLOBBERS_CARRY)

// (fn, (arg, locator)...)
SSA_DEF(goto_mode, -1, INPUT_VALUE, SSAF_WRITE_GLOBALS | SSAF_IO_IMPURE | SSAF_NO_GVN)

////////////
// ARRAYS //
////////////

// (array, locator, index)
SSA_DEF(read_array, 3, INPUT_VALUE, SSAF_INDEXES_ARRAY | SSAF_READ_ARRAY)
// Same as above, but more efficient in code gen.
SSA_DEF(cg_read_array_direct, 3, INPUT_VALUE, SSAF_INDEXES_ARRAY | SSAF_CG_NEVER_STORE | SSAF_READ_ARRAY)

// (array, locator, index, value)
SSA_DEF(write_array, 4, INPUT_VALUE, SSAF_WRITE_ARRAY | SSAF_INDEXES_ARRAY)

// (array dest, new locator, array src) TODO
// SSA_DEF(copy_array, 2, INPUT_ORDER, SSAF_WRITE_ARRAY | SSAF_READ_ARRAY) TODO

// (value) TODO
// SSA_DEF(fill_array, 3, INPUT_ORDER, 0) TODO

// (values...)
SSA_DEF(init_array, -1, INPUT_VALUE, 0)

//////////////
// POINTERS //
//////////////

// (hi, lo) - combines two bytes into a pointer
SSA_DEF(make_ptr_lo, 2, INPUT_NONE, SSAF_COPY)
// (lo, hi) - combines two bytes into a pointer
SSA_DEF(make_ptr_hi, 2, INPUT_NONE, SSAF_COPY)

// (ptr, ptr_hi, bank, index)
// TODO: should this be impure, because of bankswitches?
SSA_DEF(read_ptr, 4, INPUT_VALUE, SSAF_INDEXES_PTR | SSAF_NULL_INPUT_VALID)

// (ptr, ptr_hi, bank, index, value)
SSA_DEF(write_ptr, 5, INPUT_VALUE, SSAF_IO_IMPURE | SSAF_INDEXES_PTR | SSAF_NULL_INPUT_VALID)

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
SSA_DEF(phi_copy,      1, INPUT_VALUE, SSAF_COPY)

// Stores into a locator at some point earlier in the program than needed.
// Used in out-of-SSA as an optimization.
// (node)
SSA_DEF(early_store, 1, INPUT_VALUE,  SSAF_COPY)
SSA_DEF(aliased_store, 1, INPUT_VALUE,  SSAF_COPY) // TODO: is this really necessary?

#undef SSA_DEF
