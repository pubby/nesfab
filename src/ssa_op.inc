SSA_DEF(null, 0, INPUT_NONE, 0) // Keep as first!

SSA_DEF(if,     1, INPUT_VALUE, 0)

// ((arg, locator)...)
SSA_DEF(return, -1, INPUT_VALUE, SSAF_WRITE_GLOBALS)

// (defs...)
SSA_DEF(phi,   -1, INPUT_VALUE, 0)

// ()
SSA_DEF(entry, 0, INPUT_NONE, 0)

// ()
SSA_DEF(uninitialized, 0, INPUT_NONE, 0) // for new vars

// (op)
SSA_DEF(carry, 1, INPUT_LINK, SSAF_TRACE_INPUTS)

// (fn, (arg, locator)...)
SSA_DEF(fn_call, -1, INPUT_VALUE, SSAF_WRITE_GLOBALS | SSAF_CLOBBERS_CARRY)

// (fn, (arg, locator)...)
SSA_DEF(goto_mode, -1, INPUT_VALUE, SSAF_WRITE_GLOBALS | SSAF_IMPURE)

// (link, locator)
SSA_DEF(read_global, 2, INPUT_LINK, 0)

// (elements...)
SSA_DEF(init_array, -1, INPUT_VALUE, 0)
// (elements)
SSA_DEF(init_array_fill, -1, INPUT_VALUE, 0)
// (array, index, value)
SSA_DEF(write_array, 3, INPUT_VALUE, 0)
// (array, index)
SSA_DEF(read_array, 2, INPUT_VALUE, 0)

// (ptr, index)
SSA_DEF(read_ptr, 2, INPUT_VALUE, 0)
// (ptr, index, value)
SSA_DEF(write_ptr, 3, INPUT_VALUE, 0)

// Used to implement out-of-SSA for phi node inputs.
// (node)
SSA_DEF(phi_copy,      1, INPUT_VALUE, SSAF_COPY)

// Stores into a locator at some point earlier in the program than needed.
// Used in out-of-SSA as an optimization.
// (node)
SSA_DEF(early_store, 1, INPUT_VALUE,  SSAF_COPY)
SSA_DEF(aliased_store, 1, INPUT_VALUE,  SSAF_COPY)

 SSA_DEF(trace, -1, INPUT_VALUE, 0)
SSA_DEF(cast,   1, INPUT_VALUE, SSAF_TRACE_INPUTS)

// (lhs, rhs, carry)
SSA_DEF(add,    3, INPUT_VALUE, SSAF_TRACE_INPUTS | SSAF_CLOBBERS_CARRY)
SSA_DEF(sub,    3, INPUT_VALUE, SSAF_TRACE_INPUTS | SSAF_CLOBBERS_CARRY)

SSA_DEF(and,    2, INPUT_VALUE, SSAF_TRACE_INPUTS)
SSA_DEF(or,     2, INPUT_VALUE, SSAF_TRACE_INPUTS)
SSA_DEF(xor,    2, INPUT_VALUE, SSAF_TRACE_INPUTS)

SSA_DEF(eq,     -1, INPUT_VALUE, SSAF_TRACE_INPUTS | SSAF_CLOBBERS_CARRY)
SSA_DEF(not_eq, -1, INPUT_VALUE, SSAF_TRACE_INPUTS | SSAF_CLOBBERS_CARRY)
SSA_DEF(lt,     -1, INPUT_VALUE, SSAF_TRACE_INPUTS | SSAF_CLOBBERS_CARRY)
SSA_DEF(lte,    -1, INPUT_VALUE, SSAF_TRACE_INPUTS | SSAF_CLOBBERS_CARRY)

SSA_DEF(jump,           0, INPUT_VALUE, 0)
SSA_DEF(branch_eq,     -1, INPUT_VALUE, SSAF_CLOBBERS_CARRY)
SSA_DEF(branch_not_eq, -1, INPUT_VALUE, SSAF_CLOBBERS_CARRY)
SSA_DEF(branch_lt,     -1, INPUT_VALUE, SSAF_CLOBBERS_CARRY)
SSA_DEF(branch_lte,    -1, INPUT_VALUE, SSAF_CLOBBERS_CARRY)

// With carry versions: TODO
// SSA_DEF(rolc, 2, SSAF_TRAC_INPUTS)
// SSA_DEF(rorc, 2, SSAF_TRAC_INPUTS)

#undef SSA_DEF
