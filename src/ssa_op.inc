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

// (fn, (arg, locator)...)
SSA_DEF(fn_call, -1, INPUT_VALUE, SSAF_WRITE_GLOBALS | SSAF_CLOBBERS_CARRY)

// (link, locator)
SSA_DEF(read_global,  2, INPUT_LINK, 0)

SSA_DEF(write_array, 3, INPUT_VALUE, 0) // (array, index, value)
SSA_DEF(read_array, 2, INPUT_VALUE, 0)  // (array, index)
SSA_DEF(init_array, -1, INPUT_VALUE, 0)  // (array, elements...)

// SSA_DEF(entry_copy, -1, SSAF_COPY) // TODO
// SSA_DEF(exit_copy, -1, SSAF_COPY) // TODO
// SSA_DEF(copy, -1, SSAF_COPY) // TODO

 SSA_DEF(trace, -1, INPUT_VALUE, 0)
SSA_DEF(cast,   1, INPUT_VALUE, SSAF_TRACE_INPUTS)

SSA_DEF(add,    3, INPUT_CARRY, SSAF_TRACE_INPUTS | SSAF_CLOBBERS_CARRY)
SSA_DEF(sub,    3, INPUT_CARRY, SSAF_TRACE_INPUTS | SSAF_CLOBBERS_CARRY)

SSA_DEF(and,    2, INPUT_VALUE, SSAF_TRACE_INPUTS)
SSA_DEF(or,     2, INPUT_VALUE, SSAF_TRACE_INPUTS)
SSA_DEF(xor,    2, INPUT_VALUE, SSAF_TRACE_INPUTS)

SSA_DEF(eq,     2, INPUT_VALUE, SSAF_TRACE_INPUTS | SSAF_CLOBBERS_CARRY)
SSA_DEF(not_eq, 2, INPUT_VALUE, SSAF_TRACE_INPUTS | SSAF_CLOBBERS_CARRY)
SSA_DEF(lt,     2, INPUT_VALUE, SSAF_TRACE_INPUTS | SSAF_CLOBBERS_CARRY)
SSA_DEF(lte,    2, INPUT_VALUE, SSAF_TRACE_INPUTS | SSAF_CLOBBERS_CARRY)

// With carry versions: TODO
// SSA_DEF(rolc, 2, SSAF_TRAC_INPUTS)
// SSA_DEF(rorc, 2, SSAF_TRAC_INPUTS)

#undef SSA_DEF
