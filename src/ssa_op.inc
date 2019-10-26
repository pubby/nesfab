SSA_DEF(pruned, 0, 0) // Keep as first!

SSA_DEF(fence, -1, 0)

SSA_DEF(if,     1, 0)

SSA_DEF(return, 1, 0)

SSA_DEF(phi,   -1, 0)

SSA_DEF(argument, 1, 0)
SSA_DEF(uninitialized, 0, 0) // for new vars

SSA_DEF(fn_call, -1, 0)

// SSA_DEF(read, 0)
// SSA_DEF(write, 0)

SSA_DEF(trace, -1, 0)
SSA_DEF(cast,   1, SSAF_TRACE_INPUTS)

SSA_DEF(add,    2, SSAF_TRACE_INPUTS)
SSA_DEF(sub,    2, SSAF_TRACE_INPUTS)

SSA_DEF(and,    2, SSAF_TRACE_INPUTS)
SSA_DEF(or,     2, SSAF_TRACE_INPUTS)
SSA_DEF(xor,    2, SSAF_TRACE_INPUTS)

SSA_DEF(eq,     2, SSAF_TRACE_INPUTS)
SSA_DEF(not_eq, 2, SSAF_TRACE_INPUTS)
SSA_DEF(lt,     2, SSAF_TRACE_INPUTS)
SSA_DEF(lte,    2, SSAF_TRACE_INPUTS)

#undef SSA_DEF
