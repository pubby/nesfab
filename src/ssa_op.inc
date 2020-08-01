SSA_DEF(pruned, 0, 0) // Keep as first!

SSA_DEF(fence, -1, 0)

SSA_DEF(if,     1, SSAF_EFFECTFUL)

SSA_DEF(return, 1, SSAF_EFFECTFUL)

SSA_DEF(phi,   -1, 0)

SSA_DEF(argument, 1, 0)
SSA_DEF(uninitialized, 0, 0) // for new vars

SSA_DEF(fn_call, -1, SSAF_EFFECTFUL | SSAF_CLOBBERS_CARRY)
// SSA_DEF(pure_call, -1, 0) // TODO
SSA_DEF(get_return, 2, 0) // Used by callers to implement multi-byte returns.

SSA_DEF(read_global,  2, SSAF_EFFECTFUL)
SSA_DEF(write_global, 3, SSAF_EFFECTFUL)

SSA_DEF(entry_copy, -1, SSAF_COPY) // TODO
SSA_DEF(exit_copy, -1, SSAF_COPY) // TODO
SSA_DEF(copy, -1, SSAF_COPY) // TODO

SSA_DEF(trace, -1, 0)
SSA_DEF(cast,   1, SSAF_TRACE_INPUTS)

SSA_DEF(add,    3, SSAF_TRACE_INPUTS | SSAF_CLOBBERS_CARRY)
SSA_DEF(sub,    3, SSAF_TRACE_INPUTS | SSAF_CLOBBERS_CARRY)

SSA_DEF(and,    2, SSAF_TRACE_INPUTS)
SSA_DEF(or,     2, SSAF_TRACE_INPUTS)
SSA_DEF(xor,    2, SSAF_TRACE_INPUTS)

SSA_DEF(eq,     2, SSAF_TRACE_INPUTS)
SSA_DEF(not_eq, 2, SSAF_TRACE_INPUTS)
SSA_DEF(lt,     2, SSAF_TRACE_INPUTS | SSAF_CLOBBERS_CARRY)
SSA_DEF(lte,    2, SSAF_TRACE_INPUTS | SSAF_CLOBBERS_CARRY)

// With carry versions: TODO
// SSA_DEF(rolc, 2, SSAF_TRAC_INPUTS)
// SSA_DEF(rorc, 2, SSAF_TRAC_INPUTS)

#undef SSA_DEF
