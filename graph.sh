#!/bin/bash
dot graphs/foo_ssa.gv -T svg -o foo_ssa.svg
dot graphs/bar_ssa.gv -T svg -o bar_ssa.svg
dot graphs/foo_cfg.gv -T svg -o foo_cfg.svg
