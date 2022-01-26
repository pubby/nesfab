#!/bin/bash
for f in graphs/*.gv; 
do 
    dot $f -T svg -o ${f%%.*}.svg
done
