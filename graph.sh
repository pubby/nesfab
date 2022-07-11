#!/bin/bash
for f in graphs/*.gv; 
do 
    echo $f
    dot $f -T svg -o ${f%%.*}.svg
done
