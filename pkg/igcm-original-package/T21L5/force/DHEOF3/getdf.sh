#!/usr/bin/sh

echo compiling getdf.f
f77 -r8 getdf.f
echo running getdf.f
a.out

rm -f a.out 
