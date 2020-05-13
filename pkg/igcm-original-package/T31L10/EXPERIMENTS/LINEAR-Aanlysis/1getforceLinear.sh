#!/usr/bin/sh

echo compiling getforceLinear.f
f77 -r8 getforceLinear.f
echo running getforceLinear.f
a.out

rm -f a.out 
