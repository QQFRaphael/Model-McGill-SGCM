#!/usr/bin/sh

echo compiling getforce.f 
f77 -r8 getforce.f 
echo running getforce.f
a.out

rm -f a.out 
