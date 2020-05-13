#!/usr/bin/sh

echo compiling getdf.f
f77 -r8 getdh.f
echo running getdh.f
a.out

rm -f a.out 
