#!/bin/bash 
#
# Convert standard in F77 style .h file 'C' comments to F90 style '!' comments on standard out
#
# e.g.
# cat foo.h | ./prep_header_4f90.sh > foo_new.h

(
while read line ; do
 echo $line
done
) | sed s'/^[Cc]/!/'
