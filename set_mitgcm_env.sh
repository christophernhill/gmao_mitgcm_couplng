#!/bin/bash
if [ "x${MITGCM_ROOT}" == "x" ]; then
 echo 'ERROR: Environment variable ${MITGCM_ROOT} not set'
 exit
fi

# Move Fortran files from subdirectories in mitgcm_setup/code_split_driver
# to a flat set of files in the code_split_driver directory.
# Original instructions are from:
# https://github.com/christophernhill/gmao_mitgcm_couplng/blob/master/chris_notes/dimitris_notes.md
# using: find driver/ utils/ state/ -type f | awk '{print "cp "$1" ."}'
echo "Linked code from subdirectories to code_split_driver"
cd $GMAO_MITGCM_COUPLNG/mitgcm_setup/code_split_driver
./mk_src_links

