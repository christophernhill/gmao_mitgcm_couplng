#!/bin/bash
if [ "x${MITGCM_ROOT}" == "x" ]; then
 echo 'ERROR: Environment variable ${MITGCM_ROOT} not set'
 exit
fi

# unpack Makefile and inc/*h files 
# **** this needs to be cleaned up                         ****
# **** the files should be generated from a script somehow ****
(
cd mitgcm_setup
tar -xf mitgcm_special_bits.tgz
echo "Unpacked mitgcm_special_bits.tgz"
)

echo "Linked code from subdirectories to code_split_driver"
cd $GMAO_MITGCM_COUPLNG/mitgcm_setup/code_split_driver
ln -sf driver/*.F* .
ln -sf state/dynvars_h/* .
ln -sf state/export/* .
ln -sf state/ffields_h/*.F90 .
ln -sf state/ffields_h/*.h .
ln -sf state/import/* .
ln -sf state/mitgcm_state/* .
ln -sf state/stackvars/* .
ln -sf state/timevars/* .
ln -sf utils/*.F90 .
