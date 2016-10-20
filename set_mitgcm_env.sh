#!/bin/bash
if [ "x${MITGCM_ROOT}" == "x" ]; then
 echo 'ERROR: Environment variable ${MITGCM_ROOT} not set'
 exit
fi

(
cd ..
for d in model eesupp tools pkg ; do
 \rm -f ${d}
 ln -s ${MITGCM_ROOT}/${d} ${d}
 echo "Creating link \"${d}/\" to \"${MITGCM_ROOT}/${d}/\""
done
)

# unpack Makefile and inc/*h files 
# **** this needs to be cleaned up                         ****
# **** the files should be generated from a script somehow ****
(
cd mitgcm_setup
tar -xf mitgcm_special_bits.tgz
echo "Unpacked mitgcm_special_bits.tgz"
)

echo "Applied Atanas' changes to MITgcm"
cd $GMAO_MITGCM_COUPLNG/modifications
cp timers.F $WorkingDir/MITgcm/eesupp/src
cd ../mitgcm_setup/build
ln -sf /usr/lib/openmpi/include/mpif.h .

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
