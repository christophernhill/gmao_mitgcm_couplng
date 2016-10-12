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
\rm -f mit
ln -s gmao_mitgcm_couplng mit
echo "Creating link \"mit/\" to \"gmao_mitgcm_couplng/\""
)

# unpack Makefile and inc/*h files 
# **** this needs to be cleaned up                         ****
# **** the files should be generated from a script somehow ****
(
cd mitgcm_setup
tar -xf mitgcm_special_bits.tar
echo "Unpacked mitgcm_special_bits.tar"
)

# copy custom g5_modules into place
# *** this is for VM work ***                       
# *** if GMAO/Atanas change g5_modules elsewhere this will break! ***
# ***
# *** I think this should be commited in Atanas' b_geos5mit branch,
# *** so I removed from script and added to (temporary) instructions in
# *** gmao_mitgcm_couplng/notes/dimitris_notes/Heracles-5_4onLubuntu.txt
# ***
#(
#if [ -d ../../../../../../../src/ ]; then
# cp modifications/g5_modules ../../../../../../../src/
#fi
#)
