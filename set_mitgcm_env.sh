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
