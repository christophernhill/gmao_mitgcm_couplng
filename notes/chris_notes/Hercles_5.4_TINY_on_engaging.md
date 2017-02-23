
```
srun -N 1 -n 16 --constraint=centos7 -p sched_mit_hill --pty /bin/bash
```

```
tcsh

module use /opt/ohpc/pub/modulefiles
module use /opt/ohpc/pub/moduledeps
module use /nfs/cnhlab002/cnh/software/lapack/modulefiles
module use /nfs/cnhlab002/cnh/software/blas/modulefiles
module load gnu/5.4.0
module load gnu/openmpi/1.10.4
module load lapack/default
module load blas/default
module load engaging/python/2.7.8

setenv MYROOT /nfs/cnhlab002/cnh
setenv MYDIR  geos5_2017022
cd ${MYROOT}
mkdir ${MYDIR}
cd ${MYROOT}/${MYDIR}
setenv WorkingDir `pwd`
setenv SITE GenericLinux

\rm ~/geos5
ln -s `pwd` ~/geos5
mkdir tarballs
cd tarballs
\rm ~/tarballs
ln -s `pwd` ~/tarballs
curl http://engaging-web.mit.edu/~cnh/geos5_coupling/tarballs/GMAO-Baselibs-4_0_7.withNewCDO.tgz > GMAO-Baselibs-4_0_7.withNewCDO.tgz
curl http://engaging-web.mit.edu/~cnh/geos5_coupling/tarballs/mitocean_run.tgz > mitocean_run.tgz
curl http://engaging-web.mit.edu/~cnh/geos5_coupling/tarballs/geos5-tiny.tgz > geos5-tiny.tgz
cd ~/geos5
tar -xzvf ~/tarballs/GMAO-Baselibs-4_0_7.withNewCDO.tgz
cd GMAO-Baselibs-4_0_7/src
make -j 4 install ESMF_COMM=openmpi | & tee makeinstall.log

cd $WorkingDir
git clone http://engaging-web.mit.edu/~cnh/geos5_coupling/geos5mit_write/GEOSodas.git
cd GEOSodas
git checkout b_geos5mit
cd $WorkingDir/GEOSodas/src/GEOSgcs_GridComp/GEOSgcm_GridComp
cd GEOSogcm_GridComp/GEOSocean_GridComp/GuestOcean_GridComp
git clone https://github.com/christophernhill/gmao_mitgcm_couplng
# user, password or key needed
cd gmao_mitgcm_couplng
git checkout 479072938b48c87e54ee6c6f19678a8fa75beea2

cd $WorkingDir
setenv CVSROOT :pserver:cvsanon@mitgcm.org:/u/gcmpack
# curl http://engaging-web.mit.edu/~cnh/geos5_coupling/.cvspass > .cvspass
# cvs login
cvs co -r checkpoint66a MITgcm_code
cvs co -r checkpoint66a MITgcm/verification/testreport
cvs co -r checkpoint66a MITgcm/verification/tutorial_held_suarez_cs
cvs co -r checkpoint66a MITgcm/verification/global_ocean.cs32x15

cd $WorkingDir/GEOSodas/src
source g5_modules
cd $GMAO_MITGCM_COUPLNG
./set_mitgcm_env.sh
setenv MPI_INC_DIR ${MPI_DIR}/include
setenv MPI_INCLUDE ${MPI_DIR}/include
cd $WorkingDir/MITgcm/verification

./testreport -mpi -j 4 -t global_ocean.cs32x15

cd $GMAO_MITGCM_COUPLNG/mitgcm_setup/build
ln -sf Makefile_gfortran Makefile
cd $WorkingDir/GEOSodas/src
mkdir $WorkingDir/include
\rm $WorkingDir/include/mpi
ln -s ${MPI_INCLUDE} $WorkingDir/include/mpi
setenv LIBRARY_PATH ${LD_LIBRARY_PATH}
gmake pinstall -j 4 | & tee makeinstall.log

cd $WorkingDir/GEOSodas/src/Applications/GEOSgcm_App
./gcm_setup
cd $WorkingDir
tar -xvf ~/tarballs/geos5-tiny.tgz
cd $WorkingDir/TEST
$GMAO_MITGCM_COUPLNG/experiments/exp01/makeoneday.bash TINY
cp $GMAO_MITGCM_COUPLNG/experiments/exp01/HISTORY.rc .
cd $WorkingDir/TEST
setenv GFORTRAN_CONVERT_UNIT 'little_endian:20-25'
./gcm_run.j |&  tee gcm_run.out

```
