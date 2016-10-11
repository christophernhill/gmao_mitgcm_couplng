mitgcm 2016
```
export CVSROOT=':pserver:cvsanon@mitgcm.org:/u/gcmpack'
cd projects/
cd geos-5-coupling/
cd mitgcm-base/
mkdir 20160825_version
cvs co  MITgcm_code
cvs co MITgcm/verification/testreport
cvs co MITgcm/verification/tutorial_held_suarez_cs
cvs co MITgcm/verification/global_ocean.cs32x15
cd MITgcm/verification/
cp ../tools/build_options/linux_amd64_gfortran ..
./testreport -of ../linux_amd64_gfortran -t global_ocean.cs32x15

# with MPI
unsetenv FC
export MPI_INC_DIR=/usr/lib/openmpi/include
setenv MPI_INC_DIR /usr/lib/openmpi/include
./testreport -of ../linux_amd64_gfortran -mpi -t global_ocean.cs32x15 
```

