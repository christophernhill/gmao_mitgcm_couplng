AWS ami
```
ubuntu/images/hvm-ssd/ubuntu-xenial-16.04-amd64-server-20160815 (ami-c60b90d1)
c4.8xlarge
```

login
```
ssh -i ~/.ssh/XXXMYKEYXXX -l ubuntu IPADDRESS
```

setup
```
apt install gcc
apt install gfortran-5
apt install g++
apt-get install gfortran
apt-get install openmpi-'*'
apt-get install libopenmpi-'*'
apt install cvs
apt install vncserver
apt install xutils-dev
apt install make
apt install bc
apt install csh
apt install tcsh
```

mitgcm 2007
```
export CVSROOT=':pserver:cvsanon@mitgcm.org:/u/gcmpack'
cvs login
cd projects/
cd geos-5-coupling/
cd mitgcm-base/
mkdir 20073101_version
cd 20073101_version/
cvs co -D "01/31/07" MITgcm_code
cvs co -D "01/31/07" MITgcm/verification/testreport
cvs co -D "01/31/07" MITgcm/verification/tutorial_held_suarez_cs
cvs co -D "01/31/07" MITgcm/verification/global_ocean.cs32x15
cd MITgcm/
cd verification/
cp ../tools/build_options/linux_amd64_gfortran ..
./testreport -of ../linux_amd64_gfortran -mpi -t global_ocean.cs32x15
```

mitgcm 2016
```
export CVSROOT=':pserver:cvsanon@mitgcm.org:/u/gcmpack'
cd projects/
cd geos-5-coupling/
cd mitgcm-base/
mkdir 20160824_version
cvs co  MITgcm_code
cvs co MITgcm/verification/testreport
cvs co MITgcm/verification/tutorial_held_suarez_cs
cvs co MITgcm/verification/global_ocean.cs32x15
cd MITgcm/verification/
cp ../tools/build_options/linux_amd64_gfortran ..
./testreport -of ../linux_amd64_gfortran -t global_ocean.cs32x15

# with MPI
export MPI_INC_DIR=/usr/lib/openmpi/include
./testreport -of ../linux_amd64_gfortran -mpi -t global_ocean.cs32x15 
```

Some possible commands for gfortran-6
```
add-apt-repository ppa:ubuntu-toolchain-r/test
apt-get update
apt-get install gfortran-6
```

