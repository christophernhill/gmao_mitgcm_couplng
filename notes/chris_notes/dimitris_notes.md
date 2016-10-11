```
    testing find driver/ utils/ state/ -type f | awk '{print "cp "$1" ."}'
    in
    GEOSagcm/src/GEOSgcs_GridComp/GEOSgcm_GridComp/GEOSogcm_GridComp/GEOSocean_GridComp/GuestOcean_GridComp/mit/mitgcm_setup/code_split_driver
    cp driver/driver_init_mod.FOR .
cp driver/driver_run_mod.FOR .
cp driver/mitgcm_driver_mod.F90 .
cp utils/README.md .
cp utils/str4c.F90 .
cp state/README.md .
cp state/dynvars_h/dynvars_h_state_create_destroy_mod.F90 .
cp state/dynvars_h/dynvars_h_state_mod.F90 .
cp state/dynvars_h/dynvars_h_state_save_restore_mod.FOR .
cp state/dynvars_h/dynvars_h_state_types_mod.F90 .
cp state/export/export_state_create_destroy_mod.F90 .
cp state/export/export_state_fill_mod.FOR .
cp state/export/export_state_mod.F90 .
cp state/export/export_state_types_mod.F90 .
cp state/ffields_h/FFIELDS.h .
cp state/ffields_h/README .
cp state/ffields_h/ffields_h_state_mod.F90 .
cp state/import/import_state_create_destroy_mod.F90 .
cp state/import/import_state_fill_mod.FOR .
cp state/import/import_state_mod.F90 .
cp state/import/import_state_types_mod.F90 .
cp state/mitgcm_state/mitgcm_state_create_destroy_mod.F90 .
cp state/mitgcm_state/mitgcm_state_getdp_mod.F90 .
cp state/mitgcm_state/mitgcm_state_mod.F90 .
cp state/mitgcm_state/mitgcm_state_save_restore_mod.F90 .
cp state/mitgcm_state/mitgcm_state_types_mod.F90 .
cp state/stackvars/stackvars_state_mod.F90 .
cp state/timevars/timevars_state_create_destroy_mod.F90 .
cp state/timevars/timevars_state_mod.F90 .
cp state/timevars/timevars_state_save_restore_mod.FOR .
cp state/timevars/timevars_state_types_mod.F90 .

```
```
  testing adding build/ in mitgcm_setup and build/Makefile
  and adding inc/ and code/ in mitgcm_setup
```

```
 Using git on discover with tcsh
   258  16:00   ssh-agent -c
   259  16:00   setenv SSH_AUTH_SOCK /gpfsm/dnb32/tdirs/login/discover17.22204.cnhill1/ssh-d6sasX3B9ljk/agent.23149 ;
   260  16:00   setenv SSH_AGENT_PID 23150 ;
   261  16:00   echo Agent pid 23150 ;
   262  16:00   ssh-add ~/.ssh/id_rsa # ( or ssh-add ~/.ssh/id_rsa_toeofe )
   265  16:00   git clone git@github.com:christophernhill/gmao_mitgcm_couplng.git

```

```
The values for mitgcmIState%import%tx below are assigned in 
  GEOSgcs_GridComp/GEOSgcm_GridComp/GEOSogcm_GridComp/GEOSocean_GridComp/GuestOcean_GridComp/mit/mitgcm_setup/code_split_driver/driver_set_import_state.FOR
  
This is called from

  GEOSgcs_GridComp/GEOSgcm_GridComp/GEOSogcm_GridComp/GEOSocean_GridComp/GuestOcean_GridComp/mit/MIT_GEOS5PlugMod.F90
  
For an export the assignments need to go in
   GEOSgcs_GridComp/GEOSgcm_GridComp/GEOSogcm_GridComp/GEOSocean_GridComp/GuestOcean_GridComp/mit/mitgcm_setup/code_split_driver/driver_get_export_state.FOR
   
This is also called from

 GEOSgcs_GridComp/GEOSgcm_GridComp/GEOSogcm_GridComp/GEOSocean_GridComp/GuestOcean_GridComp/mit/MIT_GEOS5PlugMod.F90
 
 
The driver_set_import_state.FOR calls occur directly before calling the MITgcm run() step.
The driver_get_export_state.FOR calls occur directly after calling the MITgcm run() step.  

  
Aug 23, 2016
/discover/nobackup/cnhill1/test_006/forChrisHill/

reading taux from GEOS-5 in
GEOSgcs_GridComp/GEOSgcm_GridComp/GEOSogcm_GridComp/GEOSocean_GridComp/GuestOcean_GridComp/mit/mitgcm_setup/code_split_driver/driver_run_mod.FOR

      _RL, pointer                         ::   TAUX(:,:  )
      _RL, pointer                         ::   TAUY(:,:  )
      CHARACTER*1024  txFnam
      INTEGER dUnit
      INTEGER i0, j0

      TAUX => mitgcmIState%import%tx
      TAUY => mitgcmIState%import%ty

      WRITE(txFnam,'(A12,I6.6,A,I10.10,A5)') 'taux_import.',myRank,'.',
     &       myCurrentIter,'.data'
      CALL MDSFINDUNIT( dUnit, myThid )
      OPEN( dUnit, file=txFnam, status='UNKNOWN',
     &        form='UNFORMATTED')
      WRITE(dUnit) TAUX
      CLOSE(dUnit)

### in matlab

chrishill$ cat mkplot.m 
fpref='taux_import';
inum=72004;

figure(1)
clf
phics=zeros(32,32,6);

for i = 0:5
fn=sprintf('%s.%6.6d.%10.10d.data',fpref,i,inum);
fid=fopen(fn,'r','ieee-be');
i0=fread(fid,1,'int');
phi=fread(fid,[32 32],'float64');
i0=fread(fid,1,'int');
fclose(fid);
phics(:,:,i+1)=phi;
end
phimin=min(phics(:));
phimax=max(phics(:));
phistd=std(phics(:));
phiave=mean(phics(:));

plotphi=phics-cave;
cmin=-2*phistd;
cmax=2*phistd;
subplot(3,4,9 );imagesc(plotphi(:,:,1) );caxis([cmin cmax]);axis equal; axis square; axis tight
subplot(3,4,10);imagesc(plotphi(:,:,2) );caxis([cmin cmax]);axis equal; axis square; axis tight
subplot(3,4,6 );imagesc(plotphi(:,:,3) );caxis([cmin cmax]);axis equal; axis square; axis tight
subplot(3,4,7 );imagesc(plotphi(:,:,4)');caxis([cmin cmax]);axis equal; axis square; axis tight
subplot(3,4,3 );imagesc(plotphi(:,:,5)');caxis([cmin cmax]);axis equal; axis square; axis tight
subplot(3,4,4 );imagesc(plotphi(:,:,6)');caxis([cmin cmax]);axis equal; axis square; axis tight

```


tel - goddard user services, 301-286-9120 

```
March 12, 2015
cd /discover/nobackup/cnhill1/gmao_201503/forChrisHill/GEOSagcm/src/GEOSgcs_GridComp/GEOSgcm_GridComp/GEOSogcm_GridComp/GEOSocean_GridComp/GuestOcean_GridComp/mit/mitgcm_setup/

March 10, 2015 log

# set shell and working directory
tcsh
set WorkingDir=/discover/nobackup/dmenemen

# get GEOS5 and MITgcm code
cd $WorkingDir
tar xvf /discover/nobackup/dmenemen/forChrisHill.tar
cd forChrisHill
setenv CVSROOT ':pserver:cvsanon@mitgcm.org:/u/gcmpack'
cvs login
  ==> enter the CVS password: "cvsanon"
cvs co -D "01/31/07" MITgcm_code
cvs co -D "01/31/07" MITgcm/verification/testreport
cvs co -D "01/31/07" MITgcm/verification/tutorial_held_suarez_cs
cvs co -D "01/31/07" MITgcm/verification/global_ocean.cs32x15

# request compute node and set re-shell and working directory
xsub -I -l select=1,walltime=8:00:00
tcsh
set WorkingDir=/discover/nobackup/dmenemen
setenv ESMADIR $WorkingDir/forChrisHill/GEOSagcm
source $ESMADIR/src/g5_modules

# run MITgcm verification to make sure it compiles, links, and runs
cd $WorkingDir/forChrisHill/MITgcm
cp /discover/nobackup/dmenemen/mitgcm/MITgcm/tools/build_options/linux_amd64_ifort_discover .
module purge
module load comp/intel-13.1.2.183 other/mpi/mvapich2-1.8.1/intel-13.1.2.183
cd verification
./testreport -of ../linux_amd64_ifort_discover -t global_ocean.cs32x15

# test parallel MITgcm code
./testreport -of ../linux_amd64_ifort_discover -mpi -t global_ocean.cs32x15
cd global_ocean.cs32x15/build
mpirun -np 2 ./mitgcmuv

# link correct MITgcm directories to GEOS5
cd $ESMADIR/src/GEOSgcs_GridComp/GEOSgcm_GridComp
cd GEOSogcm_GridComp/GEOSocean_GridComp/GuestOcean_GridComp
\rm Guest_chris
ln -sf $WorkingDir/forChrisHill/MITgcm/eesupp .
ln -sf $WorkingDir/forChrisHill/MITgcm/model .
ln -sf $WorkingDir/forChrisHill/MITgcm/pkg .
ln -sf $WorkingDir/forChrisHill/MITgcm/tools .
cd mit/mitgcm_setup/build
make clean

# compile GEOS5 code
cd $ESMADIR/src
gmake realclean
gmake install

# a sign of success is that following executable was built:
# Applications/GEOSgcm_App/GEOSgcm.x

# run CEOS5
cd $ESMADIR/../Run/test200
ln -sf $ESMADIR/src/Applications/GEOSgcm_App/GEOSgcm.x .
cd $ESMADIR/../Run/test200/scratch/
mpirun -n 6 ./GEOSgcm.x >& output.txt

==============

# HISTORY.rc was modified to:
  test_ocn2d.template:  '%y4%m2%d2_%h2%n2z.nc4',
  test_ocn2d.archive:   '%c/Y%y4',
  test_ocn2d.format: 'CFIO',
  test_ocn2d.resolution: 180 91,
  test_ocn2d.frequency:  003000,
  test_ocn2d.fields: 'TAUX'  , 'SURFACE',

Then look at test200.test_ocn2d.19940201_0600z.nc4

# MITgcm output is under
cd $ESMADIR/../Run/test200/scratch/mitocean_run

module load other/octave
cd /discover/nobackup/dmenemen/forChrisHill/Run/test200/scratch
octave

NCCS web site: http://www.nccs.nasa.gov
message of the day: http://www.nccs.nasa.gov/motd_include.html

to use matlab:
ssh -X dmenemen@dali.nccs.nasa.gov
module load *matlab*

===========================================================
November 11, 2014 log

cd ~/store
tar cvf forChrisHill.tar /gpfsm/dnb31/atrayano/forChrisHill
tar xvf forChrisHill.tar
mv gpfsm/dnb31/atrayano/forChrisHill .
cd ~/store/forChrisHill
tcsh
setenv ESMADIR /discover/nobackup/dmenemen/forChrisHill/GEOSagcm
source $ESMADIR/src/g5_modules
cd $ESMADIR/src
gmake realclean
gmake install              (for normal executable)
gmake install BOPT=g       (for debug)

a sign of success is that following executable was built:
Applications/GEOSgcm_App/GEOSgcm.x

cd $ESMADIR/../Run/test200
ln -sf $ESMADIR/src/Applications/GEOSgcm_App/GEOSgcm.x .

xsub -I -l select=1,walltime=1:00:00 -q debug
xsub -I -l select=1,walltime=8:00:00 -q general

tcsh
setenv ESMADIR /discover/nobackup/dmenemen/forChrisHill/GEOSagcm
source $ESMADIR/src/g5_modules
cd $ESMADIR/../Run/test200/scratch/
mpirun -n 6 ./GEOSgcm.x >& output.txt


in scratch, the following files are important

CAP.rc       controls the main GEOS5 execution
this line:
JOB_SGMT: 00000000 030000
tells you how much simulation time the model will run.
in this case it is 3 hours
format is YYYYMMDD HHMMSS

NUM_SGMT: 1
this line can run multiple segments of 3 hours each

HEARTBEAT_DT: 1800
this is the time step of the model clock


AGCM.rc      controls GEOS5

MITgcm output is under
cd $ESMADIR/../Run/test200/scratch/mitocean_run


-> the MITgcm is compiled under:
cd $ESMADIR/src/GEOSgcs_GridComp/GEOSgcm_GridComp/GEOSogcm_GridComp/GEOSocean_GridComp/GuestOcean_GridComp
if you look in GNUmakefile ALLDIRS, it tells gmake which subdirectories to
compile first. In this case all of the names are nonexistent except "mit"

in "mit" GNUmakefile has:
ALLDIRS = mitgcm_setup/build \
          mitgcm_setup/code_split_driver

mit/MIT_GEOS5PlugMod.F90
is the wrapper for MITgcm

mit/mitgcm_setup/code_split_driver/driver_set_import_state.FOR
(MITgcm receives stuff from GEOS5)
mit/mitgcm_setup/code_split_driver/driver_get_export_state.FOR
(MITgcm sends stuff to GEOS5)
import and export variables from MITgcm, respectively

These need to be updated.

=========

showquota : shows disk quotas
usually number of files is problem
can move files to $ARCHIVE

=========

cap_restart contains starting time
CAP.rc contains how long it runs: JOB_SGMT

Buidling and running the code:
cd /home/dmenemen/store
mkdir Sep2_2014
cd Sep2_2014
cp -a /gpfsm/dnb31/atrayano/forChrisHill/GEOSagcm .
csh
setenv ESMADIR /home/dmenemen/store/Sep2_2014/GEOSagcm
source $ESMADIR/src/g5_modules
cd GEOSagcm/src
gmake install

####################
September 2, 2014 instructions from Atanas

To build:
=====
1) copy the source code (I recommend that you:
cd to /gpfsm/dnb31/atrayano/forChrisHill
and make a tarball from GEOSagcm and untar it in your area
This way you will preserve the symlinks
2) setenv ESMADIR to one level above src (i.e. xxx/GEOSagcm)
3) source $ESMADIR/src/g5_modules
4) gmake install

This will take a long time, but eventually it should build an executable
(originally in src/Applications/GEOSgcmApp/GEOSgcm.x). This file is also
copied by the build into $ESMADIR/Linux/bin/


(I left many debug flags in the two of Makefile(s) inside mitgcm_setup/build
and mitgcm_setup/code_split_driver

Feel free to adjust as needed.

The only CPU dependent part of the build is SIZE.h inside mitgcm_setup/inc

To run:
JOB_SGMNT
====
For the time being you can copy (again, making a tarball is better)
/gpfsm/dnb31/atrayano/forChrisHill/Run/test200/scratch

In there there a complete GEOS-5 setup + a directory mitocean_run for the
ocean files

place your executable (or better yet a symlink to your executable in the
scratch directory

Once you have all this, you just go to scratch and do

mpirun -n 6 ./GEOSgcm.x

There are many .rc files that control the run. Currently this setup is for 3
hours (controlled by
JOB_SGMT: inside CAP.rc)

Once you get the job running we should talk to help you with the setup you
would like to have.


#####################
July 2, 2014
Getting "01/31/07" MITgcm to compile on discover
cd /discover/nobackup/dmenemen
cd mitgcm
mkdir 010307
cd 013107
cp ../MITgcm/tools/build_options/linux_amd64_ifort_discover .
cvs co -D "01/31/07" MITgcm_code
cvs co -D "01/31/07" MITgcm/verification/testreport
cvs co -D "01/31/07" MITgcm/verification/tutorial_held_suarez_cs
cvs co -D "01/31/07" MITgcm/verification/global_ocean.cs32x15
cd MITgcm/verification/
module purge
module load comp/intel-13.1.2.183 other/mpi/mvapich2-1.8.1/intel-13.1.2.183
./testreport -of ../../linux_amd64_ifort_discover -t global_ocean.cs32x15

# test parallel code
# there seems to be a bug with mpi testing of testreport
# but mpi code compiles and runs fine in build, instead of run
./testreport -of ../../linux_amd64_ifort_discover -mpi -t global_ocean.cs32x15
cd global_ocean.cs32x15/build
mpirun -np 2 ./mitgcmuv

#############
# Building MITgcm library using "01/31/07" MITgcm_code

# Get copy of GEOSagcm from atrayano
cd /discover/nobackup/dmenemen
cp -r /discover/nobackup/atrayano/forChrisHill/GEOSagcm .
cd GEOSagcm/src/GEOSgcs_GridComp/GEOSgcm_GridComp
cd GEOSogcm_GridComp/GEOSocean_GridComp/GuestOcean_GridComp
ln -sf /discover/nobackup/dmenemen/mitgcm/013107/MITgcm/eesupp .
ln -sf /discover/nobackup/dmenemen/mitgcm/013107/MITgcm/model .
ln -sf /discover/nobackup/dmenemen/mitgcm/013107/MITgcm/pkg .
ln -sf /discover/nobackup/dmenemen/mitgcm/013107/MITgcm/tools .
cd mit/mitgcm_setup/build
module purge
module load comp/intel-13.1.2.183 other/mpi/mvapich2-1.8.1/intel-13.1.2.183

# modify Makefile to make it compatible with
# ~/mitgcm/010307/MITgcm/verification/global_ocean.cs32x15/build/Makefile
# in particular replace
# /home/cnh/usr/local/mpich-1.2.7p1/intel_9.1.040/include/mpif.h
# with mpif.h
# and adding the "clog_commset.h to opa_util.h" files to HEADERFILES
make clean
make lib

#############
# Build and run 12-cpu c32 test case
cd /discover/nobackup/dmenemen/mitgcm/013107/MITgcm/verification/global_ocean.cs32x15
mkdir bin
cd bin
cp ../code/* .
rm SIZE.h*
cp /discover/nobackup/dmenemen/GEOSagcm/src/GEOSgcs_GridComp/GEOSgcm_GridComp/GEOSogcm_GridComp/GEOSocean_GridComp/GuestOcean_GridComp/mit/mitgcm_setup/build/SIZE.h .
module purge
module load comp/intel-13.1.2.183 other/mpi/mvapich2-1.8.1/intel-13.1.2.183
../../../tools/genmake2 -mo ../code -mpi -of ../../../../linux_amd64_ifort_discover
make depend
make -j 16
cd ..
mkdir run
cd run
cp ../input/eedata .
cp ../input/data .
cp ../input/data.pkg .
cp ../input/data.gmredi .
cp ../input/lev* .
cp ../input/shi* .
cp ../input/tre* .
cp ../input/pic* .
cp ../input/bathy_Hmin50.bin .
cp ../../tutorial_held_suarez_cs/input/grid* .
cp ../bin/mitgcmuv .
mpirun -np 6 ./mitgcmuv


============

GEOSagcm was obtained from:
cp -r /discover/nobackup/atrayano/forChrisHill/GEOSagcm .

cd /discover/nobackup/dmenemen
cd GEOSagcm/src/GEOSgcs_GridComp/GEOSgcm_GridComp
cd GEOSogcm_GridComp/GEOSocean_GridComp/GuestOcean_GridComp
cd mit/mitgcm_setup/build

cd ../../old
cvs co -D "01/31/07" MITgcm_code
cvs co -D "01/31/07" MITgcm/verification/testreport
cvs co -D "01/31/07" MITgcm/verification/global_ocean.cs32x15
cd MITgcm/verification/
./testreport

cd /discover/nobackup/dmenemen
cd GEOSagcm/src/GEOSgcs_GridComp/GEOSgcm_GridComp
cd GEOSogcm_GridComp/GEOSocean_GridComp/GuestOcean_GridComp
cd mit/mitgcm_setup/build
cd ../../../

ln -sf mit/old/MITgcm/eesupp .
ln -sf mit/old/MITgcm/model .
ln -sf mit/old/MITgcm/pkg .

module purge
module load comp/intel-13.1.2.183 other/mpi/mvapich2-1.8.1/intel-13.1.2.183

============

cd /discover/nobackup/atrayano/forChrisHill
cd GEOSagcm/src/GEOSgcs_GridComp/GEOSgcm_GridComp
cd GEOSogcm_GridComp/GEOSocean_GridComp/GuestOcean_GridComp
cd mit/mitgcm_setup/build

Make sure that MITgcm can compile,
then send a working example of cs32 to Atanas.

============

February 28, 2014 email from Atanas
I am going slowly over things on my end, and I noticed that 2 important
subroutines for interfacing GEOS-5 and MIT ocean are stubbed:

DRIVER_SET_IMPORT_STATE
and
DRIVER_GET_IMPORT_STATE

Currently they life under:
mitgcm_setup/code_split_driver/driver_set_import_state.FOR
and
mitgcm_setup/code_split_driver/driver_get_export_state.FOR

We are going to need them to interface vars on our end with the vars on the
MIT end
Basically on our end I would have an array (or pointer) with dimensions (i,j),
or if you prefer, (i,face,j). In the first case i would vary from 1 to 6*IM,
in the second case, both i and j would vary from 1 to IM (or JM)
Some copies need to take place in these routines for all the variables listed
in MOM_GEOS5PlugMod.F90 (except possible atmospheric surface pressure PS), but
definitely:
TAUX, TAUY, SWHEAT, QFLX, HFLX, SFLX, and
US, VS, SS, MASK
Moreover, I would like to have the MASK available at Initialize (i.e. before I
go into Run), if possible.

None of this is very urgent, because I am in a process of modifying our code
to create cubed-sphere grid, setup proper transforms between our grid and MIT
grid, etc. This would take me at least most of today.

============

I managed to create an interactive job

cnhill1@discover17:~> qsub -V -I -l select=4,walltime=1:00:00
salloc: Pending job allocation 1701952
salloc: job 1701952 queued and waiting for resources
salloc: job 1701952 has been allocated resources
salloc: Granted job allocation 1701952
srun.slurm: cluster configuration lacks support for cpu binding
srun.slurm: Job step created
cnhill1@borg01v020:~> srun pwd
srun.slurm: cluster configuration lacks support for cpu binding
/gpfsm/dhome/cnhill1
/gpfsm/dhome/cnhill1
/gpfsm/dhome/cnhill1
/gpfsm/dhome/cnhill1
cnhill1@borg01v020:~> srun hostname
srun.slurm: cluster configuration lacks support for cpu binding
borg01v020
borg01v022
borg01v024
borg01v023
```
