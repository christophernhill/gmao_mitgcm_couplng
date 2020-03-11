- checkpoint_1_A (2020/03/1, compatible with MITgcm checkpoint67p)
  * update tags for the verification experiments
  * Add seaice masking code
  * Fix seaice stress problem
  * update verification exp. c12_cs32_01 
  * update and activate seaice in verification exp. c48_llc90_02
  
- checkpoint_0_Z (2019/11/06, compatible with MITgcm checkpoint67i)
  * update tags for the verification experiments
  * Add new c1440_llc2160_01 experiment
  * Add new c720_llc1080_02 experiment
  * update exp. c12_cs32_01 code with latest MITgcm changes (post c67g)
    including PR #193 (to fix missing update of sIceLoad)
  * Experiment c90_llc90_05 for HiRes was added

- checkpoint_0_Y (2018/12/07, compatible with MITgcm checkpoint67f)
  * Added instructions CheckpointTest.txt in experiments/c12_cs32_01
    and c12_cs32_03 for running the "1+1=2" restart test
  * Added sea ice to c90_llc90_01 & reduced integration period to 1hr
  * Updated c90_llc90_01 & c90_llc90_03 to MITgcm to checkpoint67f

- checkpoint_0_X (2018/11/05, compatible with MITgcm checkpoint67e)
  * New experiment with updated moist - c90_llc90_02a
  * New HiRes experiment - c2880_llc4320_01
  * Experiments c90_llc90_03 and c90_llc90_04 added
    as stepping stones towards c2880_llc4320_01
  * New experiment with sea-ice advection - c90_llc90_02
  * Added new experiment with sea ice energy advected - c48_llc90_07
  * Added new experiment with sea ice advected - c48_llc90_06
  * Cleaning up and consolidating instructions
    - Replace $WorkingDir with $ESMADIR/..
      and add MITGCM_ROOTDIR and GMAO_MITGCM_COUPLNG
      definitions to NCCS and NAS sites in g5_modules
    - Modified experiments/c12_cs32_01 to test pLoad issue
    - Removing duplicate environment variable MITGCM_ROOT
      and replacing with MITGCM_ROOTDIR
    - g5_modules now handles default OPENMPI for *buntu 16.04 and
      defines MITGCM_ROOTDIR and MPI_INC_DIR for site=GenericLinux
    - Replaced "&end" and "/ with "&" in namelists
    - Set "debugLevel=2" in data and "#debugMode=.TRUE.," in eedata
    - Replaced "\rm" with "rm"
    - Adding spaces before commands for easier cut&paste
    - Replaced deprecated ROOTDIR with MITGCM_ROOTDIR in instructions
    - Moved experiments/*/verification to experiments/*/results
    - experiments/c12_cs32_0*/results/STDOUT.0000 and gcm_run.out
      renamed STDOUT.0000_gfortran and gcm_run.out_gfortran
      (these are for comparison with Linux gfortran tests)
    - notes/build_GMAO-Baselibs.txt contains
      instructions for building GMAO Baselibs
    - notes/get_GEOS-MITgcm.txt contains
      instructions for getting the GEOS-MITgcm code

- checkpoint_0_W (2018/06/21, compatible with MITgcm checkpoint67b)
  * Separating and consolidating instructions
    - notes/dimitris_notes contains
      instructions for building VirtualBox environments
    - notes/Build_GMAO_Baselibs.txt contains
      instructions for building GMAO Baselibs
  * add to imports grid-cell ocean fraction from GEOS (WGHT) and
    propagate it to MITgcm
  * Designating experiments/c12_cs32_01 and c48_llc90_02
    as "verification experiments".
  * Added new c720_llc1080_Restart experiment - after 5 days passive ocean
  * Added new c720_llc1080_Debug experiment - runs for 5 time steps
  * Added new c720_llc1080_01 experiment - setup for relaxation with passive ocean

- March 2018
  * get MITgcm code from GitHub
  * allow 3-D fields to have different 3rd dim in MITgcm State
  * get seaice fields imported from GEOS to the Plug and then to MITgcm

- checkpoint_0_V (2017/10/25, compatible with MITgcm checkpoint66l)
  * Add pkg/ctrl at compile and run time
  * Created c90_llc90_02, a c90/llc90, 1-day experiment

- checkpoint_0_U (2017/10/15, compatible with MITgcm -D "10/15/17")
  * Adding Pleiades instructions for experiments/c48_llc90_02

- checkpoint_0_T (2017/10/14, compatible with MITgcm -D "10/14/17")
  * Code used for first attempt to run c2880/llc4320 on Electra
  * Adding experiments/c2880_llc4320_01
  * Mods related to regridding at hi-res and to llc4320 set-up
  * Adding experiments/c12_cs32_02 in order to test:
    OGCM_DT=25, AGCM_DT=90, Coupling_DT=900

- checkpoint_0_S (2017/10/11, compatible with MITgcm -D "10/11/17")
  * Switching to comp-intel/2017 and mpt.2.15r20
  * Replaced g5_modules with that of
    Icarus-3_0_UNSTABLE_FV_R8-NAS_C2880-Intel17
  * Added NASYNC in CAP.rc, which controls number of nodes used by asyncio
  * Write asyncio files to scratch/mitocean_run
  * Added coe to automatically set CoresPerNode in asyncio

- checkpoint_0_R (2017/10/09, compatible with MITgcm -D "10/09/17")
  * Created experiments/c48_llc90_05 to test GEOSodas b_geos5mit-i30
    inclusing GEOS USE_IOSERVER and MITgcm asyncio
  * Created experiments/c48_llc90_04 to test GEOSodas b_geos5mit-i30

- checkpoint_0_Q (2017/10/07, compatible with MITgcm -D "10/07/17")
  * Moved gcm_run_restart.j to gcm_run.j
  * All MITgcm input files are now expected to be in
    $EXPDIR/mit_input instead $EXPDIR/input_mit

- checkpoint_0_P (2017/10/01, compatible with MITgcm -D "10/01/17")
  * Added instructions and verification output for pleiades
  * Added new gcm_run_restart.j to the GEOS5 repository with restart capability
  * Created 48_llc90_03, a c48/llc90, 1-day experiment with
    llc4320-like set-up
  * All MITgcm input files are expected to be in $EXPDIR/input_mit,
    which is populated separately.
  * Created 48_llc90_02, a c48/llc90, 1-day experiment with
    ECCOv4r2-like set-up

- checkpoint_0_O (2017/09/21, compatible with MITgcm checkpoint66k)
  * Created c48_llc90_01, a c48/llc90, 1-day experiment
    with simplified llc90 set-up for testing.

- checkpoint_0_N (2017/09/20, compatible with MITgcm checkpoint66k)
  * update MITgcm code from checkpoint66a to latest:
    This changes results at truncation level (due to ordering of
    sum in ini_masks_etc.F);
  * semi-automatic generation of MITgcm makefile in 2 steps:
    a) run genmake2 + "make depend" in experiments/*/build using local code dir
    b) generate modified Makefile in mitgcm_setup/build by using (new) local 
       script: "mk_local"
  * generate config-specific version of header files (*.h) in mitgcm_setup/inc
    by using (new) local script: "mk_local"
  * for discover, switch to new optfile: tools/build_options/linux_amd64_ifort+gcc

- checkpoint_0_M (2017/09/17, compatible with MITgcm checkpoint66a)
  * Moving experiments/exp01 to experiments/c12_cs32_01
  * Explicitly require CC=ESMA_CC=mpicc (instead of gcc)
  * Creating custom Makefiles for experiments/*
  * Changes to MITgcm plug and export/import files that add
    sea ice variables.
  * Moving code and input from mitgcm_setup to experiments/exp01
    The will allow multiple configurations to be checked in.

- checkpoint_0_L (2017/09/01, compatible with MITgcm checkpoint66a)
  (to run gmao_mitgcm_couplng/experiments/exp01, need to manually
   update mitgcm_setup/code/SIZE.h and TEST/AGCM.rc)
  * Lots of undocumented modifications for first clunky but
    working llc90 + c48 configuration includes thermodynamics
    sea ice but no runoff
  * Move modifications and verification to experiments/exp01
  * Check-in a c48/cs32 year-long set-up in experiments/exp02

- checkpoint_0_K (2016/11/21, compatible with MITgcm checkpoint66a)
  * Matlab script matlab/view_output.m to compare
    GEOS-5 *.nc4 and MITgcm pkg/diagnostics output files.
  * New verification output.
  * Checking and documenting units consistency for
    import/export variables.
  * Rotate/interpolate MITgcm uVel and vVel to A-grid and
    geographical North/East directions.
  * Rotate/interpolate GEOS-5 TAUX and TAUY to MITgcm grid.
  * MITgcm detlaT = GEOS-5 HEARTBEAT_DT = 900 s
  * Removing WRITE_FLD_XY_RLs of FU, FV, EMPMR, QNET, QSW,
    and SALTFLUX from driver_run_mod.FOR, since they can
    be obtained from pkg/diagnostics oceTAUX, oceTAUY,
    -oceFWflx, -oceQnet, -oceQsw, and -oceSflux.
    (Note sign reversal of last 4 fields.)

- checkpoint_0_J (2016/11/16, compatible with MITgcm checkpoint66a)
  * Modified input/data.pkg and data.diagnostics so that
    diagnostic output fluxes (oceTAUX, oceTAUY, oceFWflx,
    oceQnet, oceQsw, oceSflux) can be directly compared
    with MITgcm debug outputs of these same variable
    (FU, FV, EMPMR, QNET, QSW, SALTFLUX).
    Matlab script matlab/get_MITgcm.m does this comparison
    and shows that they are bit-identical, which in turn
    are bit-identical to the *import* output fields of
    previous checkpoint.
  * Modified mitgcm_setup/input/data so that fluxes are
    obtained from GEOS-5 instead of read-in. The model
    is now officially 2-way coupled! But there is still
    some interpolation, rotation, and units issues to be
    sorted out.
  * Removed *import* debug output files and matlab scripts to
    look at them. They are replaced by MITgcm output files.
  * Moved driver_get* and driver_set* to subdirectory driver.
  * Removed mk_src_links. Links are now created by Makefile.

- checkpoint_0_I (2016/11/15, compatible with MITgcm checkpoint66a)
  * Have verified with matlab/get_import.m that *_import.* files
    are identical to MITgcm output files.
  * The export fields are now updated every time step, instead
    of only during initialization.
  * Transferring import fields to MITgcm variables and writing
    them to files for comparison with the *import* output files.
  * Replacing verification output. The output files changed
    when one-way coupling was enabled.
  * Adding figures in verification output for comparison.

- checkpoint_0_H (2016/11/10, compatible with MITgcm checkpoint66a)
  * Adding remaining import and export fields (except sea-ice).
  * Adding matlab scripts for looking at MITgcm and GEOS-5 output.

- checkpoint_0_G (2016/11/09, compatible with MITgcm checkpoint66a)
  * Export of 3-D mask, removal of 3d mask stub, and fix
    for null pointer issue in MIT_GEOS5PlugMod.F90
  * Changes to Makefile_gfortran to make it compatible
    with GMAO desktops.

- checkpoint_0_F (2016/11/07, compatible with MITgcm checkpoint66a)
  * Updating to latest MITgcm checkpoint (checkpoint66a)

- checkpoint_0_E (2016/11/06, compatible with MITgcm -D "01/31/07")
  * Removing mitgcm_special_bits.tgz and adding files to git

- checkpoint_0_D (2016/11/05, compatible with MITgcm -D "01/31/07")
  * Adding recipe for generating custom Makefile
  * Remove links to MITgcm code in GuestOcean_GridComp
  * Remove link to mpif.h from mitgcm_setup/build
  * Remove mitocean_run.tgz
     Boundary condition files are now linked from $MITGCM_ROOT/verification
     and runtime parameter files from $GMAO_MITGCM_COUPLNG/mitgcm_setup/input
  * Remove DESKTOP_SESSION check for setting "site" in g5_modules.
     It is now set by checking "uname -n" or environment variable "SITE".
  * Reduce optimization from O3 to O1 for GEOS_MoistGridComp.F90
     because compilation was failing for gfortran/gcc 6.2.1
  * Adding GenericLinux site to g5_modules
  * Adding instructions for compiling and running on Fedora 24 Workstation.

- checkpoint_0_C (2016/11/01, compatible with MITgcm -D "01/31/07")
  * Runs and builds repeatebly on lubuntu, discover and fedora.

- checkpoint_0_B (2016/10/12, compatible with MITgcm -D "01/31/07")
  * Merged some cleanup, still MITgcm 2007.
  * Changes to mesh GEOS-5 git
     (`git clone discover:/discover/swdev/adasilva/bridge/GEOSodas`)
     and MIT-GMAO coupling git
     (`git clone git@github.com:christophernhill/gmao_mitgcm_couplng.git`).
     See notes for details.

- checkpoint_0_A (2016/08/26, compatible with MITgcm -D "01/31/07")
  * Base code for MITgcm 2007 from discover.
