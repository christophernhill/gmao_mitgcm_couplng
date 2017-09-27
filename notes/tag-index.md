  * All MITgcm input files are expected to be in $EXPDIR/input_mit,
    which is populated separately.
  * Created 48_llc90_02, a c48/llc90, 1-day experiment with ECCOv4r2 set-up

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
