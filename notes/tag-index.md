  * Creating custom Makefiles for experiments/*
  * Changes to MITgcm plug and export/import files that add
    sea ice variables.
  * Moving code and input from mitgcm_setup to experiments/exp01
    The will allow multiple configurations to be checked in.

- checkpoint_0_L (2017/09/01)
  * Lots of undocumented modifications for first clunky but
    working llc90 + c48 configuration includes thermodynamics
    sea ice but no runoff
  * Move modifications and verification to experiments/exp01
  * Check-in a c48/cs32 year-long set-up in experiments/exp02

- checkpoint_0_K (2016/11/21)
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

- checkpoint_0_J (2016/11/16)
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

- checkpoint_0_I (2016/11/15)
  * Have verified with matlab/get_import.m that *_import.* files
    are identical to MITgcm output files.
  * The export fields are now updated every time step, instead
    of only during initialization.
  * Transferring import fields to MITgcm variables and writing
    them to files for comparison with the *import* output files.
  * Replacing verification output. The output files changed
    when one-way coupling was enabled.
  * Adding figures in verification output for comparison.

- checkpoint_0_H (2016/11/10)
  * Adding remaining import and export fields (except sea-ice).
  * Adding matlab scripts for looking at MITgcm and GEOS-5 output.

- checkpoint_0_G (2016/11/09)
  * Export of 3-D mask, removal of 3d mask stub, and fix
    for null pointer issue in MIT_GEOS5PlugMod.F90
  * Changes to Makefile_gfortran to make it compatible
    with GMAO desktops.

- checkpoint_0_F (2016/11/07)
  * Updating to latest MITgcm checkpoint (checkpoint66a)

- checkpoint_0_E (2016/11/06)
  * Removing mitgcm_special_bits.tgz and adding files to git

- checkpoint_0_D (2016/11/05)
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

- checkpoint_0_C (2016/11/01)
  * Runs and builds repeatebly on lubuntu, discover and fedora.

- checkpoint_0_B (2016/10/12)
  * Merged some cleanup, still MITgcm 2007.
  * Changes to mesh GEOS-5 git
     (`git clone discover:/discover/swdev/adasilva/bridge/GEOSodas`)
     and MIT-GMAO coupling git
     (`git clone git@github.com:christophernhill/gmao_mitgcm_couplng.git`).
     See notes for details.

- checkpoint_0_A (2016/08/26)
  * Base code for MITgcm 2007 from discover.
