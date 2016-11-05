  *  Remove links to MITgcm code in GuestOcean_GridComp
  *  Remove link to mpif.h from mitgcm_setup/build
  * Remove mitocean_run.tgz
     Boundary condition files are now linked from $MITGCM_ROOT/verification
     and runtime parameter files from $GMAO_MITGCM_COUPLNG/mitgcm_setup/input
  * Remove DESKTOP_SESSION check for setting "site" in g5_modules.
     Instead it is now set by checking "uname -n" or environment variable "SITE".
  * Reduce optimization from O3 to O1 for GEOS_MoistGridComp.F90
     because compilation was failing for gfortran/gcc 6.2.1.
  * Adding GenericLinux site to g5_modules.
  * Adding instructions for compiling and running on Fedora 24 Workstation.

- checkpoint_0_C (2016/11/01)
  * runs and builds repeatebly on lubuntu, discover and fedora

- checkpoint_0_B (2016/10/12)
  * merged some cleanup, still MITgcm 2007
  * changes to mesh GEOS-5 git (`git clone discover:/discover/swdev/adasilva/bridge/GEOSodas`) and 
     MIT-GMAO coupling git (`git clone git@github.com:christophernhill/gmao_mitgcm_couplng.git`). See
     notes for details.

- checkpoint_0_A (2016/08/26)
  * base code for MITgcm 2007 from discover
