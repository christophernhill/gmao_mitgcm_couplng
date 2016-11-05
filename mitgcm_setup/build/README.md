Recipe for constructing Makefile

The recipe is obtained by comparing
$GMAO_MITGCM_COUPLNG/mitgcm_setup/build/Makefile to
$MITGCM_ROOT/verification/global_ocean.cs32x15/build/Makefile
in the current (checkpoint_0_D) set-up.
Eventually this needs to be done by a genmake script.

 * Use genmake2 (or run a verification experiment)
   to build a standalone MITgcm Makefile

 * Add "include g5_make_head.mk" at beggining of Makefile

 * Replace MITgcm root directory with $(MITGCM_ROOT) throughout the Makefile

 * Replace MPI include directory with $(MPI_INC_DIR) in INCLUDES and INCLUDEDIRS

 * Remove eeboot.* and eeboot_minimal.* throughout the Makefile

 * Add $(SPECIAL_FILES) to libmitgcmuv.a recipe

 * Add "include $(ESMADIR)/Config/ESMA_post.mk" at end of Makefile
 