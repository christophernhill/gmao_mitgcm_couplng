* ../code_split_driver/eeboot.F
   - current `eeboot.F` has small by hand modifications wrt to 2007 sources

* ../code_split_driver/eeboot.F -> ../code_split_driver/eeboot_minimal.F
   - current `eeboot_minimal.F` has small by hand modifications wrt to 2007 sources

* code_split_driver/driver_init_mod.FOR -> ../code_split_driver/eeboot.F

* code_split_driver/driver_init_mod.FOR -> initialise_fixed.F
  - `initialise_fixed.F` uses original 2007 sources

* code_split_driver/driver_init_mod.FOR -> initialise_varia.F
  - `initialise_varia.F` uses original 2007 sources

* code_split_driver/driver_run_mod.FOR -> forward_step.F
  - `forward_step.F` uses original 2007 sources
  
* `CPP_EEMACROS.h`
  - change comment `C` to `!`
  
* `CPP_EEOPTIONS.h`
  - change comment `C` to `!`

* `CPP_OPTIONS.h`
  - change comment `C` to `!`

* `DYNVARS.h`
  - change comment `C` to `!`

* `SIZE.h`
  - change comment `C` to `!`
