```
GEOSagcm/
 src/
  GEOSgcs_GridComp/
   GEOSgcm_GridComp/
    GEOSogcm_GridComp/
     GEOSocean_GridComp/
      GuestOcean_GridComp/
       mit/                 - MITgcm interface code is under here

mit/
 MIT_GEOS5PlugMod.F90   - driver for MITgcm code, stages imports for reading in MITgcm
                          extract exports.
                        - contains methods
                          o InitializeM() 
                            This calls the MITgcm DRIVER_INIT which carries out MITgcm
                            initialization.
                            The DRIVER_INIT step adds a "private state" pointer
                            back to the calling layer "internalStatePointer". This is 
                            stored for the MITgcm component.
                          o Run()
                          o Finalize()

mit/
 mitgcm_setup/
  code_split_driver/    - MITgcm routines to drive the ocean model and to exchange code
                          between MAPL space data structures and MITgcm internal 
                          data structures.

code_split_driver/
 Makefile                     - Controls build that works for fitting with GEOS5/MAPL
 driver_set_import_state.FOR  - Invoked from GEOS5/MAPL layer via MIT_GEOS5PlugMod.F90.
                                It is passed fields from GEOS5/MAPL data structure to add
                                to intermediate MITgcm "MITGCM_ISTATE" data structure.
 driver_get_export_state.FOR  - Invoked from GEOS5/MAPL layer via MIT_GEOS5PlugMod.F90.
                                It passes fields from intermediate MITgcm 
                                "MITGCM_ISTATE" data structure back to GEOS5/MAPL
                                to update export state.
 driver_init_mod.FOR          
 driver_run_mod.FOR
 dynvars_h_state_create_destroy_mod.F90
 dynvars_h_state_mod.F90
 dynvars_h_state_save_restore_mod.FOR
 dynvars_h_state_types_mod.F90
 eeboot.F
 eeboot_minimal.F
 export_state_create_destroy_mod.F90
 export_state_fill_mod.FOR
 export_state_mod.F90
 export_state_types_mod.F90
 ffields_h_state_mod.F90
 import_state_create_destroy_mod.F90
 import_state_fill_mod.FOR
 import_state_mod.F90
 import_state_types_mod.F90
 mitgcm_driver_mod.F90
 mitgcm_state_create_destroy_mod.F90
 mitgcm_state_getdp_mod.F90
 mitgcm_state_mod.F90
 mitgcm_state_save_restore_mod.F90
 mitgcm_state_types_mod.F90
 stackvars_state_mod.F90
 str4c.F90
 timevars_state_create_destroy_mod.F90
 timevars_state_mod.F90
 timevars_state_save_restore_mod.FOR
 timevars_state_types_mod.F90


code_split_driver/
 driver/                      - MITgcm init() and run() bits
 state/                       - MITgcm internal state storing and managing pieces
  dynvars_h/                  - Storing MITgcm state for swapping contexts as needed ( 
                                not active in current GEOS-5 work, used in PRM)
  export/                     - MITgcm intermediate data structures for interfacing
                                to GEOS5/MAPL "export get" functions.
  import/                     - MITgcm intermediate data structures for interfacing
                                to GEOS5/MAPL "import set" functions.
  mitgcm_state/               - Grouping of all MITgcm state sub-entities
                                (dynvars, timevars, ffields, stackvars, import, export )
                                into a single instantiable "internal state" 
                                entity, MITGCM_ISTATE.
  ffields_h/                  - MITgcm forcing fields intermediate data structures 
  stackvars/                  - MITgcm stack based vairable intermediate data structures
                                for storage.
 utils/                       - Any special utility programs
 
utils/
 str4c.F90                    - Code for transfering string characters to 8-bit integer
                                array, for passing to C functions without implicit arguments.

driver/
 mitgcm_driver_mod.F90        - Code for collecting driver functions into a module
 driver_init_mod.FOR          - Interface to core MITgcm code for initialization
 driver_run_mod.FOR           - Interface to core MITgcm code for run
```
