!  $Id: MIT_GEOS5PlugMod.F90,v 1.3 2013-09-18 20:41:52 atrayano Exp $

#include "MAPL_Generic.h"

! GEOS-5 default real kind

#define G5KIND      4
#define REAL_       real(kind=G5KIND)

#ifndef _RL
#define _RL Real*8
#endif

module MIT_GEOS5PlugMod

! A  MAPL/ESMF Gridded Component tha acts as a wrapper for MOM.
! It uses ESMF AND MAPL. It has heavy dependencies on FMS and MOM.
! This should be built like MOM, so that its default reals
! are the same as MOM's. It may also be an adequate plug for HIM.

  use ESMF
  use MAPL_Mod

  USE MITGCM_STATE_MOD , ONLY :   &
       MITGCM_ISTATE_CONTAINER,       &
       MITGCM_ISTATE,                 &
       MITGCM_ISTATE_WRAP_TYPE,       &
       GETDP

  USE MITGCM_DRIVER_MOD , ONLY :  &
       DRIVER_INIT,                   &
       DRIVER_RUN

  USE STR4C_MOD
  USE DRIVER_SET_IMPORT_STATE_MOD
  USE DRIVER_GET_EXPORT_STATE_MOD


! Nothing on the MOM side is visible through this module.

  implicit none
  private

! Only public things are the IRF routines. Note that they don't use the standard
! ESMF interface for ESMF IRF methods.

  public :: SetServices

  type :: T_PrivateState
!     type(MAPL_LocStream) :: LocStream_O
!     type(MAPL_LocStream) :: LocStream_P     
!     type(MAPL_LocStreamXform) :: XFORM_P2O
!     type(MAPL_LocStreamXform) :: XFORM_O2P
!     integer                   :: ntO
!     integer                   :: ntP
!     logical                   :: initialized=.false.
!     logical                   :: transformNeeded
     type(MITGCM_ISTATE),  pointer :: ptr
  end type T_PrivateState

  type :: T_PrivateState_Wrap
     type(T_PrivateState), pointer :: ptr
  end type T_PrivateState_Wrap

  type(T_PrivateState), pointer :: privateState

contains


!BOP

! !IROUTINE: SetServices -- Sets ESMF services for this component

! !INTERFACE:

    subroutine SetServices ( GC, RC )

! !ARGUMENTS:

    type(ESMF_GridComp), intent(INOUT) :: GC  ! gridded component
    integer, optional                  :: RC  ! return code

! !DESCRIPTION:  The SetServices for the PhysicsGcm GC needs to register its
!   Initialize and Run.  It uses the GEOS_Generic construct for defining 
!   state specs and couplings among its children.  In addition, it creates the   
!   children GCs (AGCM and OGCM) and runs their
!   respective SetServices.

!EOP

!=============================================================================
!
! ErrLog Variables

    character(len=ESMF_MAXSTR)          :: IAm
    integer                             :: STATUS
    character(len=ESMF_MAXSTR)          :: COMP_NAME

!   Variables for setting MAPL import/export specs
    TYPE MSTATE
     CHARACTER(len=ESMF_MAXSTR)        :: short_name
     CHARACTER(len=ESMF_MAXSTR)        :: long_name
     CHARACTER(len=ESMF_MAXSTR)        :: units
     INTEGER                           :: dims
     INTEGER                           :: vlocation
    END TYPE

    TYPE( MSTATE ), POINTER  :: imports(:)
    TYPE( MSTATE ), POINTER  :: exports(:)
    INTEGER nImports
    INTEGER nExports
    INTEGER I

! Locals
    type (MAPL_MetaComp),  pointer     :: MAPL  
    type  (ESMF_Config)                :: CF

    integer            :: NUM_ICE_CATEGORIES
    integer            :: NUM_ICE_LAYERS
    integer, parameter :: NUM_SNOW_LAYERS=1
    integer            :: NUM_ICE_LAYERS_ALL
    integer            :: NUM_SNOW_LAYERS_ALL

!=============================================================================

! Begin...

! Get my name and set-up traceback handle
! ---------------------------------------

    Iam = 'SetServices'
    call ESMF_GridCompGet( GC, NAME=COMP_NAME, CONFIG=CF, RC=STATUS )
    VERIFY_(STATUS)
    Iam = trim(COMP_NAME) // Iam

!   Imports and exports specification
!   ---------------------------------

    nimports = 16
    allocate(imports(nimports))
    imports    = (/                                                                                                 &
    mstate('TAUX',  'Agrid_eastward_stress_on_skin',         'N m-2',     MAPL_DimsHorzOnly,MAPL_VLocationNone),    &
    mstate('TAUY',  'Agrid_northward_stress_on_skin',        'N m-2',     MAPL_DimsHorzOnly,MAPL_VLocationNone),    &
    mstate('TAUXI',  'Agrid_eastward_stress_on_ice',         'N m-2',     MAPL_DimsHorzOnly,MAPL_VLocationNone),    &
    mstate('TAUYI',  'Agrid_northward_stress_on_ice',        'N m-2',     MAPL_DimsHorzOnly,MAPL_VLocationNone),    &
    mstate('PS',    'Surface Atmospheric Pressure',          'Pa',        MAPL_DimsHorzOnly,MAPL_VLocationNone),    &
    mstate('SWHEAT','solar_heating_rate',                    'W m-2',     MAPL_DimsHorzVert,MAPL_VLocationCenter),  &
    mstate('QFLX',  'freshwater_flux_from_skin_to_ocean',    'kg m-2 s-1',MAPL_DimsHorzOnly,MAPL_VLocationNone),    &
    mstate('HFLX',  'turbulent_heat_flux_from_skin_to_ocean','W m-2',     MAPL_DimsHorzOnly,MAPL_VLocationNone),    &
    mstate('SFLX',  'salt_flux_from_skin_to_ocean',          'kg m-2 s-1',MAPL_DimsHorzOnly,MAPL_VLocationNone),    &
    mstate('PENUVR','net_downward_penetrating_direct_UV_flux','W m-2',    MAPL_DimsHorzOnly,MAPL_VLocationNone),    &
    mstate('PENPAR','net_downward_penetrating_direct_PAR_flux','W m-2',   MAPL_DimsHorzOnly,MAPL_VLocationNone),    &
    mstate('PENUVF','net_downward_penetrating_diffuse_UV_flux','W m-2',   MAPL_DimsHorzOnly,MAPL_VLocationNone),    &
    mstate('PENPAF','net_downward_penetrating_diffuse_PAR_flux','W m-2',  MAPL_DimsHorzOnly,MAPL_VLocationNone),    &
    mstate('DISCHARGE','river_discharge_at_ocean_points',  'kg m-2 s-1',  MAPL_DimsHorzOnly,MAPL_VLocationNone),    &
    mstate('PICE','pressure due to ice weight',             'Pa',         MAPL_DimsHorzOnly,MAPL_VLocationNone),     &
    mstate('WGHT', 'weight_for_ocean_grid','1',     MAPL_DimsHorzOnly,MAPL_VLocationNone)    &
    /)

    DO I=1,NIMPORTS
     CALL MAPL_AddImportSpec(GC,            &
      SHORT_NAME = imports(i)%short_name,   &
      LONG_NAME  = imports(i)%long_name,    &
      UNITS      = imports(i)%units,        &
      DIMS       = imports(i)%dims,         &
      VLOCATION  = imports(i)%vlocation,    &
      RC         =status); VERIFY_(STATUS)
     call WRITE_PARALLEL("MAPL: adding import "//trim(imports(i)%short_name))

     ! ALT: Mirroring the Imports to Exports for diagnostic purposes
     CALL MAPL_AddExportSpec(GC,            &
      SHORT_NAME = imports(i)%short_name,   &
      LONG_NAME  = imports(i)%long_name,    &
      UNITS      = imports(i)%units,        &
      DIMS       = imports(i)%dims,         &
      VLOCATION  = imports(i)%vlocation,    &
      RC         =status); VERIFY_(STATUS)
     call WRITE_PARALLEL("MAPL: adding export "//trim(imports(i)%short_name))

  ENDDO
    deallocate(imports)

!   -------------------------

    nexports  = 14
    allocate(exports(nexports))
    exports    = (/ &
    mstate('US',    'top_layer_Agrid_eastward_velocity',     'm s-1',     MAPL_DimsHorzOnly,MAPL_VLocationNone),    &
    mstate('VS',    'top_layer_Agrid_northward_velocity',    'm s-1',     MAPL_DimsHorzOnly,MAPL_VLocationNone),    &
    mstate('TS',    'top_layer_temperature',                 'K',         MAPL_DimsHorzOnly,MAPL_VLocationNone),    &
    mstate('SS',    'top_layer_salinity',                    'psu',       MAPL_DimsHorzOnly,MAPL_VLocationNone),    &
    mstate('MIT_3D_MASK',  'ocean mask at t-points',         '1',         MAPL_DimsHorzVert,MAPL_VLocationCenter),  &
    mstate('DH',    'layer_thickness',                       'm',         MAPL_DimsHorzVert,MAPL_VLocationCenter),  &
    mstate('SLV',   'sea_level_with_ice_loading',            'm',         MAPL_DimsHorzOnly,MAPL_VLocationNone),    &
    mstate('USB',   'surface_Bgrid_X_velocity',            'm s-1 ',      MAPL_DimsHorzOnly,MAPL_VLocationNone),    &
    mstate('VSB',   'surface_Bgrid_Y_velocity',            'm s-1 ',      MAPL_DimsHorzOnly,MAPL_VLocationNone),    &
    mstate('SSH',   'sea_level_height',                    'm',           MAPL_DimsHorzOnly,MAPL_VLocationNone),    &
    mstate('T',   'potential_temperature',                 'm',           MAPL_DimsHorzVert,MAPL_VLocationCenter),    &
    mstate('S',   'salinity',                              'psu',         MAPL_DimsHorzVert,MAPL_VLocationCenter),    &
    mstate('DISCHARGEe','river_discharge_at_ocean_points',  'kg m-2 s-1',  MAPL_DimsHorzOnly,MAPL_VLocationNone),    &
    mstate('PBO','pressure_at_bottom_of_ocean',  'N m-2',  MAPL_DimsHorzOnly,MAPL_VLocationNone)    &
     /)

    DO I=1,nexports
     call MAPL_AddExportSpec(GC,                                    &
          SHORT_NAME         = exports(i)%short_name,               &
          LONG_NAME          = exports(i)%long_name,                &
          UNITS              = exports(i)%units,                    &
          DIMS               = exports(i)%dims,                     &
          VLOCATION          = exports(i)%vlocation,                &
          RC=STATUS  )
     VERIFY_(STATUS)
     call WRITE_PARALLEL("MAPL: adding export "//trim(exports(i)%short_name))
    ENDDO

    deallocate(exports)

!ALT The following mods are necessary to handle SeaIce

    call MAPL_GetObjectFromGC ( GC, MAPL, RC=STATUS)
    VERIFY_(STATUS)

! Get constants from CF
! ---------------------

    call ESMF_ConfigGetAttribute(CF, NUM_ICE_CATEGORIES, Label="CICE_N_ICE_CATEGORIES:" , RC=STATUS)
    VERIFY_(STATUS)

    call ESMF_ConfigGetAttribute(CF, NUM_ICE_LAYERS,     Label="CICE_N_ICE_LAYERS:" ,     RC=STATUS)
    VERIFY_(STATUS)

    NUM_ICE_LAYERS_ALL  = NUM_ICE_LAYERS  * NUM_ICE_CATEGORIES
    NUM_SNOW_LAYERS_ALL = NUM_SNOW_LAYERS * NUM_ICE_CATEGORIES

!ALT: These were brought from  the originalCICEdyna
! CICE orininal imports/exports/internal
!================= begin section for sea ice needed imports/exports ==========

! !Import state:

    call MAPL_AddImportSpec(GC,                            &
         SHORT_NAME         = 'FRACICE',                           &
         LONG_NAME          = 'fractional_cover_of_seaice',        &
         UNITS              = '1',                                 &
         DIMS               = MAPL_DimsHorzOnly,                   &
         UNGRIDDED_DIMS     = (/NUM_ICE_CATEGORIES/),              &
         VLOCATION          = MAPL_VLocationNone,                  &
         RC=STATUS  )
    VERIFY_(STATUS)

  call MAPL_AddImportSpec(GC,                            &
    SHORT_NAME         = 'TI',                                &
    LONG_NAME          = 'seaice_skin_temperature',           &
    UNITS              = 'K',                                 &
    DIMS               = MAPL_DimsHorzOnly,                   &
    UNGRIDDED_DIMS     = (/NUM_ICE_CATEGORIES/),              &
    VLOCATION          = MAPL_VLocationNone,                  &
    RESTART            = MAPL_RestartOptional,                &
    DEFAULT            = MAPL_TICE,                           &
                                                   RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddImportSpec(GC,                            &
    SHORT_NAME         = 'SI',                                &
    LONG_NAME          = 'seaice_skin_salinity',              &
    UNITS              = 'psu',                               &
    DIMS               = MAPL_DimsHorzOnly,                   &
    VLOCATION          = MAPL_VLocationNone,                  &
    RESTART            = MAPL_RestartOptional,                &
    DEFAULT            = 4.,                                  &
                                                   RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddImportSpec(GC,                                &
    SHORT_NAME         = 'VOLICE',                            &
    LONG_NAME          = 'ice_category_volume_per_unit_area_of_grid_cell',&
    UNITS              = 'm',                                 &
    DIMS               = MAPL_DimsHorzOnly,                   &
    UNGRIDDED_DIMS     = (/NUM_ICE_CATEGORIES/),              &
    VLOCATION          = MAPL_VLocationNone,                  &
    RESTART            = MAPL_RestartOptional,                &
    DEFAULT            = 0.0,                                 &
                                                       RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddImportSpec(GC,                                &
    SHORT_NAME         = 'VOLSNO',                            &
    LONG_NAME          = 'sno_category_volume_per_unit_area_of_grid_cell',&
    UNITS              = 'm',                                 &
    DIMS               = MAPL_DimsHorzOnly,                   &
    UNGRIDDED_DIMS     = (/NUM_ICE_CATEGORIES/),              &
    VLOCATION          = MAPL_VLocationNone,                  &
    RESTART            = MAPL_RestartOptional,                &
    DEFAULT            = 0.0,                                 &
                                                       RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddImportSpec(GC,                                &
        SHORT_NAME         = 'ERGICE',                            &
        LONG_NAME          = 'ice_category_layer_internal_energy',&
        UNITS              = 'J m-2',                             &
        DIMS               = MAPL_DimsHorzOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        UNGRIDDED_DIMS     = (/NUM_ICE_LAYERS_ALL/),              &
        !VLOCATION          = MAPL_VLocationCenter,                 &
    !    DEFAULT            = 0.0,                                 &
        RESTART            = MAPL_RestartSkip,                    &
                                                       RC=STATUS  )
   VERIFY_(STATUS)

   call MAPL_AddImportSpec(GC,                                &
        SHORT_NAME         = 'ERGSNO',                            &
        LONG_NAME          = 'snow_category_layer_internal_energy',&
        UNITS              = 'J m-2',                             &
        !DIMS               = MAPL_DimsHorzVert,                   &
        DIMS               = MAPL_DimsHorzOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        UNGRIDDED_DIMS     = (/NUM_SNOW_LAYERS_ALL/),             &
        !VLOCATION          = MAPL_VLocationCenter,                 &
        RESTART            = MAPL_RestartSkip,                    &
        !DEFAULT            = 0.0,                                 &
                                                       RC=STATUS  )
   VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC                                     ,&
        LONG_NAME          = 'melt_pond_volume'                     ,&
        UNITS              = 'm'                                ,&
        SHORT_NAME         = 'MPOND'                                 ,&
        !DIMS               = MAPL_DimsHorzVert,                   &
        UNGRIDDED_DIMS     = (/NUM_ICE_CATEGORIES/),              &
        DIMS               = MAPL_DimsHorzOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        !VLOCATION          = MAPL_VLocationCenter,                 &
        !DEFAULT            = 0.0                                    ,&
        RESTART            = MAPL_RestartSkip,                    &
        RC=STATUS                                                 )

     VERIFY_(STATUS)

   call MAPL_AddImportSpec(GC,                                &
        SHORT_NAME         = 'TAUAGE',                            &
        LONG_NAME          = 'volume_weighted_mean_ice_age',      &
        UNITS              = 's',                                 &
        !DIMS               = MAPL_DimsHorzVert,                   &
        UNGRIDDED_DIMS     = (/NUM_ICE_CATEGORIES/),              &
        DIMS               = MAPL_DimsHorzOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        !VLOCATION          = MAPL_VLocationCenter,                 &
        RESTART            = MAPL_RestartSkip,                    &
        !DEFAULT            = 0.0,                                 &
                                                       RC=STATUS  )
   VERIFY_(STATUS)

  call MAPL_AddImportSpec(GC,                            &
         SHORT_NAME         = 'HI',                                &
         LONG_NAME          = 'seaice_skin_layer_depth',            &
         UNITS              = 'm',                                 &
         DIMS               = MAPL_DimsHorzOnly,                   &
         VLOCATION          = MAPL_VLocationNone,                  &
                                                   RC=STATUS  )
    VERIFY_(STATUS)


! !Internal state: NO INTERNAL STATE for MITgcm!


!  !Export state:

  call MAPL_AddExportSpec(GC,                            &
    SHORT_NAME         = 'UI',                                &
    LONG_NAME          = 'zonal_velocity_of_surface_seaice',   &
    UNITS              = 'm s-1 ',                            &
    DIMS               = MAPL_DimsHorzOnly,                   &
    VLOCATION          = MAPL_VLocationNone,                  &
                                                   RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                            &
    SHORT_NAME         = 'VI',                                &
    LONG_NAME          = 'meridional_velocity_of_surface_seaice',&
    UNITS              = 'm s-1 ',                            &
    DIMS               = MAPL_DimsHorzOnly,                   &
    VLOCATION          = MAPL_VLocationNone,                  &
                                                   RC=STATUS  )
  VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                  &
        SHORT_NAME         = 'TAUXBOT',                           &
        LONG_NAME          = 'eastward_stress_at_base_of_ice',    &
        UNITS              = 'N m-2',                             &
        DIMS               = MAPL_DimsHorzOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                                  &
        SHORT_NAME         = 'TAUYBOT',                           &
        LONG_NAME          = 'northward_stress_at_base_of_ice',   &
        UNITS              = 'N m-2',                             &
        DIMS               = MAPL_DimsHorzOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)


  call MAPL_AddExportSpec(GC,                            &
    SHORT_NAME         = 'FRACICE',                           &
    LONG_NAME          = 'fractional_cover_of_seaice',        &
    UNITS              = '1',                                 &
    DIMS               = MAPL_DimsHorzOnly,                   &
    UNGRIDDED_DIMS     = (/NUM_ICE_CATEGORIES/),              &
    VLOCATION          = MAPL_VLocationNone,                  &
                                                   RC=STATUS  )
  VERIFY_(STATUS)


  call MAPL_AddExportSpec(GC,                            &
    SHORT_NAME         = 'TI',                                &
    LONG_NAME          = 'seaice_skin_temperature',           &
    UNITS              = 'K',                                 &
    DIMS               = MAPL_DimsHorzOnly,                   &
    UNGRIDDED_DIMS     = (/NUM_ICE_CATEGORIES/),              &
    VLOCATION          = MAPL_VLocationNone,                  &
    DEFAULT            = MAPL_TICE,                           &
                                                   RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                            &
    SHORT_NAME         = 'SI',                                &
    LONG_NAME          = 'seaice_skin_salinity',              &
    UNITS              = 'psu',                               &
    DIMS               = MAPL_DimsHorzOnly,                   &
    VLOCATION          = MAPL_VLocationNone,                  &
    DEFAULT            = 4.,                                  &
                                                   RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                                &
    SHORT_NAME         = 'VOLICE',                            &
    LONG_NAME          = 'ice_category_volume_per_unit_area_of_grid_cell',&
    UNITS              = 'm',                                 &
    DIMS               = MAPL_DimsHorzOnly,                   &
    UNGRIDDED_DIMS     = (/NUM_ICE_CATEGORIES/),              &
    VLOCATION          = MAPL_VLocationNone,                  &
    DEFAULT            = 0.0,                                 &
                                                       RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                                &
    SHORT_NAME         = 'VOLSNO',                            &
    LONG_NAME          = 'sno_category_volume_per_unit_area_of_grid_cell',&
    UNITS              = 'm',                                 &
    DIMS               = MAPL_DimsHorzOnly,                   &
    UNGRIDDED_DIMS     = (/NUM_ICE_CATEGORIES/),              &
    VLOCATION          = MAPL_VLocationNone,                  &
    DEFAULT            = 0.0,                                 &
                                                       RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                                &
        SHORT_NAME         = 'ERGICE',                            &
        LONG_NAME          = 'ice_category_layer_internal_energy',&
        UNITS              = 'J m-2',                             &
        DIMS               = MAPL_DimsHorzOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        UNGRIDDED_DIMS     = (/NUM_ICE_LAYERS_ALL/),              &
        !VLOCATION          = MAPL_VLocationCenter,                 &
    !    DEFAULT            = 0.0,                                 &
                                                       RC=STATUS  )
   VERIFY_(STATUS)

   call MAPL_AddExportSpec(GC,                                &
        SHORT_NAME         = 'ERGSNO',                            &
        LONG_NAME          = 'snow_category_layer_internal_energy',&
        UNITS              = 'J m-2',                             &
        !DIMS               = MAPL_DimsHorzVert,                   &
        DIMS               = MAPL_DimsHorzOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        UNGRIDDED_DIMS     = (/NUM_SNOW_LAYERS_ALL/),             &
        !VLOCATION          = MAPL_VLocationCenter,                 &
        !DEFAULT            = 0.0,                                 &
                                                       RC=STATUS  )
   VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC                                     ,&
        LONG_NAME          = 'melt_pond_volume'                     ,&
        UNITS              = 'm'                                ,&
        SHORT_NAME         = 'MPOND'                                 ,&
        !DIMS               = MAPL_DimsHorzVert,                   &
        UNGRIDDED_DIMS     = (/NUM_ICE_CATEGORIES/),              &
        DIMS               = MAPL_DimsHorzOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        !VLOCATION          = MAPL_VLocationCenter,                 &
        !DEFAULT            = 0.0                                    ,&
        RC=STATUS                                                 )

     VERIFY_(STATUS)

   call MAPL_AddExportSpec(GC,                                &
        SHORT_NAME         = 'TAUAGE',                            &
        LONG_NAME          = 'volume_weighted_mean_ice_age',      &
        UNITS              = 's',                                 &
        !DIMS               = MAPL_DimsHorzVert,                   &
        UNGRIDDED_DIMS     = (/NUM_ICE_CATEGORIES/),              &
        DIMS               = MAPL_DimsHorzOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        !VLOCATION          = MAPL_VLocationCenter,                 &
        !DEFAULT            = 0.0,                                 &
                                                       RC=STATUS  )
   VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                            &
         SHORT_NAME         = 'HI',                                &
         LONG_NAME          = 'seaice_skin_layer_depth',            &
         UNITS              = 'm',                                 &
         DIMS               = MAPL_DimsHorzOnly,                   &
         VLOCATION          = MAPL_VLocationNone,                  &
                                                   RC=STATUS  )
    VERIFY_(STATUS)




! !Export state - increments for seaice:

    call MAPL_AddExportSpec(GC,                                    &
         SHORT_NAME         = 'DELFRACICE',                        &
         LONG_NAME          = 'delta_fractional_cover_of_seaice',  &
         UNITS              = '1',                                 &
         DIMS               = MAPL_DimsHorzOnly,                   &
         UNGRIDDED_DIMS     = (/NUM_ICE_CATEGORIES/),              &
         VLOCATION          = MAPL_VLocationNone,                  &
         RC=STATUS  )
    VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                            &
    SHORT_NAME         = 'DELTI',                                &
    LONG_NAME          = 'delta_seaice_skin_temperature',           &
    UNITS              = 'K',                                 &
    DIMS               = MAPL_DimsHorzOnly,                   &
    UNGRIDDED_DIMS     = (/NUM_ICE_CATEGORIES/),              &
    VLOCATION          = MAPL_VLocationNone,                  &
    DEFAULT            = MAPL_TICE,                           &
                                                   RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                            &
    SHORT_NAME         = 'DELSI',                                &
    LONG_NAME          = 'delta_seaice_skin_salinity',              &
    UNITS              = 'psu',                               &
    DIMS               = MAPL_DimsHorzOnly,                   &
    VLOCATION          = MAPL_VLocationNone,                  &
    DEFAULT            = 4.,                                  &
                                                   RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                                &
    SHORT_NAME         = 'DELVOLICE',                            &
    LONG_NAME          = 'delta_ice_category_volume_per_unit_area_of_grid_cell',&
    UNITS              = 'm',                                 &
    DIMS               = MAPL_DimsHorzOnly,                   &
    UNGRIDDED_DIMS     = (/NUM_ICE_CATEGORIES/),              &
    VLOCATION          = MAPL_VLocationNone,                  &
    DEFAULT            = 0.0,                                 &
                                                       RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                                &
    SHORT_NAME         = 'DELVOLSNO',                            &
    LONG_NAME          = 'delta_sno_category_volume_per_unit_area_of_grid_cell',&
    UNITS              = 'm',                                 &
    DIMS               = MAPL_DimsHorzOnly,                   &
    UNGRIDDED_DIMS     = (/NUM_ICE_CATEGORIES/),              &
    VLOCATION          = MAPL_VLocationNone,                  &
   DEFAULT            = 0.0,                                 &
                                                       RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                                &
        SHORT_NAME         = 'DELERGICE',                            &
        LONG_NAME          = 'delta_ice_category_layer_internal_energy',&
        UNITS              = 'J m-2',                             &
        DIMS               = MAPL_DimsHorzOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        UNGRIDDED_DIMS     = (/NUM_ICE_LAYERS_ALL/),              &
        !VLOCATION          = MAPL_VLocationCenter,                 &
    ! DEFAULT            = 0.0,                                 &
                                                       RC=STATUS  )
   VERIFY_(STATUS)

   call MAPL_AddExportSpec(GC,                                &
        SHORT_NAME         = 'DELERGSNO',                            &
        LONG_NAME          = 'delta_snow_category_layer_internal_energy',&
        UNITS              = 'J m-2',                             &
        !DIMS               = MAPL_DimsHorzVert,                   &
        DIMS               = MAPL_DimsHorzOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        UNGRIDDED_DIMS     = (/NUM_SNOW_LAYERS_ALL/),             &
        !VLOCATION          = MAPL_VLocationCenter,                 &
       !DEFAULT            = 0.0,                                 &
                                                       RC=STATUS  )
   VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC                                     ,&
        LONG_NAME          = 'delta_melt_pond_volume'                     ,&
        UNITS              = 'm'                                ,&
        SHORT_NAME         = 'DELMPOND'                                 ,&
        !DIMS               = MAPL_DimsHorzVert,                   &
       UNGRIDDED_DIMS     = (/NUM_ICE_CATEGORIES/),              &
        DIMS               = MAPL_DimsHorzOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        !VLOCATION          = MAPL_VLocationCenter,                 &
        !DEFAULT            = 0.0                                    ,&
        RC=STATUS                                                 )

     VERIFY_(STATUS)

   call MAPL_AddExportSpec(GC,                                &
        SHORT_NAME         = 'DELTAUAGE',                            &
        LONG_NAME          = 'delta_volume_weighted_mean_ice_age',      &
        UNITS              = 's',                                 &
        !DIMS               = MAPL_DimsHorzVert,                   &
        UNGRIDDED_DIMS     = (/NUM_ICE_CATEGORIES/),              &
        DIMS               = MAPL_DimsHorzOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        !VLOCATION          = MAPL_VLocationCenter,                 &
        !DEFAULT            = 0.0,                                 &
                                                       RC=STATUS  )
   VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                            &
         SHORT_NAME         = 'DELHI',                                &
         LONG_NAME          = 'delta_seaice_skin_layer_depth',            &
         UNITS              = 'm',                                 &
         DIMS               = MAPL_DimsHorzOnly,                   &
         VLOCATION          = MAPL_VLocationNone,                  &
                                                   RC=STATUS  )
    VERIFY_(STATUS)



  !ALT: TAUXI and TAUYI are also exports but they are added by 
  !     mirroring the imports 


!================= end of the sea ice needed imports/exports ==========

!EOP

! Set the Initialize, Run, Finalize entry points
! ----------------------------------------------

    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_INITIALIZE,   InitializeM, RC=status)
    VERIFY_(STATUS)
    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_RUN,          Run,        RC=status)
    VERIFY_(STATUS)
    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_FINALIZE,     Finalize,   RC=status)
    VERIFY_(STATUS)


! Set the Profiling timers
! ------------------------

    call MAPL_TimerAdd(GC,   name="INITIALIZE" ,RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC,   name="RUN"        ,RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC,   name="FINALIZE"   ,RC=STATUS)
    VERIFY_(STATUS)

! Generic SetServices
! -------------------

    call MAPL_GenericSetServices    ( GC, RC=STATUS )
    VERIFY_(STATUS)

! All done
! --------

    RETURN_(ESMF_SUCCESS)
  
  end subroutine SetServices


!BOP

! !IROUTINE: INITIALIZE -- Initialize method for ExternalOcean wrapper

! !INTERFACE:

  subroutine InitializeM ( GC, IMPORT, EXPORT, CLOCK, RC )



! !ARGUMENTS:

    type(ESMF_GridComp),     intent(INOUT) :: GC     ! Gridded component 
    type(ESMF_State),        intent(INOUT) :: IMPORT ! Import state
    type(ESMF_State),        intent(INOUT) :: EXPORT ! Export state
    type(ESMF_Clock),        intent(INOUT) :: CLOCK  ! The clock
    integer, optional,       intent(  OUT) :: RC     ! Error code:

!EOP

! ErrLog Variables

    character(len=ESMF_MAXSTR)             :: IAm
    integer                                :: STATUS
    character(len=ESMF_MAXSTR)             :: COMP_NAME

! Locals


! Locals with ESMF and MAPL types

    type (MAPL_MetaComp), pointer          :: MAPL 
    type(ESMF_Grid)                        :: Grid

! Locals

!   Variable to hold model state for each instance
    TYPE(MITGCM_ISTATE_CONTAINER) :: mitgcmIState(1)
!   TYPE(MITGCM_ISTATE_WRAP_TYPE) wrap
    TYPE(T_PrivateState_Wrap) wrap

!   Variables for getting MPI communicator that component will use
    type(ESMF_VM)       :: vm
    INTEGER myComm

!   Variables for holding and setting run directory
    character(len=ESMF_MAXSTR)            :: ocean_dir
    integer*1, pointer                    :: iarr(:)

!   Local variables used for allocating exports pointers
    REAL_, pointer                         :: US  (:,:)
    REAL_, pointer                         :: VS  (:,:)
    REAL_, pointer                         :: TS  (:,:)
    REAL_, pointer                         :: SS  (:,:)
    REAL_, pointer                         :: pMASK(:,:,:)
    REAL_, pointer                         :: mMASK(:,:,:) => NULL()
    REAL_, pointer                         :: pMASK2d(:,:)
    REAL_, pointer                         :: mMASK2d(:,:)
    REAL_, pointer                         :: DH(:,:,:)
    integer                                :: L
    INTEGER :: iLoop
    INTEGER :: myThid
    INTEGER :: myCurrentIter
    _RL     :: myCurrentTime
    integer :: chdir
    external chdir 

! Begin... 

! Get the target components name and set-up traceback handle.
! -----------------------------------------------------------

    Iam = "Initialize"
    call ESMF_GridCompGet( gc, NAME=comp_name, RC=status )
    VERIFY_(STATUS)
    Iam = trim(comp_name) // trim(Iam)

! Allocate the private state...
!------------------------------
    
    allocate( PrivateSTATE , stat=STATUS )
    VERIFY_(STATUS)

    wrap%ptr => PrivateState

! And put it in the GC
!---------------------

    CALL ESMF_UserCompSetInternalState( GC, trim(comp_name)//'_internal_state',&
         WRAP, STATUS )
    VERIFY_(status)

!-------------------
    CALL ESMF_GridCompGet(gc, vm=vm, RC=status); VERIFY_(STATUS)
    CALL ESMF_VMGet(VM, mpiCommunicator=myComm, rc=RC)


! Get my internal MAPL_Generic state
!-----------------------------------

    call MAPL_GetObjectFromGC ( GC, MAPL, RC=STATUS)
    VERIFY_(STATUS)

! Profilers
!----------

    call MAPL_TimerOn(MAPL,"TOTAL"     )
    call MAPL_TimerOn(MAPL,"INITIALIZE")

! Get the grid, configuration
!----------------------------

    call MAPL_GetResource( MAPL, ocean_dir, label='OCEAN_DIR:', rc=status ) ; VERIFY_(STATUS)
    call str4c( iarr, TRIM(ocean_dir) )

! Profilers
! ---------

    call MAPL_TimerOff(MAPL,"INITIALIZE")
    call MAPL_TimerOff(MAPL,"TOTAL"     )

! Generic initialize
! ------------------

    call MAPL_GenericInitialize( GC, IMPORT, EXPORT, CLOCK, RC=status )
    VERIFY_(STATUS)

! Now do component specific initialization
! ----------------------------------------
!    call system('pushd '//trim(ocean_dir))
!    status = chdir(trim(ocean_dir))
    call setdir(iarr)
    call WRITE_PARALLEL("Calling DRIVER_INIT")
    CALL DRIVER_INIT( mitgcmIState=mitgcmIState(1)%p, myComm=myComm)
    call WRITE_PARALLEL("Done DRIVER_INIT")
    deallocate(iarr)
    call popdir
!    status = chdir('..')

    PrivateState%ptr => mitgcmIState(1)%p

    CALL ESMF_UserCompSetInternalState ( GC, 'MITgcm_istate',wrap,status )
    VERIFY_(STATUS)


!   Force allocation of export arrays for this component
!   ----------------------------------------------------
    call MAPL_GetPointer(EXPORT, DH, 'DH',  alloc=.true., RC=STATUS)
    VERIFY_(STATUS)
!!  DH=1000.
!!  Remove set to 1000 m depths, include info from mitgcm input data file.

!! Hard-wired for now

    DH(:,:, 1) = 50.
    DH(:,:, 2) = 70.
    DH(:,:, 3) = 100.
    DH(:,:, 4) = 140.
    DH(:,:, 5) = 190.
    DH(:,:, 6) = 240.
    DH(:,:, 7) = 290.
    DH(:,:, 8) = 340.
    DH(:,:, 9) = 390.
    DH(:,:,10) = 440.
    DH(:,:,11) = 490.
    DH(:,:,12) = 540.
    DH(:,:,13) = 590.
    DH(:,:,14) = 640.
    DH(:,:,15:) = 690.

    call MAPL_GetPointer(EXPORT, pMASK, trim(COMP_NAME)//'_3D_MASK',  alloc=.true., RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(EXPORT, TS,  'TS',  alloc=.true., RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(EXPORT, SS,  'SS',  alloc=.true., RC=STATUS)
    VERIFY_(STATUS)

    call WRITE_PARALLEL("Calling DRIVER_Get_ExportState")
    CALL DRIVER_GET_EXPORT_STATE(privateState%ptr, 'MASK', pMASK )
    CALL DRIVER_GET_EXPORT_STATE(privateState%ptr, 'TS', TS )
    CALL DRIVER_GET_EXPORT_STATE(privateState%ptr, 'SS', SS )
     call WRITE_PARALLEL("Done DRIVER_Get_ExportState")


! All Done
!---------
    call WRITE_PARALLEL("Done MIT_PlugInit")

    RETURN_(ESMF_SUCCESS)
  end subroutine InitializeM

!=================================================================================
!=================================================================================

!BOP

! !IROUTINE: Run        -- Run method for ExternalModel wrapper

! !INTERFACE:

  subroutine Run ( gc, import, export, clock, rc )


! !ARGUMENTS:

    type(ESMF_GridComp), intent(INOUT) :: gc     ! Gridded component 
    type(ESMF_State),    intent(INOUT) :: import ! Import state
    type(ESMF_State),    intent(INOUT) :: export ! Export state
    type(ESMF_Clock),    intent(INOUT) :: clock  ! The supervisor clock
    integer, optional,   intent(  OUT) :: rc     ! Error code:

!EOP

! ErrLog Variables

    character(len=ESMF_MAXSTR)             :: IAm
    integer                                :: STATUS
    character(len=ESMF_MAXSTR)             :: COMP_NAME

! Locals
    type (MAPL_MetaComp), pointer          :: MAPL 

    integer :: IM, JM
    REAL_, pointer :: FRESHW(:,:)

    REAL*8, POINTER :: uVel(:,:,:,:,:)

!   Pointers for passing import state values to component
    REAL_, pointer                         ::   TAUX(:,:  )
    REAL_, pointer                         ::   TAUY(:,:  )
    REAL_, pointer                         ::   TAUXI(:,:  )
    REAL_, pointer                         ::   TAUYI(:,:  )
    REAL_, pointer                         ::     PS(:,:  )
    REAL_, pointer                         :: SWHEAT(:,:,:)
    REAL_, pointer                         ::   QFLX(:,:  )
    REAL_, pointer                         ::   HFLX(:,:  )
    REAL_, pointer                         ::   SFLX(:,:  )
    REAL_, pointer                         ::   DISCHARGE(:,:  )
    REAL_, pointer                         ::   LATS(:,:  )
    REAL_, pointer                         ::   LONS(:,:  )
    REAL_, pointer                         ::   WGHT(:,:  )

! Sea ice vars
    REAL_, pointer                         :: FRI(:,:,:)

!   Pointers for mirroring import state values to exports
    REAL_, pointer                         ::   TAUXe(:,:  )
    REAL_, pointer                         ::   TAUYe(:,:  )
    REAL_, pointer                         ::     PSe(:,:  )
    REAL_, pointer                         :: SWHEATe(:,:,:)
    REAL_, pointer                         ::   QFLXe(:,:  )
    REAL_, pointer                         ::   HFLXe(:,:  )
    REAL_, pointer                         ::   SFLXe(:,:  )
    REAL_, pointer                         ::   DISCHARGEe(:,:  )
    REAL_, pointer                         ::   WGHTe(:,:  )

!   Pointers for fetching export state values from component
    REAL_, pointer                         :: US  (:,:)
    REAL_, pointer                         :: VS  (:,:)
    REAL_, pointer                         :: TS  (:,:)
    REAL_, pointer                         :: SS  (:,:)
    REAL_, pointer                         :: MASK(:,:,:)

! Sea ice vars
    REAL_, pointer                         :: FRACICE(:,:,:)
    REAL_, pointer                         :: VOLICE(:,:,:)
    REAL_, pointer                         :: VOLSNO(:,:,:)
    REAL_, pointer                         :: ERGICE(:,:,:)
    REAL_, pointer                         :: ERGSNO(:,:,:)
    REAL_, pointer                         :: TI(:,:,:)
    REAL_, pointer                         :: SI(:,:)
    REAL_, pointer                         :: HI(:,:)
    REAL_, pointer                         :: MPOND (:,:,:)
    REAL_, pointer                         :: TAUAGE(:,:,:)
    REAL_, pointer                         :: UI(:,:)
    REAL_, pointer                         :: VI(:,:)
    REAL_, pointer                         :: TAUXIe(:,:)
    REAL_, pointer                         :: TAUYIe(:,:)
    REAL_, pointer                         :: TAUXBOT(:,:)
    REAL_, pointer                         :: TAUYBOT(:,:)

    REAL_, pointer                         :: FRACICEe(:,:,:)
    REAL_, pointer                         :: TIe(:,:,:)
    REAL_, pointer                         :: SIe(:,:)
    REAL_, pointer                         :: VOLICEe(:,:,:)
    REAL_, pointer                         :: VOLSNOe(:,:,:)
    REAL_, pointer                         :: ERGICEe(:,:,:)
    REAL_, pointer                         :: ERGSNOe(:,:,:)
    REAL_, pointer                         :: MPONDe(:,:,:)
    REAL_, pointer                         :: TAUAGEe(:,:,:)
    REAL_, pointer                         :: HIe(:,:)

    REAL_, pointer                         :: DELFRACICE(:,:,:)
    REAL_, pointer                         :: DELTI(:,:,:)
    REAL_, pointer                         :: DELSI (:,:)
    REAL_, pointer                         :: DELVOLICE(:,:,:)
    REAL_, pointer                         :: DELVOLSNO(:,:,:)
    REAL_, pointer                         :: DELERGICE(:,:,:)
    REAL_, pointer                         :: DELERGSNO(:,:,:)
    REAL_, pointer                         :: DELMPOND(:,:,:)
    REAL_, pointer                         :: DELTAUAGE(:,:,:)
    REAL_, pointer                         :: DELHI(:,:)

!   Type for getting MITgcm internal state pointer
    TYPE(T_PrivateState_Wrap) wrap
    type(T_PrivateState), pointer :: privateState

!   Variables for holding and setting run directory
    character(len=ESMF_MAXSTR)            :: ocean_dir
    integer*1, pointer                    :: iarr(:)
    integer                               :: passive_ocean

! Begin
!------

! Get the component's name and set-up traceback handle.
! -----------------------------------------------------
    call WRITE_PARALLEL( ' Starting plug run method ' )
    Iam = "Run"
    call ESMF_GridCompGet( gc, NAME=comp_name, RC=status )
    VERIFY_(status)
    Iam = trim(comp_name) // Iam

! Get the wrapped MIT state
!--------------------------
    call ESMF_UserCompGetInternalState( GC, trim(comp_name)//'_internal_state',&
         WRAP, STATUS )
    VERIFY_(status)

    PrivateState => wrap%ptr


! Get my internal MAPL_Generic state
!-----------------------------------

    call MAPL_GetObjectFromGC ( GC, MAPL, RC=STATUS)
    VERIFY_(STATUS)

! Profilers
!----------

    call MAPL_TimerOn (MAPL,"TOTAL")
    call MAPL_TimerOn (MAPL,"RUN"  )

! Get IMPORT pointers
!--------------------
    call MAPL_GetPointer(IMPORT,   TAUX,   'TAUX', RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(IMPORT,   TAUY,   'TAUY', RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(IMPORT,  TAUXI,  'TAUXI', RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(IMPORT,  TAUYI,  'TAUYI', RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(IMPORT,     PS,     'PS', RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(IMPORT, SWHEAT, 'SWHEAT', RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(IMPORT,   QFLX,   'QFLX', RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(IMPORT,   HFLX,   'HFLX', RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(IMPORT,   SFLX,   'SFLX', RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(IMPORT,   DISCHARGE, 'DISCHARGE', RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(IMPORT,   WGHT,   'WGHT', RC=STATUS); VERIFY_(STATUS)
    call MAPL_Get(MAPL, LATS=LATS, LONS=LONS, RC=status); VERIFY_(STATUS)

! Sea ice vars
    call MAPL_GetPointer(IMPORT,     HI,     'HI', RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(IMPORT,     TI,     'TI', RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(IMPORT,     SI,     'SI', RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(IMPORT, VOLICE, 'VOLICE', RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(IMPORT, VOLSNO, 'VOLSNO', RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(IMPORT, ERGICE, 'ERGICE', RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(IMPORT, ERGSNO, 'ERGSNO', RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(IMPORT, TAUAGE, 'TAUAGE', RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(IMPORT,  MPOND,  'MPOND', RC=STATUS); VERIFY_(STATUS)

    call MAPL_GetPointer(IMPORT, FRI,'FRACICE', RC=STATUS); VERIFY_(STATUS)

! Get EXPORT pointers to mirror imports
!--------------------------------------
    call MAPL_GetPointer(EXPORT,   TAUXe,   'TAUX', RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(EXPORT,   TAUYe,   'TAUY', RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(EXPORT,     PSe,     'PS', RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(EXPORT, SWHEATe, 'SWHEAT', RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(EXPORT,   QFLXe,   'QFLX', RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(EXPORT,   HFLXe,   'HFLX', RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(EXPORT,   SFLXe,   'SFLX', RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(EXPORT,   WGHTe,   'WGHT', RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(EXPORT,   DISCHARGEe, 'DISCHARGEe', RC=STATUS); VERIFY_(STATUS)

    CALL MAPL_GetPointer(EXPORT, TIe,   'TI', RC=STATUS); VERIFY_(STATUS)
    CALL MAPL_GetPointer(EXPORT, SIe,   'SI', RC=STATUS); VERIFY_(STATUS)
    CALL MAPL_GetPointer(EXPORT, VOLICEe, 'VOLICE', RC=STATUS); VERIFY_(STATUS)
    CALL MAPL_GetPointer(EXPORT, VOLSNOe, 'VOLSNO', RC=STATUS); VERIFY_(STATUS)
    CALL MAPL_GetPointer(EXPORT, ERGICEe, 'ERGICE', RC=STATUS); VERIFY_(STATUS)
    CALL MAPL_GetPointer(EXPORT, ERGSNOe, 'ERGSNO', RC=STATUS); VERIFY_(STATUS)
    CALL MAPL_GetPointer(EXPORT, MPONDe, 'MPOND', RC=STATUS); VERIFY_(STATUS)
    CALL MAPL_GetPointer(EXPORT, TAUAGEe, 'TAUAGE', RC=STATUS); VERIFY_(STATUS)
    CALL MAPL_GetPointer(EXPORT, HIe, 'HI', RC=STATUS); VERIFY_(STATUS)

    ! Actual copy (only if needed)
    if (associated(TAUXe)) TAUXe = TAUX
    if (associated(TAUYe)) TAUYe = TAUY
    if (associated(PSe)) PSe = PS
    if (associated(SWHEATe)) SWHEATe = SWHEAT
    if (associated(QFLXe)) QFLXe = QFLX
    if (associated(HFLXe)) HFLXe = HFLX
    if (associated(SFLXe)) SFLXe = SFLX
    if (associated(DISCHARGEe)) DISCHARGEe = DISCHARGE
    if (associated(WGHTe)) WGHTe = WGHT

    if (associated(TIe)) TIe = TI
    if (associated(SIe)) SIe = SI
    if (associated(VOLICEe)) VOLICEe = VOLICE
    if (associated(VOLSNOe)) VOLSNOe = VOLSNO
    if (associated(ERGICEe)) ERGICEe = ERGICE
    if (associated(ERGSNOe)) ERGSNOe = ERGSNO
    if (associated(MPONDe)) MPONDe = MPOND
    if (associated(TAUAGEe)) TAUAGEe = TAUAGE
    if (associated(HIe)) HIe = HI

    IM = size(DISCHARGE,1)
    JM = size(DISCHARGE,2)
    allocate(FRESHW(IM,JM), STAT=status)
    VERIFY_(STATUS)
    ! ALT: As suggested by JMC, 
    ! adding river routing (DISCHARGE) to QFLX
    FRESHW = QFLX + DISCHARGE

    call MAPL_GetResource( MAPL, ocean_dir, label='OCEAN_DIR:', rc=status ) ; VERIFY_(STATUS)
    call str4c( iarr, TRIM(ocean_dir) )

! We may need this in the future (Udi)
!    where (WGHT<=0.0) PS = 0.0 

! Put import data into internal state
!------------------------------------
    CALL DRIVER_SET_IMPORT_STATE( PrivateState%ptr,   'TAUX',   TAUX )
    CALL DRIVER_SET_IMPORT_STATE( PrivateState%ptr,   'TAUY',   TAUY )
    CALL DRIVER_SET_IMPORT_STATE( PrivateState%ptr,  'TAUXI',  TAUXI )
    CALL DRIVER_SET_IMPORT_STATE( PrivateState%ptr,  'TAUYI',  TAUYI )
    CALL DRIVER_SET_IMPORT_STATE( PrivateState%ptr,     'PS',     PS )
    CALL DRIVER_SET_IMPORT_STATE( PrivateState%ptr, 'SWHEAT', SWHEAT )
    CALL DRIVER_SET_IMPORT_STATE( PrivateState%ptr,   'QFLX',   FRESHW )
    deallocate(FRESHW)
    CALL DRIVER_SET_IMPORT_STATE( PrivateState%ptr,   'HFLX',   HFLX )
    CALL DRIVER_SET_IMPORT_STATE( PrivateState%ptr,   'SFLX',   SFLX )
    CALL DRIVER_SET_IMPORT_STATE( PrivateState%ptr,   'LATS',   LATS )
    CALL DRIVER_SET_IMPORT_STATE( PrivateState%ptr,   'LONS',   LONS )
    CALL DRIVER_SET_IMPORT_STATE( PrivateState%ptr,   'WGHT',   WGHT )

    CALL DRIVER_SET_IMPORT_STATE( PrivateState%ptr,   'FRACICE', FRI )
    CALL DRIVER_SET_IMPORT_STATE( PrivateState%ptr,   'VOLICE',  VOLICE )
    CALL DRIVER_SET_IMPORT_STATE( PrivateState%ptr,   'VOLSNO',  VOLSNO )
    CALL DRIVER_SET_IMPORT_STATE( PrivateState%ptr,   'ERGICE',  ERGICE )
    CALL DRIVER_SET_IMPORT_STATE( PrivateState%ptr,   'ERGSNO',  ERGSNO )
    CALL DRIVER_SET_IMPORT_STATE( PrivateState%ptr,   'MPOND',   MPOND )
    CALL DRIVER_SET_IMPORT_STATE( PrivateState%ptr,   'TAUAGE',  TAUAGE )
    CALL DRIVER_SET_IMPORT_STATE( PrivateState%ptr,   'TI',  TI )
    CALL DRIVER_SET_IMPORT_STATE( PrivateState%ptr,   'SI',  SI )
    CALL DRIVER_SET_IMPORT_STATE( PrivateState%ptr,   'HI',  HI )

    call MAPL_GetResource( MAPL, passive_ocean, label='STEADY_STATE_OCEAN:', &
         default=1, rc=status ) ; VERIFY_(STATUS)

    call setdir(iarr)
    if (passive_ocean /= 0) CALL DRIVER_RUN( PrivateState%ptr, 1 )
    deallocate(iarr)
    call popdir

    CALL MAPL_GetPointer(EXPORT,   US,   'US', RC=STATUS); VERIFY_(STATUS)
    CALL MAPL_GetPointer(EXPORT,   VS,   'VS', RC=STATUS); VERIFY_(STATUS)
    CALL MAPL_GetPointer(EXPORT,   TS,   'TS', RC=STATUS); VERIFY_(STATUS)
    CALL MAPL_GetPointer(EXPORT,   SS,   'SS', RC=STATUS); VERIFY_(STATUS)
    CALL MAPL_GetPointer(EXPORT, MASK, trim(COMP_NAME)//'_3D_MASK', RC=STATUS)

! Sea ice exports
    call MAPL_GetPointer(EXPORT, FRACICE,'FRACICE', RC=STATUS); VERIFY_(STATUS)
    CALL MAPL_GetPointer(EXPORT,   UI,   'UI', RC=STATUS); VERIFY_(STATUS)
    CALL MAPL_GetPointer(EXPORT,   VI,   'VI', RC=STATUS); VERIFY_(STATUS)
    CALL MAPL_GetPointer(EXPORT, TAUXIe, 'TAUXI', RC=STATUS); VERIFY_(STATUS)
    CALL MAPL_GetPointer(EXPORT, TAUYIe, 'TAUYI', RC=STATUS); VERIFY_(STATUS)
    CALL MAPL_GetPointer(EXPORT, TAUXBOT, 'TAUXBOT', RC=STATUS); VERIFY_(STATUS)
    CALL MAPL_GetPointer(EXPORT, TAUYBOT, 'TAUYBOT', RC=STATUS); VERIFY_(STATUS)
    
    CALL MAPL_GetPointer(EXPORT, DELFRACICE,'DELFRACICE', alloc=.true., RC=STATUS); VERIFY_(STATUS)
    CALL MAPL_GetPointer(EXPORT, DELTI,   'DELTI', alloc=.true., RC=STATUS); VERIFY_(STATUS)
    CALL MAPL_GetPointer(EXPORT, DELSI,   'DELSI', alloc=.true., RC=STATUS); VERIFY_(STATUS)
    CALL MAPL_GetPointer(EXPORT, DELVOLICE, 'DELVOLICE', alloc=.true., RC=STATUS); VERIFY_(STATUS)
    CALL MAPL_GetPointer(EXPORT, DELVOLSNO, 'DELVOLSNO', alloc=.true., RC=STATUS); VERIFY_(STATUS)
    CALL MAPL_GetPointer(EXPORT, DELERGICE, 'DELERGICE', alloc=.true., RC=STATUS); VERIFY_(STATUS)
    CALL MAPL_GetPointer(EXPORT, DELERGSNO, 'DELERGSNO', alloc=.true., RC=STATUS); VERIFY_(STATUS)
    CALL MAPL_GetPointer(EXPORT, DELMPOND, 'DELMPOND', alloc=.true., RC=STATUS); VERIFY_(STATUS)
    CALL MAPL_GetPointer(EXPORT, DELTAUAGE, 'DELTAUAGE', alloc=.true., RC=STATUS); VERIFY_(STATUS)
    CALL MAPL_GetPointer(EXPORT, DELHI, 'DELHI', alloc=.true., RC=STATUS); VERIFY_(STATUS)

    CALL DRIVER_GET_EXPORT_STATE( PrivateState%ptr,   'US',   US )
    CALL DRIVER_GET_EXPORT_STATE( PrivateState%ptr,   'VS',   VS )
    CALL DRIVER_GET_EXPORT_STATE( PrivateState%ptr,   'TS',   TS )
    CALL DRIVER_GET_EXPORT_STATE( PrivateState%ptr,   'SS',   SS )
    CALL DRIVER_GET_EXPORT_STATE( PrivateState%ptr, 'MASK', MASK )

    CALL DRIVER_GET_EXPORT_STATE( PrivateState%ptr,   'UI',   UI )
    CALL DRIVER_GET_EXPORT_STATE( PrivateState%ptr,   'VI',   VI )
!    CALL DRIVER_GET_EXPORT_STATE( PrivateState%ptr,'TAUXIe',TAUXI )
!    CALL DRIVER_GET_EXPORT_STATE( PrivateState%ptr,'TAUYIe',TAUYI )
    TAUXIe = TAUXI
    TAUYIe = TAUYI
    CALL DRIVER_GET_EXPORT_STATE( PrivateState%ptr,'TAUXBOT',TAUXBOT )
    CALL DRIVER_GET_EXPORT_STATE( PrivateState%ptr,'TAUYBOT',TAUYBOT )
!    CALL DRIVER_GET_EXPORT_STATE( PrivateState%ptr,'FRACICE',FRACICE )

!    CALL DRIVER_GET_EXPORT_STATE( PrivateState%ptr,'DELFRACICE',   DELFRACICE )
!    CALL DRIVER_GET_EXPORT_STATE( PrivateState%ptr,'DELTI',   DELTI )
!    CALL DRIVER_GET_EXPORT_STATE( PrivateState%ptr,'DELSI',DELSI )
!    CALL DRIVER_GET_EXPORT_STATE( PrivateState%ptr,'DELVOLICE',DELVOLICE )
!    CALL DRIVER_GET_EXPORT_STATE( PrivateState%ptr,'DELVOLSNO',DELVOLSNO )
!    CALL DRIVER_GET_EXPORT_STATE( PrivateState%ptr,'DELERGICE',   DELERGICE )
!    CALL DRIVER_GET_EXPORT_STATE( PrivateState%ptr,'DELERGSNO',   DELERGSNO )
!    CALL DRIVER_GET_EXPORT_STATE( PrivateState%ptr,'DELMPOND',DELMPOND )
!    CALL DRIVER_GET_EXPORT_STATE( PrivateState%ptr,'DELTAUAGE',DELTAUAGE )
!    CALL DRIVER_GET_EXPORT_STATE( PrivateState%ptr,'DELHI',DELHI )

! ALT: for now, until JM/DM fix the code, we do this
    FRACICE = FRI

!ALT: next lines are totally fake 
!     There somewhat realistic to avoid GEOS-5 hick-ups until we have coupling!
!!!    TS = 285.
!@    US = 0.01
!@    VS = 0.01
!@    SS = 30.
!@    MASK=1.

    CALL MAPL_TimerOff(MAPL,"RUN"   )
    CALL MAPL_TimerOff(MAPL,"TOTAL" )

! All Done
!---------
    call WRITE_PARALLEL( ' Finished plug run method ' )
    RETURN_(ESMF_SUCCESS)
  end subroutine Run

    !BOP
    
! !IROUTINE: Finalize        -- Finalize method for GuestOcean wrapper

! !INTERFACE:

  subroutine Finalize ( gc, import, export, clock, rc )

! !ARGUMENTS:

  type(ESMF_GridComp), intent(INOUT) :: gc     ! Gridded component 
  type(ESMF_State),    intent(INOUT) :: import ! Import state
  type(ESMF_State),    intent(INOUT) :: export ! Export state
  type(ESMF_Clock),    intent(INOUT) :: clock  ! The supervisor clock
  integer, optional,   intent(  OUT) :: rc     ! Error code:

!EOP

    type (MAPL_MetaComp), pointer          :: MAPL 

! ErrLog Variables

    character(len=ESMF_MAXSTR)       :: IAm
    integer                          :: STATUS
    character(len=ESMF_MAXSTR)       :: COMP_NAME

! Locals with MOM types


! Get the target components name and set-up traceback handle.
! -----------------------------------------------------------

    Iam = "Finalize"
    call ESMF_GridCompGet( gc, NAME=comp_name, RC=status )
    VERIFY_(STATUS)
    Iam = trim(comp_name) // Iam

! Get my internal MAPL_Generic state
!-----------------------------------

    call MAPL_GetObjectFromGC ( GC, MAPL, RC=STATUS)
    VERIFY_(STATUS)

! Profilers
!----------

    call MAPL_TimerOn(MAPL,"TOTAL"   )
    call MAPL_TimerOn(MAPL,"FINALIZE")

    call MAPL_GenericFinalize( GC, IMPORT, EXPORT, CLOCK, RC=status )
    VERIFY_(STATUS)

! All Done
!---------

    RETURN_(ESMF_SUCCESS)
  end subroutine Finalize

!====================================================================

end module MIT_GEOS5PlugMod
