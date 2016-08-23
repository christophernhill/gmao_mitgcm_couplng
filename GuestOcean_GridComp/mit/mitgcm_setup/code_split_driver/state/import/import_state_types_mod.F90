! $Id: $

#include "CPP_OPTIONS.h"

      MODULE IMPORT_STATE_TYPES_MOD
!     Dynamically allocated type for the import information in MITgcm
!     ** No halos **

      IMPLICIT NONE

!     TX     - surface stress (cell-centered, for now 
!              along EW latitude, +ve E)
!     TY     - surface stress (cell-centered, for now 
!              along NS longitude, +ve N => a mess at the pole)
!     PS     - surface pressure (cell-centered)
!     SWHEAT - shortwave heating (spread over several levels)
!     QFLX   - freshwater flux (from skin layer)
!     HFLX   - atmos boundary layer heat flux (sensible and latent I think).
!     SFLX   - salt flux (from skin layer)
      TYPE MITGCM_IMPORT  
       SEQUENCE
       _RL , POINTER :: TX(       :,:  ) => NULL()
       _RL , POINTER :: TY(       :,:  ) => NULL()
       _RL , POINTER :: PS(       :,:  ) => NULL()
       _RL , POINTER :: SWHEAT(   :,:,:) => NULL()
       _RL , POINTER :: QFLX(     :,:  ) => NULL()
       _RL , POINTER :: HFLX(     :,:  ) => NULL()
       _RL , POINTER :: SFLX(     :,:  ) => NULL()
      END TYPE

      END MODULE
