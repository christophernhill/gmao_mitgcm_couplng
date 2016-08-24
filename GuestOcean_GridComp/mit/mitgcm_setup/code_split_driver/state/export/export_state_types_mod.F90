! $Id: $

#include "CPP_OPTIONS.h"

      MODULE EXPORT_STATE_TYPES_MOD
!     Dynamically allocated type for the export information in MITgcm

      IMPLICIT NONE

!     US - surface currents (cell-centered, along latitude circles)
!     VS - surface currents (cell-centered, along longitude lines)
!     SA - sin of angle between local coordinate lines and lat-lon coordinate lines
!     CA - cos of angle between local coordinate lines and lat-lon coordinate lines
!     TS - sea surface temperature in K
!     SS - sea surface salinty     in g/kg
!     DS - ocean surface-level thickness in m
      TYPE MITGCM_EXPORT  
       SEQUENCE
       _RL , POINTER :: US(   :,:) => NULL()
       _RL , POINTER :: VS(   :,:) => NULL()
       _RL , POINTER :: SA(   :,:) => NULL()
       _RL , POINTER :: CA(   :,:) => NULL()
       _RL , POINTER :: TS(   :,:) => NULL()
       _RL , POINTER :: CA(   :,:) => NULL()
       _RL , POINTER :: DS(   :,:) => NULL()
      END TYPE

      END MODULE
