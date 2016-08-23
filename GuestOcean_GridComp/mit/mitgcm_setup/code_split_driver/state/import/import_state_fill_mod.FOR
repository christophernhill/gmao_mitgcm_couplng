! $Id: $

#include "CPP_OPTIONS.h"

      MODULE IMPORT_STATE_FILL_MOD

      USE IMPORT_STATE_TYPES_MOD
      IMPLICIT NONE

      INTERFACE FILL
       MODULE PROCEDURE MITGCM_IMPORT_FILL
      END INTERFACE

      CONTAINS

      SUBROUTINE MITGCM_IMPORT_FILL  ( importPtr )
!     -- Fill the imports for an MITgcm instance.
#include "SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"

!     -- Routine arguments    --
!     -- Export state pointer --
      TYPE(MITGCM_IMPORT), POINTER :: importPtr

!     -- Local variables --
      TYPE(MITGCM_IMPORT), POINTER :: p
      INTEGER bi, bj, i, j, k, jSLo, iSLo

      RETURN
      END SUBROUTINE

      END MODULE
