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
#include "FFIELDS.h"

!     -- Routine arguments    --
!     -- Export state pointer --
      TYPE(MITGCM_IMPORT), POINTER :: importPtr

!     -- Local variables --
      TYPE(MITGCM_IMPORT), POINTER :: p
      INTEGER bi, bj, i, j, k, jSLo, iSLo

!     -- Put flow vectors on a-grid and rotate to geographical coordinate system.
      p => importPtr

!     store surface fields to import
!DM These are temporary transfers for comparison with the *import*
!DM output files.  They will need to be rotatated, interpolated,
!DM and scaled as needed.
      DO bj=1,nSy
       DO bi=1,nSx
        jSLo = (bj-1)*sNy
        iSLo = (bi-1)*sNx
        DO j=1,sNy
         DO i=1,sNx
          EMPMR   (i,j,bi,bj) = p%QFLX(iSLo+i,jSLo+j)
          FU      (i,j,bi,bj) = p%TAUX(iSLo+i,jSLo+j)
          FV      (i,j,bi,bj) = p%TAUY(iSLo+i,jSLo+j)
          QNET    (i,j,bi,bj) = p%HFLX(iSLo+i,jSLo+j)          
          SALTFLUX(i,j,bi,bj) = p%SFLX(iSLo+i,jSLo+j)
          QSW     (i,j,bi,bj) = 0.0D0
          DO k=1,Nr
           QSW(i,j,bi,bj) = QSW(i,j,bi,bj) + p%SWHEAT(iSLo+i,jSLo+j,k)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      RETURN
      END SUBROUTINE

      END MODULE
