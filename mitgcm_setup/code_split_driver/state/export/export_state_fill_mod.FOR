! $Id: $

#include "CPP_OPTIONS.h"

      MODULE EXPORT_STATE_FILL_MOD

      USE EXPORT_STATE_TYPES_MOD
      IMPLICIT NONE

      INTERFACE FILL
       MODULE PROCEDURE MITGCM_EXPORT_FILL
      END INTERFACE

      CONTAINS

      SUBROUTINE MITGCM_EXPORT_FILL  ( exportPtr )
!     -- Fill the exports for an MITgcm instance.
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

!     -- Routine arguments    --
!     -- Export state pointer --
      TYPE(MITGCM_EXPORT), POINTER :: exportPtr

!     -- Local variables --
      TYPE(MITGCM_EXPORT), POINTER :: p
      INTEGER bi, bj, i, j, k, jSLo, iSLo
      _RL tmpU, tmpV

!     -- Put flow vectors on a-grid and rotate to geographical coordinate system.
      p => exportPtr

!     store surface fields to export
      k=1
      DO bj=1,nSy
       DO bi=1,nSx
        jSLo = (bj-1)*sNy
        iSLo = (bi-1)*sNx
        DO j=1,sNy
         DO i=1,sNx

          p%TS(iSLo+i,jSLo+j) = theta(i,j,k,bi,bj) + celsius2K
          p%SS(iSLo+i,jSLo+j) =  salt(i,j,k,bi,bj)

!     Interpolate stress from C-grid to A-grid locations.
          tmpU = 0.5*(uVel(i  ,j,k,bi,bj) + uVel(i+1,j,k,bi,bj))
          tmpV = 0.5*(vVel(i,j  ,k,bi,bj) + vVel(i,j+1,k,bi,bj))

!     Rotate stress from MITgcm grid orientation to East/North.
          p%US(iSLo+i,jSLo+j) = angleCosC(i,j,bi,bj) * tmpU -
     &                          angleSinC(i,j,bi,bj) * tmpV
          p%VS(iSLo+i,jSLo+j) = angleSinC(i,j,bi,bj) * tmpU +
     &                          angleCosC(i,j,bi,bj) * tmpV

         ENDDO
        ENDDO

       ENDDO
      ENDDO

!     store 3-D mask to export
      DO bj=1,nSy
       DO bi=1,nSx
        jSLo = (bj-1)*sNy
        iSLo = (bi-1)*sNx
        DO k=1,Nr
         DO j=1,sNy
          DO i=1,sNx
           p%MASK(iSLo+i,jSLo+j,k) =  maskC(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      RETURN
      END SUBROUTINE

      END MODULE
