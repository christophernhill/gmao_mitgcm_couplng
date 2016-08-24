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
      _RL, POINTER :: tmp1(:,:,:,:)
      _RL, POINTER :: tmp2(:,:,:,:)
      _RL, POINTER :: tmp3(:,:,:,:)
      _RL, POINTER :: tmp4(:,:,:,:)
      _RL, POINTER :: tmp5(:,:,:,:)

!     -- Put flow vectors on a-grid and rotate to geographical coordinate system.
      p => exportPtr

#if 0
!     o 1 Create dummy flow field from a stream function that varies with latitude only.
      DO bj=1,nSy
       DO bi=1,nSx
        jSLo = (bj-1)*sNy
        iSLo = (bi-1)*sNx
        DO j=1-1,sNy+1
         DO i=1-1,sNx+1
!         o Flow on C-grid
          p%US(iSlo+1,jSlo+1) =  ( yG(i,j+1,bi,bj) - yG(i,j,bi,bj) )*
     &                           _recip_dyG(i,j,bi,bj)
          p%VS(iSlo+1,jSlo+1) = -( yG(i+1,j,bi,bj) - yG(i,j,bi,bj) )*
     &                           _recip_dxG(i,j,bi,bj)
          iSLo=iSlo+1
         ENDDO
         jSLo=jSlo+1
        ENDDO
       ENDDO
      ENDDO
      DO bj=1,nSy
       DO bi=1,nSx
        jSLo = (bj-1)*sNy
        iSLo = (bi-1)*sNx
        DO j=1-1,sNy+1
         DO i=1-1,sNx+1
!         o Interpolate to A-grid
          p%US(iSlo+1,jSlo+1) =  ( yG(i,j+1,bi,bj) - yG(i,j,bi,bj) )*
     &                           _recip_dyG(i,j,bi,bj)
          p%VS(iSlo+1,jSlo+1) = -( yG(i+1,j,bi,bj) - yG(i,j,bi,bj) )*
     &                           _recip_dxG(i,j,bi,bj)
          iSLo=iSlo+1
         ENDDO
         jSLo=jSlo+1
        ENDDO
       ENDDO
      ENDDO
#endif

!     store surface fields to export
      k=1
      DO bj=1,nSy
       DO bi=1,nSx
        jSLo = (bj-1)*sNy
        iSLo = (bi-1)*sNx
        DO j=1,sNy
         DO i=1,sNx
          tmpU = 0.5*(
     &                uVel(i  ,j,k,bi,bj) +
     &                uVel(i+1,j,k,bi,bj)
     &               )
          tmpV = 0.5*(
     &                vVel(i,j  ,k,bi,bj) +
     &                vVel(i,j+1,k,bi,bj)
     &               )
          p%US(iSLo+i,jSLo+j) = tmpU
          p%VS(iSLo+i,jSLo+j) = tmpV
!@#          iSLo=iSlo+1
         ENDDO
!@#         jSLo=jSlo+1
        ENDDO

        DO j=1,sNy
         DO i=1,sNx
          p%TS(iSLo+i,jSLo+j) = theta(i,j,k,bi,bj) + celsius2K
          p%SS(iSLo+i,jSLo+j) =  salt(i,j,k,bi,bj)
          p%DS(iSLo+i,jSLo+j) = hFacC(i,j,k,bi,bj)*drF(k)
         ENDDO
        ENDDO

       ENDDO
      ENDDO

      RETURN
      END SUBROUTINE

      END MODULE
