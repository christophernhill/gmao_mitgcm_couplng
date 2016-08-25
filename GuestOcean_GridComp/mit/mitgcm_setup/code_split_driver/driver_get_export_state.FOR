      MODULE DRIVER_GET_EXPORT_STATE_MOD
C     A GEOS-5 compatible version of get export state.
C     Exports variable from MITgcm that are used by other 
C     components in a MAPL tree.
C     Mapping from MITgcm five-dimensional to MAPL data
C     structures takes place here.

#define G5KIND      4
#define REAL_       real(kind=G5KIND)

      INTERFACE DRIVER_GET_EXPORT_STATE
       MODULE PROCEDURE DRIVER_GET_EXPORT_STATE_2D
       MODULE PROCEDURE DRIVER_GET_EXPORT_STATE_3D
      END INTERFACE

      CONTAINS
CBOP
C     !ROUTINE: DRIVER_GET_EXPORT_STATE
C     !INTERFACE:
      SUBROUTINE DRIVER_GET_EXPORT_STATE_2D( mitgcmIState, code, fld)
C     !USES:
      USE MITGCM_STATE_MOD , ONLY :
     &    MITGCM_ISTATE
      IMPLICIT NONE
#include "SIZE.h"

C     == Routine arguments ==
      TYPE(MITGCM_ISTATE), POINTER :: mitgcmIState
      CHARACTER*(*) code
      REAL_, pointer :: fld(:,:)

C     == Local variables ==
      INTEGER i, j, bi, bj, iSLo, jSLo

      IF ( code .EQ. 'TS' ) THEN
!      Copy TS to fld
       DO bj=1,nSy
        DO bi=1,nSx
         jSLo = (bj-1)*sNy
         iSLo = (bi-1)*sNx
         DO j=1,sNy
          DO i=1,sNx
          fld(iSLo+i,jSLo+j) = mitgcmIState%export%TS(iSLo+i,jSLo+j)
          ENDDO
         ENDDO
        ENDDO
       ENDDO

      ELSEIF ( code .EQ. 'DS' ) THEN
!      Copy DS to fld
       DO bj=1,nSy
        DO bi=1,nSx
         jSLo = (bj-1)*sNy
         iSLo = (bi-1)*sNx
         DO j=1,sNy
          DO i=1,sNx
          fld(iSLo+i,jSLo+j) = mitgcmIState%export%DS(iSLo+i,jSLo+j)
          ENDDO
         ENDDO
        ENDDO
       ENDDO

      ENDIF

      RETURN
      END SUBROUTINE

      SUBROUTINE DRIVER_GET_EXPORT_STATE_3D( mitgcmIState, code, fld)
      USE MITGCM_STATE_MOD , ONLY :
     &    MITGCM_ISTATE
C     == Routine arguments ==
      TYPE(MITGCM_ISTATE), POINTER :: mitgcmIState
      CHARACTER*(*) code
      REAL_, pointer :: fld(:,:,:)

      print *,'DRIVER_GET_EXPORT_STATE_3D:'//trim(code)
     &      //'. This code is stubbed!'

      RETURN
      END SUBROUTINE

      END MODULE
