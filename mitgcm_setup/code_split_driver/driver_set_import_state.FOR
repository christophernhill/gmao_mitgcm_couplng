#include "CPP_OPTIONS.h"
      MODULE DRIVER_SET_IMPORT_STATE_MOD
C     A GEOS-5 compatible version of set import state.
C     Imports variables from MAPL that are used as
C     boundary/forcing conditions of MITgcm.
C     Mapping from MAPL data structure to MITgcm 
C     five-dimensional structures takes place here.
C
C     o called from "plug" code as
C     CALL DRIVER_SET_IMPORT_STATE( wrap%ptr,   'TAUX',   TAUX )
C

C     Temp until BNDY_STATE code is written
      USE IMPORT_STATE_MOD

#define G5KIND      4
#define REAL_       real(kind=G5KIND)

      INTERFACE DRIVER_SET_IMPORT_STATE
       MODULE PROCEDURE DRIVER_SET_IMPORT_STATE_2D
       MODULE PROCEDURE DRIVER_SET_IMPORT_STATE_3D
      END INTERFACE

      CONTAINS
CBOP
C     !ROUTINE: DRIVER_SET_IMPORT_STATE
C     !INTERFACE:
      SUBROUTINE DRIVER_SET_IMPORT_STATE_2D( mitgcmIState, code, fld)
C     !USES:
      USE MITGCM_STATE_MOD , ONLY :
     &    MITGCM_ISTATE
#include "SIZE.h"

C     == Routine arguments ==
      TYPE(MITGCM_ISTATE), POINTER :: mitgcmIState
      CHARACTER*(*) code
      REAL_, pointer :: fld(:,:)

C     == Local variables ==
      INTEGER i, j, bi, bj, iSLo, jSLo

      IF ( code .EQ. 'TAUX' ) THEN
!      Copy fld to TX
       DO bj=1,nSy
        DO bi=1,nSx
         jSLo = (bj-1)*sNy
         iSLo = (bi-1)*sNx
         DO j=1,sNy
          DO i=1,sNx
           mitgcmIState%import%TX(iSLo+i,jSLo+j) = fld(iSLo+i,jSLo+j)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
      IF ( code .EQ. 'TAUY' ) THEN
!      Copy fld to TY
       DO bj=1,nSy
        DO bi=1,nSx
         jSLo = (bj-1)*sNy
         iSLo = (bi-1)*sNx
         DO j=1,sNy
          DO i=1,sNx
           mitgcmIState%import%TY(iSLo+i,jSLo+j) = fld(iSLo+i,jSLo+j)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
      IF ( code .EQ. 'PS' ) THEN
!      Copy fld to PS
       DO bj=1,nSy
        DO bi=1,nSx
         jSLo = (bj-1)*sNy
         iSLo = (bi-1)*sNx
         DO j=1,sNy
          DO i=1,sNx
           mitgcmIState%import%PS(iSLo+i,jSLo+j) = fld(iSLo+i,jSLo+j)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
      IF ( code .EQ. 'QFLX' ) THEN
!      Copy fld to QFLX
       DO bj=1,nSy
        DO bi=1,nSx
         jSLo = (bj-1)*sNy
         iSLo = (bi-1)*sNx
         DO j=1,sNy
          DO i=1,sNx
           mitgcmIState%import%QFLX(iSLo+i,jSLo+j) = fld(iSLo+i,jSLo+j)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
      IF ( code .EQ. 'HFLX' ) THEN
!      Copy fld to HFLX
       DO bj=1,nSy
        DO bi=1,nSx
         jSLo = (bj-1)*sNy
         iSLo = (bi-1)*sNx
         DO j=1,sNy
          DO i=1,sNx
           mitgcmIState%import%HFLX(iSLo+i,jSLo+j) = fld(iSLo+i,jSLo+j)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
      IF ( code .EQ. 'SFLX' ) THEN
!      Copy fld to SFLX
       DO bj=1,nSy
        DO bi=1,nSx
         jSLo = (bj-1)*sNy
         iSLo = (bi-1)*sNx
         DO j=1,sNy
          DO i=1,sNx
           mitgcmIState%import%SFLX(iSLo+i,jSLo+j) = fld(iSLo+i,jSLo+j)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
      IF ( code .EQ. 'LATS' ) THEN
!      Copy fld to LAT
       DO bj=1,nSy
        DO bi=1,nSx
         jSLo = (bj-1)*sNy
         iSLo = (bi-1)*sNx
         DO j=1,sNy
          DO i=1,sNx
           mitgcmIState%import%LAT(iSLo+i,jSLo+j) = fld(iSLo+i,jSLo+j)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
      IF ( code .EQ. 'LONS' ) THEN
!      Copy fld to LON
       DO bj=1,nSy
        DO bi=1,nSx
         jSLo = (bj-1)*sNy
         iSLo = (bi-1)*sNx
         DO j=1,sNy
          DO i=1,sNx
           mitgcmIState%import%LON(iSLo+i,jSLo+j) = fld(iSLo+i,jSLo+j)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF

C
      RETURN
      END SUBROUTINE

      SUBROUTINE DRIVER_SET_IMPORT_STATE_3D( mitgcmIState, code, fld)
C     !USES:
      USE MITGCM_STATE_MOD , ONLY :
     &    MITGCM_ISTATE
      IMPLICIT NONE
#include "SIZE.h"

C     == Routine arguments ==
      TYPE(MITGCM_ISTATE), POINTER :: mitgcmIState
      CHARACTER*(*) code
      REAL_, pointer :: fld(:,:,:)

C     == Local variables ==
      INTEGER i, j, k, bi, bj, iSLo, jSLo

      IF ( code .EQ. 'SWHEAT' ) THEN
!      Copy fld to SWHEAT
       DO k=1,Nr
        DO bj=1,nSy
         DO bi=1,nSx
          jSLo = (bj-1)*sNy
          iSLo = (bi-1)*sNx
          DO j=1,sNy
           DO i=1,sNx
            mitgcmIState%import%SWHEAT(iSLo+i,jSLo+j,k)
     &             = fld(iSLo+i,jSLo+j,k)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END SUBROUTINE
      END MODULE

