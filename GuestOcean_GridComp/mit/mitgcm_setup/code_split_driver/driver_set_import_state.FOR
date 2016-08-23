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
!#include "GRID.h"
!#include "DYNVARS.h"
C     == Routine arguments ==
      TYPE(MITGCM_ISTATE), POINTER :: mitgcmIState
      CHARACTER*(*) code
      REAL_, pointer :: fld(:,:)
C
      IF ( code .EQ. 'TAUX' ) THEN
!      Copy fld to US
       DO bj=1,nSy
        DO bi=1,nSx
         jSLo = (bj-1)*sNy
         iSLo = (bi-1)*sNx
         DO j=1,sNy
          DO i=1,sNx
           mitgcmIState%import%TX(iSLo+i,jSLo+j) = fld(iSLo+i,jSLo+j)
!@#           iSLo=iSlo+1
          ENDDO
!@#          jSLo=jSlo+1
         ENDDO
        ENDDO
       ENDDO
      ENDIF

C
      RETURN
      END SUBROUTINE

      SUBROUTINE DRIVER_SET_IMPORT_STATE_3D( mitgcmIState, code, fld)
      USE MITGCM_STATE_MOD , ONLY :
     &    MITGCM_ISTATE
C     == Routine arguments ==
      TYPE(MITGCM_ISTATE), POINTER :: mitgcmIState
      CHARACTER*(*) code
      REAL_, pointer :: fld(:,:,:)

      print *,'DRIVER_SET_IMPORT_STATE_3D:'//trim(code)//'. This code is stubbed!'
      END SUBROUTINE
      END MODULE

