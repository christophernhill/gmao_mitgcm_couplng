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

      IF ( code .EQ. 'US' ) THEN
!     Copy US to fld
         fld = mitgcmIState%export%US

      ELSEIF ( code .EQ. 'VS' ) THEN
!     Copy VS to fld
         fld = mitgcmIState%export%VS

      ELSEIF ( code .EQ. 'TS' ) THEN
!     Copy TS to fld
         fld = mitgcmIState%export%TS

      ELSEIF ( code .EQ. 'SS' ) THEN
!     Copy SS to fld
          fld = mitgcmIState%export%SS
!=========== JM/DM => Please note that this sea ice section is stubbed!
!=====================================================================
! just look for !@@@@ and uncomment once done (and delete 0.0)
      ELSEIF ( code .EQ. 'UI' ) THEN
!     Copy UI to fld
         fld = 0.0 !@@@@ mitgcmIState%export%UI
      ELSEIF ( code .EQ. 'VI' ) THEN
!     Copy VI to fld
         fld = 0.0 !@@@@ mitgcmIState%export%VI
      ELSEIF ( code .EQ. 'TAUXI' ) THEN
!     Copy TAUXI to fld
         fld = 0.0 !@@@@ mitgcmIState%export%TAUXI
      ELSEIF ( code .EQ. 'TAUYI' ) THEN
!     Copy TAUYI to fld
         fld = 0.0 !@@@@ mitgcmIState%export%TAUYI
      ELSEIF ( code .EQ. 'TAUXBOT' ) THEN
!     Copy TAUXBOT to fld
         fld = 0.0 !@@@@ mitgcmIState%export%TAUXBOT
      ELSEIF ( code .EQ. 'TAUYBOT' ) THEN
!     Copy TAUYBOT to fld
         fld = 0.0 !@@@@ mitgcmIState%export%TAUYBOT

      ELSE

!      print *,'DRIVER_GET_EXPORT_STATE_2D:'//trim(code)
!    &      //'. This code is stubbed!'

      ENDIF

      RETURN
      END SUBROUTINE

      SUBROUTINE DRIVER_GET_EXPORT_STATE_3D( mitgcmIState, code, fld)
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

      IF ( code .EQ. 'MASK' ) THEN
       IF ( ASSOCIATED(fld) ) THEN
             fld = mitgcmIState%export%MASK
       ELSE
        print *,'DRIVER_GET_EXPORT_STATE_3D:'//trim(code)
     &      //' : Pointer not associated!'
       ENDIF

      ELSEIF ( code .EQ. 'FRACICE' ) THEN
!     Copy FRACICE to fld
!ALT: Attention JM/DM: the next line is stubbed!
!==============================================
! This should be uncommented and the line after that deleted!
! Until fixed, we are copying FRACICE from the Import
!@@@@         fld = mitgcmIState%export%FRAICE
         fld = mitgcmIState%import%FRAICE
      ELSE

       print *,'DRIVER_GET_EXPORT_STATE_3D:'//trim(code)
     &      //'. This code is stubbed!'

      ENDIF

      RETURN
      END SUBROUTINE

      END MODULE
