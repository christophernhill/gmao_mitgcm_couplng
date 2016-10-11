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
C     !ROUTINE: DRIVER_SET_IMPORT_STATE
C     !INTERFACE:
      SUBROUTINE DRIVER_GET_EXPORT_STATE_2D( mitgcmIState, code, fld)
C     !USES:
      USE MITGCM_STATE_MOD , ONLY :
     &    MITGCM_ISTATE
C     == Routine arguments ==
      TYPE(MITGCM_ISTATE), POINTER :: mitgcmIState
      CHARACTER*(*) code
      REAL_, pointer :: fld(:,:)
      RETURN
      END SUBROUTINE
      SUBROUTINE DRIVER_GET_EXPORT_STATE_3D( mitgcmIState, code, fld)
      USE MITGCM_STATE_MOD , ONLY :
     &    MITGCM_ISTATE
C     == Routine arguments ==
      TYPE(MITGCM_ISTATE), POINTER :: mitgcmIState
      CHARACTER*(*) code
      REAL_, pointer :: fld(:,:,:)
      RETURN
      END SUBROUTINE

      END MODULE
