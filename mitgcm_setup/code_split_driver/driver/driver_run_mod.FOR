      MODULE DRIVER_RUN_MOD
C $Header: /u/gcmpack/MITgcm_contrib/PRM/multi_comp_setup/comp_mitgcm/code_basic/driver_run.F,v 1.1.1.1 2006/10/10 18:17:28 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      CONTAINS

CBOP
C     !ROUTINE: DRIVER_RUN
C     !INTERFACE:
      SUBROUTINE DRIVER_RUN( mitgcmIState, nTimeS)
!     SUBROUTINE DRIVER_RUN( mitgcmIState, nTimeS,
!    &                         iLoop,
!    &                         myCurrentTime,
!    &                         myCurrentIter )


C     !DESCRIPTION: \bv
C     *==================================================================
C     | SUBROUTINE driver_run
C     | o External driver control routine for MITgcm forward step
C     |   execution phase. 
C     *==================================================================
C     |
C     | DRIVER routines are used to control the MITgcm code from an external
C     | driver. This routine invokes the forward phase of code execution.
C     | The driver here is written for an ocean configuration and is designed
C     | for use with either an argument based call/return interface or with a
C     | messaging based "event loop" interface.
C     |
C     *==================================================================
C     \ev

C     !USES:
      USE MITGCM_STATE_MOD , ONLY :
     &    MITGCM_ISTATE,
     &    MITGCM_STACKVARS,
     &    RESTORE_STATE,
     &    SAVE_STATE

      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "PARAMS.h"
#include "GRID.h"
C #include "OCNCPL.h"

C     == Routine arguments ==
      INTEGER nTimeS
      TYPE(MITGCM_ISTATE), POINTER :: mitgcmIState

C     == Local variables ==
      INTEGER i,j,bi,bj
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*13 fNam
      INTEGER iStep
      TYPE(MITGCM_STACKVARS) sv
      INTEGER myThid
      INTEGER myCurrentIter
      _RL     myCurrentTime
      INTEGER iLoop

C     == Variables for reading TAUX from import state part of the internal
C     == state pointer that is passed in as an argument.
      _RL, pointer                         ::   TAUX(:,:  )
      _RL, pointer                         ::   TAUY(:,:  )
      _RL, pointer                         ::   PS(  :,:  )
      _RL, pointer                         ::   LAT( :,:  )
      _RL, pointer                         ::   LON( :,:  )
      CHARACTER*1024  tFnam
      INTEGER dUnit
      INTEGER i0, j0

!     Variables used in getting MPI rank
      INTEGER myRank, mpiRC
CEOP

C     CALL COMP_OCN_PUSH_CONTEXT(1)
!     Get MPI global rank
!     RETURN

      myThid = 1

C     Extract the import fields
C     MITgcm FFIELDS taux0
      TAUX => mitgcmIState%import%tx
      TAUY => mitgcmIState%import%ty
      PS   => mitgcmIState%import%ps
      LAT  => mitgcmIState%import%lat
      LON  => mitgcmIState%import%lon


C     myThid = 1
C     myCurrentTime = startTime
C     myCurrentIter = nIter0
C     myThid        = 1
C     nTimesteps    = 1

C     CALL MONITOR( myCurrentIter, myCurrentTime, myThid )
      CALL RESTORE_STATE( mitgcmIState, sv )
      myCurrentTime = sv%myTime
      myCurrentIter = sv%myIter
      iLoop         = sv%iLoop
      myRank = mpiMyId

C     Write TAUX to a current iter numbered file
      WRITE(tFnam,'(A12,I6.6,A,I10.10,A5)') 'taux_import.',myRank,'.',
     &       myCurrentIter,'.data'
      CALL MDSFINDUNIT( dUnit, myThid )
      OPEN( dUnit, file=tFnam, status='UNKNOWN',
     &        form='UNFORMATTED')
      WRITE(dUnit) TAUX
      CLOSE(dUnit)
C     Write TAUY to a current iter numbered file
      WRITE(tFnam,'(A12,I6.6,A,I10.10,A5)') 'tauy_import.',myRank,'.',
     &       myCurrentIter,'.data'
      CALL MDSFINDUNIT( dUnit, myThid )
      OPEN( dUnit, file=tFnam, status='UNKNOWN',
     &        form='UNFORMATTED')
      WRITE(dUnit) TAUY
      CLOSE(dUnit)
C     Write PS to a current iter numbered file
      WRITE(tFnam,'(A12,I6.6,A,I10.10,A5)') 'ps_import.',myRank,'.',
     &       myCurrentIter,'.data'
      CALL MDSFINDUNIT( dUnit, myThid )
      OPEN( dUnit, file=tFnam, status='UNKNOWN',
     &        form='UNFORMATTED')
      WRITE(dUnit) PS
      CLOSE(dUnit)
C     Write LAT to a current iter numbered file
      WRITE(tFnam,'(A12,I6.6,A,I10.10,A5)') 'lat_import.',myRank,'.',
     &       myCurrentIter,'.data'
      CALL MDSFINDUNIT( dUnit, myThid )
      OPEN( dUnit, file=tFnam, status='UNKNOWN',
     &        form='UNFORMATTED')
      WRITE(dUnit) LAT
      CLOSE(dUnit)
C     Write LON to a current iter numbered file
      WRITE(tFnam,'(A12,I6.6,A,I10.10,A5)') 'lon_import.',myRank,'.',
     &       myCurrentIter,'.data'
      CALL MDSFINDUNIT( dUnit, myThid )
      OPEN( dUnit, file=tFnam, status='UNKNOWN',
     &        form='UNFORMATTED')
      WRITE(dUnit) LON
      CLOSE(dUnit)

C     OPEN()
C     CLOSE()
      

      DO iStep = 1, nTimeS
       CALL FORWARD_STEP( iLoop, myCurrentTime, 
     &                    myCurrentIter, myThid )
       iLoop = iLoop+1
      ENDDO

      sv%myTime = myCurrentTime
      sv%myIter = myCurrentIter
      sv%iLoop  = iLoop
      CALL SAVE_STATE( mitgcmIState, sv )
C     Fill the export fields

!cnhdbg      CLOSE(errorMessageUnit)
!cnhdbg      CLOSE(standardMessageUnit)
C     CALL COMP_OCN_POP_CONTEXT(1)
      CALL POPDIR

      RETURN
      END SUBROUTINE

      END MODULE
