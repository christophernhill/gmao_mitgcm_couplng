!#ifdef ALLOW_AUTODIFF_TAMC

!#ifdef ALLOW_TAMC_CHECKPOINTING

!     nchklev_1 :: length of inner loop (=size of storage in memory)
!     nchklev_2 :: length of second loop (stored on disk)
!     nchklev_3 :: length of outer loop of 3-level checkpointing
      INTEGER    nchklev_1
      PARAMETER( nchklev_1 =   2 )
      INTEGER    nchklev_2
      PARAMETER( nchklev_2 =  50 )
      INTEGER    nchklev_3
      PARAMETER( nchklev_3 =  50 )

!#endif /* ALLOW_TAMC_CHECKPOINTING */

!     TAMC keys:
!     ==========

      COMMON /TAMC_KEYS_I/ ikey_dynamics
      INTEGER ikey_dynamics

!     isbyte :: precision of tapes (both memory and disk).
!               For smaller tapes replace 8 by 4.
      INTEGER    isbyte
      PARAMETER( isbyte    = 8 )

!     maxpass :: maximum number of (active + passive) tracers
!                Note: defined in PTRACERS_SIZE.h if compiling pkg/ptracers
#ifndef ALLOW_PTRACERS
      INTEGER    maxpass
      PARAMETER( maxpass   = 3 )
#endif
!     maxcube :: for Multi-Dim advection, max number of horizontal directions
      INTEGER    maxcube
      PARAMETER( maxcube   = 3 )

#ifdef ALLOW_CG2D_NSA
!     Parameter that is needed for the tape complev_cg2d_iter
!     cannot be smaller than the allowed number of iterations in cg2d
!     (numItersMax >= cg2dMaxIters in data-file)
      INTEGER numItersMax
      PARAMETER ( numItersMax = 200 )
#endif

!#endif /* ALLOW_AUTODIFF_TAMC */
