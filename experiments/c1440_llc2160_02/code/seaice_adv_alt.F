#include "SEAICE_OPTIONS.h"

CBOP
C !ROUTINE: SEAICE_ADV_ALT

C !INTERFACE: ==========================================================
      SUBROUTINE SEAICE_ADV_ALT(
     U                  uc, vc,
     I                  myTime, myIter, myThid )

C !DESCRIPTION: \bv
C     *===========================================================*
C     | SUBROUTINE SEAICE_ADVDIFF
C     | o driver for different advection routines
C     |   calls an adaption of gad_advection to call different
C     |   advection routines of pkg/generic_advdiff
C     *===========================================================*
C \ev

C !USES: ===============================================================
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"
#include "SEAICE_TRACER.h"
#ifdef HACK_FOR_GMAO_CPL
# include "SEAICE_LAYERS.h"
#endif

C !INPUT/OUTPUT PARAMETERS: ===================================================
C     === Routine arguments ===
C     uc/vc     :: current ice velocity on C-grid;
C               :: C-Grid : Input only ; B-grid : Output only
C     myTime    :: current time in simulation
C     myIter    :: current iteration number in simulation
C     myThid    :: my Thread Id number
      _RL uc   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vc   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL myTime
      INTEGER myIter
      INTEGER myThid

#ifdef SEAICE_ITD
C !LOCAL VARIABLES: ====================================================
C     === Local variables ===
C     i,j,bi,bj :: Loop counters
C     n         :: Loop counter for ice thickness categories
C     uTrans    :: volume transport, x direction
C     vTrans    :: volume transport, y direction
C     afx       :: horizontal advective flux, x direction
C     afy       :: horizontal advective flux, y direction
C     gFld      :: tendency of seaice field
C     xA,yA     :: "areas" of X and Y face of tracer cells
      INTEGER i, j, bi, bj
      INTEGER n
      _RL uTrans    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL afx       (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL afy       (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#ifdef HACK_FOR_GMAO_CPL
      INTEGER l
      _RS maskOce   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uTrIce    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrIce    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL iceVol    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL iceFld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL iceSav    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL meanCellArea, areaEpsil, vol_Epsil
#endif /* HACK_FOR_GMAO_CPL */
      _RL gFld      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS xA        (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS yA        (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL recip_heff(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP
#ifdef HACK_FOR_GMAO_CPL
      INTEGER SEAICEadvSchQice, SEAICEadvSchQsnow
      INTEGER SEAICEadvSchMltPd
      _RL SIq0
      SEAICEadvSchQice  = 0
      SEAICEadvSchQsnow = 0
      SEAICEadvSchMltPd = 0
      SEAICEadvSchQice  = SEAICEadvSchHeff
      SEAICEadvSchQsnow = SEAICEadvSchSnow
c     SEAICEadvSchMltPd = SEAICEadvSchSnow
      SIq0 = -SEAICE_lhFusion
#endif /* HACK_FOR_GMAO_CPL */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      IF ( .NOT.SEAICEmultiDimAdvection ) THEN
C     just for safety
        WRITE(msgBuf,'(2A)') 'SEAICE_ADVDIFF: ',
     &    'ITD with SEAICEmultiDimAdvection=.False. is not allowed,'
        CALL PRINT_ERROR( msgBuf , myThid )
        WRITE(msgBuf,'(2A)') 'SEAICE_ADVDIFF: ',
     &    'use a multidimensional advection scheme (in data.seaice)'
        CALL PRINT_ERROR( msgBuf , myThid )
        CALL ALL_PROC_DIE( myThid )
        STOP 'ABNORMAL END: S/R SEAICE_ADVDIFF'
      ENDIF

C--   make a local copy of the velocities for compatibility with B-grid
C--   alternatively interpolate to C-points if necessary
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
#ifndef SEAICE_CGRID /* not SEAICE_CGRID = BGRID */
C--   hack to ensure backward compatibility:
C     average B-grid seaice velocity to C-grid
        DO j=1-OLy,sNy+OLy-1
         DO i=1-OLx,sNx+OLx-1
          uc(i,j,bi,bj)=.5 _d 0*(UICE(i,j,bi,bj)+UICE(i,j+1,bi,bj))
          vc(i,j,bi,bj)=.5 _d 0*(VICE(i,j,bi,bj)+VICE(i+1,j,bi,bj))
         ENDDO
        ENDDO
#endif /* SEAICE_CGRID */
       ENDDO
      ENDDO

#ifndef SEAICE_CGRID
C     Do we need this? I am afraid so.
      CALL EXCH_UV_XY_RL(uc,vc,.TRUE.,myThid)
#endif /* not SEAICE_CGRID */

#ifdef HACK_FOR_GMAO_CPL
C-    Just to get the right order of magnitude for mean grid-cell area:
      meanCellArea = Nx*Ny
      meanCellArea = globalArea / meanCellArea
      areaEpsil = 1. _d -10 * meanCellArea
      vol_Epsil = 1. _d -15 * meanCellArea
#endif /* HACK_FOR_GMAO_CPL */

#ifdef ALLOW_GENERIC_ADVDIFF

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
C---   loops on tile indices bi,bj

C-    compute cell areas used by all tracers
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
#ifdef HACK_FOR_GMAO_CPL
          iceVol(i,j) = 0.
          iceFld(i,j) = 0.
          iceSav(i,j) = 0.
          maskOce(i,j) = HEFFM(i,j,bi,bj)
#endif /* HACK_FOR_GMAO_CPL */
          xA(i,j) = _dyG(i,j,bi,bj)*SIMaskU(i,j,bi,bj)
          yA(i,j) = _dxG(i,j,bi,bj)*SIMaskV(i,j,bi,bj)
          recip_heff(i,j)    = 1. _d 0
         ENDDO
        ENDDO

C-    Calculate "volume transports" through tracer cell faces.
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          uTrans(i,j) = uc(i,j,bi,bj)*xA(i,j)
          vTrans(i,j) = vc(i,j,bi,bj)*yA(i,j)
         ENDDO
        ENDDO

        IF ( SEAICEadvArea ) THEN
C-    open water fraction needs to be advected for the ridging scheme
         CALL SEAICE_ADVECTION(
     I        GAD_AREA, SEAICEadvSchArea,
     I        uc(1-OLx,1-OLy,bi,bj), vc(1-OLx,1-OLy,bi,bj),
     I        uTrans, vTrans, opnWtrFrac(1-OLx,1-OLy,bi,bj), recip_heff,
     O        gFld, afx, afy,
     I        bi, bj, myTime, myIter, myThid )
C-    Add tendency due to diffusion
         IF ( SEAICEdiffKhArea .GT. 0. _d 0 )
     &        CALL SEAICE_DIFFUSION(
     I        GAD_AREA, SEAICEdiffKhArea, ONE,
     I        opnWtrFrac(1-OLx,1-OLy,bi,bj), HEFFM,
     I        xA(1-OLx,1-OLy), yA(1-OLx,1-OLy),
     U        gFld,
     I        bi, bj, myTime, myIter, myThid )
C     now do the "explicit" time step
         DO j=1,sNy
          DO i=1,sNx
           opnWtrFrac(i,j,bi,bj) = HEFFM(i,j,bi,bj) * (
     &          opnWtrFrac(i,j,bi,bj) + SEAICE_deltaTtherm * gFld(i,j)
     &          )
          ENDDO
         ENDDO
        ENDIF

C--   Fractional area
        IF ( SEAICEadvArea ) THEN
         DO n=1,SEAICE_multDim
C--   Fractional area for each category
          CALL SEAICE_ADVECTION(
     I         GAD_AREA, SEAICEadvSchArea,
     I         uc(1-OLx,1-OLy,bi,bj), vc(1-OLx,1-OLy,bi,bj),
     I         uTrans, vTrans, AREAITD(1-OLx,1-OLy,n,bi,bj),
     I         recip_heff,
     O         gFld, afx, afy,
     I         bi, bj, myTime, myIter, myThid )
C-    Add tendency due to diffusion
          IF ( SEAICEdiffKhArea .GT. 0. _d 0 )
     &         CALL SEAICE_DIFFUSION(
     I         GAD_AREA, SEAICEdiffKhArea, ONE,
     I         AREAITD(1-OLx,1-OLy,n,bi,bj), HEFFM,
     I         xA(1-OLx,1-OLy), yA(1-OLx,1-OLy),
     U         gFld,
     I         bi, bj, myTime, myIter, myThid )
C     now do the "explicit" time step
          DO j=1,sNy
           DO i=1,sNx
            AREAITD(i,j,n,bi,bj) = HEFFM(i,j,bi,bj) * (
     &           AREAITD(i,j,n,bi,bj) + SEAICE_deltaTtherm * gFld(i,j)
     &           )
           ENDDO
          ENDDO
         ENDDO
        ENDIF

#ifdef HACK_FOR_GMAO_CPL
        IF ( SEAICEadvSchMltPd.NE.0 ) THEN
         DO n=1,SEAICE_multDim
          CALL GAD_ICE_ADVECTION(
     I           GAD_QICE2, SEAICEadvSchMltPd, .TRUE.,
     I           uTrans, vTrans, maskOce, SEAICE_deltaTtherm, areaEpsil,
     U           iceVol, SImeltPd(1-OLx,1-OLy,n,bi,bj),
     O           afx, afy,
     I           bi, bj, myTime, myIter, myThid )
         ENDDO
        ENDIF
#endif /* HACK_FOR_GMAO_CPL */

C--   Effective Snow Thickness (Volume)
        IF ( SEAICEadvSnow ) THEN
         DO n=1,SEAICE_multDim
#ifdef HACK_FOR_GMAO_CPL
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             iceSav(i,j) = HSNOWITD(i,j,n,bi,bj)
           ENDDO
          ENDDO
          CALL GAD_ICE_ADVECTION(
     I           GAD_SNOW, SEAICEadvSchSnow, .TRUE.,
     I           uTrans, vTrans, maskOce, SEAICE_deltaTtherm, areaEpsil,
     U           iceVol, HSNOWITD(1-OLx,1-OLy,n,bi,bj),
     O           uTrIce, vTrIce,
     I           bi, bj, myTime, myIter, myThid )
          IF ( SEAICEadvSchQsnow.NE.0 ) THEN
C--   Advect snow enthaly with snow-volume fluxes (for each snow category)
           DO l=1,nIceLayers
            DO j=1-OLy,sNy+OLy
             DO i=1-OLx,sNx+OLx
              iceVol(i,j) = iceSav(i,j)*rA(i,j,bi,bj)
              IF ( iceSav(i,j).GE.vol_Epsil ) THEN
                iceFld(i,j) = SIqSnow(i,j,l,n,bi,bj)/iceSav(i,j)
              ELSE
                iceFld(i,j) = SIq0
              ENDIF
             ENDDO
            ENDDO
            CALL GAD_ICE_ADVECTION(
     I           GAD_QICE2, SEAICEadvSchQsnow, .FALSE.,
     I           uTrIce, vTrIce, maskOce, SEAICE_deltaTtherm, vol_Epsil,
     U           iceVol, iceFld,
     O           afx, afy,
     I           bi, bj, myTime, myIter, myThid )
            DO j=1-OLy,sNy+OLy
             DO i=1-OLx,sNx+OLx
              SIqSnow(i,j,l,n,bi,bj) = iceFld(i,j)
     &                               * HSNOWITD(i,j,n,bi,bj)
             ENDDO
            ENDDO
           ENDDO
          ENDIF
#else /* HACK_FOR_GMAO_CPL */
          CALL SEAICE_ADVECTION(
     I         GAD_SNOW, SEAICEadvSchSnow,
     I         uc(1-OLx,1-OLy,bi,bj), vc(1-OLx,1-OLy,bi,bj),
     I         uTrans, vTrans, HSNOWITD(1-OLx,1-OLy,n,bi,bj),
     I         recip_heff,
     O         gFld, afx, afy,
     I         bi, bj, myTime, myIter, myThid )
C--   Add tendency due to diffusion
          IF ( SEAICEdiffKhSnow .GT. 0. _d 0 )
     &         CALL SEAICE_DIFFUSION(
     I         GAD_SNOW, SEAICEdiffKhSnow, ONE,
     I         HSNOWITD(1-OLx,1-OLy,n,bi,bj), HEFFM,
     I         xA(1-OLx,1-OLy), yA(1-OLx,1-OLy),
     U         gFld,
     I         bi, bj, myTime, myIter, myThid )
C     now do the "explicit" time step
          DO j=1,sNy
           DO i=1,sNx
            HSNOWITD(i,j,n,bi,bj) = HEFFM(i,j,bi,bj) * (
     &           HSNOWITD(i,j,n,bi,bj) + SEAICE_deltaTtherm*gFld(i,j)
     &           )
           ENDDO
          ENDDO
#endif /* HACK_FOR_GMAO_CPL */
         ENDDO
        ENDIF

C--   Effective Thickness (Volume)
        IF ( SEAICEadvHeff ) THEN
         DO n=1,SEAICE_multDim
#ifdef HACK_FOR_GMAO_CPL
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             iceSav(i,j) =  HEFFITD(i,j,n,bi,bj)
           ENDDO
          ENDDO
          CALL GAD_ICE_ADVECTION(
     I           GAD_HEFF, SEAICEadvSchHeff, .TRUE.,
     I           uTrans, vTrans, maskOce, SEAICE_deltaTtherm, areaEpsil,
     U           iceVol, HEFFITD(1-OLx,1-OLy,n,bi,bj),
     O           uTrIce, vTrIce,
     I           bi, bj, myTime, myIter, myThid )
          IF ( SEAICEadvSchQice.NE.0 ) THEN
C--   Advect ice enthaly with ice-volume fluxes (for each ice category)
           DO l=1,nIceLayers
            DO j=1-OLy,sNy+OLy
             DO i=1-OLx,sNx+OLx
              iceVol(i,j) = iceSav(i,j)*rA(i,j,bi,bj)
              IF ( iceSav(i,j).GE.vol_Epsil ) THEN
                iceFld(i,j) = SIqIce(i,j,l,n,bi,bj)/iceSav(i,j)
              ELSE
                iceFld(i,j) = SIq0
              ENDIF
             ENDDO
            ENDDO
            CALL GAD_ICE_ADVECTION(
     I           GAD_QICE1, SEAICEadvSchQice, .FALSE.,
     I           uTrIce, vTrIce, maskOce, SEAICE_deltaTtherm, vol_Epsil,
     U           iceVol, iceFld,
     O           afx, afy,
     I           bi, bj, myTime, myIter, myThid )
            DO j=1-OLy,sNy+OLy
             DO i=1-OLx,sNx+OLx
              SIqIce(i,j,l,n,bi,bj) = iceFld(i,j)
     &                              * HEFFITD(i,j,n,bi,bj)
             ENDDO
            ENDDO
           ENDDO
          ENDIF
#else /* HACK_FOR_GMAO_CPL */
          CALL SEAICE_ADVECTION(
     I         GAD_HEFF, SEAICEadvSchHeff,
     I         uc(1-OLx,1-OLy,bi,bj), vc(1-OLx,1-OLy,bi,bj),
     I         uTrans, vTrans, HEFFITD(1-OLx,1-OLy,n,bi,bj),
     I         recip_heff,
     O         gFld, afx, afy,
     I         bi, bj, myTime, myIter, myThid )
C-    Add tendency due to diffusion
          IF ( SEAICEdiffKhHeff .GT. 0. _d 0 )
     &         CALL SEAICE_DIFFUSION(
     I         GAD_HEFF, SEAICEdiffKhHeff, ONE,
     I         HEFFITD(1-OLx,1-OLy,n,bi,bj), HEFFM,
     I         xA(1-OLx,1-OLy), yA(1-OLx,1-OLy),
     U         gFld,
     I         bi, bj, myTime, myIter, myThid )
C     now do the "explicit" time step
          DO j=1,sNy
           DO i=1,sNx
            HEFFITD(i,j,n,bi,bj) = HEFFM(i,j,bi,bj) * (
     &           HEFFITD(i,j,n,bi,bj) + SEAICE_deltaTtherm * gFld(i,j)
     &           )
           ENDDO
          ENDDO
#endif /* HACK_FOR_GMAO_CPL */
         ENDDO
        ENDIF

C     update mean ice thickness HEFF and total ice concentration AREA
C     to match single category values
C     (necessary here because updated HEFF is used below for SItracer)
        CALL SEAICE_ITD_SUM(bi, bj, myTime, myIter, myThid)

C---   end bi,bj loops
       ENDDO
      ENDDO

#else /* not ALLOW_GENERIC_ADVDIFF */
      WRITE(msgBuf,'(2A)')
     &  'SEAICE_ADVDIFF: cannot use SEAICEmultiDimAdvection',
     &  ' without pkg/generic_advdiff'
      CALL PRINT_ERROR( msgBuf , myThid )
      WRITE(msgBuf,'(2A)') 'SEAICE_ADVDIFF: ',
     &  'Re-compile with pkg "generic_advdiff" in packages.conf'
      CALL PRINT_ERROR( msgBuf , myThid )
      CALL ALL_PROC_DIE( myThid )
      STOP 'ABNORMAL END: S/R SEAICE_ADVDIFF'
#endif /* ALLOW_GENERIC_ADVDIFF */

#endif /* SEAICE_ITD */

      RETURN
      END
