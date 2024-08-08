#include "EXF_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: EXF_GEOS_BULKF
C     !INTERFACE:
      SUBROUTINE EXF_GEOS_BULKF( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE EXF_GEOS_BULKF
C     | o Calculate bulk formula fluxes over open ocean
C     |   using GEOS formulation
C     *==========================================================*
C     \ev
C
C     !USES:
       IMPLICIT NONE
C     === Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"

#include "EXF_PARAM.h"
#include "EXF_FIELDS.h"
#include "EXF_CONSTANTS.h"

#ifdef ALLOW_AUTODIFF_TAMC
#include "tamc.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     input:
C     myTime  :: Current time in simulation
C     myIter  :: Current iteration number in simulation
C     myThid  :: My Thread Id number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
C     output:
CEOP

#ifdef USE_GEOS_BULKF
#ifdef ALLOW_ATM_TEMP
C     == external Functions

C     == Local variables ==
C     i,j      :: grid point indices
C     bi,bj    :: tile indices
      INTEGER i, j, ij
      INTEGER bi, bj
C---------------
      INTEGER IVWATER(sNx*sNy), Niter, IRUN, CHOOSEZ0
      _RL VUS(sNx*sNy), VVS(sNx*sNy), VT1(sNx*sNy), VT2(sNx*sNy)
      _RL VSH1(sNx*sNy), VSH2(sNx*sNy), VP(sNx*sNy), VPE(sNx*sNy)
      _RL VZ0(sNx*sNy), LAI(sNx*sNy), VHS(sNx*sNy)
      _RL VRHO(sNx*sNy), VKH(sNx*sNy), VKM(sNx*sNy), VUSTAR(sNx*sNy)
      _RL VXX(sNx*sNy), VYY(sNx*sNy), VCU(sNx*sNy), VCT(sNx*sNy)
      _RL VRIB(sNx*sNy), VZETA(sNx*sNy), VWS(sNx*sNy)
      _RL t2m(sNx*sNy), q2m(sNx*sNy), u2m(sNx*sNy), v2m(sNx*sNy)
      _RL t10m(sNx*sNy), q10m(sNx*sNy), u10m(sNx*sNy), v10m(sNx*sNy)
      _RL u50m(sNx*sNy), v50m(sNx*sNy)
      _RL dP_lev, dH_lev, tmpQs
C---------------
      _RL qSat(sNx*sNy)
      _RL recip_rhoConstFresh
      INTEGER ks, kl
      INTEGER tkey

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( usingPCoords ) THEN
       ks = Nr
       kl = Nr-1
      ELSE
       ks = 1
       kl = 2
      ENDIF

C--   for conversion (will multiply again by rhoConstFresh in EXF_MAPFIELDS)
      recip_rhoConstFresh = 1. _d 0/rhoConstFresh

C--   Atmospheric model surface level half dP (hPa) and Thickens (m)
C     for now fill in with constant value:
C     take 60 for current vertical resolution
c     dH_lev = 60.
c     dP_lev = 6.
      dH_lev = hu
      dP_lev = hu*0.1 _d 0

CADJ STORE vct,vp,vpe,vsh1,vt1,vus,vvs 
CADJ &     = comlev1, key=ikey_dynamics, kind=isbyte

C     Loop over tiles.
      DO bj = myByLo(myThid),myByHi(myThid)
       DO bi = myBxLo(myThid),myBxHi(myThid)

#ifdef ALLOW_AUTODIFF_TAMC
        tkey = bi + (bj-1)*nSx + (ikey_dynamics-1)*nSx*nSy
CADJ STORE vct,vp,vpe,vsh1,vt1,vus,vvs
CADJ &     = comlev1_bibj, key=tkey, byte=isbyte
#endif

C-    Initialise with constant value:
         DO ij = 1,sNx*sNy
           VKH(ij) =  0.
           VKM(ij) =  0.
           VT2(ij) = celsius2K
           VSH2(ij)= 0.
           t2m(ij) = celsius2K
           q2m(ij) = 0.
           u2m(ij) = 0.
           v2m(ij) = 0.
           VZ0(ij) = 0.
           LAI(ij) = 0.
           IVWATER(ij) = 1
           VHS(ij) = dH_lev
           qSat(ij) = 0.
         ENDDO

         ij = 0
         DO j = 1,sNy
          DO i = 1,sNx
           IF ( maskC(i,j,ks,bi,bj).EQ.oneRS ) THEN
            ij = ij+1
            VUS(ij) = uwind(i,j,bi,bj)
            VVS(ij) = vwind(i,j,bi,bj)
            VT1(ij) = atemp(i,j,bi,bj)
            VT2(ij) = theta(i,j,ks,bi,bj) + celsius2K
            VPE(ij) = apressure(i,j,bi,bj)*1. _d -2
            VP(ij)  = VPE(ij) - dP_lev
            VSH1(ij) = aqh(i,j,bi,bj)
C-  Using pkg/exf qSat expression:
c       tmpbulk = cvapor_fac*exp(-cvapor_exp/Tsf)
c       VSH2(i,j) = saltsat*tmpbulk/atmrho
c#ifdef EXF_CALC_ATMRHO
            VRHO(ij) = apressure(i,j,bi,bj)
     &       / ( 287.04 _d 0*VT1(ij)*( oneRL + humid_fac*VSH1(ij) ) )
c#else
c           VRHO(ij) = atmrho
c#endif
            tmpQs = cvapor_fac*EXP( -cvapor_exp/VT2(ij) )
            qSat(ij) = saltsat*tmpQs/VRHO(ij)
            VSH2(ij) = qSat(ij)
C-  Using GEOS qSat expression (taken from pkg/fizhi)
c       CALL QSAT(TT,P,Q,DQDT,.FALSE.)
c       VSH2(i,j) = Q
c          CALL QSAT(VT2(i,j),VPE(i,j),VSH2(i,j),tmpQs,.FALSE.)
C-- Setting to constant values was done above (commented out here):
C-  VZ0(i,j) = 'SURFACE ROUGHNESS (m)', but reset to 3.mm over WATER
c           VZ0(ij) = 0.
c           LAI(ij) = 0.
c           IVWATER(ij) = 1
C-  VHS = 'DEPTH OF SURFACE LAYER', in m,
c           VHS(ij) = dH_lev
           ENDIF
          ENDDO
         ENDDO
         Niter = 6   ! number of iterations
         IRUN = ij
         CHOOSEZ0 = 3  ! choice between 0,1,2,3

CADJ STORE vct = comlev1_bibj, key=tkey, byte=isbyte

         CALL helfsurface(
     &                     VUS, VVS, VT1, VT2, VSH1, VSH2, VP, VPE,
     &                     VZ0, LAI, IVWATER, VHS, Niter, IRUN,
     &                     VRHO, VKH, VKM, VUSTAR, VXX, VYY, VCU, VCT,
     &                     VRIB, VZETA, VWS, t2m, q2m, u2m, v2m,
     &                     t10m, q10m, u10m, v10m, u50m, v50m,
     &                     CHOOSEZ0 )

         ij = 0
         DO j = 1,sNy
          DO i = 1,sNx
C--------------
c         VRHO(i,j)    ! DENSITY AT SURFACE
c         VKH(i,j)     ! HEAT TRANSFER COEFFICIENT (CT*USTAR)
c         VKM(i,j)     ! MOMENTUM TRANSFER COEFFICIENT (CU*USTAR)
c         VUSTAR(i,j)  ! FRICTION VELOCITY
c         VXX(i,j)     ! PHIM(ZETA) - DIMENSIONLESS WIND SHEAR
c         VYY(i,j)     ! PHIH(ZETA) - DIMENSIONLESS TEMP GRADIENT
c         VCU(i,j)     ! MOMENTUM TRANSPORT COEFFICIENT
c         VCT(i,j)     ! HEAT TRANSPORT COEFFICIENT
c         VRIB(i,j)
c         VZETA(i,j)
c         VWS(i,j)
C-- all output arrays:
c         t2m(i,j), q2m(i,j), u2m(i,j), v2m(i,j), t10m(i,j),
c         q10m(i,j), u10m(i,j), v10m(i,j), u50m(i,j), v50m(i,j)
C--------------
           IF ( maskC(i,j,ks,bi,bj).EQ.oneRS ) THEN
            ij = ij+1
C-   Turbulent Fluxes:
C    Note: first version was using 2-m value: t2m,q2m,u2m,v2m
C          instead of input fields: atemp,aqh,uwind,vwind
            hs(i,j,bi,bj) = -atmcp*VKH(ij)
     &                     *( (theta(i,j,ks,bi,bj) + celsius2K) 
     &                       - atemp(i,j,bi,bj) )
            hl(i,j,bi,bj) = -flamb*VKH(ij)
     &                     *( qSat(ij) - aqh(i,j,bi,bj) )
            evap(i,j,bi,bj) = recip_rhoConstFresh*VKH(ij)
     &                     *( qSat(ij) - aqh(i,j,bi,bj) )
                           
c         IF ( useAtmWind .AND. useRelativeWind ) THEN
c         ELSEIF ( useAtmWind ) THEN
            ustress(i,j,bi,bj) = VKM(ij)*uwind(i,j,bi,bj)
            vstress(i,j,bi,bj) = VKM(ij)*vwind(i,j,bi,bj)
c         ENDIF

           ELSE
C-    else if ( maskC(i,j,ks,bi,bj) <> 1 )
            ustress(i,j,bi,bj) = 0. _d 0
            vstress(i,j,bi,bj) = 0. _d 0
            hflux  (i,j,bi,bj) = 0. _d 0
            evap   (i,j,bi,bj) = 0. _d 0
            hs     (i,j,bi,bj) = 0. _d 0
            hl     (i,j,bi,bj) = 0. _d 0
C-    end IF ( maskC(i,j,ks,bi,bj) = 1 ) / else
           ENDIF
          ENDDO
         ENDDO

C--   end bi,bj loops
       ENDDO
      ENDDO

#endif /* ALLOW_ATM_TEMP */
#endif /* USE_GEOS_BULKF */

      RETURN
      END
