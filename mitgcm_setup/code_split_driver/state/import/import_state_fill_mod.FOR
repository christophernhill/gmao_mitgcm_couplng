! $Id: $

#include "CPP_OPTIONS.h"

      MODULE IMPORT_STATE_FILL_MOD

      USE IMPORT_STATE_TYPES_MOD
      IMPLICIT NONE

      INTERFACE FILL
       MODULE PROCEDURE MITGCM_IMPORT_FILL
      END INTERFACE

      CONTAINS

      SUBROUTINE MITGCM_IMPORT_FILL  ( importPtr )
!     -- Fill the imports for an MITgcm instance.
#include "SIZE.h"
#include "GRID.h"
#include "FFIELDS.h"

!     -- Routine arguments    --
!     -- Import state pointer --
      TYPE(MITGCM_IMPORT), POINTER :: importPtr

!     -- Local variables --
      TYPE(MITGCM_IMPORT), POINTER :: p
      INTEGER bi, bj, i, j, k, jSLo, iSLo
      INTEGER imin, imax, jmin, jmax
      PARAMETER ( imin = 1-OLx , imax = sNx+OLx )
      PARAMETER ( jmin = 1-OLy , jmax = sNy+OLy )
      _RL tmpU (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL tmpV (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

!     Transfer fields from importPtr to MITgcm forcing variables.
      p => importPtr
      DO bj=1,nSy
       DO bi=1,nSx
        jSLo = (bj-1)*sNy
        iSLo = (bi-1)*sNx
        DO j=1,sNy
         DO i=1,sNx

!     Transfer SWHEAT to Qsw.
!     SWHEAT : Solar heating rate spread over several levels in W/m^2.
!              > 0 for increase in theta (ocean warming)
!     Qsw    : Net upward shortwave radiation in W/m^2.
!              Qsw = - ( downward - ice and snow absorption - reflected )
!              > 0 for decrease in theta (ocean cooling)
!              Typical range: -350 < Qsw < 0
!              Southwest C-grid tracer point (i.e., A-grid)
          Qsw(i,j,bi,bj) = 0.0D0
          DO k=1,Nr
           Qsw(i,j,bi,bj) = Qsw(i,j,bi,bj) - p%SWHEAT(iSLo+i,jSLo+j,k)
          ENDDO

!     Transfer HFLX + SWHEAT to Qnet.
!     HFLX   : Turbulent heat flux from skin to ocean in W/m^2.
!              Includes sensible, latent, and longwave heat flux
!              that is transferred from skin to atmosphere.
!              > 0 for increase in theta (ocean warming)
!     Qnet   : Net upward surface heat flux (including shortwave) in W/m^2.
!              Qnet = latent + sensible + net longwave + net shortwave
!              > 0 for decrease in theta (ocean cooling)
!              Typical range: -250 < Qnet < 600
!              Southwest C-grid tracer point (i.e., A-grid)
          Qnet(i,j,bi,bj) = Qsw(i,j,bi,bj) - p%HFLX(iSLo+i,jSLo+j)          
          
!     Transfer QFLX to EMPMR.
!     QFLX   : Freshwater flux from skin to ocean in kg/m2/s.
!     EmPmR  : Net upward freshwater flux in kg/m2/s.
!              EmPmR = Evaporation - precipitation - runoff
!              > 0 for increase in salt (ocean salinity)
!              Typical range: -1e-4 < EmPmR < 1e-4
!              Southwest C-grid tracer point (i.e., A-grid)
! Note that runoff is missing.
! Will need to be specified separately in MITgcm or obtained from
! MIT_GEOS5PlugMod.F90 DISCHARGE (river_discharge_at_ocean_points).
          EmPmR(i,j,bi,bj) = - p%QFLX(iSLo+i,jSLo+j)

!     Transfer of SFLX to saltFlux
!     SFLX   : Salt flux from skin to ocean in TBD units.
!   saltFlux : Net upward salt flux in psu.kg/m^2/s.
!              flux of Salt taken out of the ocean per time unit (second).
!              Note: a) only used when salty sea-ice forms or melts.
!                    b) units: when salinity (unit= psu) is expressed
!                       in g/kg, saltFlux unit becomes g/m^2/s.
!              > 0 for decrease in SSS.
!              Southwest C-grid tracer point (i.e., A-grid)
! I am guessing that when there is evaporation:
!    (i) "salty water" flows into the skin layer from the ocean,
!    (ii) freshwater flows from skin layer to atmosphere, and
!    (iii) salt flows from skin layer to ocean.
! If SFLX is this third component of evaporation, it can be ignored for driving
! MITgcm since MITgcm only exports freshwater from ocean during evaporation.
! But SFLUX has both positive and negative numbers, so above decomposition may not be correct.
! Another detail that we will need to worry about later on is that in the presence of sea-ice
! the MITgcm exports “salty water” to the ice and then rejects some of that salt back to
! the ocean.  Also, the unit of SFLX specified in MIT_GEOS5PlugMod.F90 seem wrong.
! They probably should be g/m^2/s instead of N/m^2.
!         saltFlux(i,j,bi,bj) = p%SFLX(iSLo+i,jSLo+j)
          saltFlux(i,j,bi,bj) = 0.0D0

!     Transfer of PS to pLoad
!     PS     : Surface atmospheric pressure in Pa
!     pLoad  : for the ocean:      atmospheric pressure at z=eta
!              Units are           Pa=N/m^2
! For some reason code crashes if pLoad is set to p%PS.
!         pLoad(i,j,bi,bj) = p%PS(iSLo+i,jSLo+j)
          pLoad(i,j,bi,bj) = 0.0D0

!     Transfer TAUX and TAUY to fu and fv.
!     TAUX   : A-grid eastward stress on skin in N/m^2.
!     TAUY   : A-grid northward stress on skin in N/m^2.
!     fu     : Zonal surface wind stress in N/m^2.
!              > 0 for increase in uVel
!              Typical range: -0.5 < fu < 0.5
!              Southwest C-grid U point
!     fv     : Meridional surface wind stress in N/m^2.
!              > 0 for increase in vVel
!              Typical range: -0.5 < fv < 0.5
!              Southwest C-grid V point

!     Rotate stress from East/North to MITgcm grid orientation.
          tmpU(i,j,bi,bj) =
     &         angleCosC(i,j,bi,bj) * p%TAUX(iSLo+i,jSLo+j) +
     &         angleSinC(i,j,bi,bj) * p%TAUY(iSLo+i,jSLo+j)
          tmpV(i,j,bi,bj) = -
     &         angleSinC(i,j,bi,bj) * p%TAUX(iSLo+i,jSLo+j) +
     &         angleCosC(i,j,bi,bj) * p%TAUY(iSLo+i,jSLo+j)

         ENDDO
        ENDDO
       ENDDO
      ENDDO

!     Interpolate stress from A-grid to C-grid locations.
      CALL EXCH_UV_AGRID_3D_RL(tmpU, tmpV, .TRUE., 1, 1)
      DO bj=1,nSy
       DO bi=1,nSx
        DO j = jmin,jmax
         DO i = imin,imax
          fu(i,j,bi,bj) = (tmpU(i,j,bi,bj)+tmpU(i-1,j,bi,bj)) *
     &                    0.5D0 * maskW(i,j,1,bi,bj)
          fv(i,j,bi,bj) = (tmpV(i,j,bi,bj)+tmpV(i,j-1,bi,bj)) *
     &                    0.5D0 * maskS(i,j,1,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      RETURN
      END SUBROUTINE

      END MODULE
