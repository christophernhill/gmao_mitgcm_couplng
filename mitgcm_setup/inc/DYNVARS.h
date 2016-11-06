! $Header: /u/gcmpack/MITgcm/model/inc/DYNVARS.h,v 1.29 2005/11/24 03:44:50 heimbach Exp $
! $Name:  $

!BOP
!     !ROUTINE: DYNVARS.h
!     !INTERFACE:
!     include "DYNVARS.h"
!     !DESCRIPTION:
!     \bv
!     *==========================================================*
!     | DYNVARS.h                                                 
!     | o Dynamical model variables (common block DYNVARS_R)      
!     *==========================================================*
!     | The value and two levels of time tendency are held for    
!     | each prognostic variable.                                 
!     *==========================================================*
!     \ev
!EOP
!
!     etaN  - free-surface r-anomaly (r unit) at current time level
!     uVel  - zonal velocity (m/s, i=1 held at western face)
!     vVel  - meridional velocity (m/s, j=1 held at southern face)
!     theta - potential temperature (oC, held at pressure/tracer point)
!     salt  - salinity (ppt, held at pressure/tracer point)
!     gX, gxNm1 - Time tendencies at current and previous time levels.
!     etaH   - surface r-anomaly, advanced in time consistently 
!              with 2.D flow divergence (Exact-Conservation): 
!                etaH^n+1 = etaH^n - delta_t*Div.(H^n U^n)   
!  note: a) used with "exactConserv" but strictly necessary for NonLinFreeSurf
!        b) same as etaN but not necessarely at the same time, e.g.:
!           implicDiv2DFlow=0 => etaH=etaN ; =1 => etaH=etaNm1 ;

#ifdef ALLOW_ADAMSBASHFORTH_3
      COMMON /DYNVARS_R/
     &                   etaN,
     &                   uVel,vVel,wVel,theta,salt,
     &                   gU,   gV,   gT,   gS,
     &                   guNm, gvNm, gtNm, gsNm
#else /* ALLOW_ADAMSBASHFORTH_3 */
      COMMON /DYNVARS_R/
     &                   etaN,
     &                   uVel,vVel,wVel,theta,salt,
     &                   gU,   gV,   gT,   gS,
     &                   guNm1,gvNm1,gtNm1,gsNm1
#endif /* ALLOW_ADAMSBASHFORTH_3 */
      _RL  etaN  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  uVel (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  vVel (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  wVel (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  theta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  salt (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gT(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#ifdef ALLOW_ADAMSBASHFORTH_3
      _RL  guNm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,2)
      _RL  gvNm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,2)
      _RL  gtNm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,2)
      _RL  gsNm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,2)
#else /* ALLOW_ADAMSBASHFORTH_3 */
      _RL  guNm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gvNm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gtNm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gsNm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif /* ALLOW_ADAMSBASHFORTH_3 */

      COMMON /DYNVARS_R_2/
     &                   etaH
      _RL  etaH  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

!ph(
!ph the following block will eventually move to a separate
!ph header file containing requires anomaly fields of control vars.
!ph
#if (defined (ALLOW_AUTODIFF_TAMC) && defined (ALLOW_DIFFKR_CONTROL))
      COMMON /DYNVARS_DIFFKR/
     &                       diffKr
      _RL  diffKr (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif
#if (defined (ALLOW_AUTODIFF_TAMC) && defined (ALLOW_KAPGM_CONTROL))
      COMMON /DYNVARS_KAPGM/
     &                       kapgm
      _RL  kapgm  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif
#if (defined (ALLOW_AUTODIFF_TAMC) && defined (ALLOW_BOTTOMDRAG_CONTROL))
      COMMON /DYNVARS_BOTTOMDRAG/
     &                       bottomdragfld
      _RL  bottomdragfld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif
!ph
!ph)

!     diagnostic variables: 
!     phiHydLow  :: Phi-Hydrostatic at r-lower boundary
!                  (bottom in z-coordinates, top in p-coordinates)
!     totPhiHyd :: total hydrostatic Potential (anomaly, for now), 
!                  at cell center level ; includes surface contribution.
!                 (for diagnostic + used in Z-coord with EOS_funct_P)
!     IVDConvCount :: Impl.Vert.Diffusion convection counter:
!                   = 0 (not convecting) or 1 (convecting)
      COMMON /DYNVARS_DIAG/ phiHydLow, totPhiHyd, IVDConvCount
      _RL  phiHydLow(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  totPhiHyd(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  IVDConvCount(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
