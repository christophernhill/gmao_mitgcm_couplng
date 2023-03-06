#include "PACKAGES_CONFIG.h"

! !IROUTINE: helfsurface
! !INTERFACE:
       SUBROUTINE helfsurface( &
        VUS,VVS,VT1,VT2,VSH1,VSH2,VP,VPE, &
        VZ0,LAI,IVWATER,VHS,N,IRUN_LOC, &
        VRHO,VKH,VKM,VUSTAR,VXX,VYY,VCU,VCT,VRIB,VZETA,VWS, &
        t2m,q2m,u2m,v2m,t10m,q10m,u10m,v10m,u50m,v50m,CHOOSEZ0)
!**********************************************************************
!  SUBROUTINE helfsurface - COMPUTES SURFACE TRANSFER COEFFICIENTS
!
!   ARGUMENTS ::
!
!     INPUT:
!     ------
!    US            -         U - COMPONENT OF SURFACE WIND (m/sec)
!    VS            -         V - COMPONENT OF SURFACE WIND (m/sec)
!    THV1          -         VIRTUAL POTENTIAL TEMPERATURE AT TOP OF SURFACE LAYER (K)
!    TH1           -         POTENTIAL TEMPERATURE AT TOP OF SURFACE LAYER (K)
!    TH2           -         POTENTIAL TEMPERATURE AT GROUND (K)
!    SH1           -         SPECIFIC HUMIDITY AT TOP OF SURFACE LAYER (kg/kg)
!    SH2           -         SPECIFIC HUMIDITY AT GROUND (kg/kg)
!    PK            -         EVEN LEVEL PRESSURE ** KAPPA AT LEVEL TOP OF SURFACE LAYER
!    PKE           -         EDGE LEVEL PRESSURE ** KAPPA AT GROUND
!    PE            -         SURFACE PRESSURE (hPa)
!    Z0            -         SURFACE ROUGHNESS (m)
!    WATER         -         ARRAY WITH '1' OVER OCEANS
!    HS            -         DEPTH OF SURFACE LAYER
!    N             -         NUMBER OF helfsurface ITERATIONS (NOMINALLY 5 or 6)
!    CHOOSEZ0      -         INTEGER FLAG: 0 - L&P Z0, no high wind limit
!                                          1 - Edson Z0 for mom. and heat, high wind limit
!                                          2 - L&P Z0, high wind limit
!                                          3 - Edson Z0 for mom. only, high wind limit   ****** Default for GEOS
!     OUTPUT:
!     -------
!    RHO           -         DENSITY AT SURFACE
!    KH            -         HEAT TRANSFER COEFFICIENT (CT*USTAR)
!    KM            -         MOMENTUM TRANSFER COEFFICIENT (CU*USTAR)
!    USTAR         -         FRICTION VELOCITY
!    XX            -         PHIM(ZETA) - DIMENSIONLESS WIND SHEAR
!    YY            -         PHIH(ZETA) - DIMENSIONLESS TEMP GRADIENT
!    CU            -         MOMENTUM TRANSPORT COEFFICIENT
!    CT            -         HEAT TRANSPORT COEFFICIENT
!
!**********************************************************************
      implicit none
#include "SIZE_f90.h"
#ifdef ALLOW_AUTODIFF
#include "tamc_f90.h"
#endif

! Argument List Declarations
      integer lNx, lNy
      integer IRUN, IRUN_LOC
      integer n,CHOOSEZ0
      REAL*8 VUS(sNx*sNy),VVS(sNx*sNy),VT1(sNx*sNy),VT2(sNx*sNy),VSH1(sNx*sNy),VSH2(sNx*sNy)
      REAL*8 VPE(sNx*sNy),VP(sNx*sNy),VZ0(sNx*sNy),LAI(sNx*sNy),VHS(sNx*sNy)
      integer IVWATER(sNx*sNy)
      REAL*8 VRHO(sNx*sNy)
      REAL*8 VKM(sNx*sNy),VKH(sNx*sNy),VUSTAR(sNx*sNy),VXX(sNx*sNy)
      REAL*8 VYY(sNx*sNy),VCU(sNx*sNy),VCT(sNx*sNy),VRIB(sNx*sNy)
      REAL*8 VZETA(sNx*sNy),VWS(sNx*sNy)
      REAL*8, intent(OUT) :: t2m(sNx*sNy),q2m(sNx*sNy),u2m(sNx*sNy),v2m(sNx*sNy)
      REAL*8, intent(OUT) :: t10m(sNx*sNy),q10m(sNx*sNy),u10m(sNx*sNy),v10m(sNx*sNy)
      REAL*8, intent(OUT) :: u50m(sNx*sNy),v50m(sNx*sNy)
      LOGICAL LWATER
      integer IVBITRIB(sNx*sNy)

! Local Variables
      REAL*8 VHZ(sNx*sNy),VPSIM(sNx*sNy),VAPSIM(sNx*sNy),VPSIG(sNx*sNy),VPSIHG(sNx*sNy)
      REAL*8 VTEMP(sNx*sNy),VDZETA(sNx*sNy),VDZ0(sNx*sNy),VDPSIM(sNx*sNy)
      REAL*8 VDPSIH(sNx*sNy),VZH(sNx*sNy),VXX0(sNx*sNy),VYY0(sNx*sNy)
      REAL*8 VAPSIHG(sNx*sNy),VRIB1(sNx*sNy)
      REAL*8 VPSIH(sNx*sNy),VPSIH2(sNx*sNy),VH0(sNx*sNy)
      REAL*8 VX0PSIM(sNx*sNy),VG(sNx*sNy),VG0(sNx*sNy),VR1MG0(sNx*sNy)
      REAL*8 VZ2(sNx*sNy),VDZSEA(sNx*sNy),VAZ0(sNx*sNy),VXNUM1(sNx*sNy)
      REAL*8 VPSIGB2(sNx*sNy),VDX(sNx*sNy),VDXPSIM(sNx*sNy),VDY(sNx*sNy)
      REAL*8 VXNUM2(sNx*sNy),VDEN(sNx*sNy),VAWS1(sNx*sNy),VXNUM3(sNx*sNy)
      REAL*8 VXNUM(sNx*sNy),VDZETA1(sNx*sNy),VDZETA2(sNx*sNy)
      REAL*8 VZCOEF2(sNx*sNy),VZCOEF1(sNx*sNy),VTEMPLIN(sNx*sNy)
      REAL*8 VDPSIMC(sNx*sNy),VDPSIHC(sNx*sNy),VAHS(sNx*sNy)
      REAL*8 VTHV1(sNx*sNy),VTHV2(sNx*sNy),VTH1(sNx*sNy),VTH2(sNx*sNy),VPKE(sNx*sNy),VPK(sNx*sNy)

      REAL*8 vz0h(sNx*sNy),vh0h(sNx*sNy)
      REAL*8 dummy1(sNx*sNy),dummy2(sNx*sNy),dummy3(sNx*sNy),dummy4(sNx*sNy),dummy5(sNx*sNy),dummy6(sNx*sNy),dummy7(sNx*sNy)

! Local Variables
      REAL*8 USTMX3,USTZ0S,Z0MIN,H0BYZ0,USTH0S,H0VEG,Z0VEGM,PRFAC,Z0MAX
      REAL*8 XPFAC,DIFSQT
      PARAMETER ( USTMX3 =   0.0632456)
      PARAMETER ( USTZ0S =   0.2030325E-5)
      PARAMETER ( Z0MIN  =  USTZ0S/USTMX3)
      PARAMETER ( Z0MAX  =  USTZ0S/USTMX3)
      PARAMETER ( H0BYZ0 =    30.0    )
      PARAMETER ( USTH0S =  H0BYZ0*USTZ0S )
      PARAMETER ( Z0VEGM =   0.005    )
      PARAMETER ( H0VEG  =  H0BYZ0*Z0VEGM )  !! This prevents discontinuity
      PARAMETER ( PRFAC  = 0.595864   )
      PARAMETER ( XPFAC  = .55        )
      PARAMETER ( DIFSQT  = 3.872983E-3)

      REAL*8 psihdiag(sNx*sNy),psimdiag(sNx*sNy)
      REAL*8 rvk,vk2,bmdl(sNx*sNy)
      integer iwater,itype
      integer i,iter
!
! -- added by jmc:
#include "MAPL_CONST.h"
! -- end of jmc addition

      rvk = 1./MAPL_KARMAN
      vk2 = MAPL_KARMAN*MAPL_KARMAN
      DO I = 1,IRUN
      if( ivwater(i) .eq. 3 ) then
       BMDL(i)    = 0.
!scale BMDL(i)    = (MAPL_KARMAN * XPFAC * PRFAC / DIFSQT) * exp(-lai(i)*2.)
      else
       BMDL(i)    = (MAPL_KARMAN * XPFAC * PRFAC / DIFSQT)
      endif
      enddo

!     INITIALIZATION

      DO I = 1, sNx*sNy
       dummy1(I) = 0.
       dummy2(I) = 0.
       dummy3(I) = 0.
       dummy4(I) = 0.
       dummy5(I) = 0.
       dummy6(I) = 0.
       dummy7(I) = 0.
      ENDDO
      DO I = 1,IRUN
       VAHS(I) = 1. / VHS(I)
       VPKE(I) = VPE(I) ** MAPL_KAPPA
       VPK(I) = VP(I) ** MAPL_KAPPA
       VTH1(I) = VT1(I)/VPK(I)
       VTH2(I) = VT2(I)/VPKE(I)
       VTHV1(I) = VTH1(I)*( 1.0 + MAPL_VIREPS*VSH1(I))
       VTHV2(I) = VTH2(I)*( 1.0 + MAPL_VIREPS*VSH2(I))
      ENDDO

!     DETERMINE SURFACE WIND MAGNITUDE AND BULK RICHARDSON NUMBER
!
      DO I = 1,IRUN
       VWS(I) = max(VUS(I)*VUS(I) + VVS(I)*VVS(I),1.e-4)
       VRIB(I) = MAPL_CP*(VPKE(I)-VPK(I))*(VTHV1(I)-VTHV2(I)) / VWS(I)
       VWS(I) = SQRT( VWS(I) )
      ENDDO

!  INITIAL GUESS FOR ROUGHNESS LENGTH Z0 OVER WATER
!
      IWATER = 0
      DO 9002 I = 1,IRUN
       IF (IVWATER(I).EQ.1)  IWATER = IWATER + 1
 9002 CONTINUE
      LWATER = .FALSE.
      IF(IWATER.GE.1)LWATER = .TRUE.
!
      IF(LWATER)THEN
       DO 9004 I = 1,IRUN
        IF (IVWATER(I).EQ.1) VZ0(I) = 0.0003
 9004  CONTINUE
      ENDIF
!ADJ STORE vz0 = comlev1, key=ikey_dynamics, kind=isbyte
      do i = 1,irun
       vh0(i) = h0byz0 * vz0(i)
       if(vz0(i).ge.z0vegm)vh0(i) = h0veg
      enddo
       DO I = 1,IRUN
        VZ0H(I) = 0.001
       ENDDO

!     CU AND PSIHG FOR NEUTRALLY STRATIFIED FLOW
!
      DO 9006 I = 1,IRUN
       VHZ(I) = (VHS(I) / VZ0(I) + 1.)
       VPSIM(I) = LOG( VHZ(I) )
       VAPSIM(I) = 1. / VPSIM(I)
       VCU(I) = MAPL_KARMAN * VAPSIM(I)
       VUSTAR(I) = VCU(I) * VWS(I)
!
       VPSIG(I) = BMDL(i)*sqrt(max(VH0(I)*VUSTAR(I)-USTH0S,0.))
       VPSIHG(I) = VPSIM(I) + VPSIG(I)
 9006 CONTINUE

!
!     LINEAR CORRECTION FOR ERROR IN ROUGHNESS LENGTH Z0
!
!ADJ STORE vaz0,vden,vdpsih,vdpsim,vdx,vdxpsim, &
!ADJ       vdy,vdzeta,vtemp,vtemplin,vx0psim,vxnum1,vxnum2,vz0 &
!ADJ       = comlev1, key=ikey_dynamics, kind=isbyte
!ADJ STORE vz0 = comlev1, key=ikey_dynamics, kind=isbyte

       DO 9008 I = 1,IRUN
        VTEMP(I) = 0.
 9008  CONTINUE

      IF (LWATER) THEN

!ADJ STORE vapsim,vaz0,vdpsih,vdpsim,vdx,vdxpsim,vdy, &
!ADJ       vdzeta,vdzsea,vtemplin,vustar,vx0psim,vxnum1,vxnum2,vz0 &
!ADJ       = comlev1, key=ikey_dynamics, kind=isbyte
!ADJ STORE vcu = comlev1, key=ikey_dynamics, kind=isbyte

       CALL LINADJ(VRIB,VRIB,VWS,VWS,VZ0,VUSTAR,IVWATER,VAPSIM, &
        dummy1,dummy2,dummy3,dummy4,dummy5,dummy6,dummy7,1,.TRUE.,IRUN,VDZETA, &
        VDZ0,VDPSIM,VDPSIH,IVBITRIB, &
        VX0PSIM,VG,VG0,VR1MG0,VZ2,VDZSEA,VAZ0,VXNUM1,VPSIGB2,VDX, &
        VDXPSIM,VDY,VXNUM2,VDEN,VAWS1,VXNUM3,VXNUM,VDZETA1,VDZETA2, &
        VZCOEF2,VZCOEF1,VTEMPLIN,VDPSIMC,VDPSIHC,MAPL_KARMAN,bmdl,CHOOSEZ0)

!ADJ STORE vcu = comlev1, key=ikey_dynamics, kind=isbyte
       DO 9010 I = 1,IRUN
        IF ( IVWATER(I).EQ.1 ) THEN
         VCU(I) = VCU(I) * (1. - VDPSIM(I)*VAPSIM(I))
         VZ0(I) = VZ0(I) + VDZ0(I)
         ENDIF
         IF ( IVWATER(I).EQ.1) THEN
         IF ( VZ0(I) .LE. Z0MIN ) VZ0(I) = Z0MIN
         vh0(i) = h0byz0 * vz0(i)
         VPSIG(I) = VH0(I) * VCU(I) * VWS(I) - USTH0S
         if(VPSIG(I).lt.0.)  VPSIG(I) = 0.
         VPSIG(I) = SQRT( VPSIG(I) )
         VPSIG(I) = BMDL(i) * VPSIG(I)
         VPSIHG(I) = VPSIM(I) + VDPSIH(I) + VPSIG(I)
        ENDIF
 9010  CONTINUE
!
      ENDIF
!
!  INITIAL GUESS FOR STABILITY PARAMETER ZETA
!
      DO 9012 I = 1,IRUN
       VZETA(I) = VK2 * VRIB(I) / (VCU(I) * VCU(I) * VPSIHG(I))
 9012 CONTINUE
!
!  RECOMPUTE CU, ESTIMATE PSIHG AND UPDATE ZETA AND Z0
!
      DO 9014 I = 1,IRUN
!      VZH(I) = VZ0(I) * VAHS(I)
       VZH(I) = VZ0(I) / (VHS(I) + VZ0(I))
 9014 CONTINUE

      CALL PSI (VZETA,VZH,VPSIM,VTEMP,IRUN,VXX,VXX0,VYY,VYY0,2)

      DO 9016 I = 1,IRUN
       VCU(I) = MAPL_KARMAN / VPSIM(I)
       VPSIG(I) = VH0(I) * VCU(I) * VWS(I) - USTH0S
       if(VPSIG(I).lt.0.)  VPSIG(I) = 0.
       VPSIG(I) = SQRT(VPSIG(I))
       VPSIG(I) = BMDL(i) * VPSIG(I)
       VPSIHG(I) = VPSIM(I) + VPSIG(I)
       VZETA(I) = VK2 * VRIB(I) / (VCU(I) * VCU(I) * VPSIHG(I))
 9016 CONTINUE
!
      IF(LWATER)THEN
       DO 9018 I = 1,IRUN
        IF (IVWATER(I).EQ.1) VUSTAR(I) = VCU(I) * VWS(I)
 9018  CONTINUE
       CALL ZCSUB ( VUSTAR,VHZ,IVWATER,.FALSE.,IRUN,VTEMP,CHOOSEZ0)
       CALL ZCSUB ( VUSTAR,VHZ,IVWATER,.FALSE.,IRUN,vz0h,2)
       DO 9020 I = 1,IRUN
        IF (IVWATER(I).EQ.1 ) then
         VZ0(I) = VTEMP(I)
         IF ( VZ0(I) .LE. Z0MIN ) VZ0(I) = Z0MIN
         IF ( VZ0H(I) .LE. Z0MIN ) VZ0H(I) = Z0MIN
         vh0(i) = h0byz0 * vz0(i)
         vh0h(i) = h0byz0 * vz0h(i)
        endif
 9020  CONTINUE
      ENDIF
!
! ------------------------------------
!  ITERATIVE LOOP - N ITERATIONS
!     COMPUTE CU AND CT
! ------------------------------------
!
!ADJ STORE vaz0,vden,vdpsih,vdpsim,vdx,vdxpsim,vdy, &
!ADJ       vdzeta,vtemplin,vx0psim,vxnum1,vxnum2,vz0 &
!ADJ       = comlev1, key=ikey_dynamics, kind=isbyte

      DO 200 ITER = 1,N
!
!ADJ STORE vaz0,vden,vdpsih,vdpsim,vdx,vdxpsim,vdy, &
!ADJ       vdzeta,vtemplin,vx0psim,vxnum1,vxnum2,vz0 &
!ADJ       = comlev1, key=ikey_dynamics, kind=isbyte
!ADJ STORE vz0h,vzeta = comlev1, key=ikey_dynamics, kind=isbyte

       DO 9026 I = 1,IRUN
!       VZH(I) = VZ0(I) * VAHS(I)
        VZH(I) = VZ0(I) / (VHS(I) + VZ0(I))
 9026  CONTINUE

       CALL PSI (VZETA,VZH,VPSIM,VPSIH,IRUN,VXX,VXX0,VYY,VYY0,1)

       DO I = 1,IRUN
!       VZH(I) = VZ0H(I) * VAHS(I)
        VZH(I) = VZ0H(I) / (VHS(I) + VZ0H(I))
       ENDDO

       if( choosez0.eq.3 .AND. Lwater ) &
        CALL PSI (VZETA,VZH,dummy1,VPSIH,IRUN,dummy2,dummy3,dummy4,dummy5,3)

!ADJ STORE vh0,vz0,vz0h = comlev1, key=ikey_dynamics, kind=isbyte

       DO 9028 I = 1,IRUN
        VCU(I) = MAPL_KARMAN / VPSIM(I)
        VUSTAR(I) = VCU(I) * VWS(I)
!
        VPSIG(I) = VH0(I) * VUSTAR(I) - USTH0S
        if(VPSIG(I).lt.0.)  VPSIG(I) = 0.
        VPSIG(I) = SQRT(VPSIG(I))
        VPSIG(I) = BMDL(i) * VPSIG(I)
        VPSIHG(I) = VPSIH(I) + VPSIG(I)
!
!  LINEAR CORRECTIONS FOR CU, CT, ZETA, AND Z0
!
        VAPSIM(I) = VCU(I) * RVK
        VAPSIHG(I) = 1. / VPSIHG(I)
        VRIB1(I) = VAPSIM(I) * VAPSIM(I) * VPSIHG(I) * VZETA(I)
 9028  CONTINUE
!
       ITYPE = 3
       IF(ITER.EQ.N) ITYPE = 5
!
!ADJ STORE vaz0,vdpsih,vdpsim,vdx,vdxpsim,vdy,vdzeta,vdzsea,vh0,vtemplin,vx0psim,vxnum1,vxnum2,vz0,vz0h &
!ADJ       = comlev1, key=ikey_dynamics, kind=isbyte

       CALL LINADJ(VRIB1,VRIB,VWS, &
        VWS,VZ0,VUSTAR,IVWATER, &
        VAPSIM,VAPSIHG,VPSIH, &
        VPSIG,VXX,VXX0, &
        VYY,VYY0,ITYPE,LWATER,IRUN,VDZETA, &
        VDZ0,VDPSIM,VDPSIH, &
        IVBITRIB, &
       VX0PSIM,VG,VG0,VR1MG0,VZ2,VDZSEA,VAZ0,VXNUM1,VPSIGB2,VDX, &
       VDXPSIM,VDY,VXNUM2,VDEN,VAWS1,VXNUM3,VXNUM,VDZETA1,VDZETA2, &
       VZCOEF2,VZCOEF1,VTEMPLIN,VDPSIMC,VDPSIHC,MAPL_KARMAN,bmdl,CHOOSEZ0)
!
!  UPDATES OF ZETA, Z0, CU AND CT
!
!ADJ STORE vz0 = comlev1, key=ikey_dynamics, kind=isbyte
       DO I = 1,IRUN
        VZETA(I) = VZETA(I) * ( 1. + VDZETA(I) )
       ENDDO
!ADJ STORE vz0 = comlev1, key=ikey_dynamics, kind=isbyte
       DO I = 1,IRUN
        IF (IVBITRIB(I).EQ.1 ) VZETA(I) = VPSIM(I) * VPSIM(I) * VRIB(I) * VAPSIHG(I)
       ENDDO 
!
       IF ( LWATER ) THEN
!ADJ STORE vz0,vz0h = comlev1, key=ikey_dynamics, kind=isbyte
        DO I = 1,IRUN
         IF (IVWATER(I).EQ.1 ) then
          VZ0(I) = VZ0(I) * ( 1. + VDZ0(I) )
          VZ0H(I) = VZ0H(I) * ( 1. + VDZ0(I) )
         ENDIF
        ENDDO
!ADJ STORE vz0,vz0h = comlev1, key=ikey_dynamics, kind=isbyte
        DO I = 1,IRUN
         IF (IVWATER(I).EQ.1 ) then
          IF (VZ0(I) .LE. Z0MIN ) VZ0(I) = Z0MIN
          IF (VZ0H(I) .LE. Z0MIN ) VZ0H(I) = Z0MIN
         ENDIF
        ENDDO
!ADJ STORE vz0,vz0h = comlev1, key=ikey_dynamics, kind=isbyte
        DO I = 1,IRUN
         IF (IVWATER(I).EQ.1 ) then
          vh0(i) = h0byz0 * vz0(i)
          vh0h(i) = h0byz0 * vz0h(i)
         ENDIF
        ENDDO
       ENDIF
!
       IF ( ITER .EQ. N ) THEN
        DO 9036 I = 1,IRUN
         VPSIM(I) = VPSIM(I) + VDPSIM(I)
         VCU(I) = MAPL_KARMAN / VPSIM(I)
         VUSTAR(I) = VCU(I) * VWS(I)
!
         VPSIG(I) = VH0(I) * VUSTAR(I) - USTH0S
         if(VPSIG(I).lt.0.)  VPSIG(I) = 0.
         VPSIG(I) = SQRT(VPSIG(I))
         VPSIG(I) = BMDL(i) * VPSIG(I)
         VPSIHG(I) = VPSIH(I) + VDPSIH(I) + VPSIG(I)
         VCT(I) = MAPL_KARMAN / VPSIHG(I)
 9036   CONTINUE
       ENDIF

!
!  SAVE VALUES OF RIB AND WS
!
        DO 9038 I = 1,IRUN
         VRIB1(I) = VRIB(I)
 9038   CONTINUE
!
 200  CONTINUE
!
! ------------------------------------
! ------ END 200 ITERATION LOOP ------
! ------------------------------------
!
!  CALCULATE RHO-SURFACE ( KG / M**3 )
!
!ADJ STORE vzh = comlev1, key=ikey_dynamics, kind=isbyte
       DO I = 1,IRUN
        VTEMP(I) =  10. * VAHS(I) * VZETA(I)
!       VZH(I) = VZ0(I) * 0.1
        VZH(I) = VZ0(I) / (10. + VZ0(I))
       ENDDO
!ADJ STORE vtemp = comlev1, key=ikey_dynamics, kind=isbyte
!ADJ STORE vzh, vz0h = comlev1, key=ikey_dynamics, kind=isbyte
       CALL PSI (VTEMP,VZH,dummy1,VPSIH2,IRUN,dummy2,dummy3,dummy4,dummy5,3)
       DO I = 1,IRUN
        VTEMP(I) = min(( VPSIH2(I) + VPSIG(I) ) / VPSIHG(I),1.)
        VRHO(I) = VPKE(I)*( VTH2(I) + VTEMP(I) * (VTH1(I)-VTH2(I)) )
        VRHO(I) = VPE(I)*100. / ( MAPL_RGAS * VRHO(I) )
       ENDDO
!
! interpolate uvtq to 2, 10 and 50 meters for diagnostic output
!  use psih and psim which represent non-dim change from ground
!                 to specified level
! and multiply theta by surface p**kappa to get temperatures
!
!ADJ STORE vzh = comlev1, key=ikey_dynamics, kind=isbyte
        do i = 1,irun
         vtemp(i) = 2. * vahs(i) * vzeta(i)
!        vzh(i) = min(vz0(i),2.) * 0.5
         VZH(I) = min(VZ0(I),2.) / (2. + min(VZ0(I),2.))
        enddo
!ADJ STORE vtemp,vzh = comlev1, key=ikey_dynamics, kind=isbyte
!ADJ STORE psihdiag = comlev1, key=ikey_dynamics, kind=isbyte
        CALL PSI(vtemp,vzh,psimdiag,psihdiag,irun,dummy2,dummy3,dummy4,dummy5,1)
!ADJ STORE vtemp = comlev1, key=ikey_dynamics, kind=isbyte
!ADJ STORE psihdiag = comlev1, key=ikey_dynamics, kind=isbyte
        do i = 1,irun
         vtemp(i) = min(( psihdiag(i) + vpsig(i) ) / vpsihg(i),1.)
         t2m(i) = ( (vth2(i) + vtemp(i)* (vth1(i)-vth2(i))) ) * vpke(i)
         q2m(i) = (vsh2(i) + vtemp(i)* (vsh1(i)-vsh2(i)))
         u2m(i) = (psimdiag(i)/vpsim(i) * vus(i))
         v2m(i) = (psimdiag(i)/vpsim(i) * vvs(i))
        enddo

        do i = 1,irun
         vtemp(i) = 10. * vahs(i) * vzeta(i)
!        vzh(i) = vz0(i) * 0.1
         VZH(I) = VZ0(I) / (10. + VZ0(I))
        enddo
!ADJ STORE vtemp, vzh = comlev1, key=ikey_dynamics, kind=isbyte
        CALL PSI(vtemp,vzh,psimdiag,psihdiag,irun,dummy2,dummy3,dummy4,dummy5,1)
        do i = 1,irun
         vtemp(i) = min(( psihdiag(i) + vpsig(i) ) / vpsihg(i),1.)
         t10m(i) = ( (vth2(i) + vtemp(i)* (vth1(i)-vth2(i))) ) * vpke(i)
         q10m(i) = (vsh2(i) + vtemp(i)* (vsh1(i)-vsh2(i)))
         u10m(i) = (psimdiag(i)/vpsim(i) * vus(i))
         v10m(i) = (psimdiag(i)/vpsim(i) * vvs(i))
        enddo

!ADJ STORE vz0, vz0h = comlev1, key=ikey_dynamics, kind=isbyte
        do i = 1,irun
         vtemp(i) = 50. * vahs(i) * vzeta(i)
!        vzh(i) = vz0(i) * 0.02
         VZH(I) = VZ0(I) / (50. + VZ0(I))
        enddo
!ADJ STORE vtemp, vzh = comlev1, key=ikey_dynamics, kind=isbyte
        CALL PSI(vtemp,vzh,psimdiag,psihdiag,irun,dummy2,dummy3,dummy4,dummy5,1)
        do i = 1,irun
         u50m(i) = (psimdiag(i)/vpsim(i) * vus(i))
         v50m(i) = (psimdiag(i)/vpsim(i) * vvs(i))
        enddo
!
!  EVALUATE TURBULENT TRANSFER COEFFICIENTS
!

      DO 9044 I = 1,IRUN
!!     VKH(I) = VUSTAR(I) * VCT(I)
!!     VKM(I) = VUSTAR(I) * VCU(I)
       VKH(I) = VUSTAR(I) * VCT(I) * VRHO(I)
       VKM(I) = VUSTAR(I) * VCU(I) * VRHO(I)
 9044 CONTINUE

!ADJ STORE vz0 = comlev1, key=ikey_dynamics, kind=isbyte
!ADJ STORE vz0h = comlev1, key=ikey_dynamics, kind=isbyte
      DO I = 1,IRUN
       VRIB(I) = MAPL_CP*(VPKE(I)-VPK(I))*(VTHV1(I)-VTHV2(I)) /    &
                max(VUS(I)*VUS(I) + VVS(I)*VVS(I),1.e-1)
      ENDDO

end subroutine helfsurface
