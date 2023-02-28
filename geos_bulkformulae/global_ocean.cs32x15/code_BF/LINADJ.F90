      SUBROUTINE LINADJ ( VRIB1,VRIB2,VWS1,VWS2,VZ1,VUSTAR,IWATER, &
       VAPSIM, VAPSIHG,VPSIH,VPSIG,VX,VX0,VY,VY0,ITYPE,LWATER,IRUN, &
       VDZETA,VDZ0,VDPSIM,VDPSIH,INTRIB, &
       VX0PSIM,VG,VG0,VR1MG0,VZ2,VDZSEA,VAZ0,VXNUM1,VPSIGB2,VDX, &
       VDXPSIM,VDY,VXNUM2,VDEN,VAWS1,VXNUM3,VXNUM,VDZETA1,VDZETA2, &
       VZCOEF2,VZCOEF1,VTEMPLIN,VDPSIMC,VDPSIHC,vk,bmdl,CHOOSEZ0)
!
!**********************************************************************
!
!  ARGUMENTS ::
!
!     INPUT:
!     ------
!    RIB1          -         BULK RICHARDSON NUMBER OF INPUT STATE
!    RIB2          -         DESIRED BULK RICH NUMBER OF OUTPUT STATE
!    WS1           -         SURFACE WIND SPEED OF INPUT STATE
!    WS2           -         DESIRED SURFACE WIND SPEED OF OUTPUT STATE
!    Z1            -         INPUT VALUE OF ROUGHNESS HEIGHT
!    USTAR         -         INPUT VALUE OF CU * WS
!    WATER         -         BIT ARRAY - '1' WHERE OCEAN
!    APSIM         -         (1/PSIM)
!    APSIHG        -         ( 1 / (PSIH+PSIG) )
!    PSIH          -         NON-DIM TEMP GRADIENT
!    PSIG          -         PSIH FOR THE MOLECULAR LAYER
!    X             -         PHIM(ZETA) - DERIVATIVE OF PSIM
!    X0            -         PHIM(ZETA0)
!    Y             -         PHIH(ZETA) - DERIVATIVE OF PSIH
!    Y0            -         PHIH(ZETA0)
!    ITYPE         -         INTEGER FLAG :
!                               1    = NEUTRAL ADJUSTMENT
!                               3, 5 = ADJUSTMENT INSIDE LOOP
!                               5    = ADJUST CU AND CT
!    LWATER        -         LOGICAL - .TRUE. IF THERE ARE WATER POINTS
!    CHOOSEZ0      -         INTEGER FLAG: 0 - L&P Z0, no high wind limit
!                                          1 - Edson Z0 for mom. and heat, high wind limit
!                                          2 - L&P Z0, high wind limit
!                                          3 - Edson Z0 for mom. only, high wind limit
!
!     OUTPUT:
!     -------
!    DZETA         -         D LOG ZETA
!    DZ0           -         D Z0 (ITYPE 1) OR D LOG Z0 (ITYPE 2-5)
!    DPSIM         -         D PSIM
!    DPSIH         -         D PSIH
!    BITRIB        -         BIT ARRAY - '1' WHERE RIB1 = 0
!
!**********************************************************************
      implicit none

! Argument List Declarations
      integer irun,itype,CHOOSEZ0
      REAL*8 VRIB1(*),VRIB2(*)
      REAL*8 VWS1(*),VWS2(*),VZ1(*),VUSTAR(*)
      integer IWATER(*)
      REAL*8 VAPSIM(*),VAPSIHG(*)
      REAL*8 VPSIH(*),VPSIG(*),VX(*)
      REAL*8 VX0(*),VY(*),VY0(*)
      LOGICAL LWATER
      REAL*8 VDZETA(*),VDZ0(*),VDPSIM(*)
      REAL*8 VDPSIH(*)
      integer INTRIB(*)
      REAL*8 VX0PSIM(*),VG(*),VG0(*),VR1MG0(*)
      REAL*8 VZ2(*),VDZSEA(*),VAZ0(*),VXNUM1(*)
      REAL*8 VPSIGB2(*),VDX(*),VDXPSIM(*),VDY(*)
      REAL*8 VXNUM2(*),VDEN(*),VAWS1(*),VXNUM3(*)
      REAL*8 VXNUM(*),VDZETA1(*),VDZETA2(*)
      REAL*8 VZCOEF2(*),VZCOEF1(*),VTEMPLIN(*)
      REAL*8 VDPSIMC(*),VDPSIHC(*),bmdl(*)

! Local Variables
      REAL*8 xx0max,prfac,xpfac,difsqt,ustz0s,h0byz0,usth0s
      PARAMETER ( XX0MAX  =   1.49821 )
      PARAMETER ( PRFAC  = 0.595864   )
      PARAMETER ( XPFAC  = .55        )
      PARAMETER ( DIFSQT  = 3.872983E-3)
      PARAMETER ( USTZ0S =   0.2030325E-5)
      PARAMETER ( H0BYZ0 =    30.0    )
      PARAMETER ( USTH0S =  H0BYZ0*USTZ0S )

      integer VINT1(irun),VINT2(irun)
      REAL*8 vk,b2uhs(irun)
      integer i
!
      do i = 1,irun
      B2UHS(i)   = BMDL(i) * BMDL(i) * USTH0S
      enddo

!   COMPUTE X0/PSIM, 1/Z0, G, G0, 1/(1-G0),
!     DEL LOG Z0, D LOG ZO / D USTAR
!
      IF ( (ITYPE.EQ.1) .AND. LWATER ) THEN
       DO 9000 I = 1,IRUN
        IF (IWATER(I).EQ.1) VX0PSIM(I) = VAPSIM(I)
 9000  CONTINUE
      ENDIF
      IF ( ITYPE .GE. 3 ) THEN
       DO 9002 I = 1,IRUN
        VX0PSIM(I) = VX0(I) * VAPSIM(I)
 9002  CONTINUE
      ENDIF
!
       DO 9004 I = 1,IRUN
        VDZ0(I) = 0.
        VG(I) = 0.
        VG0(I) = 0.
        VR1MG0(I) = 1.
 9004  CONTINUE
!
       IF ( LWATER ) THEN
        CALL ZCSUB ( VUSTAR,VDZSEA,IWATER,.TRUE.,IRUN,VZ2,CHOOSEZ0)

!       VDZSEA = min( VDZSEA, 0.2*VZ1/VAPSIM ) ! To prevent Divide by Zero as VG0 => 1.0
! -- changed by jmc: original (above) / modified (below):
        VDZSEA(1:irun) = min( VDZSEA(1:irun), 0.2*VZ1(1:irun)/VAPSIM(1:irun) )
!
        DO 9006 I = 1,IRUN
         IF ( IWATER(I).EQ.1) THEN
          VAZ0(I) = 1. / VZ1(I)
          VG(I) = VDZSEA(I) * VAZ0(I)
          VG0(I) = VX0PSIM(I) * VG(I)
          VR1MG0(I) = 1. / ( 1. - VG0(I) )
          VDZ0(I) = ( VZ2(I) - VZ1(I) ) * VR1MG0(I)
         ENDIF
 9006   CONTINUE
       ENDIF
!
      IF ( LWATER .AND. (ITYPE.GE.3) ) THEN
       DO 9008 I = 1,IRUN
        IF (IWATER(I).EQ.1) VDZ0(I) = VDZ0(I) * VAZ0(I)
 9008  CONTINUE
      ENDIF
!
!   COMPUTE NUM1,NUM2,NUM3, DEN
!
      IF (ITYPE.GE.3) THEN
       DO 9010 I = 1,IRUN
        VXNUM1(I) = 0.
        IF (VRIB1(I).EQ.0.) THEN
         INTRIB(I) = 1
        ELSE
         INTRIB(I) = 0
        ENDIF
        IF ( INTRIB(I).EQ.0 ) VXNUM1(I) = 1. / VRIB1(I)
        VPSIGB2(I) = 0.
        if(vpsig(i).gt.0.)VPSIGB2(I) = &
              0.5 * ( vpsig(i)*vpsig(i) + b2uhs(i) ) / vpsig(i)
        VDX(I) = VX(I) - VX0(I)
        VDXPSIM(I) = VDX(I) * VAPSIM(I)
        VDY(I) = VY(I) - VY0(I)
        VXNUM3(I) = - VPSIGB2(I)
!
        IF ( LWATER ) THEN
         IF (IWATER(I).EQ.1) THEN
          VDXPSIM(I) = VDXPSIM(I) * VR1MG0(I)
          VXNUM3(I) = VXNUM3(I) + VG(I) * ( VY0(I) - VPSIGB2(I) )
          VXNUM2(I) = VY0(I) - VPSIGB2(I) - VX0PSIM(I) * VPSIGB2(I)
          VXNUM2(I) = (VXNUM2(I) * VAPSIHG(I)) - 2. * VX0PSIM(I)
          VXNUM2(I) = VXNUM2(I) * VDZ0(I)
         ENDIF
        ENDIF
!
        VDEN(I) = VDY(I) + VDXPSIM(I) * VXNUM3(I)
        VDEN(I) = ( 1. + VDEN(I) * VAPSIHG(I) ) - 2. * VDXPSIM(I)
 9010  CONTINUE
      ENDIF
!
      IF (ITYPE.EQ.5) THEN
       DO 9012 I = 1,IRUN
        VAWS1(I) = VR1MG0(I) / VWS1(I)
        VXNUM3(I) = VXNUM3(I) * VAPSIHG(I)
!
        IF ( LWATER ) THEN
         IF(IWATER(I).EQ.1) THEN
          VXNUM3(I) = VXNUM3(I) - 2. * VG0(I)
          VXNUM3(I) = VAWS1(I) * VXNUM3(I)
         ENDIF
        ENDIF
 9012  CONTINUE
      ENDIF
!
!   COMPUTE D LOG ZETA
!
      IF (ITYPE.GE.3) THEN
       DO 9014 I = 1,IRUN
        VXNUM(I) = VRIB2(I) - VRIB1(I)
        IF( (VX0(I).GT.XX0MAX).AND.(VXNUM(I).GE.0.) )VXNUM(I) = 0.
        VXNUM(I) = VXNUM1(I) * VXNUM(I)
 9014  CONTINUE
!
       DO 9018 I = 1,IRUN
        VDZETA1(I) = VXNUM(I)
        IF(LWATER.AND.(IWATER(I).EQ.1)) VXNUM(I) = VXNUM(I) + VXNUM2(I)
        IF ( VDEN(I) .LT.0.1 ) VDEN(I) = 0.1
 9018  CONTINUE
!
       DO 9020 I = 1,IRUN
        VDZETA(I) = VXNUM(I) / VDEN(I)
 9020  CONTINUE
       DO 9022 I = 1,IRUN
        IF((VRIB2(I).EQ.0.).OR.(VDZETA(I).LE.-1.))VDZETA(I) = VDZETA1(I)
 9022  CONTINUE
      ENDIF
!
!   COMPUTE D LOG Z0
!
      IF ( LWATER .AND. (ITYPE.GE.3) )THEN
       DO 9026 I = 1,IRUN
        IF( IWATER(I).EQ.1 ) THEN
         VZCOEF2(I) = VG(I) * VDXPSIM(I)
         VDZ0(I) = VDZ0(I) - VZCOEF2(I) * VDZETA(I)
        ENDIF
 9026  CONTINUE
      ENDIF
!
      IF ( LWATER .AND. (ITYPE.EQ.5) ) THEN
       DO 9028 I = 1,IRUN
        IF(IWATER(I).EQ.1) VZCOEF1(I) = VG(I) * VAWS1(I)
 9028  CONTINUE
      ENDIF
!
!   CALCULATE D PSIM AND D PSIH
!
      IF ( (ITYPE.EQ.1) .AND. LWATER ) THEN
       DO 9032 I = 1,IRUN
        IF (IWATER(I).EQ.1) THEN
         VDPSIM(I) = - VDZ0(I) * VAZ0(I)
         VDPSIH(I) = VDPSIM(I)
        ENDIF
 9032  CONTINUE
      ENDIF
!
      IF (ITYPE.GE.3) THEN
       DO 9034 I = 1,IRUN
        VDPSIM(I) = VDX(I) * VDZETA(I)
        VDPSIH(I) = VDY(I) * VDZETA(I)
        IF ( LWATER ) THEN
         IF (IWATER(I).EQ.1 ) THEN
          VDPSIM(I) = VDPSIM(I) - VX0(I) * VDZ0(I)
          VDPSIH(I) = VDPSIH(I) - VY0(I) * VDZ0(I)
         ENDIF
        ENDIF
 9034  CONTINUE
      ENDIF
!
!   PREVENT OVERCORRECTION OF PSIM OR PSIH FOR UNSTABLE CASE
!
      IF (ITYPE.GE.4) THEN
       DO 9036 I = 1,IRUN
        VDPSIMC(I) = -0.9 - VDPSIM(I) * VAPSIM(I)
        VDPSIHC(I) = -0.9 *  VPSIH(I) - VDPSIH(I)
        IF ( VDPSIMC(I).GT.0.  ) THEN
         VINT1(I) = 1
        ELSE
         VINT1(I) = 0
        ENDIF
        IF ( VDPSIHC(I).GT.0.  ) THEN
         VINT2(I) = 1
        ELSE
         VINT2(I) = 0
        ENDIF
        VDZETA1(I) = 0.
        IF(VINT1(I).EQ.1) VDZETA1(I) = VDPSIMC(I) / VDXPSIM(I)
        IF((VINT1(I).EQ.1).OR.(VINT2(I).EQ.1)) VTEMPLIN(I) = &
              VDY(I) + VY0(I) * VG(I) * VDXPSIM(I)
!AMM    IF (VINT2(I).EQ.1 .and. VTEMPLIN(I).GT.tiny(1.0)) then
        IF (VINT2(I).EQ.1) then
             VDZETA2(I) =  VDPSIHC(I) / VTEMPLIN(I)
        IF ( VDZETA2(I).LT.VDZETA1(I) ) VDZETA1(I) = VDZETA2(I)
        endif
        IF((VINT1(I).EQ.1).OR.(VINT2(I).EQ.1)) THEN
         VDZETA(I) = VDZETA1(I) + VDZETA(I)
         VDPSIM(I) = VDPSIM(I) + VDX(I) * VR1MG0(I) * VDZETA1(I)
         VDPSIH(I) = VDPSIH(I) + VTEMPLIN(I) * VDZETA1(I)
         IF ( IWATER(I).EQ.1 ) &
           VDZ0(I) = VDZ0(I) - VG(I) * VDXPSIM(I) * VDZETA1(I)
        ENDIF
 9036  CONTINUE
      ENDIF
!
end subroutine linadj
