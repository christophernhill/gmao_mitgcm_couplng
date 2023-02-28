      SUBROUTINE PSI(VZZ,VZH,VPSIM,VPSIH,IRUN,VX,VXS,VY,VYS,IFLAG)
!**********************************************************************
!
!  SUBROUTINE PSI - DETERMINES DIMENSIONLESS WIND AND
!                    SCALAR PROFILES IN SURFACE LAYER
!                 - CALLED FROM helfsurface
!
!  DESCRIPTION OF PARAMETERS
!     ZZ   -  INPUTED VALUE OF MONIN- OBUKHOV STABILITY PARAMETER ZETA
!     ZH   -  INPUTED VALUE OF Z0 DIVIDED BY SFC LAYER HEIGHT
!     PSIM -  OUTPUTED VALUE OF DIMENSIONLESS WIND
!     PSIH -  OUTPUTED VALUE OF DIMENSIONLESS SCALAR
!     X    -  OUTPUTED VALUE OF PHIM(ZETA)
!     XS   -  OUTPUTED VALUE OF PHIM(ZETA0)
!     Y    -  OUTPUTED VALUE OF PHIH(ZETA)
!     YS   -  OUTPUTED VALUE OF PHIH(ZETA0)
!     IFLAG-  FLAG TO DETERMINE IF CU IS NEEDED (IFLAG=2),
!                  IF CT IS NEEDED (IFLAG=3), OR BOTH (IFLAG=1)
!  SUBPROGRAMS NEEDED
!     PHI  -  COMPUTES SIMILARITY FUNCTION FOR MOMENTUM AND SCALARS
!
!**********************************************************************
      implicit none

! Argument List Declarations
      integer irun,iflag
      REAL*8 VZZ(*),VZH(*),VPSIM(*),VPSIH(*), &
           VX(*),VXS(*),VY(*),VYS(*)

! Local Variables
      REAL*8 ZWM,RZWM,Z0M,ZCM,RZCM,CM1,CM2,CM6,CM7,CM8ARG,YCM
      PARAMETER ( ZWM     =    1.    )
      PARAMETER ( RZWM    =  1./ZWM  )
      PARAMETER ( Z0M     =    0.2    )
      PARAMETER ( ZCM     =    42.    )
      PARAMETER ( RZCM    =  1./ZCM  )
      PARAMETER ( CM1     =  1./126. )
      PARAMETER ( CM2     =  1./(6.*CM1)  )
      PARAMETER ( CM6     =  6. / ( 1. + 6.*CM1 )  )
      PARAMETER ( CM7     =  CM2 + ZWM  )
      PARAMETER ( CM8ARG  =  CM7*ZCM*RZWM / (CM2+ZCM)  )
      PARAMETER ( YCM     =  6. / ( 1. + 6.*CM1*ZCM )  )

      integer INTSTB(irun),INTZ0(irun)
      REAL*8 ZZ0(irun),Z(irun),Z2(irun),Z1(irun),Z0(irun)
      REAL*8 X0(irun),X1(irun),Y0(irun),Y1(irun)
      REAL*8 PSI2(irun),TEMP(irun)
      REAL*8 HZ(irun),ARG0(irun),ARG1(irun),DX(irun)
      REAL*8 X0NUM(irun),X1NUM(irun),X0DEN(irun)
      REAL*8 X1DEN(irun),Y1DEN(irun),Z2ZWM(irun)
      REAL*8 cm3,cm4,cm5,cm8
      integer ibit,indx
      integer i
!
      CM3 =   sqrt( 0.2/CM1-0.01 )
      CM4 =   1./CM3
      CM5 =  (10.-CM1) / (10.*CM1*CM3)
      CM8 =   6. * LOG(CM8ARG)
!
      DO 9000 I = 1,IRUN
       VPSIM(I) = 0.
       VPSIH(I) = 0.
       VX(I) = 0.
       VXS(I) = 0.
       VY(I) = 0.
       VYS(I) = 0.
       ZZ0(I) = VZH(I)*VZZ(I)
 9000 CONTINUE
      IBIT = 0
      DO 9122 I = 1,IRUN
       IF(VZZ(I).LE.-1.e-7)IBIT = IBIT + 1
 9122 CONTINUE
      DO 9022 I = 1,IRUN
       IF(VZZ(I).LE.-1.e-7)THEN
        INTSTB(I) = 1
       ELSE
        INTSTB(I) = 0
       ENDIF
 9022 CONTINUE
!
! ****************************************
! *****    UNSTABLE SURFACE LAYER    *****
! ****************************************
!
      IF(IBIT.LE.0)  GO TO 100
!
      indx = 0
      DO 9002 I = 1,IRUN
       IF (INTSTB(I).EQ.1)THEN
        indx = indx + 1
        Z(indx) = VZZ(I)
        Z0(indx) = ZZ0(I)
       ENDIF
 9002 CONTINUE
!
      DO 9004 I = 1,IBIT
       Z(I) = -18. * Z(I)
       Z0(I) = -18. * Z0(I)
 9004 CONTINUE

      CALL PHI( Z,X1,Y1,IFLAG,IBIT )
      CALL PHI( Z0,X0,Y0,IFLAG,IBIT )

! ****************************
! *****    COMPUTE PSIM  *****
! ****************************
!
      IF(IFLAG.GE.3) GO TO 75
!
      DO 9006 I = 1,IBIT
       ARG1(I) = 1. - X1(I)
       IF ( Z(I) .LT. 0.013 ) ARG1(I) = Z(I) * ( 0.25 -  0.09375 * Z(I) )
!
       ARG0(I)  = 1. - X0(I)
       IF ( Z0(I) .LT. 0.013 ) ARG0(I) = Z0(I) * ( 0.25 -  0.09375 * Z0(I) )
!
       ARG1(I) = ARG1(I) * ( 1.+X0(I) )
       ARG0(I) = ARG0(I) * ( 1.+X1(I) )
       DX(I) = X1(I) - X0(I)
       ARG1(I) = ARG1(I) / ARG0(I)
       ARG0(I) = -DX(I) / ( 1. + X1(I)*X0(I) )
       ARG0(I) = ATAN( ARG0(I) )
       ARG1(I) = LOG( ARG1(I) )
       PSI2(I) = 2. * ARG0(I) + ARG1(I)
       PSI2(I) = PSI2(I) + DX(I)
 9006 CONTINUE
!
      indx = 0
      DO 9008 I = 1,IRUN
       IF( INTSTB(I).EQ.1 ) THEN
        indx = indx + 1
        VPSIM(I) = PSI2(indx)
        VX(I) = X1(indx)
        VXS(I) = X0(indx)
       ENDIF
 9008 CONTINUE
!
! ****************************
! *****    COMPUTE PSIH  *****
! ****************************
!
      IF(IFLAG.EQ.2) GO TO 100
!
  75  CONTINUE
      DO 9010 I = 1,IBIT
       ARG1(I) = 1. - Y1(I)
       IF( Z(I) .LT. 0.0065 ) ARG1(I) = Z(I) * ( 0.5 -  0.625 * Z(I) )
!
       ARG0(I)  = 1. - Y0(I)
       IF( Z0(I) .LT. 0.0065 ) ARG0(I) = Z0(I) * ( 0.5 -  0.625 * Z0(I) )
!
       ARG1(I) = ARG1(I) * ( 1. + Y0(I) )
       ARG0(I) = ARG0(I) * ( 1. + Y1(I) )
       ARG1(I) = ARG1(I) / ARG0(I)
       PSI2(I) = LOG( ARG1(I) )
       PSI2(I) = PSI2(I) - Y1(I) + Y0(I)
 9010 CONTINUE
!
      indx = 0
      DO 9012 I = 1,IRUN
       IF( INTSTB(I).EQ.1 ) THEN
       indx = indx + 1
       VPSIH(I) = PSI2(indx)
       VY(I) = Y1(indx)
       VYS(I) = Y0(indx)
       ENDIF
 9012 CONTINUE
!
! **************************************
! *****    STABLE SURFACE LAYER    *****
! **************************************
!
  100 CONTINUE
      IBIT = 0
      DO 9114 I = 1,IRUN
       IF(VZZ(I).GT.-1.e-7)THEN
        IBIT = IBIT + 1
       ENDIF
 9114 CONTINUE
      DO 9014 I = 1,IRUN
       IF(VZZ(I).GT.-1.e-7)THEN
        INTSTB(I) = 1
       ELSE
        INTSTB(I) = 0
       ENDIF
 9014 CONTINUE
      IF(IBIT.LE.0)  GO TO 300
      indx = 0
      DO 9016 I = 1,IRUN
       IF (INTSTB(I).EQ.1)THEN
        indx = indx + 1
        Z(indx) = VZZ(I)
        Z0(indx) = ZZ0(I)
        ARG1(indx) = VZH(I)
       ENDIF
 9016 CONTINUE

      DO 9018 I = 1,IBIT
       HZ(I) = 1. / ARG1(I)
       Z1(I) = Z(I)
       Z2(I) = ZWM
!
       IF ( Z(I) .GT. ZWM ) THEN
        Z1(I) = ZWM
        Z2(I) = Z(I)
       ENDIF
!
       IF ( Z0(I) .GT. Z0M ) THEN
        Z0(I) = Z0M
        INTZ0(I) = 1
       ELSE
        INTZ0(I) = 0
       ENDIF
!
       X1NUM(I) = 1. + 5. * Z1(I)
       X0NUM(I) = 1. + 5. * Z0(I)
       X1DEN(I) = 1. / (1. + CM1 * (X1NUM(I) * Z1(I)) )
       X0DEN(I) = 1. + CM1 * (X0NUM(I) * Z0(I))
!
       IF ( (INTZ0(I).EQ.1) .OR. (Z(I).GT.ZWM) ) &
            HZ(I) = Z1(I) / Z0(I)
       ARG1(I) = HZ(I)*HZ(I)*X0DEN(I)*X1DEN(I)
       ARG1(I) = LOG( ARG1(I) )
       ARG1(I) = 0.5 * ARG1(I)
       ARG0(I) = (Z1(I) + 0.1) * (Z0(I) + 0.1)
       ARG0(I) = CM3 + ARG0(I) * CM4
       ARG0(I) = ( Z1(I) - Z0(I) ) / ARG0(I)
       ARG0(I) = ATAN( ARG0(I) )
       TEMP(I) = ARG1(I) + CM5 * ARG0(I)
!
       X0(I) = X0NUM(I) / X0DEN(I)
       IF ( INTZ0(I).EQ.1 ) X0(I) = 0.
       Z2ZWM(I) = Z2(I) * RZWM
 9018 CONTINUE
!
! ****************************
! *****    COMPUTE PSIM  *****
! ****************************
!
      IF( IFLAG.GE.3 ) GO TO 225
!
      DO 9020 I = 1,IBIT
       X1(I) = X1NUM(I) * X1DEN(I)
       ARG1(I) = LOG( Z2ZWM(I) )
       PSI2(I) = TEMP(I) + CM6 * ARG1(I)
 9020 CONTINUE
!
      indx = 0
      DO 9030 I = 1,IRUN
       IF( INTSTB(I).EQ.1 ) THEN
       indx = indx + 1
       VPSIM(I) = PSI2(indx)
       VX(I) = X1(indx)
       VXS(I) = X0(indx)
       ENDIF
 9030 CONTINUE
!
! ****************************
! *****    COMPUTE PSIH  *****
! ****************************
!
       IF(IFLAG.EQ.2)GO TO 300
!
  225 CONTINUE
      DO 9024 I = 1,IBIT
       Y1DEN(I) = 1. + CM1 * ( X1NUM(I) * Z(I) )
       Y1(I) = X1NUM(I) / Y1DEN(I)
       ARG1(I) = CM7 * Z2ZWM(I) / ( CM2 + Z2(I) )
       ARG0(I) = 6.
       IF ( Z2(I) .GT. ZCM ) THEN
        Y1(I) = YCM
        ARG1(I) = Z2(I) * RZCM
        ARG0(I) = YCM
        TEMP(I) = TEMP(I) + CM8
       ENDIF
       ARG1(I) = LOG( ARG1(I) )
       PSI2(I) = TEMP(I) + ARG0(I) * ARG1(I)
 9024 CONTINUE
!
      indx = 0
      DO 9026 I = 1,IRUN
       IF( INTSTB(I).EQ.1 ) THEN
       indx = indx + 1
       VPSIH(I) = PSI2(indx)
       VY(I) = Y1(indx)
       VYS(I) = X0(indx)
       ENDIF
 9026 CONTINUE
!
  300 CONTINUE
!
end subroutine psi
