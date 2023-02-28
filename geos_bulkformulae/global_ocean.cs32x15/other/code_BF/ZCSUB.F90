      SUBROUTINE ZCSUB (VUSTAR,VDZSEA,IWATER,LDZSEA,IRUN,VZSEA,CHOOSEZ0)
!**********************************************************************
!  FUNCTION ZSEA
!  PURPOSE
!     COMPUTES Z0 AS A FUNCTION OF USTAR OVER WATER SURFACES
!  USAGE
!     CALLED BY helfsurface
!  DESCRIPTION OF PARAMETERS
!     USTAR    -  INPUTED VALUE OF SURFACE-STRESS VELOCITY
!     DZSEA    -  OUTPUTED VALUE OF DERIVATIVE  D(ZSEA)/D(USTAR)
!     WATER    -  INPUTED BIT VECTOR TO DETERMINE WATER POINTS
!     LDZSEA   -  LOGICAL FLAG TO DETERMINE IF DZSEA SHOULD BE COMPUTED
!     ZSEA     -  OUTPUTED VALUE OF ROUGHNESS LENGTH
!     CHOOSEZ0 -  INTEGER FLAG: 0 - L&P Z0, no high wind limit
!                               1 - Edson Z0 for mom. and heat, high wind limit
!                               2 - L&P Z0, high wind limit
!                               3 - Edson Z0 for mom. only, high wind limit
!  SUBPROGRAMS NEEDED
!     NONE
!  RECORD OF MODIFICATIONS
!   Molod 6/8/2011 - Implement new choozez0 options (expand from 0,1 choice)
!  REMARKS:
!        COMPUTE ROUGHNESS LENGTH FOR OCEAN POINTS
!          BASED ON FUNCTIONS OF LARGE AND POND
!          AND OF KONDO --- DESIGNED FOR K = .4
! *********************************************************************
      implicit none

! Argument List Delcarations
      integer irun, CHOOSEZ0
      REAL*8 VZSEA(*),VUSTAR(*),VDZSEA(*)
      integer IWATER(*)
      LOGICAL LDZSEA

! Local Variables
      REAL*8 USTMX1_OLD,USTMX2_OLD
      REAL*8 USTMX1_NEW,USTMX2_NEW
      REAL*8 USTMX1,USTMX2,USTMX3

      PARAMETER ( USTMX1_NEW =   0.80 )
      PARAMETER ( USTMX2_NEW =   0.80 )
      PARAMETER ( USTMX1_OLD =   1.1  )
      PARAMETER ( USTMX2_OLD =   0.381844 )
      PARAMETER ( USTMX3     =   0.0632456)

      REAL*8 AA(IRUN,5),TEMP(IRUN)
      integer INT2(IRUN),INT3(IRUN),INT4(IRUN)
      integer i,k
      REAL*8 ustloc(irun)

      REAL*8 AA1(5),AA2(5),AA3(5),AA4(5)
      REAL*8 AA2_NEW(5),AA3_NEW(5),AA4_NEW(5)
      REAL*8 AA2_OLD(5),AA3_OLD(5),AA4_OLD(5)

      DATA AA1/.2030325E-5,0.0,0.0,0.0,0.0/

      DATA AA2_NEW/-1.102451E-08,0.1593E-04,0.1E-03,2.918E-03, &
               0.695649E-04/
      DATA AA3_NEW/-1.102451E-08,0.12E-04,0.1E-03,2.918E-03, &
               1.5649E-04/
      DATA AA4_NEW/0.085E-03,1.5E-03,-0.210E-03,0.215E-02, &
               -0.0/

      DATA AA2_OLD/-0.402451E-08,0.239597E-04,0.117484E-03,0.191918E-03, &
               0.395649E-04/
      DATA AA3_OLD/-0.237910E-04,0.228221E-03,-0.860810E-03,0.176543E-02, &
               0.784260E-04/
      DATA AA4_OLD/-0.343228E-04,0.552305E-03,-0.167541E-02,0.250208E-02, &
               -0.153259E-03/

      if( CHOOSEZ0.eq.0 .OR. CHOOSEZ0.eq.2) then
          USTMX1 = USTMX1_OLD
          USTMX2 = USTMX2_OLD
             AA2 =    AA2_OLD
             AA3 =    AA3_OLD
             AA4 =    AA4_OLD
      else
          USTMX1 = USTMX1_NEW
          USTMX2 = USTMX2_NEW
             AA2 =    AA2_NEW
             AA3 =    AA3_NEW
             AA4 =    AA4_NEW
      endif
!
!**********************************************************************
!*****              LOWER CUTOFF CONDITION FOR USTAR                ***
!**********************************************************************
!
      DO 9000 I = 1,IRUN
       IF(VUSTAR(I) .LT. 1.e-6)THEN
        INT3(I) = 1
       ELSE
        INT3(I) = 0
       ENDIF
 9000 CONTINUE
      DO 9002 I = 1,IRUN
       IF(INT3(I).EQ.1) VUSTAR(I) = 1.e-6
 9002 CONTINUE

!     ustloc = vustar
! -- changed by jmc: original (above) / modified (below):
      ustloc(1:irun) = vustar(1:irun)
!
!***********************************
!*****  LOAD THE ARRAY A(I,K)  *****
!***********************************
!
      DO 9004 I = 1,IRUN
       IF( (ustloc(I) .GT. USTMX1) .AND. (IWATER(I).EQ.1) ) THEN
        if( CHOOSEZ0.gt.0 ) ustloc(i) = ustmx1
        INT4(I) = 1
       ELSE
        INT4(I) = 0
       ENDIF
 9004 CONTINUE
      DO 9006 I = 1,IRUN
       IF(ustloc(I) .GT. USTMX2) THEN
        INT3(I) = 1
       ELSE
        INT3(I) = 0
       ENDIF
 9006 CONTINUE
      DO 9008 I = 1,IRUN
       IF(ustloc(I) .GE. USTMX3) THEN
        INT2(I) = 1
       ELSE
        INT2(I) = 0
       ENDIF
 9008 CONTINUE
!
      DO 100 K=1,5
       DO 9010 I = 1,IRUN
        AA(I,K) = AA1(K)
        IF( INT2(I).EQ.1 )  AA(I,K) = AA2(K)
        IF( INT3(I).EQ.1 )  AA(I,K) = AA3(K)
        IF( INT4(I).EQ.1 )  AA(I,K) = AA4(K)
 9010  CONTINUE
  100 CONTINUE
!
!********************************************************
!*****  EVALUATE THE ENHANCED POLYNOMIAL FOR ZSEA  *****
!********************************************************
!
      DO 9012 I = 1,IRUN
       VDZSEA(I)  =  ( AA(I,4) + AA(I,5) * ustloc(I) ) * ustloc(I)
       VZSEA(I)  =  AA(I,2) + ( AA(I,3) + VDZSEA(I) ) * ustloc(I)
       TEMP(I) = AA(I,1) / ustloc(I)
       VZSEA(I)  =  VZSEA(I) + TEMP(I)
 9012 CONTINUE
!
!**********************************************************************
!*****        EVALUATE THE DERIVATIVE DZSEA IF LDZSEA IS TRUE       ***
!**********************************************************************
!
      IF( LDZSEA ) THEN
       DO 9014 I = 1,IRUN
        VDZSEA(I)  =  3. * VDZSEA(I) -(AA(I,4)*ustloc(I) - AA(I,3))
        VDZSEA(I)  =  VDZSEA(I) * ustloc(I) - TEMP(I)
 9014  CONTINUE
      ENDIF
!
end subroutine zcsub
