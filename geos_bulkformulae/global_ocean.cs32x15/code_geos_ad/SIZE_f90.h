!BOP
!    !ROUTINE: SIZE_f90.h
!EOP
      INTEGER sNx
      INTEGER sNy
      INTEGER OLx
      INTEGER OLy
      INTEGER nSx
      INTEGER nSy
      INTEGER nPx
      INTEGER nPy
      INTEGER Nx
      INTEGER Ny
      INTEGER Nr
      PARAMETER ( &
                sNx =  16, &
                sNy =  16, &
                OLx =   4, &
                OLy =   4, &
                nSx =  24, &
                nSy =   1, &
                nPx =   1, &
                nPy =   1, &
                Nx  = sNx*nSx*nPx, &
                Ny  = sNy*nSy*nPy, &
                Nr  =  50)

!     MAX_OLX :: Set to the maximum overlap region size of any array
!     MAX_OLY    that will be exchanged. Controls the sizing of exch
!                routine buffers.
      INTEGER MAX_OLX
      INTEGER MAX_OLY
      PARAMETER ( MAX_OLX = OLx, &
                 MAX_OLY = OLy )

