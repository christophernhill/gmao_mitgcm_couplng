! $Header: /u/gcmpack/MITgcm/verification/global_ocean.cs32x15/code/SIZE.h,v 1.4 2005/10/01 21:13:42 jmc Exp $
! $Name:  $

!
!     /==========================================================\
!     | SIZE.h Declare size of underlying computational grid.    |
!     |==========================================================|
!     | The design here support a three-dimensional model grid   |
!     | with indices I,J and K. The three-dimensional domain     |
!     | is comprised of nPx*nSx blocks of size sNx along one axis|
!     | nPy*nSy blocks of size sNy along another axis and one    |
!     | block of size Nz along the final axis.                   |
!     | Blocks have overlap regions of size OLx and OLy along the|
!     | dimensions that are subdivided.                          |
!     \==========================================================/
!     Voodoo numbers controlling data layout.
!     sNx - No. X points in sub-grid.
!     sNy - No. Y points in sub-grid.
!     OLx - Overlap extent in X.
!     OLy - Overlat extent in Y.
!     nSx - No. sub-grids in X.
!     nSy - No. sub-grids in Y.
!     nPx - No. of processes to use in X.
!     nPy - No. of processes to use in Y.
!     Nx  - No. points in X for the total domain.
!     Ny  - No. points in Y for the total domain.
!     Nr  - No. points in Z for full process domain.
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
      PARAMETER (
     &           sNx =  32,
     &           sNy =  32,
     &           OLx =   3,
     &           OLy =   3,
     &           nSx =   1,
     &           nSy =   1,
     &           nPx =   6,
     &           nPy =   1,
     &           Nx  = sNx*nSx*nPx,
     &           Ny  = sNy*nSy*nPy,
     &           Nr  =  15)

!     MAX_OLX  - Set to the maximum overlap region size of any array
!     MAX_OLY    that will be exchanged. Controls the sizing of exch
!                routine buufers.
      INTEGER MAX_OLX
      INTEGER MAX_OLY
      PARAMETER ( MAX_OLX = OLx,
     &            MAX_OLY = OLy )
