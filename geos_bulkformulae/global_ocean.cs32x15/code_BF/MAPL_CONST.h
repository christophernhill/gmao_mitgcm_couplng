      REAL*8 MAPL_CP, MAPL_RGAS, MAPL_KAPPA, MAPL_KARMAN, MAPL_VIREPS
      PARAMETER( MAPL_CP     = 1004.16d0 ) 
      PARAMETER( MAPL_KAPPA  = 2.d0/7.d0 )
      PARAMETER( MAPL_RGAS   = MAPL_CP*MAPL_KAPPA )
      PARAMETER( MAPL_KARMAN = 0.4d0   )
      PARAMETER( MAPL_VIREPS = 0.609d0 )

!--  took this from pkg/fizhi/getcon.F
!     PARAMETER ( CPD    = 1004.16d0  )
!     PARAMETER ( CPV    = 1869.46    )
!     PARAMETER ( ALHL   = 2.499d6    )
!     PARAMETER ( ALHS   = 2.845d6    )
!     PARAMETER ( STFBOL = 5.67d-8    )
!     PARAMETER ( AIRMW  = 28.97d0    )
!     PARAMETER ( H2OMW  = 18.01d0    )
!     PARAMETER ( RUNIV  = 8314.3d0   )
!     PARAMETER ( RGAS   = RUNIV/AIRMW)
!     PARAMETER ( RVAP   = RUNIV/H2OMW)
!     PARAMETER ( RKAP   = RGAS/CPD   )
!     PARAMETER ( HEATW  = 597.2      )
!     PARAMETER ( HEATI  = 680.0      )
!     PARAMETER ( TICE   = 273.16     )
!
!     PARAMETER ( VKRM   = 0.4        )
!     PARAMETER ( VIRTCON= 0.609      )
!--
