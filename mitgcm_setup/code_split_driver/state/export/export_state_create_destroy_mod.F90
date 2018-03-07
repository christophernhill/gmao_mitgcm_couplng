! $Id: $

#include "MEM_MANAGE_MACROS.h"

      MODULE EXPORT_STATE_CREATE_DESTROY_MOD

      USE EXPORT_STATE_TYPES_MOD
      IMPLICIT NONE

      INTERFACE CREATE
       MODULE PROCEDURE MITGCM_EXPORT_CREATE
      END INTERFACE
      INTERFACE DESTROY
       MODULE PROCEDURE MITGCM_EXPORT_DESTROY
      END INTERFACE

      CONTAINS

      SUBROUTINE MITGCM_EXPORT_CREATE( exportPtr,        &
                 snx, sny, olx, oly, n3ds, nSx, nSy      &
                 )
!     -- Allocate memory for MITgcm dynvars_h type of a specific size --
      TYPE(MITGCM_EXPORT), POINTER :: exportPtr
      INTEGER                      :: snx, sny, olx, oly
      INTEGER, intent(in), dimension(:) :: n3ds
      INTEGER                      :: nSx, nSy

!     -- Local variables --
      INTEGER                      :: Nr
      TYPE(MITGCM_EXPORT), POINTER :: p

      Nr = n3ds(1)
      ALLOCATE( exportPtr )
      p => exportPtr
      ALLOCATE ( p%US(1:snx*nSx,1:sny*nSy)  )
      ALLOCATE ( p%VS(1:snx*nSx,1:sny*nSy)  )
      ALLOCATE ( p%CA(1:snx*nSx,1:sny*nSy)  )
      ALLOCATE ( p%SA(1:snx*nSx,1:sny*nSy)  )
      ALLOCATE ( p%TS(1:snx*nSx,1:sny*nSy)  )
      ALLOCATE ( p%SS(1:snx*nSx,1:sny*nSy)  )
      ALLOCATE ( p%MASK(1:snx*nSx,1:sny*nSy,1:Nr)  )

      RETURN
      END SUBROUTINE

      SUBROUTINE MITGCM_EXPORT_DESTROY( exportPtr )
!     -- Deallocate memory for an array of MITgcm exports.
      TYPE(MITGCM_EXPORT), POINTER :: exportPtr

!     -- Local variables --
      TYPE(MITGCM_EXPORT), POINTER :: p

      IF ( ASSOCIATED(exportPtr) ) THEN
       p => exportPtr
       _DEALLOC ( p%US     )
       _DEALLOC ( p%VS     )
       _DEALLOC ( p%CA     )
       _DEALLOC ( p%SA     )
       _DEALLOC ( p%TS     )
       _DEALLOC ( p%SS     )
       _DEALLOC ( p%MASK   )
       _DEALLOC( exportPtr )
      ENDIF

      RETURN
      END SUBROUTINE

      END MODULE
