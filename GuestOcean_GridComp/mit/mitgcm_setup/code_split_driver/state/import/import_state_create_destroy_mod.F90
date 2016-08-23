! $Id: $

#include "MEM_MANAGE_MACROS.h"

      MODULE IMPORT_STATE_CREATE_DESTROY_MOD

      USE IMPORT_STATE_TYPES_MOD
      IMPLICIT NONE

      INTERFACE CREATE
       MODULE PROCEDURE MITGCM_IMPORT_CREATE
      END INTERFACE
      INTERFACE DESTROY
       MODULE PROCEDURE MITGCM_IMPORT_DESTROY
      END INTERFACE

      CONTAINS

      SUBROUTINE MITGCM_IMPORT_CREATE( importPtr,        &
                 snx, sny, olx, oly, nr, nsx, nsy        &
                 )
!     -- Allocate memory for MITgcm import state type of a specific size --
      TYPE(MITGCM_IMPORT), POINTER :: importPtr
      INTEGER                      :: snx, sny, olx, oly
      INTEGER                      :: nr, nsx, nsy

!     -- Local variables --
      TYPE(MITGCM_IMPORT), POINTER :: p

      ALLOCATE( importPtr )
      p => importPtr
      ALLOCATE ( p%TX(1:snx*nSx,1:sny*nSy)  )
      ALLOCATE ( p%TY(1:snx*nSx,1:sny*nSy)  )
      ALLOCATE ( p%SWHEAT(1:snx*nSx,1:sny*nSy,1:nr)  )
      ALLOCATE ( p%QFLX(1:snx*nSx,1:sny*nSy)  )
      ALLOCATE ( p%HFLX(1:snx*nSx,1:sny*nSy)  )
      ALLOCATE ( p%SFLX(1:snx*nSx,1:sny*nSy)  )
     
      RETURN
      END SUBROUTINE

      SUBROUTINE MITGCM_IMPORT_DESTROY( importPtr )          
!     -- Deallocate memory for an array of MITgcm imports.
      TYPE(MITGCM_IMPORT), POINTER :: importPtr

!     -- Local variables --
      TYPE(MITGCM_IMPORT), POINTER :: p

      IF ( ASSOCIATED(importPtr) ) THEN
       p => importPtr
       _DEALLOC ( p%TX     )
       _DEALLOC ( p%TY     )
       _DEALLOC ( p%SWHEAT )
       _DEALLOC ( p%QFLX   )
       _DEALLOC ( p%HFLX   )
       _DEALLOC ( p%SFLX   )
       _DEALLOC( importPtr )
      ENDIF
 
      RETURN
      END SUBROUTINE

      END MODULE
