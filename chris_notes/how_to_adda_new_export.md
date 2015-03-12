**Adding a new export field**

1. Add an entry to _driver_get_export_state.FOR_, which is under _GuestOcean_GridComp/mit/mitgcm_setup/code_split_driver_.
   e.g.
   
  ```
    MODULE DRIVER_GET_EXPORT_STATE_MOD
         :
         :
    SUBROUTINE DRIVER_GET_EXPORT_STATE_2D( mitgcmIState, code, fld)
         :
         :
      IF ( code .EQ. 'MASK' ) THEN
     !      Copy internal state mask to export argument fld
       DO bj=1,nSy
        DO bi=1,nSx
         jSLo = (bj-1)*sNy
         iSLo = (bi-1)*sNx
         DO j=1,sNy
          DO i=1,sNx
           fld(iSLo+i,jSLo+j) =
           mitgcmIState%import%TX(iSLo+i,jSLo+j)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
       :
       :
  ```
