**How to add an export field**

1. Export fields are read by the so-called "plug" routine
2. The plug routine for MITgcm is _GEOSocean_GridComp/GuestOcean_GridComp/mit/MIT_GEOS5PlugMod.F90_
3. The plug 
     1. sets/writes the imports for the component
     2. invokes the component
     3. gets/reads the exports from the component
1. The component is responsible for filling out the export field with valid information before
   control returns to the plug

Example plug code for reading an export field is
```
    subroutine Run ( gc, import, export, clock, rc )
      :
      :
      :
    CALL DRIVER_RUN( PrivateState%ptr, 1 )
      :
    CALL DRIVER_GET_EXPORT_STATE( PrivateState%ptr, 'MASK', MASK )
      :
      :
      :
```

In the component there is code that handles the ``DRIVER_GET_EXPORT_STATE`` request. This code determines which internal state field to pass back based on the string field character code.

