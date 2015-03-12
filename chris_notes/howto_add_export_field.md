**How to add an export field**

1. Export fields are requested from the so-called "plug" routine
2. The plug routine for MITgcm is _GEOSocean_GridComp/GuestOcean_GridComp/mit/MIT_GEOS5PlugMod.F90_
3. The plug 
     1. sets the imports for the component
     2. invokes the component
     3. gets the exports from the component

