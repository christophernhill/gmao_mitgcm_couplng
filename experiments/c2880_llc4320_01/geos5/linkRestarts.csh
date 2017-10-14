#!/bin/csh -f

set matexp=/nobackup/mathomp4/I30UFVR8-C2880-MITOstia
set matscr=${matexp}/scratch.20121022_00z_checkpoints

set atanasdir=/nobackup/atrayano/MIT/rst/*
set key="checkpoint.20121022_0000z.nc4"

echo Linking restarts from Mat
foreach i ($matscr/*$key)
   set j=`echo $i:t | sed -e "s/${key}/rst/g"`
   set found=0
# exclude tile based restarts
   foreach k (catch_internal_rst lake_internal_rst landice_internal_rst saltwater_internal_rst saltwater_import_rst)
      if ($k == $j) set found=1
   end
   if ($found == 0) then
     echo $j
     ln -sf $i $j
   endif
end

ln -sf ${matexp}/RC.COUPLED RC

echo Linking restarts from Atanas
foreach i ($atanasdir)
   set j=$i:t
   echo $j
     ln -sf $i $j
end
