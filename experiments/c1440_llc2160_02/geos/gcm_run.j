#!/bin/csh -f

#######################################################################
#                     Batch Parameters for Run Job
#######################################################################

#PBS -l walltime=8:00:00
#PBS -l select=324:ncpus=24:mpiprocs=24:model=has
#PBS -N TEST_RUN
#PBS -q normal
#PBS -W group_list=g26209

#######################################################################
#                         System Settings 
#######################################################################

umask 022

limit stacksize unlimited

#######################################################################
#           Architecture Specific Environment Variables
#######################################################################

setenv ARCH `uname`

setenv SITE             NAS
setenv GEOSDIR          /nobackupp11/dmenemen/DYAMOND/geos5/GEOSodas 
setenv GEOSBIN          /nobackupp11/dmenemen/DYAMOND/geos5/GEOSodas/Linux/bin 
setenv RUN_CMD         "mpiexec_mpt -np "
setenv GCMVER           Heracles-5_3

source $GEOSBIN/g5_modules
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${BASEDIR}/${ARCH}/lib

#######################################################################
#             Experiment Specific Environment Variables
#######################################################################

setenv    EXPID   TEST
setenv    EXPDIR  /nobackupp11/dmenemen/DYAMOND/geos5/TEST
setenv    HOMDIR  /nobackupp11/dmenemen/DYAMOND/geos5/TEST
setenv    SCRDIR  $EXPDIR/scratch

#######################################################################
#                 Create Experiment Sub-Directories
#######################################################################

if (! -e $EXPDIR/restarts   ) mkdir -p $EXPDIR/restarts
if (! -e $EXPDIR/holding    ) mkdir -p $EXPDIR/holding
if (! -e $EXPDIR/archive    ) mkdir -p $EXPDIR/archive
if (! -e $EXPDIR/post       ) mkdir -p $EXPDIR/post
if (! -e $EXPDIR/plot       ) mkdir -p $EXPDIR/plot
if (! -e $SCRDIR            ) mkdir -p $SCRDIR

#######################################################################
#                   Set Experiment Run Parameters
#######################################################################

set       NX = `grep           NX: $HOMDIR/AGCM.rc | cut -d':' -f2`
set       NY = `grep           NY: $HOMDIR/AGCM.rc | cut -d':' -f2`
set  AGCM_IM = `grep      AGCM_IM: $HOMDIR/AGCM.rc | cut -d':' -f2`
set  AGCM_JM = `grep      AGCM_JM: $HOMDIR/AGCM.rc | cut -d':' -f2`
set  AGCM_LM = `grep      AGCM_LM: $HOMDIR/AGCM.rc | cut -d':' -f2`
set  OGCM_IM = `grep      OGCM_IM: $HOMDIR/AGCM.rc | cut -d':' -f2`
set  OGCM_JM = `grep      OGCM_JM: $HOMDIR/AGCM.rc | cut -d':' -f2`
set END_DATE = `grep     END_DATE:  $HOMDIR/CAP.rc | cut -d':' -f2`
set NUM_SGMT = `grep     NUM_SGMT:  $HOMDIR/CAP.rc | cut -d':' -f2`
set FSEGMENT = `grep FCST_SEGMENT:  $HOMDIR/CAP.rc | cut -d':' -f2`

set       NX = `grep  OGCM_NX: $HOMDIR/AGCM.rc | cut -d':' -f2`
set       NY = `grep  OGCM_NY: $HOMDIR/AGCM.rc | cut -d':' -f2`

# Check for Over-Specification of CPU Resources
# ---------------------------------------------
  if ($?PBS_NODEFILE) then
     set  NCPUS = `cat $PBS_NODEFILE | wc -l`
     @    NPES  = $NX * $NY
        if( $NPES > $NCPUS ) then
             echo "CPU Resources are Over-Specified"
             echo "--------------------------------"
             echo "Allotted NCPUs: $NCPUS"
             echo "Specified  NX : $NX"
             echo "Specified  NY : $NY"
             exit
        endif
     endif
  endif

# Set ATMOS and OCEAN Horizontal Resolution Tags
# ----------------------------------------------
set AGCM_IM_Tag = `echo $AGCM_IM | awk '{printf "%4.4i", $1}'`
set AGCM_JM_Tag = `echo $AGCM_JM | awk '{printf "%4.4i", $1}'`
set OGCM_IM_Tag = `echo $OGCM_IM | awk '{printf "%4.4i", $1}'`
set OGCM_JM_Tag = `echo $OGCM_JM | awk '{printf "%4.4i", $1}'`

set ATMOStag = CF${AGCM_IM_Tag}x6C
set OCEANtag = DE0360xPE0180
set OCEAN1tag = LL${OGCM_IM_Tag}xLL${OGCM_JM_Tag}
set OTYPE = LL

#######################################################################
#   Move to Scratch Directory and Copy RC Files from Home Directory
#######################################################################

cd $SCRDIR
/bin/rm -rf *
                             /bin/ln -sf $EXPDIR/RC/* .
                             /bin/cp     $EXPDIR/cap_restart .
                             /bin/cp -f  $HOMDIR/*.rc .
                             /bin/cp -f  $HOMDIR/*.nml .
                             /bin/cp -f  $HOMDIR/levels.center .

#######################################################################
#         Create Strip Utility to Remove Multiple Blank Spaces
#######################################################################

set      FILE = strip
/bin/rm -f $FILE
cat << EOF > $FILE
#!/bin/ksh
/bin/mv \$1 \$1.tmp
touch   \$1
while read line
do
echo \$line >> \$1
done < \$1.tmp
exit
EOF
chmod +x $FILE

#######################################################################
#              Create HISTORY Collection Directories
#######################################################################

set collections = ''
foreach line ("`cat HISTORY.rc`")
   set firstword  = `echo $line | awk '{print $1}'`
   set firstchar  = `echo $firstword | cut -c1`
   set secondword = `echo $line | awk '{print $2}'`

   if ( $firstword == "::" ) goto done

   if ( $firstchar != "#" ) then
      set collection  = `echo $firstword | sed -e "s/'//g"`
      set collections = `echo $collections $collection`
      if ( $secondword == :: ) goto done
   endif

   if ( $firstword == COLLECTIONS: ) then
      set collections = `echo $secondword | sed -e "s/'//g"`
   endif
end

done:
   foreach collection ( $collections )
      if (! -e $EXPDIR/$collection )         mkdir $EXPDIR/$collection
      if (! -e $EXPDIR/holding/$collection ) mkdir $EXPDIR/holding/$collection
   end

#######################################################################
#                        Link Boundary Datasets
#######################################################################

setenv BCSDIR    /nobackup/gmao_SIteam/ModelData/bcs/Ganymed-4_0/Ganymed-4_0_Reynolds
setenv SSTDIR    /nobackupp2/estrobac/geos5/SSTDIR
setenv CHMDIR    /nobackup/gmao_SIteam/ModelData/fvInput_nc3
setenv BCRSLV    ${ATMOStag}_${OCEANtag}
setenv BCRSLV1    ${ATMOStag}_${OCEAN1tag}
setenv DATELINE  DC
setenv EMISSIONS g5chem

setenv GRIDDIR  ${SSTDIR}
setenv BCTAG `basename $GRIDDIR`


set             FILE = linkbcs
/bin/rm -f     $FILE
cat << _EOF_ > $FILE
#!/bin/csh -f

/bin/mkdir -p RESTART
/bin/mkdir -p            ExtData
/bin/ln    -sf $CHMDIR/* ExtData

/bin/ln -sf $GRIDDIR/SEAWIFS_KPAR_mon_clim.${OGCM_IM}x${OGCM_JM} SEAWIFS_KPAR_mon_clim.data
/bin/ln -sf ../${BCRSLV1}-Pfafstetter.til   tile.data
/bin/ln -sf ../runoff_new.bin runoff.bin
/bin/ln -sf ../mit-llc2160-72x72.bin .
/bin/ln -sf $GRIDDIR/dataoceanfile_MERRA_fraci_2000.${OGCM_IM}x${OGCM_JM}.CM fraci.data
/bin/ln -sf $GRIDDIR/vgrid50.ascii ./vgrid.ascii



/bin/ln -sf $BCSDIR/Shared/pchem.species.CMIP-5.1870-2097.z_91x72.nc4 species.data
/bin/ln -sf $BCSDIR/Shared/*bin .

/bin/ln -sf ../visdf_c1440.dat visdf.dat
/bin/ln -sf ../nirdf_c1440.dat nirdf.dat
/bin/ln -sf ../vegdyn.data .
/bin/ln -sf ../lai_clim_c1440.data lai.data
/bin/ln -sf ../green_clim_c1440.data green.data
/bin/ln -sf ../topo_DYN_ave_${AGCM_IM}x${AGCM_JM}.data topo_dynave.data
/bin/ln -sf ../topo_GWD_var_${AGCM_IM}x${AGCM_JM}.data topo_gwdvar.data
/bin/ln -sf ../topo_TRB_var_${AGCM_IM}x${AGCM_JM}.data topo_trbvar.data

if(     -e  $BCSDIR/$BCRSLV/Gnomonic_$BCRSLV.dat ) then
/bin/ln -sf $BCSDIR/$BCRSLV/Gnomonic_$BCRSLV.dat .
endif

#/bin/cp -R $GRIDDIR/INPUT .
#/bin/cp $HOMDIR/input.nml .
#/bin/cp $HOMDIR/*_table .
#/bin/ln -sf $GRIDDIR/cice/kmt_cice.bin .
#/bin/ln -sf $GRIDDIR/cice/grid_cice.bin .

_EOF_


chmod +x linkbcs
/bin/cp  linkbcs $EXPDIR

#######################################################################
#          Get C2L History weights/index file for Cubed-Sphere
#######################################################################

set C_NPX = `echo $AGCM_IM | awk '{printf "%5.5i", $1}'`
set C_NPY = `echo $AGCM_JM | awk '{printf "%5.5i", $1}'`
set H_NPX = `echo 5760 | awk '{printf "%5.5i", $1}'`
set H_NPY = `echo 2881 | awk '{printf "%5.5i", $1}'`

set c2l_file = "${C_NPX}x${C_NPY}_c2l_${H_NPX}x${H_NPY}.bin"

if (-e $BCSDIR/$BCRSLV/${c2l_file}) /bin/ln -s $BCSDIR/$BCRSLV/${c2l_file} .

#######################################################################
#                    Get Executable and RESTARTS 
#######################################################################

/bin/cp $EXPDIR/GEOSgcm.x .

set rst_types = `cat AGCM.rc | grep "RESTART_FILE"    | cut -d ":" -f1 | cut -d "_" -f1-2`
set chk_types = `cat AGCM.rc | grep "CHECKPOINT_FILE" | cut -d ":" -f1 | cut -d "_" -f1-2`
set rst_files = `cat AGCM.rc | grep "RESTART_FILE"    | cut -d ":" -f2`
set chk_files = `cat AGCM.rc | grep "CHECKPOINT_FILE" | cut -d ":" -f2`

# Remove possible bootstrap parameters (+/-)
# ------------------------------------------
set dummy = `echo $rst_files`
set rst_files = ''
foreach rst ( $dummy )
  set length  = `echo $rst | awk '{print length($0)}'`
  set    bit  = `echo $rst | cut -c1`
  if(  "$bit" == "+" | \
       "$bit" == "-" ) set rst = `echo $rst | cut -c2-$length`
  set rst_files = `echo $rst_files $rst`
end

# Copy Restarts to Scratch Directory
# ----------------------------------
foreach rst ( $rst_files )
  if(-e $EXPDIR/$rst ) /bin/cp $EXPDIR/$rst . &
end
wait

/bin/cp $EXPDIR/RESTART/* INPUT

# Copy and Tar Initial Restarts to Restarts Directory
# ---------------------------------------------------
set edate = e`cat cap_restart | cut -c1-8`_`cat cap_restart | cut -c10-11`z
set numrs = `/bin/ls -1 ${EXPDIR}/restarts/*${edate}* | wc -l`
if($numrs == 0) then
   foreach rst ( $rst_files )
      if( -e $rst & ! -e ${EXPDIR}/restarts/$EXPID.${rst}.${edate}.${GCMVER}.${BCTAG}_${BCRSLV} ) then
            /bin/cp $rst ${EXPDIR}/restarts/$EXPID.${rst}.${edate}.${GCMVER}.${BCTAG}_${BCRSLV} &
      endif
   end
   wait
   /bin/cp -r $EXPDIR/RESTART ${EXPDIR}/restarts/RESTART.${edate}
   cd $EXPDIR/restarts
      tar cf  restarts.${edate}.tar $EXPID.*.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}
     /bin/rm -rf `/bin/ls -d -1     $EXPID.*.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}`
   cd $SCRDIR
endif

##################################################################
######
######         Perform multiple iterations of Model Run
######
##################################################################

@ counter    = 1
while ( $counter <= ${NUM_SGMT} )

/bin/rm -f  EGRESS
/bin/cp -f $HOMDIR/CAP.rc .
./strip            CAP.rc

# Set Time Variables for Current_(c), Ending_(e), and Segment_(s) dates 
# ---------------------------------------------------------------------
set nymdc = `cat cap_restart | cut -c1-8`
set nhmsc = `cat cap_restart | cut -c10-15`
set nymde = `cat CAP.rc | grep END_DATE:     | cut -d: -f2 | tr -s " " | cut -d" " -f2`
set nhmse = `cat CAP.rc | grep END_DATE:     | cut -d: -f2 | tr -s " " | cut -d" " -f3`
set nymds = `cat CAP.rc | grep JOB_SGMT:     | cut -d: -f2 | tr -s " " | cut -d" " -f2`
set nhmss = `cat CAP.rc | grep JOB_SGMT:     | cut -d: -f2 | tr -s " " | cut -d" " -f3`

# Compute Time Variables at the Finish_(f) of current segment
# -----------------------------------------------------------
set nyear   = `echo $nymds | cut -c1-4`
set nmonth  = `echo $nymds | cut -c5-6`
set nday    = `echo $nymds | cut -c7-8`
set nhour   = `echo $nhmss | cut -c1-2`
set nminute = `echo $nhmss | cut -c3-4`
set nsec    = `echo $nhmss | cut -c5-6`
       @ dt = $nsec + 60 * $nminute + 3600 * $nhour + 86400 * $nday

set nymdf = $nymdc
set nhmsf = $nhmsc
set date  = `$GEOSBIN/tick $nymdf $nhmsf $dt`
set nymdf =  $date[1]
set nhmsf =  $date[2]
set year  = `echo $nymdf | cut -c1-4`
set month = `echo $nymdf | cut -c5-6`
set day   = `echo $nymdf | cut -c7-8`

     @  month = $month + $nmonth
while( $month > 12 )
     @  month = $month - 12
     @  year  = $year  + 1
end
     @  year  = $year  + $nyear
     @ nymdf  = $year * 10000 + $month * 100 + $day

if( $nymdf >  $nymde )    set nymdf = $nymde
if( $nymdf == $nymde )    then
    if( $nhmsf > $nhmse ) set nhmsf = $nhmse
endif

set yearc = `echo $nymdc | cut -c1-4`
set yearf = `echo $nymdf | cut -c1-4`

# For MERRA-2 and OSTIA, Modify local CAP.rc Ending date if Finish time exceeds Current year boundary
# ---------------------------------------------------------------------------------------------------
if( ${OCEANtag} == DE1440xPE0720 | \
    ${OCEANtag} == DE2880xPE1440 ) then
    if( $yearf > $yearc ) then
       @ yearf = $yearc + 1
       @ nymdf = $yearf * 10000 + 0101
        set oldstring = `cat CAP.rc | grep END_DATE:`
        set newstring = "END_DATE: $nymdf $nhmsf"
        /bin/mv CAP.rc CAP.tmp
        cat CAP.tmp | sed -e "s?$oldstring?$newstring?g" > CAP.rc
    endif
endif

# Select proper MERRA-2 GOCART Emission RC Files
# (NOTE: MERRA2-DD has same transition date)
# ----------------------------------------------
if( ${EMISSIONS} == MERRA2 | \
    ${EMISSIONS} == MERRA2-DD ) then
    set MERRA2_Transition_Date = 20020401

    if( $nymdc < ${MERRA2_Transition_Date} ) then
         set MERRA2_EMISSIONS_DIRECTORY = $GEOSDIR/$ARCH/etc/$EMISSIONS/19600101-20000331
         if( $nymdf > ${MERRA2_Transition_Date} ) then
          set nymdf = ${MERRA2_Transition_Date}
          set oldstring = `cat CAP.rc | grep END_DATE:`
          set newstring = "END_DATE: $nymdf $nhmsf"
          /bin/mv CAP.rc CAP.tmp
                     cat CAP.tmp | sed -e "s?$oldstring?$newstring?g" > CAP.rc
         endif
    else
         set MERRA2_EMISSIONS_DIRECTORY = $GEOSDIR/$ARCH/etc/$EMISSIONS/20000401-present
    endif

    if( $AGCM_LM == 72 ) then
        /bin/cp --remove-destination ${MERRA2_EMISSIONS_DIRECTORY}/*.rc .
    else
        set files =      `/bin/ls -1 ${MERRA2_EMISSIONS_DIRECTORY}/*.rc`
        foreach file ($files)
          /bin/rm -f   `basename $file`
          /bin/rm -f    dummy
          /bin/cp $file dummy
              cat       dummy | sed -e "s|/L72/|/L${AGCM_LM}/|g" | sed -e "s|z72|z${AGCM_LM}|g" > `basename $file`
        end
    endif

endif

if(-e ExtData.rc )    /bin/rm -f   ExtData.rc
set  extdata_files = `/bin/ls -1 *_ExtData.rc`
cat $extdata_files > ExtData.rc 

# Link Boundary Conditions for Appropriate Date
# ---------------------------------------------
setenv YEAR $yearc
./linkbcs

if (! -e tile.bin) then
$GEOSBIN/binarytile.x tile.data tile.bin
endif

# Test Saltwater Restart for Number of tiles correctness
# ------------------------------------------------------

if ( -x $GEOSBIN/rs_numtiles.x ) then

   set N_SALT_TILES_EXPECTED = `grep '^ *0' tile.data | wc -l`
   set N_SALT_TILES_FOUND = `$RUN_CMD 1 $GEOSBIN/rs_numtiles.x saltwater_internal_rst | grep Total | awk '{print $3}'`
         
   if ( $N_SALT_TILES_EXPECTED != $N_SALT_TILES_FOUND ) then
      echo "Error! Found $N_SALT_TILES_FOUND tiles in saltwater. Expect to find $N_SALT_TILES_EXPECTED tiles."
      echo "Your restarts are probably for a different ocean."
      exit 7
   endif    

endif

# Check to Convert Binary Restarts
# --------------------------------
set      fvrst = `cat AGCM.rc | grep "^DYN_INTERNAL_RESTART_FILE"  | cut -d ":" -f2`
set   filetype = `/usr/bin/file -Lb --mime-type $fvrst`
if ( $filetype =~ "application/octet-stream" ) then
      cd $EXPDIR/convert
           ./gcm_convert.j
             set   rc  = $status
             if ( $rc != 0 ) then
                echo "CONVERT failed and returned with exit code $rc"
                exit $rc
             endif
      cd $SCRDIR
      foreach rst ( $rst_files )
         if(     -e $EXPDIR/convert/converted_restarts/$rst.nc4 ) then
            /bin/rm $rst
            /bin/cp $EXPDIR/convert/converted_restarts/$rst.nc4 .
            $GEOSBIN/stripname $rst.nc4 $rst
         endif
      end
     /bin/cp -f $HOMDIR/AGCM.rc $HOMDIR/AGCM.rc.orig
      sed -r -i -e "/RESTART_TYPE:/    s/binary|pbinary/pnc4/" \
                -e "/CHECKPOINT_TYPE:/ s/binary|pbinary/pnc4/"      $HOMDIR/AGCM.rc
      sed -r -i -e "/VEGDYN_INTERNAL_RESTART_TYPE:/ s/pnc4/binary/" $HOMDIR/AGCM.rc
      sed -r -i -e "/AGCM_IMPORT_RESTART_TYPE:/     s/pnc4/binary/" $HOMDIR/AGCM.rc
     /bin/cp -f $HOMDIR/AGCM.rc .
endif

# ---------------------------------------------------
# For MITgcm restarts - before running GEOSgcm.x
# ---------------------------------------------------

# Set-up MITgcm run directory
if (! -e mitocean_run) mkdir -p mitocean_run
cd mitocean_run

# link mit configuration and initialization files
ln -sf $EXPDIR/mit_input/* .
# link mitgcm restarts if exist
/bin/ln -sf $EXPDIR/restarts/pic* .
# make an archive folder for mitgcm run
mkdir $EXPDIR/mit_output

# Calculate segment time steps
set mit_nTimeSteps = `cat ${SCRDIR}/AGCM.rc | grep OGCM_RUN_DT: | cut -d: -f2 | tr -s " " | cut -d" " -f2`
@ mit_nTimeSteps = $dt / $mit_nTimeSteps

#change namelist variables in data - nTimeSteps, chkptFreq and monitorFreq
sed -i "s/nTimeSteps.*/nTimeSteps       = ${mit_nTimeSteps},/" data
sed -i "s/chkptFreq.*/chkptFreq        = ${dt}.0,/" data
sed -i "s/pChkptFreq.*/pChkptFreq        = ${dt}.0,/" data
# get nIter0

if (! -e ${EXPDIR}/restarts/MITgcm_restart_dates.txt ) then
  set nIter0 = `grep nIter0 data | tr -s " " | cut -d"=" -f2 | cut -d"," -f1 | awk '{$1=$1;print}'`
else
  set nIter0 = `grep "$nymdc $nhmsc" ${EXPDIR}/restarts/MITgcm_restart_dates.txt | cut -d" " -f5`
  if ( $nIter0 == "" ) then
    echo "No ocean restart file for $nymdc $nhmsc, exiting"
    echo "If this is a new initialized experiment, delete:"
    echo "${EXPDIR}/restarts/MITgcm_restart_dates.txt"
    echo "and restart"
    exit
  else
    sed -i "s/nIter0.*/ nIter0           = ${nIter0},/" data
  endif
endif

cd ..

# Environment variables for MPI, etc
# ----------------------------------


   setenv MPI_IB_TIMEOUT 23
   setenv MPI_COLL_REPRODUCIBLE
   setenv SLURM_DISTRIBUTION block

   # For some reason, PMI_RANK is randomly set and interferes
   # with binarytile.x and other executables.
   unsetenv PMI_RANK



# Run GEOSgcm.x
# -------------
       @  NPES = $NX * $NY
set USE_SHMEM = 1
if( $USE_SHMEM == 1 ) ../RmShmKeys_sshmpi.csh
$RUN_CMD $NCPUS ./GEOSgcm.x
if( $USE_SHMEM == 1 ) ../RmShmKeys_sshmpi.csh

if( -e EGRESS ) then
   set rc = 0
else
   set rc = -1
endif
echo GEOSgcm Run Status: $rc

# ---------------------------------------------------
# For MITgcm restarts - after running GEOSgcm.x
# ---------------------------------------------------

set STEADY_STATE_OCEAN=`grep STEADY_STATE_OCEAN AGCM.rc | cut -d':' -f2 | tr -d " "`

# update ocean only if activated. Otherwize use the same pickups (passive ocean).
if ( ${STEADY_STATE_OCEAN} != 0 ) then

  if ( ${rc} == 0 ) then

    # Update nIter0 for next segment
    set znIter00 = `echo $nIter0 | awk '{printf("%010d",$1)}'`
    @ nIter0 = $nIter0 + $mit_nTimeSteps
    set znIter0 = `echo $nIter0 | awk '{printf("%010d",$1)}'`

    # to update MITgcm restart list file
    sed -i "/${nIter0}/d" ${EXPDIR}/restarts/MITgcm_restart_dates.txt
    echo "Date_GEOS5 $nymdf $nhmsf NITER0_MITgcm ${nIter0}" >> ${EXPDIR}/restarts/MITgcm_restart_dates.txt

    /bin/mv $SCRDIR/mitocean_run/STDOUT.0000 $EXPDIR/mit_output/STDOUT.${znIter00}

  endif

  cd $SCRDIR/mitocean_run

  # Check existance of roling pickups
  set nonomatch rp =  ( pickup*ckptA* )
  echo $rp
  # Rename and move them if exist
  if ( -e $rp[1] ) then
    set timeStepNumber=`cat pickup.ckptA.meta | grep timeStepNumber | tr -s " " | cut -d" " -f5 | awk '{printf("%010d",$1)}'`
    foreach fname ( pickup*ckptA* )
      set bname = `echo ${fname} | cut -d "." -f1 | cut -d "/" -f2`
      set aname = `echo ${fname} | cut -d "." -f3`
      echo $EXPDIR/restarts/${bname}.${timeStepNumber}.${aname}
      /bin/mv ${fname} $EXPDIR/restarts/${bname}.${timeStepNumber}.${aname}
    end
  endif

  # Check existance of permanent pickups
  set nonomatch pp =  ( pickup* )
  echo $pp
  # Move them if exist
  if ( -e $pp[1] ) then
    foreach fname ( pickup* )
      if ( ! -e $EXPDIR/restarts/${fname} ) /bin/mv ${fname} $EXPDIR/restarts/${fname}
    end
  endif

  /bin/mv T.* $EXPDIR/mit_output/
  /bin/mv S.* $EXPDIR/mit_output/
  /bin/mv U.* $EXPDIR/mit_output/
  /bin/mv V.* $EXPDIR/mit_output/
  /bin/mv W.* $EXPDIR/mit_output/
  /bin/mv PH* $EXPDIR/mit_output/
  /bin/mv Eta.* $EXPDIR/mit_output/

  /bin/mv AREA.* $EXPDIR/mit_output/
  /bin/mv HEFF.* $EXPDIR/mit_output/
  /bin/mv HSNOW.* $EXPDIR/mit_output/
  /bin/mv UICE.* $EXPDIR/mit_output/
  /bin/mv VICE.* $EXPDIR/mit_output/
  /bin/mv FV.* $EXPDIR/mit_output/
  /bin/mv FU.* $EXPDIR/mit_output/
  /bin/mv EmPmR.* $EXPDIR/mit_output/
  /bin/mv Qsw.* $EXPDIR/mit_output/
  /bin/mv Qnet.* $EXPDIR/mit_output/

  #copy mit output to mit_output
  foreach i (`grep -i filename data.diagnostics  | grep "^ " | cut -d"=" -f2 | cut -d"'" -f2 | awk '{$1=$1;print}'`)
   /bin/mv ${i}* $EXPDIR/mit_output/
  end

  foreach i (`grep -i stat_fName data.diagnostics | grep "^ " | cut -d"=" -f2 | cut -d"'" -f2 | awk '{$1=$1;print}'`)
   /bin/mv ${i}* $EXPDIR/mit_output/
  end

  cd $SCRDIR

endif

#######################################################################
#   Rename Final Checkpoints => Restarts for Next Segment and Archive
#        Note: cap_restart contains the current NYMD and NHMS
#######################################################################

set edate  = e`cat cap_restart | cut -c1-8`_`cat cap_restart | cut -c10-11`z

set numrst = `echo $rst_types | wc -w`
set numchk = `echo $chk_types | wc -w`

@ n = 1
@ z = $numrst + 1
while ( $n <= $numchk )
   if ( -e $chk_files[$n] ) then
       @ m = 1
       while ( $m <= $numrst )
       if(    $chk_types[$n] == $rst_types[$m] || \
            \#$chk_types[$n] == $rst_types[$m]    ) then
              set  chk_ext = `cat AGCM.rc | grep ${chk_types[$n]}_CHECKPOINT_TYPE | cut -d: -f2`
              if( $chk_ext =~ *nc4    ) set ext = nc4
              if( $chk_ext =~ *binary ) set ext = bin
           /bin/mv $chk_files[$n] $rst_files[$m]
           /bin/cp $rst_files[$m] ${EXPDIR}/restarts/$EXPID.${rst_files[$m]}.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}.$ext &
           @ m = $numrst + 999
       else
           @ m = $m + 1
       endif
       end
       wait
       if( $m == $z ) then
           echo "Warning!!  Could not find CHECKPOINT/RESTART match for:  " $chk_types[$n]
           exit
       endif
   endif
@ n = $n + 1
end

/bin/cp -r RESTART ${EXPDIR}/restarts/RESTART.${edate}
/bin/cp RESTART/* INPUT

# Rename and Move Intermediate Checkpoints
# ----------------------------------------
/bin/mv -f *_checkpoint* ${EXPDIR}/restarts

cd $EXPDIR/restarts
   $GEOSBIN/stripname _checkpoint. _rst.e
    if( $FSEGMENT == 00000000 ) then
         tar cf  restarts.${edate}.tar $EXPID.*.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}.*
        /bin/rm -rf `/bin/ls -d -1     $EXPID.*.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}.*`
    endif
cd $SCRDIR

#######################################################################
#               Move HISTORY Files to Holding Directory
#######################################################################

# Check for files waiting in /holding
# -----------------------------------
set     waiting_files = `/bin/ls -1 $EXPDIR/holding/*/*nc4`
set num_waiting_files = $#waiting_files

# Move current files to /holding
# ------------------------------
foreach collection ( $collections )
   /bin/mv `/bin/ls -1 *.${collection}.*` $EXPDIR/holding/$collection
end

# MOM-Specific Output Files
# -------------------------
 set dsets="ocean_month"
 foreach dset ( $dsets )
 set num = `/bin/ls -1 $dset.nc | wc -l`
 if($num != 0) then
    if(! -e $EXPDIR/MOM_Output) mkdir -p $EXPDIR/MOM_Output
    /bin/mv $SCRDIR/$dset.nc $EXPDIR/MOM_Output/$dset.${edate}.nc
 endif
 end

#######################################################################
#              Submit Post-Processing and Forecasts
#######################################################################

cd   $EXPDIR/post

foreach collection ( $collections ) # Note: Change $collections to "ALL" to run Single Post Job

/bin/rm -f sedfile
cat >      sedfile << EOF
s/@POST_O/gcm_post.${collection}.${edate}/g
s/@COLLECTION/$collection/g
EOF
sed -f sedfile gcm_post.j > gcm_post.jtmp
chmod 755  gcm_post.jtmp
 source      gcm_post.jtmp
/bin/rm -f gcm_post.jtmp
/bin/rm -f sedfile
sleep 5
end

cd   $SCRDIR
 
if( $FSEGMENT != 00000000 ) then
     set REPLAY_BEG_DATE = `grep BEG_REPDATE: $HOMDIR/CAP.rc | cut -d':' -f2`
     set REPLAY_END_DATE = `grep END_REPDATE: $HOMDIR/CAP.rc | cut -d':' -f2`
     set nday  = `echo $FSEGMENT | cut -c7-8`
         @ dt  = 10800 - 86400 * $nday
     set date  = `$GEOSBIN/tick $nymdc $nhmsc $dt`
     set nymdz =  $date[1]
     set nhmsz =  $date[2]

     if( $nymdz >= ${REPLAY_BEG_DATE} & \
         $nymdz <= ${REPLAY_END_DATE} ) then
         $EXPDIR/forecasts/gcm_forecast.setup $nymdz $nymdz $FSEGMENT
         source $EXPDIR/forecasts/gcm_forecast.j${nymdz}-${nymdz}
     endif
endif

#######################################################################
#                         Update Iteration Counter
#######################################################################

set enddate = `echo  $END_DATE | cut -c1-8`
set endtime = `echo  $END_DATE | cut -c10-15`
set capdate = `cat cap_restart | cut -c1-8`
set captime = `cat cap_restart | cut -c10-15`

if ( $capdate < $enddate | \
     $capdate == $enddate & $captime < $endtime ) then
@ counter = $counter    + 1
else
@ counter = ${NUM_SGMT} + 1
endif

end

#######################################################################
#                              Re-Submit Job
#######################################################################

foreach rst ( $rst_files )
   /bin/rm -f $EXPDIR/$rst  
end
   /bin/rm -f $EXPDIR/cap_restart

foreach rst ( $rst_files )
  /bin/cp $rst $EXPDIR/$rst & 
end
wait
/bin/cp cap_restart $EXPDIR/cap_restart

/bin/cp -rf RESTART $EXPDIR

if ( $rc == 0 ) then
      cd   $HOMDIR
# To resubmit in the queue:
#      if ( $capdate < $enddate ) source $HOMDIR/gcm_run.j
#      if ( $capdate == $enddate & $captime < $endtime ) source $HOMDIR/gcm_run.j
# To resubmit in an interactive node:
      if ( $capdate < $enddate ) $HOMDIR/gcm_run.j       
      if ( $capdate == $enddate & $captime < $endtime ) $HOMDIR/gcm_run.j 
endif
