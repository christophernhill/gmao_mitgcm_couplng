#!/bin/csh -f

#######################################################################
#                     Batch Parameters for Run Job
#######################################################################

##PBS -l walltime=48:00:00
##PBS -l walltime=8:00:00
##PBS -l walltime=1:00:00

# Per an email from Greg Matthews:
#
# One quick note about use of the testing_free queue. The available time
# to run is roughly 2 days (10am on Saturday is the endpoint). qsub can
# be made less painful on future attempts by using something like this:
#
# qsub -I -l select=1:model=sky_ele:mpiprocs=40:ncpus=40 -l
# max_walltime=52:00:00,min_walltime=1:00:00 -q testing_free
#
# max_walltime can be left as 52:00:00, and min_walltime can be set to
# whatever useful amount of time is needed for the job. PBS will assign
# the job the longest walltime that will fit, >= min_walltime.

#PBS -l max_walltime=52:00:00,min_walltime=24:00:00

# For 43200 core jobs + 5 ioserver nodes + 20 asyncio 
#PBS -l select=1105:ncpus=40:mpiprocs=40:model=sky_ele

#PBS -N I30UFVR8-C2_RUN
#PBS -q testing_free
#PBS -W group_list=g26209
#@PBS -o gcm_run.o@RSTDATE
#PBS -m abe
#PBS -W umask=0022

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
setenv GEOSDIR          /nobackupp2/dmenemen/c2880_llc4320/GEOSodas 
setenv GEOSBIN          /nobackupp2/dmenemen/c2880_llc4320/GEOSodas/Linux/bin 
setenv GEOSUTIL         /nobackupp2/dmenemen/c2880_llc4320/GEOSodas/src/GMAO_Shared/GEOS_Util
setenv RUN_CMD         "mpiexec_mpt -np "
setenv GCMVER           Icarus-3_0_UNSTABLE_FV_R8-NAS_C2880

source $GEOSBIN/g5_modules
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${BASEDIR}/${ARCH}/lib

#######################################################################
#             Experiment Specific Environment Variables
#######################################################################


setenv  EXPID   llc4320
setenv  EXPDIR  /nobackupp2/dmenemen/geos5/llc4320
setenv  HOMDIR  /nobackupp2/dmenemen/geos5/llc4320

setenv  RSTDATE @RSTDATE
setenv  GCMEMIP @GCMEMIP

#######################################################################
#                     Set up RC files correctly
#######################################################################

/bin/rm $HOMDIR/input.nml

/usr/local/bin/mcp -a $HOMDIR/fvcore_layout.rc $HOMDIR/input.nml

#######################################################################
#                 Create Experiment Sub-Directories
#######################################################################

if (! -e $EXPDIR/restarts   ) mkdir -p $EXPDIR/restarts
lfs setstripe -c 16 $EXPDIR/restarts
if (! -e $EXPDIR/holding    ) mkdir -p $EXPDIR/holding
if (! -e $EXPDIR/archive    ) mkdir -p $EXPDIR/archive
if (! -e $EXPDIR/post       ) mkdir -p $EXPDIR/post
if (! -e $EXPDIR/plot       ) mkdir -p $EXPDIR/plot

if( $GCMEMIP == TRUE ) then
    if (! -e $EXPDIR/restarts/$RSTDATE ) mkdir -p $EXPDIR/restarts/$RSTDATE
    setenv  SCRDIR  $EXPDIR/scratch.$RSTDATE
else
    setenv  SCRDIR  $EXPDIR/scratch
endif

if (! -e $SCRDIR ) mkdir -p $SCRDIR
lfs setstripe -c 12 $SCRDIR

#######################################################################
#                   Set Experiment Run Parameters
#######################################################################

set       NX  = `grep      "^ *NX:" $HOMDIR/AGCM.rc | cut -d':' -f2`
set       NY  = `grep      "^ *NY:" $HOMDIR/AGCM.rc | cut -d':' -f2`
set  AGCM_IM = `grep      AGCM_IM: $HOMDIR/AGCM.rc | cut -d':' -f2`
set  AGCM_JM = `grep      AGCM_JM: $HOMDIR/AGCM.rc | cut -d':' -f2`
set  AGCM_LM = `grep      AGCM_LM: $HOMDIR/AGCM.rc | cut -d':' -f2`
set  OGCM_IM = `grep      OGCM_IM: $HOMDIR/AGCM.rc | cut -d':' -f2`
set  OGCM_JM = `grep      OGCM_JM: $HOMDIR/AGCM.rc | cut -d':' -f2`
set END_DATE = `grep     END_DATE:  $HOMDIR/CAP.rc | cut -d':' -f2`
set NUM_SGMT = `grep     NUM_SGMT:  $HOMDIR/CAP.rc | cut -d':' -f2`
set FSEGMENT = `grep FCST_SEGMENT:  $HOMDIR/CAP.rc | cut -d':' -f2`


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

#######################################################################
#                       GCMEMIP Setup
#######################################################################

if( $GCMEMIP == TRUE & ! -e $EXPDIR/restarts/$RSTDATE/cap_restart ) then

cd $EXPDIR/restarts/$RSTDATE

set      FILE = strip
/bin/rm $FILE
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

/usr/local/bin/mcp -a $HOMDIR/CAP.rc .
./strip         CAP.rc

set year  = `echo $RSTDATE | cut -d_ -f1 | cut -b1-4`
set month = `echo $RSTDATE | cut -d_ -f1 | cut -b5-6`

# Copy MERRA-2 Restarts
# ---------------------
#/usr/local/bin/mcp -a /discover/nobackup/projects/gmao/g6dev/ltakacs/MERRA2/restarts/AMIP/M${month}/restarts.${year}${month}.tar .
#mtar xf  restarts.${year}${month}.tar
#/bin/rm restarts.${year}${month}.tar
#/bin/rm MERRA2*bin


# Regrid MERRA-2 Restarts
# -----------------------
set RSTID = `/bin/ls *catch*bin | cut -d. -f1`
$GEOSBIN/regrid.pl -np -ymd ${year}${month}10 -hr 21 -grout C${AGCM_IM} -levsout ${AGCM_LM} -outdir . -d . -expid $RSTID -tagin Ganymed-4_0 -oceanin e -i -nobkg -lbl -nolcv -tagout Icarus -rs 3 -oceanout e
/bin/rm $RSTID.*.bin

     set IMC = $AGCM_IM
if(     $IMC < 10 ) then
     set IMC = 000$IMC
else if($IMC < 100) then
     set IMC = 00$IMC
else if($IMC < 1000) then
     set IMC = 0$IMC
endif

$GEOSBIN/stripname C${AGCM_IM}e_${RSTID}.
$GEOSBIN/stripname .${year}${month}10_21z.bin.Icarus_MERRA-2.CF${IMC}x6C_DE1440xPE0720
mv gocart_internal_rst gocart_internal_rst.merra2
$GEOSBIN/gogo.x -s $RSTID.Chem_Registry.rc.${year}${month}10_21z -t $EXPDIR/RC/Chem_Registry.rc -i gocart_internal_rst.merra2 -o gocart_internal_rst -r C${AGCM_IM} -l ${AGCM_LM}


# Create CAP.rc and cap_restart
# -----------------------------
set   nymd = ${year}${month}10
set   nhms = 210000
echo $nymd $nhms > cap_restart

set curmonth = $month
      @ count = 0
while( $count < 4 )
       set date  = `$GEOSBIN/tick $nymd $nhms 86400`
       set nymd  =  $date[1]
       set nhms  =  $date[2]
       set year  = `echo $nymd | cut -c1-4`
       set month = `echo $nymd | cut -c5-6`
       if( $curmonth != $month ) then
        set curmonth  = $month
             @ count  = $count + 1
       endif
end
set oldstring =  `cat CAP.rc | grep END_DATE:`
set newstring =  "END_DATE: ${year}${month}01 210000"
/bin/mv CAP.rc CAP.tmp
cat CAP.tmp | sed -e "s?$oldstring?$newstring?g" > CAP.rc
/bin/rm CAP.tmp

endif

# ----------------------------------------------
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


rm -rf input.nml
ln -s fvcore_layout.rc input.nml

#######################################################################
#   Move to Scratch Directory and Copy RC Files from Home Directory
#######################################################################

cd $SCRDIR
/bin/rm -rf *
                             /bin/ln -sf $EXPDIR/RC/* .
                             /bin/ln -sf $EXPDIR/tile.bin .
                             /bin/ln -sf $EXPDIR/*c2l*nc4 .
                             /usr/local/bin/mcp -a     $EXPDIR/cap_restart .
                             /usr/local/bin/mcp -a -f  $HOMDIR/*.rc .
                             /usr/local/bin/mcp -a -f  $HOMDIR/*.nml .

if( $GCMEMIP == TRUE ) then
    /usr/local/bin/mcp -a -f  $EXPDIR/restarts/$RSTDATE/cap_restart .
    /usr/local/bin/mcp -a -f  $EXPDIR/restarts/$RSTDATE/CAP.rc .
endif

set END_DATE  = `grep     END_DATE:  CAP.rc | cut -d':' -f2`
set NUM_SGMT  = `grep     NUM_SGMT:  CAP.rc | cut -d':' -f2`
set FSEGMENT  = `grep FCST_SEGMENT:  CAP.rc | cut -d':' -f2`
set USE_SHMEM = `grep    USE_SHMEM:  CAP.rc | cut -d':' -f2`

#######################################################################
#         Create Strip Utility to Remove Multiple Blank Spaces
#######################################################################

set      FILE = strip
/bin/rm $FILE
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

setenv BCSDIR    /nobackup/gmao_SIteam/ModelData/bcs/Icarus/Icarus_Ostia
setenv SSTDIR    /nobackupp8/dmenemen/GEOS5/experiments/llc4320/rst
setenv SSTDIR    /nobackup/atrayano/MIT/bcs
setenv CHMDIR    /nobackup/gmao_SIteam/ModelData/fvInput_nc3
setenv BCRSLV    ${ATMOStag}_${OCEANtag}
setenv BCRSLV1    ${ATMOStag}_${OCEAN1tag}
setenv DATELINE  DC
setenv EMISSIONS MERRA2-DD

setenv GRIDDIR  ${SSTDIR}
setenv BCTAG `basename $GRIDDIR`

# Set-up MITgcm run directory
mkdir mitocean_run
cd mitocean_run

# link mit configuration and initialization files
ln -sf $EXPDIR/mit_input/* .
# link mitgcm restarts if exist
/bin/ln -sf $EXPDIR/restarts/pic* .
# make an archive folder for mitgcm run
mkdir $EXPDIR/mit_output

cd ..

set             FILE = linkbcs
/bin/rm -f     $FILE
cat << _EOF_ > $FILE
#!/bin/csh -f

/bin/mkdir -p RESTART
/bin/mkdir -p            ExtData
/bin/ln    -sf $CHMDIR/* ExtData

/bin/ln -sf $GRIDDIR/${BCRSLV1}-Pfafstetter.til   tile.data
/bin/ln -sf $GRIDDIR/${BCRSLV1}-Pfafstetter.TRN   runoff.bin
/bin/ln -sf $GRIDDIR/mit-llc4320.ascii .



/bin/ln -sf $BCSDIR/Shared/pchem.species.CMIP-5.1870-2097.z_91x72.nc4 species.data
/bin/ln -sf $BCSDIR/Shared/*bin .
/bin/ln -sf $BCSDIR/Shared/*c2l*.nc4 .

/bin/ln -sf $SSTDIR/visdf.dat
/bin/ln -sf $SSTDIR/nirdf.dat
/bin/ln -sf $SSTDIR/vegdyn.data
/bin/ln -sf $SSTDIR/lai.data
/bin/ln -sf $SSTDIR/green.data
/bin/ln -sf $BCSDIR/$BCRSLV/topo_DYN_ave_${AGCM_IM}x${AGCM_JM}.data topo_dynave.data
/bin/ln -sf $BCSDIR/$BCRSLV/topo_GWD_var_${AGCM_IM}x${AGCM_JM}.data topo_gwdvar.data
/bin/ln -sf $BCSDIR/$BCRSLV/topo_TRB_var_${AGCM_IM}x${AGCM_JM}.data topo_trbvar.data

if(     -e  $BCSDIR/$BCRSLV/Gnomonic_$BCRSLV.dat ) then
/bin/ln -sf $BCSDIR/$BCRSLV/Gnomonic_$BCRSLV.dat .
endif


_EOF_


chmod +x linkbcs
/usr/local/bin/mcp -a  linkbcs $EXPDIR

#######################################################################
#          Get C2L History weights/index file for Cubed-Sphere
#######################################################################

set C_NPX = `echo $AGCM_IM | awk '{printf "%5.5i", $1}'`
set C_NPY = `echo $AGCM_JM | awk '{printf "%5.5i", $1}'`
set H_NPX = `echo 11520 | awk '{printf "%5.5i", $1}'`
set H_NPY = `echo 5761 | awk '{printf "%5.5i", $1}'`

set c2l_file = "${C_NPX}x${C_NPY}_c2l_${H_NPX}x${H_NPY}.bin"

if (-e $BCSDIR/$BCRSLV/${c2l_file}) /bin/ln -s $BCSDIR/$BCRSLV/${c2l_file} .

#######################################################################
#                    Get Executable and RESTARTS 
#######################################################################

/usr/local/bin/mcp -a $EXPDIR/GEOSgcm.x .

set rst_files      = `cat AGCM.rc | grep "RESTART_FILE"    | grep -v VEGDYN | grep -v "#" | cut -d ":" -f1 | cut -d "_" -f1-2`
set rst_file_names = `cat AGCM.rc | grep "RESTART_FILE"    | grep -v VEGDYN | grep -v "#" | cut -d ":" -f2`

set chk_files      = `cat AGCM.rc | grep "CHECKPOINT_FILE" | grep -v "#" | cut -d ":" -f1 | cut -d "_" -f1-2`
set chk_file_names = `cat AGCM.rc | grep "CHECKPOINT_FILE" | grep -v "#" | cut -d ":" -f2`

# Remove possible bootstrap parameters (+/-)
# ------------------------------------------
set dummy = `echo $rst_file_names`
set rst_file_names = ''
foreach rst ( $dummy )
  set length  = `echo $rst | awk '{print length($0)}'`
  set    bit  = `echo $rst | cut -c1`
  if(  "$bit" == "+" | \
       "$bit" == "-" ) set rst = `echo $rst | cut -c2-$length`
  set rst_file_names = `echo $rst_file_names $rst`
end

# Copy Restarts to Scratch Directory
# ----------------------------------
if( $GCMEMIP == TRUE ) then
    foreach rst ( $rst_file_names )
      if(-e $EXPDIR/restarts/$RSTDATE/$rst ) /usr/local/bin/mcp -a $EXPDIR/restarts/$RSTDATE/$rst . &
    end
else
    foreach rst ( $rst_file_names )
      if(-e $EXPDIR/$rst ) /bin/ln -s $EXPDIR/$rst . &
    end
endif
wait



# Copy and Tar Initial Restarts to Restarts Directory
# ---------------------------------------------------
set edate = e`cat cap_restart | cut -c1-8`_`cat cap_restart | cut -c10-11`z
set numrs = `/bin/ls -1 ${EXPDIR}/restarts/*${edate}* | wc -l`
if($numrs == 0) then
   foreach rst ( $rst_file_names )
      if( -e $rst & ! -e ${EXPDIR}/restarts/$EXPID.${rst}.${edate}.${GCMVER}.${BCTAG}_${BCRSLV} ) then
            /usr/local/bin/mcp -a $rst ${EXPDIR}/restarts/$EXPID.${rst}.${edate}.${GCMVER}.${BCTAG}_${BCRSLV} &
      endif
   end
   wait
   cd $EXPDIR/restarts
      mtar cf  restarts.${edate}.tar $EXPID.*.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}
     /bin/rm -rf `/bin/ls -d -1     $EXPID.*.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}`
   cd $SCRDIR
endif

# If any restart is binary, set NUM_READERS to 1 so that
# +-style bootstrapping of missing files can occur in 
# MAPL. pbinary cannot do this, but pnc4 can.
# ------------------------------------------------------
#set found_binary = 0

#foreach rst ( $rst_file_names )
   #if (-e $rst) then
      #set rst_type = `/usr/bin/file -Lb --mime-type $rst`
      #if ( $rst_type =~ "application/octet-stream" ) then
         #set found_binary = 1
      #endif
   #endif
#end

#if ($found_binary == 1) then
   #/bin/mv AGCM.rc AGCM.tmp
   #cat AGCM.tmp | sed -e "/^NUM_READERS/ s/\([0-9]\+\)/1/g" > AGCM.rc
   #/bin/rm AGCM.tmp
#endif


##################################################################
######
######         Perform multiple iterations of Model Run
######
##################################################################

@ counter    = 1
while ( $counter <= ${NUM_SGMT} )

/bin/rm -f  EGRESS

if( $GCMEMIP == TRUE ) then
    /usr/local/bin/mcp -a -f  $EXPDIR/restarts/$RSTDATE/CAP.rc .
else
    /usr/local/bin/mcp -a -f $HOMDIR/CAP.rc .
endif

./strip CAP.rc

# Set Time Variables for Current_(c), Ending_(e), and Segment_(s) dates 
# ---------------------------------------------------------------------
set nymdc = `cat cap_restart | cut -c1-8`
set nhmsc = `cat cap_restart | cut -c10-15`
set nymde = `cat CAP.rc | grep END_DATE:     | cut -d: -f2 | cut -c2-9`
set nhmse = `cat CAP.rc | grep END_DATE:     | cut -d: -f2 | cut -c11-16`
set nymds = `cat CAP.rc | grep JOB_SGMT:     | cut -d: -f2 | cut -c2-9`
set nhmss = `cat CAP.rc | grep JOB_SGMT:     | cut -d: -f2 | cut -c11-16`

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

# For Non-Reynolds SST, Modify local CAP.rc Ending date if Finish time exceeds Current year boundary
# --------------------------------------------------------------------------------------------------
if( CF2880x6C != DE0360xPE0180 ) then
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
    set MERRA2_Transition_Date = 20000401

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
        /usr/local/bin/mcp -a --remove-destination ${MERRA2_EMISSIONS_DIRECTORY}/*.rc .
    else
        set files =      `/bin/ls -1 ${MERRA2_EMISSIONS_DIRECTORY}/*.rc`
        foreach file ($files)
          /bin/rm -f   `basename $file`
          /bin/rm -f    dummy
          /usr/local/bin/mcp -a $file dummy
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

#if (! -e tile.bin) then
#$RUN_CMD 1 $GEOSBIN/binarytile.x tile.data tile.bin
#endif

# Test Saltwater Restart for Number of tiles correctness
# ------------------------------------------------------

#if ( -x $GEOSBIN/rs_numtiles.x ) then

   #set N_SALT_TILES_EXPECTED = `grep '^ *0' tile.data | wc -l`
   #set N_SALT_TILES_FOUND = `$RUN_CMD 1 $GEOSBIN/rs_numtiles.x saltwater_internal_rst | grep Total | awk '{print $3}'`
         
   #if ( $N_SALT_TILES_EXPECTED != $N_SALT_TILES_FOUND ) then
      #echo "Error! Found $N_SALT_TILES_FOUND tiles in saltwater. Expect to find $N_SALT_TILES_EXPECTED tiles."
      #echo "Your restarts are probably for a different ocean."
      #exit 7
   #endif    

#endif

# ---------------------------------------------------
# For MITgcm restarts - before running GEOSgcm.x
# ---------------------------------------------------

cd mitocean_run

# Calculate segment time steps
set mit_nTimeSteps = `cat ${SCRDIR}/AGCM.rc | grep OGCM_RUN_DT: | cut -d: -f2 | tr -s " " | cut -d" " -f2`
@ mit_nTimeSteps = $dt / $mit_nTimeSteps

#change namelist variables in data - nTimeSteps, chkptFreq and monitorFreq
if ( ${counter} == 1 ) then
  sed -i "s/nTimeSteps.*/ nTimeSteps       = ${mit_nTimeSteps},/" data
  sed -i "s/chkptFreq.*/ chkptFreq        = ${dt}.0,/" data
  sed -i "s/pChkptFreq.*/ pChkptFreq        = ${dt}.0,/" data
endif
# get nIter0

if (! -e ${EXPDIR}/restarts/MITgcm_restart_dates.txt ) then
  set nIter0 = `grep nIter0 data | tr -s " " | cut -d"=" -f2 | cut -d"," -f1 | awk '{$1=$1;print}'`
else
  set nIter0 = `grep "$nymdc $nhmsc" ${EXPDIR}/restarts/MITgcm_restart_dates.txt | cut -d" " -f5`
  if ( $nIter0 == "" ) then
    echo "No ocean restart file for $nymdc $nhmsc, exiting"
    exit
  else
    sed -i "s/nIter0.*/ nIter0           = ${nIter0},/" data
    sed -i '/${nIter0}/q' ${EXPDIR}/restarts/MITgcm_restart_dates.txt
  endif
endif

   cd ..

   module swap mpi-sgi/mpt.2.15r20 test/hpe/mpt.2.17r4

# Environment variables for MPI, etc
# ----------------------------------

   mpirun -v

   setenv MPI_COLL_REPRODUCIBLE
   setenv SLURM_DISTRIBUTION block

   #setenv MPI_VERBOSE 1
   #setenv MPI_DSM_VERBOSE 1
   #setenv MPI_DISPLAY_SETTINGS 1

   setenv MPI_FASTSTART 0

   #setenv MPI_XPMEM_ENABLED no
   #setenv SUPPRESS_XPMEM_TRIM_THRESH 1
   #setenv MPI_NUM_MEMORY_REGIONS 0
   
   #setenv MPI_DISPLAY_SETTINGS 1
   #setenv MPI_VERBOSE 1
   
   #setenv MPI_COMM_MAX  1024
   #setenv MPI_GROUP_MAX 1024
   #setenv MPI_BUFS_PER_PROC 256
   
   #setenv MPI_IB_TIMEOUT 23

   # For some reason, PMI_RANK is randomly set and interferes
   # with binarytile.x and other executables.
   #unsetenv PMI_RANK

# Split off IO Nodes
# # ------------------------------------------
@           NPES = $NX * $NY
@ CORES_PER_NODE = 40
@          NODES = $NPES / $CORES_PER_NODE
@        IONODES = 5 + 20
@           NPES = $NPES + $IONODES * $CORES_PER_NODE

setenv SHORT_JOBID `echo $PBS_JOBID | cut -f2 -d.`

# Run GEOSgcm.x
# -------------
if( $USE_SHMEM == 1 ) /u/mathomp4/bin/RmShmKeys_pdsh
$RUN_CMD $NPES ./GEOSgcm.x >& ${EXPDIR}/run_${edate}_${SHORT_JOBID}.coupled.out
if( $USE_SHMEM == 1 ) /u/mathomp4/bin/RmShmKeys_pdsh


if( -e EGRESS ) then
   set rc = 0
else
   set rc = -1
endif
echo GEOSgcm Run Status: $rc

exit
 
#######################################################################
#   Rename Final Checkpoints => Restarts for Next Segment and Archive
#        Note: cap_restart contains the current NYMD and NHMS
#######################################################################

set edate  = e`cat cap_restart | cut -c1-8`_`cat cap_restart | cut -c10-11`z

# Move Intermediate Checkpoints to RESTARTS directory
# ---------------------------------------------------
# For MITgcm restarts - after running GEOSgcm.x
# ---------------------------------------------------

# Update nIter0 for next segment
set znIter00 = `echo $nIter0 | awk '{printf("%010d",$1)}'`
@ nIter0 = $nIter0 + $mit_nTimeSteps
set znIter0 = `echo $nIter0 | awk '{printf("%010d",$1)}'`

# to update MITgcm restart list file
echo "Date_GEOS5 $nymdf $nhmsf NITER0_MITgcm ${nIter0}" >> ${EXPDIR}/restarts/MITgcm_restart_dates.txt

# rename mitgcm pickups
set timeStepNumber=`cat mitocean_run/pickup.ckptA.meta | grep timeStepNumber | tr -s " " | cut -d" " -f5 | awk '{printf("%010d",$1)}'`
/bin/mv mitocean_run/pickup.ckptA.data $EXPDIR/restarts/pickup.${timeStepNumber}.data
/bin/mv mitocean_run/pickup.ckptA.meta $EXPDIR/restarts/pickup.${timeStepNumber}.meta
if ( -e mitocean_run/pickup_ggl90.ckptA.data ) /bin/cp mitocean_run/pickup_ggl90.ckptA.data $EXPDIR/restarts/pickup_ggl90.${timeStepNumber}.data
if ( -e mitocean_run/pickup_ggl90.ckptA.meta ) /bin/cp mitocean_run/pickup_ggl90.ckptA.meta $EXPDIR/restarts/pickup_ggl90.${timeStepNumber}.meta
if ( -e mitocean_run/pickup_seaice.ckptA.data ) /bin/cp mitocean_run/pickup_seaice.ckptA.data $EXPDIR/restarts/pickup_seaice.${timeStepNumber}.data
if ( -e mitocean_run/pickup_seaice.ckptA.meta ) /bin/cp mitocean_run/pickup_seaice.ckptA.meta $EXPDIR/restarts/pickup_seaice.${timeStepNumber}.meta

#copy mit output to mit_output
foreach i (`grep filename mitocean_run/data.diagnostics | cut -d"=" -f2 | cut -d"'" -f2 | awk '{$1=$1;print}'`)
 /bin/cp mitocean_run/${i}* $EXPDIR/mit_output/
end

foreach i (`grep stat_fName mitocean_run/data.diagnostics | cut -d"=" -f2 | cut -d"'" -f2 | awk '{$1=$1;print}'`)
 /bin/cp mitocean_run/${i}* $EXPDIR/mit_output/
end

# Remove Initial RESTARTS
# -----------------------
set restarts = `/bin/ls -1 *_rst`
/bin/rm  $restarts


# Copy Renamed Final Checkpoints to RESTARTS directory
# ----------------------------------------------------
    set  restarts = `/bin/ls -1 $EXPID.*_rst.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}.*`
foreach  restart ($restarts)
/usr/local/bin/mcp -a $restart ${EXPDIR}/restarts
end

# Remove EXPID from RESTART name
# ------------------------------
    set  restarts = `/bin/ls -1 $EXPID.*_rst.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}.*`
foreach  restart ($restarts)
$GEOSBIN/stripname $EXPID. '' $restart
end

# Remove DATE and VERSION Stamps from RESTART name
# ------------------------------------------------
    set  restarts = `/bin/ls -1 *_rst.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}.*`
foreach  restart ($restarts)
$GEOSBIN/stripname .${edate}.${GCMVER}.${BCTAG}_${BCRSLV}.\* '' $restart
end

/bin/cp -r RESTART ${EXPDIR}/restarts/RESTART.${edate}
/bin/cp RESTART/* INPUT

# Rename and Move Intermediate Checkpoints
# ----------------------------------------
/bin/mv -f *_checkpoint* ${EXPDIR}/restarts

cd $EXPDIR/restarts
    if( $FSEGMENT == 00000000 ) then
         mtar cf  restarts.${edate}.tar $EXPID.*.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}.*
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
#                 Run Post-Processing and Forecasts
#######################################################################

$GEOSUTIL/post/gcmpost.script -source /nobackupp2/dmenemen/geos5/llc4320 -movefiles
 
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
         $EXPDIR/forecasts/gcm_forecast.setup $nymdz $nymdz $FSEGMENT TRUE
     endif
endif

#######################################################################
#                         Update Iteration Counter
#######################################################################

set enddate = `echo  $END_DATE | cut -c1-8`
set capdate = `cat cap_restart | cut -c1-8`

if ( $capdate < $enddate ) then
@ counter = $counter    + 1
else
@ counter = ${NUM_SGMT} + 1
endif

end

#######################################################################
#                              Re-Submit Job
#######################################################################

if( $GCMEMIP == TRUE ) then
     foreach rst ( `/bin/ls -1 *_rst` )
        /bin/rm -f $EXPDIR/restarts/$RSTDATE/$rst
     end
        /bin/rm -f $EXPDIR/restarts/$RSTDATE/cap_restart
     foreach rst ( `/bin/ls -1 *_rst` )
       /bin/mv -f $rst $EXPDIR/restarts/$RSTDATE/$rst &
     end
     /usr/local/bin/mcp -a cap_restart $EXPDIR/restarts/$RSTDATE/cap_restart
else
     foreach rst ( `/bin/ls -1 *_rst` )
        /bin/rm -f $EXPDIR/$rst
     end
        /bin/rm -f $EXPDIR/cap_restart
     foreach rst ( `/bin/ls -1 *_rst` )
       /bin/mv -f $rst $EXPDIR/$rst &
     end
     /usr/local/bin/mcp -a cap_restart $EXPDIR/cap_restart
endif

/usr/local/bin/mcp -a -rf RESTART $EXPDIR

if ( $rc == 0 ) then
      cd  $HOMDIR
      if( $GCMEMIP == TRUE ) then
          if( $capdate < $enddate ) qsub $HOMDIR/gcm_run.j$RSTDATE
      else
          if( $capdate < $enddate ) qsub $HOMDIR/gcm_run.j
      endif
endif
