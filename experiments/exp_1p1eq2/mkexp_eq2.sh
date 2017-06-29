#!/bin/csh/

#################################################
# Save a copy of the original gcm_run.j and use 
# it when a more then one submition is needed
#################################################
if (-e gcm_run.j_copy) then
  cp gcm_run.j_copy gcm_run.j
else
  cp gcm_run.j gcm_run.j_copy
endif

#################################################
# Update CAP.rc
#################################################
sed -i 's/^BEG_DATE: .*/BEG_DATE:     20000414 210000/' CAP.rc
sed -i 's/^END_DATE: .*/END_DATE:     20000416 210000/' CAP.rc
sed -i 's/^JOB_SGMT: .*/JOB_SGMT:     00000002 000000/' CAP.rc
sed -i 's/^NUM_SGMT: .*/NUM_SGMT:     1/' CAP.rc

#################################################
# Update cap_restart
#################################################
echo "20000414 210000" > cap_restart

#################################################
# Update gcm_run.j
#################################################
# run parameters
sed -i 's/^#PBS -l walltime=.*/#PBS -l walltime=00:10:00/' gcm_run.j
# remove two exit statements
sed -i -e '/echo GEOSgcm Run Status: $rc/{n;N;d}' gcm_run.j
sed -i '/echo GEOSgcm Run Status: $rc/{G;G}' gcm_run.j
# copy MITgcm files instead of linking
sed -i 's/^ln -sf $MITGCM_ROOT/\/bin\/cp $MITGCM_ROOT/' gcm_run.j
sed -i 's/^ln -sf $GMAO_MITGCM_COUPLNG/\/bin\/cp $GMAO_MITGCM_COUPLNG/' gcm_run.j
# Copy all restart files from previous runs
sed -i '/\/bin\/cp $MITGCM_ROOT\/verification\/global_ocean.cs32x15\/input\/pic\* ./a /bin/cp $EXPDIR/restarts/pic* .' gcm_run.j
# fix CAP.rc date variables
sed -i 's/cat CAP.rc | grep END_DATE:     | cut -d: -f2 | cut -c2-9/cat CAP.rc | grep END_DATE:     | cut -d: -f2 | tr -s " " | cut -d" " -f2/' gcm_run.j
sed -i 's/cat CAP.rc | grep END_DATE:     | cut -d: -f2 | cut -c11-16/cat CAP.rc | grep END_DATE:     | cut -d: -f2 | tr -s " " | cut -d" " -f3/' gcm_run.j
sed -i 's/cat CAP.rc | grep JOB_SGMT:     | cut -d: -f2 | cut -c2-9/cat CAP.rc | grep JOB_SGMT:     | cut -d: -f2 | tr -s " " | cut -d" " -f2/' gcm_run.j
sed -i 's/cat CAP.rc | grep JOB_SGMT:     | cut -d: -f2 | cut -c11-16/cat CAP.rc | grep JOB_SGMT:     | cut -d: -f2 | tr -s " " | cut -d" " -f3/' gcm_run.j
# add MITgcm code
sed -i '/set yearf = `echo $nymdf | cut -c1-4`/r add_code.txt' gcm_run.j
# to update MITgcm restart list file
sed -i '/echo GEOSgcm Run Status: $rc/a echo "Date_GEOS5 $nymdf $nhmsf NITER0_MITgcm ${nIter0}" >> ${EXPDIR}/restarts/MITgcm_restart_dates.txt' gcm_run.j
# copy restarts files to the restarts directory
sed -i '/\/bin\/cp -rf RESTART $EXPDIR/a /bin/cp mitocean_run/pic* $EXPDIR/restarts/' gcm_run.j
# rename mitgcm pickups
sed -i '/$RUN_CMD $NPES .\/GEOSgcm.x/a /bin/mv mitocean_run/pickup.ckptA.data mitocean_run/pickup.${znIter0}.data' gcm_run.j
sed -i '/$RUN_CMD $NPES .\/GEOSgcm.x/a /bin/mv mitocean_run/pickup.ckptA.meta mitocean_run/pickup.${znIter0}.meta' gcm_run.j
sed -i '/$RUN_CMD $NPES .\/GEOSgcm.x/a \ ' gcm_run.j








