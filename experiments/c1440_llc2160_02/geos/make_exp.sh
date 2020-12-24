#!/bin/csh -f

##Changes to AGCM.rc

sed -i 's/SOLAR_DT:.*/SOLAR_DT: 600/g' AGCM.rc
sed -i 's/IRRAD_DT:.*/IRRAD_DT: 600/g' AGCM.rc
sed -i 's/OGCM_RUN_DT:.*/OGCM_RUN_DT: 45/g' AGCM.rc
sed -i 's/ROUTE_INT:.*/ROUTE_INT: 600/g' AGCM.rc
sed -i 's/MIT_DT:.*/MIT_DT: 45/g' AGCM.rc
sed -i 's/MIT_TAU_MAX:.*/MIT_TAU_MAX: 3600.0/g' AGCM.rc
sed -i '$ a MIT_limit_SSS: 1' AGCM.rc
sed -i 's/NUM_READERS: 32/NUM_READERS: 36/g' AGCM.rc
sed -i 's/NUM_WRITERS: 32/NUM_WRITERS: 36/g' AGCM.rc

##Changes to CAP.rc

sed -i 's/BEG_DATE:.*/BEG_DATE: 20200119 210000/g' CAP.rc
sed -i 's/END_DATE:.*/END_DATE: 21200119 210000/g' CAP.rc
sed -i '$ a MIT_limit_SSS: 1' AGCM.rc
sed -i 's/JOB_SGMT:.*/JOB_SGMT: 00000005 000000/g' CAP.rc
sed -i 's/HEARTBEAT_DT:.*/HEARTBEAT_DT: 45/g' CAP.rc
sed -i '$ a USE_IOSERVER: 0' CAP.rc
sed -i '$ a USE_SHMEM: 1' CAP.rc

##Changes to gcm_run.j

sed -i 's/\/bin\/ln -sf $GRIDDIR\/${BCRSLV1}-Pfafstetter.til.*/\/bin\/ln -sf ..\/${BCRSLV1}-Pfafstetter.til   tile.data/g' gcm_run.j
sed -i 's/\/bin\/ln -sf $GRIDDIR\/${BCRSLV1}-Runoff.bin runoff.bin/\/bin\/ln -sf ..\/runoff_new.bin runoff.bin/g' gcm_run.j
sed -i 's/\/bin\/ln -sf $GRIDDIR\/mit-llc2160-72x72.bin.*/\/bin\/ln -sf ..\/mit-llc2160-72x72.bin ./g' gcm_run.j

sed -i 's/\/bin\/ln -sf $BCSDIR\/$BCRSLV\/visdf_${AGCM_IM}x${AGCM_JM}.dat visdf.dat/\/bin\/ln -sf ..\/visdf_c1440.dat visdf.dat/g' gcm_run.j
sed -i 's/\/bin\/ln -sf $BCSDIR\/$BCRSLV\/nirdf_${AGCM_IM}x${AGCM_JM}.dat nirdf.dat/\/bin\/ln -sf ..\/nirdf_c1440.dat nirdf.dat/g' gcm_run.j
sed -i 's/\/bin\/ln -sf $BCSDIR\/$BCRSLV\/vegdyn_${AGCM_IM}x${AGCM_JM}.dat vegdyn.data/\/bin\/ln -sf ..\/vegdyn.dat vegdyn.data/g' gcm_run.j
sed -i 's/\/bin\/ln -sf $BCSDIR\/$BCRSLV\/lai_clim_${AGCM_IM}x${AGCM_JM}.data lai.data/\/bin\/ln -sf ..\/lai_clim_c1440.data lai.data/g' gcm_run.j
sed -i 's/\/bin\/ln -sf $BCSDIR\/$BCRSLV\/green_clim_${AGCM_IM}x${AGCM_JM}.data green.data/\/bin\/ln -sf ..\/green_clim_c1440.data green.data/g' gcm_run.j
sed -i 's/\/bin\/ln -sf $BCSDIR\/$BCRSLV\/topo_DYN_ave_${AGCM_IM}x${AGCM_JM}.data topo_dynave.data/\/bin\/ln -sf ..\/topo_DYN_ave_${AGCM_IM}x${AGCM_JM}.data topo_dynave.data/g' gcm_run.j
sed -i 's/\/bin\/ln -sf $BCSDIR\/$BCRSLV\/topo_GWD_var_${AGCM_IM}x${AGCM_JM}.data topo_gwdvar.data/\/bin\/ln -sf ..\/topo_GWD_var_${AGCM_IM}x${AGCM_JM}.data topo_gwdvar.data/g' gcm_run.j
sed -i 's/\/bin\/ln -sf $BCSDIR\/$BCRSLV\/topo_TRB_var_${AGCM_IM}x${AGCM_JM}.data topo_trbvar.data/\/bin\/ln -sf ..\/topo_TRB_var_${AGCM_IM}x${AGCM_JM}.data topo_trbvar.data/g' gcm_run.j

#sed -i 's/set MERRA2_Transition_Date.*/set MERRA2_Transition_Date = 22020401/g' gcm_run.j
sed -i '/MPI_COLL_REPRODUCIBLE/i \ \ \ setenv MPI_IB_TIMEOUT 23' gcm_run.j

sed -i '/S .\/GEOSgcm.x/i set USE_SHMEM = 1' gcm_run.j
sed -i '/S .\/GEOSgcm.x/i if( $USE_SHMEM == 1 ) ../RmShmKeys_sshmpi.csh' gcm_run.j
sed -i '/S .\/GEOSgcm.x/a if( $USE_SHMEM == 1 ) ../RmShmKeys_sshmpi.csh' gcm_run.j

sed -i '/VICE/a \ \ \/bin\/mv Qnet.* $EXPDIR\/mit_output/' gcm_run.j
sed -i '/VICE/a \ \ \/bin\/mv Qsw.* $EXPDIR\/mit_output/' gcm_run.j
sed -i '/VICE/a \ \ \/bin\/mv EmPmR.* $EXPDIR\/mit_output/' gcm_run.j
sed -i '/VICE/a \ \ \/bin\/mv FU.* $EXPDIR\/mit_output/' gcm_run.j
sed -i '/VICE/a \ \ \/bin\/mv FV.* $EXPDIR\/mit_output/' gcm_run.j

sed -i 's/$rst ) \/bin\/cp/$rst ) \/bin\/cp -L/g' gcm_run.j
sed -i 's/cp $rst/cp -L $rst/g' gcm_run.j

sed -i 's/qsub/source/g' gcm_run.j
#sed -i 's/EMISSIONS MERRA2/EMISSIONS g5chem/g' gcm_run.j
