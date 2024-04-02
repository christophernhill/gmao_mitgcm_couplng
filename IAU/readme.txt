#adapted from 
#https://github.com/GEOS-ESM/GEOS_OceanGridComp/blob/develop/MIT_GEOS5PlugMod/instructions/checkout_geosmitv11.1.1.txt
git clone https://github.com/GEOS-ESM/GEOSgcm.git
cd GEOSgcm
git checkout tags/v11.1.1
git clone https://github.com/christophernhill/gmao_mitgcm_couplng


# NAS load modules

module use -a /nobackup/gmao_SIteam/modulefiles
module load GEOSenv
mepo clone
tcsh
source @env/g5_modules


cp gmao_mitgcm_couplng/IAU/code/* src/Components/@GEOSgcm_GridComp/GEOSogcm_GridComp/@GEOS_OceanGridComp/MIT_GEOS5PlugMod/configs/c90_llc90_02/code
set pp = `pwd`

# build

mkdir build
cd build

cmake .. -DBASEDIR=$BASEDIR/Linux -DCMAKE_INSTALL_PREFIX=../install -DBUILD_MIT_OCEAN=ON -DMIT_CONFIG_ID=c90_llc90_02 -DCMAKE_Fortran_COMPILER=ifort

# add at the end for debug -DCMAKE_BUILD_TYPE=Debug

make -j 12 install

cd ../install/bin/

sed -i 's|setenv MASDIR /u/$LOGNAME/GEOS5.0/$expid|#setenv MASDIR /u/$LOGNAME/GEOS5.0/$expid|g' ../post/gcmpost.script

sed -i '596i \ \ \ \ \ \ \ set DEFAULT_HISTORY_TEMPLATE="HISTORY.AOGCM.rc.tmpl"' gcm_setup



# creating exp from install/bin/ directory

./gcm_setup

# example inputs

Enter the Experiment ID: #expID will be used for line 110 below and line 11 @working.sh
GEOSMIT6
Enter a 1-line Experiment Description:
test exp
Do you wish to CLONE an old experiment? (Default: NO or FALSE)
NO
Enter the Atmospheric Horizontal Resolution code: 
c90
Enter the Atmospheric Model Vertical Resolution: LM (Default: 72)
72
Enter Choice for Atmospheric Model Microphysics: (Default: BACM_1M)
BACM_1M

Use Hydrostatic Atmosphere? (Default: TRUE)
TRUE
Do you wish to IOSERVER? (Default: NO or FALSE)
NO
Enter the Processor Type you wish to run on:
sky
Do you wish to run the COUPLED Ocean/Sea-Ice Model? (Default: NO or FALSE)
YES

Choose an Ocean Model: (Default: MOM5)
MIT

Choose a Seaice Model: (Default: CICE4)

Enter the Ocean Horizontal Resolution: 
llc90 

Enter the Ocean Model Vertical Resolution: LM (Default: 50)
50

Enter the choice of  Land Surface Boundary Conditions using: 1 (Icarus), 2 (Default: Icarus-NLv3)
2

Enter the choice of  Land Surface Model using: 1 (Default: Catchment), 2 (CatchmentCN-CLM4.0 (CN_CLM40)), 3 (CatchmentCN-CLM4.5 (CN_CLM45))
1

Do you wish to run GOCART with Actual or Climatological Aerosols? (Enter: A (Default) or C)
A

Enter the GOCART Emission Files to use: AMIP (Default) or OPS:
AMIP

Enter HEARTBEAT_DT to use
450

Enter the filename of the HISTORY template to use
-----------------------------------------------------------
As MIT was chosen, the default HISTORY Template is HISTORY.AOGCM.rc.tmpl
  1. Hit ENTER to use the default HISTORY template (HISTORY.AOGCM.rc.tmpl)
Using HISTORY.AOGCM.rc.tmpl
 
You must now copy your Initial Conditions into: 
----------------------------------------------- 
/nobackupnfs1/hzhang1/geos5/GEOSMIT6



# config exp

#cd to exp dir

cd /nobackupnfs1/hzhang1/geos5/GEOSMIT6

#copy GEOS restarts
cp /nobackupp11/afahad/GEOSMITgcmFiles/restarts_org/* .

#copy mit input dir
cp -r /nobackupp11/afahad/GEOSMITgcmFiles/mit_input_llc90_02/ mit_input
cp ${pp}/gmao_mitgcm_couplng/IAU/input/data* mit_input/
cp /nobackup/hzhang1/for_Fahad/IAU/mask3D.data mit_input/


# copy env script
cp /nobackupp11/afahad/GEOSMITgcmFiles/geosmitenv_v11.1.1.sh .

./geosmitenv_v11.1.1.sh

#change CAP.rc
sed -i 's/JOB_SGMT:     00000032/JOB_SGMT:     00000005/' CAP.rc
sed -i 's/NUM_SGMT:     4/NUM_SGMT:     1/'               CAP.rc

cd ..
mkdir -p IAU
cd IAU

cp ${pp}/gmao_mitgcm_couplng/IAU/input/data.iau .
cp ${pp}/gmao_mitgcm_couplng/IAU/working.sh .

ln -sf /nobackup/hzhang1/for_Fahad/IAU/ecco_iau
ln -sf /nobackup/hzhang1/for_Fahad/IAU/eccov4r4
mkdir -p output_noRBCS

#submit job
#qsub working.sh @IAU wrapping gcm_run.j


