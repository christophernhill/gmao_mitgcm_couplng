DYAMOND January 2020 experiment
Udi is starting from c1440_llc2160_01, which is tagged to checkpoint67h and checkpoint_0_Z
Will make new tagless (for time being) copy in c1440_llc2160_02
(We will tag when we have the production running)
Udi's experiments ran under /nobackupp11/dmenemen/DYAMOND/DYAMOND_01 and DYAMOND_02

Timing tests for ocean-only are in MITgcm_contrib/llc_hires/llc_2160

Reproduce c1440_llc2160_02 at coarser resolution in c90_llc90_05
for I/O and tidal experiments.

GEOS-5 boundary conditions for experiments/c1440_llc2160_01 are in
/nobackup/estrobac/geos5/ICDIR/c1440_llc2160_01_data/geos/*
There may also be a copy inside  DYAMOND_01 folder (irestart folder?)

Will need to replace some of the "ln -s" with "cp" because GEOS overwrites on
its initial conditions.

Udi started a new experiment in /nobackupp11/dmenemen/DYAMOND/DYAMOND_02
File make_exp.sh has sed commands to change gcm_run.j, CAP.rc and AGCM.rc.
Udi was in the middle of preparing this experiment so maybe not complete.
In particular, we may need to copy the mit_input folder and the GEOS initial conditions.

Later we will need to change HISTORY.rc to match the DYAMOND request and
also do something similar in MITgcm.

=============================================================

The following two experiments are "verification experiments".
Please be sure to check that output in "verification" subdirectory
remains unchanged or that it is updated with explanation.

c12_cs32_01 : c12/cs32, 2-hour
 - uses GEOSodas b_geos5mit (Heracles 5.4)

c48_llc90_02 : c48/llc90, 1-day ECCO_v4_r2 set-up
 - uses GEOSodas b_geos5mit (Heracles 5.4)
 - differences from Gael's simplified ECCO_v4_r2
   #undef ALLOW_ADAMSBASHFORTH_3 in CPP_OPTIONS.h
   useCTRL=.FALSE. in data.pkg
   ab2 instead of ab3 options in data

=============================================================

The following experiments are test and development experiments.

c12_cs32_02 : c12/cs32, 1-hour
 - uses GEOSodas b_geos5mit (Heracles 5.4)
 - used for tests with pkg/exf and coupled timing strategies
 - MIT_DT=DeltaT=25; HEARTBEAT_DT=90; OGCM_DT=OGCM_RUN_DT=900

c12_cs32_03 : c12/cs32, 1-day, hourly output
 - used for testing exchange of seaice state variables
   (in prep for solving seaice dyn in Ocean component):
 compared to c12_cs32_01:
 - compile with #undef ATMOSPHERIC_LOADING and pkg/seaice
 - run with useSEAICE=T but everything turned off.

c12_cs32_04 : c12/cs32, 1-day, hourly output
 - uses GEOSodas b_geos5mit (Heracles 5.4)
 - example of increased vertical resolution for Santha
 
c12_cs32_05 : c12/cs32, 2-hours, 15 minutes output
 - HiRes configuration (similar to c720_llc1080_Restart and c90-llc90_05)
 - Suggested MITgcm code changed to pass 1+1=2

c48_llc90_01 : c48/llc90, 1-day with simplified llc90 set-up
 - uses GEOSodas b_geos5mit (Heracles 5.4)

c48_llc90_03 : c48/llc90, 1-day set-up using llc4320 parameters
 - uses GEOSodas b_geos5mit (Heracles 5.4)

c48_llc90_04 : c48/llc90, 1-day set-up using llc4320 parameters
 - uses GEOSodas b_geos5mit-i30 (Icarus 3.0)

c48_llc90_05 : c48/llc90, 1-day set-up using llc4320 parameters
 - uses GEOSodas b_geos5mit-i30 (Icarus 3.0)
 - uses GEOS USE_IOSERVER and MITgcm asyncio
 
c90_llc90_01 : Experiment without sea-ice advection

c90_llc90_02 : New experiment with sea-ice advection

c90_llc90_02a : Similar to c90_llc90_02 but with Andrea's set-up
                (updated S2S physics and chemistry and modifications
                 to run script and AGCM.rc)

c90_llc90_03 : c90/llc90 test experiment for c1440_llc2160
               set-up with standard MITgcm I/O

c90_llc90_04 : c90/llc90 test experiment for c1440_llc2160
               set-up with ASYNCIO and GEOS IO server

c90_llc90_05 : Similar to c90_llc90_02a but with MITgcm HiRes
               configuration.

c720_llc1080_Restart : after 5 days passive ocean

c720_llc1080_Debug : runs for 5 time steps

c720_llc1080_01 : setup for relaxation with passive ocean

c1440_llc2160_01 : c1440/llc2160 set-up for ~10K cores

c1440_llc2160_02 : c1440/llc2160 set-up for DYAMOND experiment

c2880_llc4320_01 : c2880/llc4320 set-up
 - uses GEOSodas b_geos5mit-i30 (Icarus 3.0)
 - uses GEOS USE_IOSERVER and MITgcm asyncio

exp_1p1eq2 : c48/cs32 1day + 1day = 2days
 - USE_CICE_Thermo: 0
 - 24 processors
 - 1+1 folder /home/estrobac/data/geos5/MITgcm5_1p1_24p
 - =2  folder /home/estrobac/data/geos5/MITgcm5_eq2_24p

exp02 : c48/cs32, 1-year, daily output
 - did not complete a full year
   20000414 to 20000916, 5 months only for some reason

exp03 : c90/cs32, 1-year, 6-hourly output
 - not yet tested
