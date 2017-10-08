c12_cs32_01 : c12/cs32, 1-day, hourly output
 - uses GEOSodas b_geos5mit

c48_llc90_01 : c48/llc90, 1-day with simplified llc90 set-up
 - uses GEOSodas b_geos5mit

c48_llc90_02 : c48/llc90, 1-day ECCO_v4_r2 set-up
 - uses GEOSodas b_geos5mit
 - differences from Gael's simplified ECCO_v4_r2
   #undef ALLOW_ADAMSBASHFORTH_3 in CPP_OPTIONS.h
   useCTRL=.FALSE. in data.pkg
   ab2 instead of ab3 options in data

c48_llc90_03 : c48/llc90, 1-day set-up using llc4320 parameters
 - uses GEOSodas b_geos5mit

c48_llc90_04 : c48/llc90, 1-day set-up using llc4320 parameters
 - uses GEOSodas b_geos5mit-i30

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
