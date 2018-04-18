#!/bin/csh/

setenv EXPID `grep EXPID: HISTORY.rc | cut -d':' -f2 | tr -d " "`
setenv EXPDSC `grep EXPDSC: HISTORY.rc | cut -d':' -f2 | tr -d " "`
setenv EXPDIR `grep setenv gcm_run.j | grep " EXPDIR" | tr -s " " | cut -d" " -f3`
setenv HOMDIR `grep setenv gcm_run.j | grep " HOMDIR" | tr -s " " | cut -d" " -f3`


cp /nobackupp2/estrobac/geos5/ICDIR/c720_llc1080_Debug_data/geos/* .

sed -i "s|EXPID: .*|EXPID: ${EXPID} |" HISTORY.rc
sed -i "s|EXPDSC: .*|EXPDSC: ${EXPDSC} |" HISTORY.rc

sed -i "s|#PBS -N .*|#PBS -N ${EXPID}_RUN |" gcm_run.j

sed -i "s|setenv GEOSDIR .*|setenv GEOSDIR ${WorkingDir}/GEOSodas |" gcm_run.j
sed -i "s|setenv GEOSBIN .*|setenv GEOSBIN ${WorkingDir}/GEOSodas/Linux/bin |" gcm_run.j

sed -i "s|setenv .*EXPID .*|setenv EXPID ${EXPID} |" gcm_run.j | grep EXPID
sed -i "s|setenv .*EXPDIR .*|setenv EXPDIR ${EXPDIR} |" gcm_run.j
sed -i "s|setenv .*HOMDIR .*|setenv HOMDIR ${HOMDIR} |" gcm_run.j
