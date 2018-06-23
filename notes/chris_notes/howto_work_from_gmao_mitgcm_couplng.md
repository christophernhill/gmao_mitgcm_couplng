
Working from gmao_mitgcm_couplng

```
tcsh
setenv ESMADIR /mnt/scratch/heracles/hhome/geos5/GEOSodas
cd ${ESMADIR}/src/GEOSgcs_GridComp/GEOSgcm_GridComp/GEOSogcm_GridComp/GEOSocean_GridComp/GuestOcean_GridComp
git clone git@github.com:christophernhill/gmao_mitgcm_couplng.git
cd gmao_mitgcm_couplng
git checkout 
git checkout sync_to_current_mitgcm
setenv MITGCM_ROOTDIR /home/heracles/MITgcm
./set_mitgcm_env.sh
```
