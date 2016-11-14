% Look at GEOS-5 output with matlab.
% Compare to gmao_mitgcm_couplng/verification/mit_ocn_20000414_2200.jpg

% Replace with location of your output file
fnm='/Users/dmenemen/Desktop/TEST.mit_ocn.20000414_2200z.nc4';

% Read and plot mit_ocn variables
ncdisp(fnm)
lat=ncread(fnm,'lat');
lon=ncread(fnm,'lon');
SS=ncread(fnm,'SS');
TS=ncread(fnm,'TS');
US=ncread(fnm,'US');
VS=ncread(fnm,'VS');
clf
colormap(jet)
subplot(221)
mypcolor(lon,lat,SS')
colorbar
title('SS')
subplot(222)
mypcolor(lon,lat,TS')
colorbar
title('TS')
subplot(223)
mypcolor(lon,lat,US')
colorbar
title('US')
subplot(224)
mypcolor(lon,lat,VS')
colorbar
title('VS')
print -djpeg mit_ocn_20000414_2200.jpg
