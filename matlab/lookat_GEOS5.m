% Looking at GEOS-5 output with matlab
% Compare output from path pn1 to path pn2
pn1='/Users/dmenemen/Desktop/TEST3/'
pn2='/Users/dmenemen/Desktop/TEST4/'
fn='TEST.mit_ocn.20000415_2100z.nc4';

% readin in and plot pn1 variables
fnm=[pn1 fn];
ncdisp(fnm)
lat=ncread(fnm,'lat');
lon=ncread(fnm,'lon');
SS=ncread(fnm,'SS');
TS=ncread(fnm,'TS');
US=ncread(fnm,'US');
VS=ncread(fnm,'VS');
figure(1)
clf
subplot(221)
pcolor(lon,lat,SS')
shading flat
colorbar
title('SS')
subplot(222)
pcolor(lon,lat,TS')
shading flat
colorbar
title('TS')
subplot(223)
pcolor(lon,lat,US')
shading flat
colorbar
title('US')
subplot(224)
pcolor(lon,lat,VS')
shading flat
colorbar
title('VS')

% readin in and plot pn2 variables
fnm=[pn2 fn];
ncdisp(fnm)
lat=ncread(fnm,'lat');
lon=ncread(fnm,'lon');
SS=ncread(fnm,'SS');
TS=ncread(fnm,'TS');
US=ncread(fnm,'US');
VS=ncread(fnm,'VS');
figure(2)
clf
subplot(221)
pcolor(lon,lat,SS')
shading flat
colorbar
title('SS')
subplot(222)
pcolor(lon,lat,TS')
shading flat
colorbar
title('TS')
subplot(223)
pcolor(lon,lat,US')
shading flat
colorbar
title('US')
subplot(224)
pcolor(lon,lat,VS')
shading flat
colorbar
title('VS')
