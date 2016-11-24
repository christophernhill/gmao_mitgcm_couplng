% Compare GEOS-5 and MITgcm output files.

% Initialize variables
get_MITgcm
lon=-179.5:179.5;
lat=-89.5:90;
del=cube2latlon_preprocess(XC,YC,lon,lat);
colormap(jet)

% Plot oceQsw, oceQnet, oceFWflx, and oceSflux
% oceQsw   : Net Short-Wave radiation in W/m^2
%            > 0 increases theta (ocean warming)
%            = sum(SWHEAT(k=1:Nr))
% oceQnet  : Net surface heat flux into ocean in W/m^2
%            > 0 increases theta (ocean warming)
%            = HFLX + sum(SWHEAT(k=1:Nr))
% oceFWflx : Net surface freshwater flux into ocean in kg/m^2/s
%            > 0 decreases salinity (ocean freshening)
%            = QFLX
% oceSflux : Net surface salt flux into ocean in g/m^2/s
%            > 0 increases salinity (ocean saltening)
%            = 0 (SFLX is ignored)
% All fields on southwest C-grid tracer point (i.e., A-grid).

tmp=mean(oceQsw,3);
gm=sum(tmp(:).*RAC(:))/1e15;
fld=cube2latlon_fast(del,tmp);
clf, subplot(221)
pcolor(lon,lat,fld'), shading flat
caxis([-1 1]*150); colorbar
title(['SWHEAT (W/m^2), ' int2str(gm) ' PW warming'])

tmp=mean(oceQnet,3);
gm=sum(tmp(:).*RAC(:))/1e15;
fld=cube2latlon_fast(del,tmp);
subplot(222)
pcolor(lon,lat,fld'), shading flat
caxis([-1 1]*400), colorbar
title(['HFLX+SWHEAT (W/m^2), ' int2str(gm) ' PW warming'])

tmp=mean(oceFWflx,3);
gm=sum(tmp(:).*RAC(:))/1e9;
fld=cube2latlon_fast(del,tmp);
subplot(223)
pcolor(lon,lat,-fld'), shading flat
caxis([-1 1]*2e-4), colorbar
title(['-QFLX (kg/m^2/s), ' int2str(gm) ' Gg/s freshening'])
pause

% Plot oceTAUX, oceTAUY, UVEL1, and VVEL1
% oceTAUX : x-direction surface wind stress in N/m^2 
%           > 0 increases uVel
% oceTAUY : y-direction surface wind stress in N/m^2 
%           >0 increases vVel
%   UVEL1 : x-direction surface velocity in m/s
%   VVEL1 : y-direction surface velocity in m/s
% All fields on southwest C-grid velocity points but are
% interpolated to A-grid and rotated East/North for plotting

[tE,tN]=rotate_uv2uvEN(mean(oceTAUX,3),mean(oceTAUY,3),AngleCS,AngleSN);
fldX=cube2latlon_fast(del,tE);
fldY=cube2latlon_fast(del,tN);
clf, subplot(221)
pcolor(lon,lat,fldX'), shading flat
caxis([-1 1]/4); colorbar
title('TAUX (N/m^2)')
subplot(222)
pcolor(lon,lat,fldY'), shading flat
caxis([-1 1]/4); colorbar
title('TAUY (N/m^2)')

[uE,uN]=rotate_uv2uvEN(mean(UVEL1,3),mean(VVEL1,3),AngleCS,AngleSN);
fldX=cube2latlon_fast(del,uE);
fldY=cube2latlon_fast(del,uN);
subplot(223)
pcolor(lon,lat,fldX'), shading flat
caxis([-1 1]/4); colorbar
title('UVEL1 (m/s)')
subplot(224)
pcolor(lon,lat,fldY'), shading flat
caxis([-1 1]/4); colorbar
title('VVEL1 (m/s)')
