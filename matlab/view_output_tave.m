% Compare GEOS-5 and MITgcm output files.

% Initialize variables
get_MITgcm
mk_binavg
it=1:60;

%     Plot SWHEAT, oceQsw, HFLX, and oceQnet-oceQsw
%     SWHEAT : Solar heating rate spread over several levels in W/m^2.
%              > 0 for increase in theta (ocean warming)
%     oceQsw : W/m^2 net Short-Wave radiation (+=down), >0 increases theta
%     HFLX   : Turbulent heat flux from skin to ocean in W/m^2.
%              Includes sensible, latent, and longwave heat flux
%              that is transferred from skin to atmosphere.
%              > 0 for increase in theta (ocean warming)
%    oceQnet : W/m^2 net surface heat flux into the ocean (+=down), >0 increases theta
%              Southwest C-grid tracer point (i.e., A-grid)
cx=[-400 400];
tmp=mean(oceQsw(:,:,it),3);
gm1=sum(tmp(:).*RAC(:))/1e15;
fld2=reshape(bin_average*tmp(:),length(lon),length(lat));
fld3=mean(HFLX(:,:,it),3);
tmp=mean(oceQnet(:,:,it),3);
gm2=sum(tmp(:).*RAC(:))/1e15;
fld4=reshape(bin_average*tmp(:),length(lon),length(lat))-fld2;
clf, colormap(jet)
subplot(221)
pcolor(lon,lat,fld1'), shading flat
caxis(cx); colorbar
title('GEOS-5 SWHEAT (W/m^2)')
subplot(222)
pcolor(lon,lat,fld2'), shading flat
caxis(cx); colorbar
title(['MITgcm oceQsw (W/m^2), ' int2str(gm1) 'PW'])
subplot(223)
pcolor(lon,lat,fld3'); shading flat
caxis(cx); colorbar
title('GEOS-5 HFLX')
subplot(224)
pcolor(lon,lat,fld4'); shading flat
caxis(cx); colorbar
title('MITgcm oceQnet-oceQsw')
print -djpeg HFLX

%     Plot QFLX, oceFWflx, SFLX, and oceSflux
%     QFLX   : Freshwater flux from skin to ocean in kg/m2/s.
%   oceFWflx : kg/m^2/s net surface Fresh-Water flux into the ocean
%              (+=down), >0 decreases salinity
%     SFLX   : Salt flux from skin to ocean in TBD units.
%   oceSflux : g/m^2/s net surface Salt flux into the ocean
%              (+=down), >0 increases salinity
%              Southwest C-grid tracer point (i.e., A-grid)
cx=[-1 1]/1e4;
fld1=mean(QFLX(:,:,it),3);
tmp=mean(oceFWflx(:,:,it),3);
fld2=reshape(bin_average*tmp(:),length(lon),length(lat));
fld3=mean(SFLX(:,:,it),3);
tmp=mean(oceSflux(:,:,it),3);
fld4=reshape(bin_average*tmp(:),length(lon),length(lat));
clf, colormap(jet)
subplot(221)
pcolor(lon,lat,fld1'), shading flat
caxis(cx); colorbar
title(['QFLX ' int2str(t)])
subplot(222)
pcolor(lon,lat,fld2'), shading flat
caxis(cx); colorbar
title(['oceFWflx ' int2str(t)])
subplot(223)
pcolor(lon,lat,fld3'); shading flat
caxis(cx); colorbar
title(['SFLX ' int2str(t)])
subplot(224)
pcolor(lon,lat,fld4'); shading flat
caxis(cx); colorbar
title(['oceSflux ' int2str(t)])
print -djpeg QFLX

%     Plot SS, TS, SSS, SST
%     SS     : top_layer_salinity in psu
%     TS     : top_layer_temperature' in K
%     SSS    : psu Salinity
%     SST    : degC Potential Temperature
%              Southwest C-grid tracer point (i.e., A-grid)
cx1=[28 38];
cx2=[-2 34];
fld1=mean(SS(:,:,it),3);
tmp=mean(SSS(:,:,it),3);
fld2=reshape(bin_average*tmp(:),length(lon),length(lat));
fld3=mean(TS(:,:,it),3)-273.15;
tmp=mean(SST(:,:,it),3);
fld4=reshape(bin_average*tmp(:),length(lon),length(lat));
clf, colormap(jet)
subplot(221)
pcolor(lon,lat,fld1'), shading flat
caxis(cx1); colorbar
title(['SS ' int2str(t)])
subplot(222)
pcolor(lon,lat,fld2'), shading flat
caxis(cx1); colorbar
title(['SSS ' int2str(t)])
subplot(223)
pcolor(lon,lat,fld3'); shading flat
caxis(cx2); colorbar
title(['TS ' int2str(t)])
subplot(224)
pcolor(lon,lat,fld4'); shading flat
caxis(cx2); colorbar
title(['SST ' int2str(t)])
print -djpeg SST

%     Plot TAUX, oceTAUX, TAUY, and oceTAUY
%     TAUX   : A-grid eastward stress on skin in N/m^2.
%     TAUY   : A-grid northward stress on skin in N/m^2.
%    oceTAUX : N/m^2 x-direction surface wind stress, >0 increases uVel
%    oceTAUY : N/m^2 y-direction surface wind stress, >0 increases vVel
%              Southwest C-grid velocity locations
cx=[-1 1]/4;
fld1=mean(TAUX(:,:,it),3);
fld3=mean(TAUY(:,:,it),3);
[tE,tN]=rotate_uv2uvEN(mean(oceTAUX(:,:,it),3),mean(oceTAUY(:,:,it),3),AngleCS,AngleSN);
fld2=reshape(bin_average*tE(:),length(lon),length(lat));
fld4=reshape(bin_average*tN(:),length(lon),length(lat));
clf, colormap(jet)
subplot(221)
pcolor(lon,lat,fld1'), shading flat
caxis(cx); colorbar
title(['TAUX ' int2str(t)])
subplot(222)
pcolor(lon,lat,fld2'), shading flat
caxis(cx); colorbar
title(['oceTAUX ' int2str(t)])
subplot(223)
pcolor(lon,lat,fld3'); shading flat
caxis(cx); colorbar
title(['TAUY ' int2str(t)])
subplot(224)
pcolor(lon,lat,fld4'); shading flat
caxis(cx); colorbar
title(['oceTAUY ' int2str(t)])
print -djpeg TAUX

%     Plot US, VS, UVEL1, VVEL1
%     US     : Top layer A-grid eastward velocity in m/s
%     VS     : Top layer A-grid northward velocity in m/s
%     UVEL1  : m/s x-direction surface velocity
%     VVEL1  : m/s y-direction surface velocity
%              Southwest C-grid velocity locations
cx=[-1 1]/4;
fld1=mean(US(:,:,it),3);
fld3=mean(VS(:,:,it),3);
[uE,uN]=rotate_uv2uvEN(mean(UVEL1(:,:,it),3),mean(VVEL1(:,:,it),3),AngleCS,AngleSN);
fld2=reshape(bin_average*uE(:),length(lon),length(lat));
fld4=reshape(bin_average*uN(:),length(lon),length(lat));
clf, colormap(jet)
subplot(221)
pcolor(lon,lat,fld1'), shading flat
caxis(cx); colorbar
title(['US ' int2str(t)])
subplot(222)
pcolor(lon,lat,fld2'), shading flat
caxis(cx); colorbar
title(['UVEL1 ' int2str(t)])
subplot(223)
pcolor(lon,lat,fld3'); shading flat
caxis(cx); colorbar
title(['VS ' int2str(t)])
subplot(224)
pcolor(lon,lat,fld4'); shading flat
caxis(cx); colorbar
title(['VVEL1 ' int2str(t)])
print -djpeg UVEL
