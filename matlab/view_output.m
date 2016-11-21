% Compare GEOS-5 and MITgcm output files.

% Initialize variables
get_GEOS5
get_MITgcm
mk_binavg

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
for t=1:nt
    fld1=sum(SWHEAT(:,:,:,t),3);
    tmp=oceQsw(:,:,t);
    fld2=reshape(bin_average*tmp(:),length(lon),length(lat));
    fld3=HFLX(:,:,t);
    tmp=oceQnet(:,:,t);
    fld4=reshape(bin_average*tmp(:),length(lon),length(lat))-fld2;
    clf, colormap(jet)
    subplot(221)
    pcolor(lon,lat,fld1'), shading flat
    caxis(cx); colorbar
    title(['SWHEAT ' int2str(t)])
    subplot(222)
    pcolor(lon,lat,fld2'), shading flat
    caxis(cx); colorbar
    title(['oceQsw ' int2str(t)])
    subplot(223)
    pcolor(lon,lat,fld3'); shading flat
    caxis(cx); colorbar
    title(['HFLX ' int2str(t)])
    subplot(224)
    pcolor(lon,lat,fld4'); shading flat
    caxis(cx); colorbar
    title(['oceQnet-oceQsw ' int2str(t)])
    pause(.5)
end

%     Plot QFLX, oceFWflx, SFLX, and oceSflux
%     QFLX   : Freshwater flux from skin to ocean in kg/m2/s.
%   oceFWflx : kg/m^2/s net surface Fresh-Water flux into the ocean
%              (+=down), >0 decreases salinity
%     SFLX   : Salt flux from skin to ocean in TBD units.
%   oceSflux : g/m^2/s net surface Salt flux into the ocean
%              (+=down), >0 increases salinity
%              Southwest C-grid tracer point (i.e., A-grid)
cx=[-1 1]/1e4;
for t=1:nt
    fld1=QFLX(:,:,t);
    tmp=oceFWflx(:,:,t);
    fld2=reshape(bin_average*tmp(:),length(lon),length(lat));
    fld3=SFLX(:,:,t);
    tmp=oceSflux(:,:,t);
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
    pause(.5)
end

%     Plot SS, TS, SSS, SST
%     SS     : top_layer_salinity in psu
%     TS     : top_layer_temperature' in K
%     SSS    : psu Salinity
%     SST    : degC Potential Temperature
%              Southwest C-grid tracer point (i.e., A-grid)
cx1=[28 38];
cx2=[-2 34];
for t=1:nt
    fld1=SS(:,:,t);
    tmp=SSS(:,:,t);
    fld2=reshape(bin_average*tmp(:),length(lon),length(lat));
    fld3=TS(:,:,t)-273.15;
    tmp=SST(:,:,t);
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
    pause(.5)
end

%     Plot TAUX, oceTAUX, TAUY, and oceTAUY
%     TAUX   : A-grid eastward stress on skin in N/m^2.
%     TAUY   : A-grid northward stress on skin in N/m^2.
%    oceTAUX : N/m^2 x-direction surface wind stress, >0 increases uVel
%    oceTAUY : N/m^2 y-direction surface wind stress, >0 increases vVel
%              Southwest C-grid velocity locations
cx=[-1 1]/4;
for t=1:nt
    fld1=TAUX(:,:,t);
    fld3=TAUY(:,:,t);
    [tE,tN]=rotate_uv2uvEN(oceTAUX(:,:,t),oceTAUY(:,:,t),AngleCS,AngleSN);
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
    pause(.5)
end

%     Plot US, VS, UVEL1, VVEL1
%     US     : Top layer A-grid eastward velocity in m/s
%     VS     : Top layer A-grid northward velocity in m/s
%     UVEL1  : m/s x-direction surface velocity
%     VVEL1  : m/s y-direction surface velocity
%              Southwest C-grid velocity locations
cx=[-1 1]/4;
for t=1:nt
    fld1=US(:,:,t);
    fld3=VS(:,:,t);
    [uE,uN]=rotate_uv2uvEN(UVEL1(:,:,t),VVEL1(:,:,t),AngleCS,AngleSN);
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
    pause(.5)
end
