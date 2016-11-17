% Compare GEOS-5 and MITgcm output files.

% Initialize variables
get_GEOS5
get_MITgcm
mk_binavg

% Plot fields
c1=[-600 -1e-4 -1e-3 -.4 -.3 28 -2 -.25 -.25];
c2=[ 250  1e-4  1e-3  .4  .3 38 34  .25  .25];
for f=1:9
    eval(['FLD1=' FLD{f} ';'])
    cx=[min(FLD1(:)) max(FLD1(:))];
    for t=1:nt
        eval(['fld1=' FLD{f} '(:,:,t);'])
        eval(['tmp=' FLDmit{f} '(:,:,t);'])
        fld2=reshape(bin_average*tmp(:),length(lon),length(lat));
        clf
        colormap(jet)
        subplot(211)
        pcolor(lon,lat,fld1')
        shading flat
        caxis([c1(f) c2(f)]);
        colorbar
        title([FLD{f} ' ' int2str(t)])
        subplot(212)
        pcolor(lon,lat,fld2')
        shading flat
        caxis([c1(f) c2(f)]);
        colorbar
        title([FLDmit{f} ' ' int2str(t)])
        pause(.5)
    end
end
