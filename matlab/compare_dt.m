% Compare output files for different dt
p1='/home/heracles/geos5/TEST450/scratch/mitocean_run/';
p2='/home/heracles/geos5/TEST900/scratch/mitocean_run/';
p3='/home/heracles/geos5/TEST1800/scratch/mitocean_run/';
po='~/Desktop/';

fld='oceQsw';
fld='oceTAUX';
eval(['mkdir ' po fld])
for t=1:2:47
    hr=sprintf('%02d',(t-1)/2);
    f1=rdmds([p1 fld '.00000' int2str(72000+4*t)]);
    f2=rdmds([p2 fld '.00000' int2str(72000+2*t)]);
    f3=rdmds([p3 fld '.00000' int2str(72000+t)]);
    clf
    subplot(311), pcolor(f1'); shading flat, colorbar
    title([fld ': ogcm-run-dt=mit-dt=detat=450; hour ' hr])
    subplot(312), pcolor(f2'); shading flat, colorbar
    title([fld ': ogcm-run-dt=mit-dt=detat=900; hour ' hr])
    subplot(313), pcolor(f3'); shading flat, colorbar
    title([fld ': ogcm-run-dt=mit-dt=detat=1800; hour ' hr])
    eval(['print -djpeg ' po fld '/hour' hr])
end
