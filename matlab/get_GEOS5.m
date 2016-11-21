% Read GEOS-5 output files

% Replace with location of your GEOS-5 output files
pnm='~/geos5/TEST/scratch/';

% Initialize variables
fnm=dir([pnm 'TEST.mit*']);
%ncdisp([pnm fnm(1).name])
lon=ncread([pnm fnm(1).name],'lon');
lat=ncread([pnm fnm(1).name],'lat');
lev=ncread([pnm fnm(1).name],'lev');
nx=length(lon);
ny=length(lat);
nz=length(lev);
nt=length(fnm);
FLD={'HFLX','QFLX','SFLX','TAUX','TAUY','SS','TS','US','VS','PS'};
FLD3={'SWHEAT','MASK'};

% Read GEOS-5 mit_ocn variables
for f=1:length(FLD)
    fld=zeros(nx,ny,nt);
    for t=1:nt
        fld(:,:,t)=ncread([pnm fnm(t).name],FLD{f});
    end
    eval([FLD{f} '=fld;'])
end
for f=1:length(FLD3)
    fld=zeros(nx,ny,nz,nt);
    for t=1:nt
        fld(:,:,:,t)=ncread([pnm fnm(t).name],FLD3{f});
    end
    eval([FLD3{f} '=fld;'])
end
clear f fld fnm pnm t
