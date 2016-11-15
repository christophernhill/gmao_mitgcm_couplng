% Get MITgcm import time series with matlab.

% Replace with location of your output files
pnm='/Users/dmenemen/Desktop/scratch/mitocean_run/';

% Read MITgcm import files before transfer to MITgcm arrays
fld={'lon','lat','hflx','sflx','qflx','taux','tauy','ps','swheat'};
nx=32;
nz=15;
nt=96;
for i=1:8
    eval([fld{i} '=zeros(nx,6,nx,nt);'])
    for t=1:nt
        suf=['.' myint2str(71999+t,10) '.data'];
        for f=0:5
            fnm=[pnm fld{i} '_import.000' int2str(f) suf];
            eval([fld{i} '(:,f+1,:,t)=readbin(fnm,[nx 1 nx],0,''real*8'');'])
        end
    end
end
i=9;
eval([fld{i} '=zeros(nx,6,nx,nz,nt);'])
for t=1:nt
    suf=['.' myint2str(71999+t,10) '.data'];
    for f=0:5
        fnm=[pnm fld{i} '_import.000' int2str(f) suf];
        eval([fld{i} '(:,f+1,:,:,t)=readbin(fnm,[nx 1 nx nz],0,''real*8'');'])
    end
end

% Read MITgcm import files after transfer to MITgcm arrays
FLD={'FU','FV','EMPMR','QNET','SALTFLUX'};
for i=1:length(FLD)
    for t=1:nt
        suf=['.' myint2str(71999+t,10) '.data'];
        fnm=[pnm FLD{i} suf];
        eval([fld{i} '=zeros(nx,6,nx,nt);'])
        eval([FLD{i} '(:,:,:,t)=readbin(fnm,[nx 6 nx]);'])
    end
end

% compare time series of taux vs FU
cx=[-1 1]*.5;
for t=1:nt
    figure(1)
    clf
    colormap(jet)
    crossmap(taux(:,:,:,t),cx,['taux ' int2str(t)])
    figure(2)
    clf
    colormap(jet)
    crossmap(FU(:,:,:,t),cx,['FU ' int2str(t)])
    figure(3)
    clf
    colormap(jet)
    crossmap(FU(:,:,:,t)-taux(:,:,:,t),cx,['FU-taux ' int2str(t)])
    pause(1)
end

% look at time series of surface swheat
for t=1:nt
    clf
    crossmap(swheat(:,:,:,1,t),[min(swheat(:)) max(swheat(:))],['swheat ' int2str(t)])
    pause(1)
end
