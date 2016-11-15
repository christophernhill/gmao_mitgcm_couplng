% Get MITgcm import time series with matlab.

% Replace with location of your output files
pnm='/Users/dmenemen/Desktop/scratch/mitocean_run/';

% Read and plot MITgcm import files
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

% look at time series of taux
for t=1:nt
    clf
    crossmap(taux(:,:,:,t),[min(taux(:)) max(taux(:))],['taux ' int2str(t)])
    pause(1)
end

% look at time series of surface swheat
for t=1:nt
    clf
    crossmap(swheat(:,:,:,1,t),[min(swheat(:)) max(swheat(:))],['swheat ' int2str(t)])
    pause(1)
end
