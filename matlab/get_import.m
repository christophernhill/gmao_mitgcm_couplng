% Get MITgcm import time series with matlab.

% Replace with location of your output files
pnm='~/geos5/TEST/scratch/mitocean_run/';

% Initialize variables
fld={'taux','tauy','qflx' ,'hflx','swheat','sflx'    ,'ps','lon','lat'};
FLD={'FU'  ,'FV'  ,'EMPMR','QNET','QSW'   ,'SALTFLUX'};
nx=32;
nz=15;
nt=96;

% Read MITgcm import files before transfer to MITgcm arrays
for i=1:length(fld)
 eval([fld{i} '=zeros(nx,6,nx   ,nt);'])
 for t=1:nt
  suf=['.' myint2str(71999+t,10) '.data'];
  for f=0:5
   fnm=[pnm fld{i} '_import.000' int2str(f) suf];
   if strcmp(fld{i},'swheat')
    eval([fld{i} '(:,f+1,:,t)=sum(readbin(fnm,[nx 1 nx nz],0,''real*8''),4);'])
   else
    eval([fld{i} '(:,f+1,:,t)=readbin(fnm,[nx 1 nx   ],0,''real*8'');'])
   end
  end
 end
end

% Read MITgcm import files after transfer to MITgcm arrays
for i=1:length(FLD)
    for t=1:nt
        suf=['.' myint2str(71999+t,10) '.data'];
        fnm=[pnm FLD{i} suf];
        eval([FLD{i} '(:,:,:,t)=readbin(fnm,[nx 6 nx]);'])
    end
end

% Compare *import* to MITgcm fieds
for i=1:length(FLD)
 eval(['tmp=max(abs(' FLD{i} '(:)-' fld{i} '(:)));']);
 disp([FLD{i} ' - ' fld{i} ' = ' num2str(tmp)])
end

if 0
% look at time series of taux vs FU
cx=[-1 1]*.5;
for t=10:10
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
	tmp=FU(:,:,:,t)-taux(:,:,:,t);
    crossmap(tmp,cx,['FU-taux ' int2str(t)])
    pause(.1)
end
end

% look at time series of surface swheat
figure(4)
colormap(jet)
for t=1:nt
  clf
  crossmap(swheat(:,:,:,t),[min(swheat(:)) max(swheat(:))],['swheat ' int2str(t)])
  pause(.1)
end
