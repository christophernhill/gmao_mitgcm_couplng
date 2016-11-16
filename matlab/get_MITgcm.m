% Get MITgcm import time series with matlab.

% Replace with location of your output files
pnm='~/geos5/TEST/scratch/mitocean_run/';

% Initialize variables
FLD={'FU'     ,'FV'     ,'EMPMR'   ,'QNET'   ,'QSW'   ,'SALTFLUX', ...
     'oceTAUX','oceTAUY','oceFWflx','oceQnet','oceQsw','oceSflux'};
nx=32;
nz=15;
nt=96;

% Read MITgcm import files
for i=1:length(FLD)
    for t=2:nt
        suf=['.' myint2str(71999+t,10) '.data'];
        fnm=[pnm FLD{i} suf];
        eval([FLD{i} '(:,:,:,t)=readbin(fnm,[nx 6 nx]);'])
    end
end

% Look at time series
i=5;  % Choose FLD to look at, here QSW
colormap(jet)
eval(['fld=' FLD{i} ';'])
for t=1:nt
  clf
  crossmap(fld(:,:,:,t),[min(fld(:)) max(fld(:))],[FLD{i} ' ' int2str(t)])
  pause(.1)
end

% Look at all fields
t=5;  % Choose time step: 5 is first time step with non-zero QSW
colormap(jet)
for i=1:length(FLD)
  eval(['fld=' FLD{i} ';'])
  clf
  crossmap(fld(:,:,:,t),[min(fld(:)) max(fld(:))],[FLD{i} ' ' int2str(t)])
  pause(1)
end

% Compare two fields
for i=1:6
 eval(['fld1=' FLD{i} ';'])
 eval(['fld2=' FLD{i+6} ';'])
 if i>2
  fld2=-fld2;
 end
 disp([FLD{i} ' ' FLD{i+6} ' ' num2str(abs(max(fld1(:)-fld2(:))))])
 for t=1:nt
  clf
  subplot(311)
  mypcolor(reshape(fld1(:,:,:,t),[nx*6 nx])');
  colorbar
  title([FLD{i} ' ' int2str(t)])
  subplot(312)
  mypcolor(reshape(fld2(:,:,:,t),[nx*6 nx])');
  colorbar
  title([FLD{i+6} ' ' int2str(t)])
  subplot(313)
  mypcolor(reshape(fld2(:,:,:,t)-fld1(:,:,:,t),[nx*6 nx])');
  colorbar
  title([FLD{i+6} '-' FLD{i} ' ' int2str(t)])
  pause(.1)
 end
end
