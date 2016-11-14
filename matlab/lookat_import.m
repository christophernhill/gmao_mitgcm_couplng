% Look at MITgcm import files with matlab.
% Compare to gmao_mitgcm_couplng/verification/*_import_72007.jpg

% Replace with location of your output files
pnm='/Users/dmenemen/Desktop/mitocean_run/';

% Read and plot MITgcm import files
ts=72007;
suf=['.' myint2str(ts,10) '.data'];
fld={'lon','lat','hflx','sflx','qflx','taux','tauy','ps','swheat'};
nx=32;
nz=15;
for i=1:8
 figure(i)
 clf reset
 colormap(jet)
 tmp=zeros(nx*6,nx);
 for f=0:5
  fnm=[pnm fld{i} '_import.000' int2str(f) suf];
  tmp((f*nx+1):((f+1)*nx),:)=readbin(fnm,[nx nx],0,'real*8');
 end
 crossmap(reshape(tmp,[nx 6 nx]),[min(tmp(:)) max(tmp(:))],fld{i})
 eval(['print -djpeg ' fld{i} '_import_' int2str(ts)])
end
i=9;
figure(i)
clf reset
colormap(jet)
tmp=zeros(nx*6,nx,nz);
for f=0:5
 fnm=[pnm fld{i} '_import.000' int2str(f) suf];
 tmp((f*nx+1):((f+1)*nx),:,:)=readbin(fnm,[nx nx nz],0,'real*8');
end
for k=1:4
 subplot(4,1,k)
 mypcolor(tmp(:,:,k)');
 colorbar
 title([fld{i} ' level ' int2str(k)])
end
eval(['print -djpeg ' fld{i} '_import_' int2str(ts)])
