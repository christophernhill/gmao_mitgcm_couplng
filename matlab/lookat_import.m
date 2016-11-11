% Looking at MITgcm import files with matlab
pnm='/Users/dmenemen/Desktop/mitocean_run/';
suf='.0000072095.data';
nx=32; nz=15;

figure(1)
clf reset
orient tall
wysiwyg
colormap(jet)
fld={'lon','lat','hflx','sflx','qflx','taux','tauy','ps'};
for i=1:8
    tmp=zeros(nx*6,nx);
    for f=0:5
        fnm=[pnm fld{i} '_import.000' int2str(f) suf];
        tmp((f*nx+1):((f+1)*nx),:)=readbin(fnm,[nx nx],0,'real*8');
    end
    subplot(8,1,i)
    pcolor(tmp')
    shading flat
    colorbar
    title(fld{i})
end

figure(2)
clf reset
orient tall
wysiwyg
colormap(jet)
tmp=zeros(nx*6,nx,nz);
for f=0:5
    fnm=[pnm 'swheat_import.000' int2str(f) suf];
    tmp((f*nx+1):((f+1)*nx),:,:)=readbin(fnm,[nx nx nz],0,'real*8');
end
for i=1:8
    subplot(8,1,i)
    pcolor(tmp(:,:,i)');
    shading flat
    colorbar
    title(['swheat level ' int2str(i)])
end
