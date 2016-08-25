fpref='lon_import';
inum=72001;

% mval=123456 - use maxval from field
% mval=  and other value, use as mask val
mval=123456;
% Use mask code
usemaskval=1;
% No masking code
usemaskval=0;

figure(1)
clf
phics=zeros(32,32,6);

for i = 0:5
fn=sprintf('%s.%6.6d.%10.10d.data',fpref,i,inum);
fid=fopen(fn,'r','ieee-be');
i0=fread(fid,1,'int');
phi=fread(fid,[32 32],'float64');
i0=fread(fid,1,'int');
fclose(fid);
phics(:,:,i+1)=phi;
end

if mval == 123456
 mval=max(phics(:))
end
if usemaskval == 1
 phics(find(phics==mval))=NaN;
end

phimin=nanmin(phics(:));
phimax=nanmax(phics(:));
phistd=nanstd(phics(:));
phiave=nanmean(phics(:));

plotphi=phics-phiave;
cmin=-3*phistd;
cmax=3*phistd;
% cmin=phimin;
% cmax=phimax;
subplot(3,4,9 );imagesc(plotphi(:,:,1)');caxis([cmin cmax]);axis equal; axis square; axis tight; set(gca,'YDir','normal');
subplot(3,4,10);imagesc(plotphi(:,:,2)');caxis([cmin cmax]);axis equal; axis square; axis tight; set(gca,'YDir','normal');
subplot(3,4,6 );imagesc(plotphi(:,:,3)');caxis([cmin cmax]);axis equal; axis square; axis tight; set(gca,'YDir','normal');
subplot(3,4,7 );imagesc(plotphi(:,:,4)');caxis([cmin cmax]);axis equal; axis square; axis tight; set(gca,'YDir','normal');
subplot(3,4,3 );imagesc(plotphi(:,:,5)');caxis([cmin cmax]);axis equal; axis square; axis tight; set(gca,'YDir','normal');
subplot(3,4,4 );imagesc(plotphi(:,:,6)');caxis([cmin cmax]);axis equal; axis square; axis tight; set(gca,'YDir','normal');
