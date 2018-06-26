function []=grid_load_gmao(gridFile);
% []=grid_load_gmao(gridFile);
%
%example: load grid
%  grid_load_gmao('c12/gael.geosgcm_surf.20000415_0000z.nc4');
%
%example: gradient computation
%  [dFLDdx,dFLDdy]=calc_T_grad(mygrid.RAC,0);
%  figure; qwckplot(dFLDdx);
%
%example: geographical map of one variable
%  figure; m_map_gcmfaces(mygrid.RAC,1.2);

if isempty(which('gcmfaces')); p = genpath('gcmfaces/'); addpath(p); addpath('m_map/'); end;

gcmfaces_global;

ncload(gridFile);

%not clear how the following variables should be interpreted:
%  anchor, ncontact, contacts, orientation, Xdim, Ydim, nf

siz=size(permute(lons,[3 2 1]));

mygrid=[];
mygrid.dirGrid='./';
mygrid.nFaces=6;
mygrid.fileFormat='compact';
mygrid.gcm2facesFast=0;
mygrid.memoryLimit=2;
mygrid.ioSize=[siz(1) siz(2)*siz(3)];%this should match the above choice of 'compact' (does it?)
mygrid.facesSize=repmat(siz(1:2),[siz(3) 1]);
mygrid.facesExpand=[];
mygrid.missVal=0;
%mygrid.RC=[];
%mygrid.RF=[];
%mygrid.DRC=[];
%mygrid.DRF=[];
mygrid.XC=gcmfaces();
mygrid.YC=gcmfaces();
mygrid.XG=gcmfaces();
mygrid.YG=gcmfaces();
mygrid.DXC=gcmfaces();
mygrid.DYC=gcmfaces();
mygrid.RAC=gcmfaces();
mygrid.DXG=gcmfaces();
mygrid.DYG=gcmfaces();
mygrid.RAZ=gcmfaces();
%mygrid.hFacC=gcmfaces();
%mygrid.hFacW=gcmfaces();
%mygrid.hFacS=gcmfaces();
%mygrid.Depth=gcmfaces();
mygrid.AngleCS=gcmfaces();
mygrid.AngleSN=gcmfaces();

for ff=1:6;
  mygrid.XC{ff}=double(permute(lons(ff,:,:),[3 2 1]));
  mygrid.YC{ff}=double(permute(lats(ff,:,:),[3 2 1]));
  mygrid.DXC{ff}=double(permute(DXC(ff,:,:),[3 2 1]));
  mygrid.DYC{ff}=double(permute(DYC(ff,:,:),[3 2 1]));
  mygrid.RAC{ff}=double(permute(AREA(ff,:,:),[3 2 1]));
end;

%set longitude to -180 +180
mygrid.XC(mygrid.XC>180)=mygrid.XC(mygrid.XC>180)-360;

