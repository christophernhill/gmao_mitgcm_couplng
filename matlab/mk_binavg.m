% Build a bin-averaging kernel.

% interpolation grid
lat1=lat-mean(diff(lat));
lat2=lat+mean(diff(lat));
lon1=lon-mean(diff(lon));
lon2=lon+mean(diff(lon));
[LAT1 LON1]=meshgrid(lat1,lon1);
[LAT2 LON2]=meshgrid(lat2,lon2);

% original CS32 grid
ix=find(XC<min(lon1)); XC(ix)=XC(ix)+360;
ix=find(XC>max(lon2)); XC(ix)=XC(ix)-360;

% Compute bin-averaging template
LON1v=LON1(:); LAT1v=LAT1(:);
LON2v=LON2(:); LAT2v=LAT2(:);
XCv=XC(:); YCv=YC(:);
Mask=hFacC(:,:,1); Mask=Mask(:);
bin_average=spalloc(length(LON1v),length(XCv),length(XCv));
for i=1:length(LON1v)
    ix = find(XCv>=LON1v(i) & XCv<LON2v(i) & ...
              YCv>=LAT1v(i) & YCv<LAT2v(i) & Mask);
    if length(ix)>0
        bin_average(i,ix)=1/length(ix);
    else
        bin_average(i,1)=nan;
    end
end

% example interpolation
%xc=reshape(bin_average*XC(:),length(lon),length(lat));
%figure(1), clf, mypcolor(xc'); colorbar
%yc=reshape(bin_average*YC(:),length(lon),length(lat));
%figure(2), clf, mypcolor(yc'); colorbar
clear i ix lat1 lat2 lon1 lon2 xc yc LAT* LON* XCv YCv
