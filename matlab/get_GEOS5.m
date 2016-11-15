% Get GEOS-5 output time series with matlab.

% Replace with location of your output file
pnm='/Users/dmenemen/Desktop/scratch/';

% Read and plot mit_ocn variables
fnm=dir([pnm 'TEST.mit*']);
nx=48;
ny=25;
nt=length(fnm);
lat=ncread(fnm(1).name,'lat');
lon=ncread(fnm(1).name,'lon');
SS=zeros(nx,ny,nt);
TS=zeros(nx,ny,nt);
US=zeros(nx,ny,nt);
VS=zeros(nx,ny,nt);
for t=1:nt
    SS(:,:,t)=ncread([pnm fnm(t).name],'SS');
    TS(:,:,t)=ncread([pnm fnm(t).name],'TS');
    US(:,:,t)=ncread([pnm fnm(t).name],'US');
    VS(:,:,t)=ncread([pnm fnm(t).name],'VS');
end

% look at time series of US
for t=1:nt
    clf
    mypcolor(US(:,:,t)');
    caxis([-1 1]*.1)
    colorbar
    title(t)
    pause(1)
end
