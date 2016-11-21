% Read MITgcm output files

% Replace with location of your MITgcm output files
pnm='~/geos5/TEST/scratch/mitocean_run/';

% Initialize variables
FLDmit={'oceQnet','oceFWflx','oceSflux','oceTAUX', ...
        'oceTAUY','SSS','SST','UVEL1','VVEL1','oceQsw'};
XC=rdmds([pnm 'XC']);
YC=rdmds([pnm 'YC']);
hFacC=rdmds([pnm 'hFacC']);
AngleCS=rdmds([pnm 'AngleCS']);
AngleSN=rdmds([pnm 'AngleSN']);

% Read MITgcm output files
for f=1:length(FLDmit)
    eval([FLDmit{f} '=rdmds(''' pnm FLDmit{f} ''',nan);'])
end
clear f pnm
