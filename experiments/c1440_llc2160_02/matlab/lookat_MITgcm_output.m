% look at output from MITgcm in native format
pn='/nobackupp11/dmenemen/DYAMOND/geos5/TEST/mit_output/';
nx=2160;
ts=1;
for fld={'EmPmR','Qnet','Qsw','FU','FV'}
    fn=[pn fld{1} '.' myint2str(ts,10) '.data'];
    tmp=quikread_llc(fn,nx);
    clf, quikplot_llc(tmp), colorbar
    title([fld{1} '.' myint2str(ts,10) '.data'])
    pause
end

fld={'EmPmR'};
fn=[pn fld{1} '.' myint2str(ts,10) '.data'];
tmp=quikread_llc(fn,nx);
%clf, quikplot_llc(tmp), colorbar
%title([fld{1} '.' myint2str(ts,10) '.data'])

f3=tmp(:,(6*nx+1):(7*nx));
disp(minmax(f3))
[i,j]=find(f3<-7e3)
figure(1),mypcolor(f3');

fld={'Eta'};
fn=[pn fld{1} '.' myint2str(ts,10) '.data'];
tmp=quikread_llc(fn,nx);
%clf, quikplot_llc(tmp), colorbar
%title([fld{1} '.' myint2str(ts,10) '.data'])

e3=tmp(:,(6*nx+1):(7*nx));
disp(minmax(e3))
[i,j]=find(e3>120)
figure(2),mypcolor(e3');
