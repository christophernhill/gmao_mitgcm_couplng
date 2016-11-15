function record = read_record (file_id, record_type, record_length)
% function record = read_record (file_id, record_type, record_length)
% read a fortran record
% file needs to be previously opened in read binary mode, i.e.,
% file_id=fopen(fname,'r','b');
% record_type defaults to real*4
% record_length is optional and is checked for consistency if specified

if nargin<2, record_type='real*4'; end
if nargin<3, record_lenght=0; end
r_length=fread(file_id,1,'uint32'); % read in record length
if nargin==3 & r_length~=record_length
  error('record length differs from that specified')
end
switch lower(record_type)
  case {'uint8','integer*1','int8', ...
        'schar','signed char','uchar','unsigned char','char','char*1'}
    record=fread(file_id,r_length,record_type);
  case {'uint16','integer*2','int16','integer*2'}
    record=fread(file_id,r_length/2,record_type);
  case {'float32','real*4','uint32','integer*4','int32','integer*4'}
    record=fread(file_id,r_length/4,record_type);
  case {'float64','real*8','uint64','integer*8','int64','integer*8'}
    record=fread(file_id,r_length/8,record_type);
end
if fread(file_id,1,'uint32')~=r_length
  error('beginning record length is different from ending record length')
end
