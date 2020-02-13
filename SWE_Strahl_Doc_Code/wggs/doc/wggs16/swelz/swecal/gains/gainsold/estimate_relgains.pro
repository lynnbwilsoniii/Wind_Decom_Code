;pro estimate_relgains

flnm=getenv('IDLSAV')+'savegains_tmp.dat'

relg=fltarr(6)
relgx=fltarr(6)
n=0
nx=0

lun=1
close,lun
openr,lun,flnm

i=-1
while not eof(lun) do begin
  s=''
  readf,lun,s 
  
  sdate=''  
  readf,lun,sdate
  readf,lun,s
  en=fix(strmid(s,0,4))

  pb5=lonarr(3)
  nacc=0
  ebias=intarr(2)
  b=fltarr(3)
  s='mean(0),sdev,rtchi2 '
  mean=0.
  sdev=0.
  rtchi2=0.
  detslct=''
  readf,lun,pb5,nacc,ebias,b,detslct
  readf,lun,s,mean,sdev,rtchi2,format='(a20,3f13.3,2x,a10)'
  rgn=fltarr(6)
  rgnml=fltarr(6)
  s=''
  readf,lun,s,rgn,format='(a11,6f7.3)'
  readf,lun,s
  readf,lun,s,rgnml,format='(a11,6f7.3)'
  if strmid(sdate,0,1) eq 'b' then goto,endwhl

  i=i+1
  ;print,detslct
  if strcompress(detslct,/remove_all) eq '012345' then begin
    print,'012345'
    relg=relg+rgnml
    n=n+1
  endif  
  if strcompress(detslct,/remove_all) eq '01345' then begin
    print,'01345'
    relgx=relgx+rgnml
    nx=nx+1
  endif  
  
  endwhl:
endwhile
close,1


relg=relg/n
print,'relg:  ',n,relg,format='(a7,i6,6f10.3)'

relgx=relgx/nx
print,'relgx: ',nx,relgx,format='(a7,i6,6f10.3)'


end
