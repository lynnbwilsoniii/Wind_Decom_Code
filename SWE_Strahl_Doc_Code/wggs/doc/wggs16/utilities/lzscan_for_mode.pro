;scans lz file for mode

common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common m2stuff,hkind,ghk,vblhsp,sblhsp
common m1stuff,hkm1,vsm1,vdatc,sdatc
common m6stuff,hkm6,vsm6,vdatc6,sdatc6,bxyzdat6

;scans swe lz files for mode, specie, energy range etc
;when search = 0, lz file selection is through a pickfile
;when search = 1, mode of first record in science mode in each file on CD saved

search=1
printfile=1

allday=0   ;print every record in day

if allday then search=0   ;if allday then only do one day

if printfile then openw,lun,getenv('IDLSAV')+'lzmodescan.dat',/get_lun,/append

md=strarr(257)
md(1)='sci92' & md(3)='man92' & md(4)='con92' & md(5)='sci46' & md(7)='man46' 
md(8)='con46' & md(128)='trans' & md(256)='u'

tmm=[' u ','man','sci','eng']
specie_mode=['elecs','ions','bckg test','unknown']

;get indices of instrument housekeeping into mjf array, lz.mf   
  ihkmap,ihk 

;get mode1, mode2 tm map of science and genl hk data offsets into lz.mf
   mode1map
   mode6map
   mode2map

;select file (by date or by file)

  lzfltr='*.dat'

if search then begin
  fdir='/cdrom/cdrom0/data/wi/swe/lz/'
  spawn,'ls '+fdir+lzfltr,result
endif else begin
  fdir=getenv('LZPATH')
  result=pickfile(/read,get_path=fdir,path=fdir(0),filter=lzfltr,$
               title='Open SWE LZ Data Files',/must_exist)
endelse

print,'files to be scanned:'
print,result
print,'hit return to continue' & answ='' & read,answ & if answ ne '' then stop

for ifl=0,n_elements(result)-1 do begin

  lzfile=result(ifl) 
  lzdate=''
  case strlen(lzfile)-strlen(fdir) of
       12: lzdate='19'+strmid(lzfile,strlen(lzfile)-12,6)
       13: lzdate='19'+strmid(lzfile,strlen(lzfile)-13,6)
       26: lzdate=strmid(lzfile,strlen(lzfile)-16,8)
       else:   ;'lzdate undetermined'
  endcase
  print,'data file to be scanned:'
  print,lzfile,'  ',lzdate
  
  if strmid(lzdate,0,3) ne '199' then goto,endfileloop


  ;open lz file
    openr,lundat,lzfile,/get_lun
      fh=read_lzhdr(lundat) ;fh=file header structure 

  print,' ' & print,'reading.....'
  print,lzfile
  scimode=-1

  openr,lun1,'/cdrom/cdrom0/voldesc.sfd',/get_lun
  s=''
  for i=0,3 do readf,lun1,s
  seqno=''
  readf,lun1,seqno
  print,seqno
  free_lun,lun1

  for recn=1,fh.nmf-1 do begin
    lz=read_lzrcd(lundat,recn,fh.rln,fh.spcid) ;lz=data structure incl header

    ms_hms,lz.ms,h,m,s  ;get hhmmss.ms from msec of day
    date_time=string(lz.yr,format='(i4)') + ' ' + string(lz.dy,format='(i3)') +$
      '  ' + string(h,format='(i2)') + ':' + string(m,format='(i2)') +$
      ':' + string(s,format='(f6.3)')
    minday=h*60+m

    ;determine tm mode, tm rate, science mode, and mjf count from instr hk
      tmmode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(1).p,ihk(2).bv(1).n)
      scimode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(0).p,ihk(2).bv(0).n)
      if tmmode_ihk eq 2 and (scimode_ihk eq 0 or scimode_ihk eq 1) then $
        scimode=1 else $
      if tmmode_ihk eq 2 and (scimode_ihk eq 2 or scimode_ihk eq 11) then $
        scimode=2 else $
      if tmmode_ihk eq 2 and scimode_ihk eq 4  then $
        scimode=4 else $ 
      if tmmode_ihk eq 2 and scimode_ihk eq 6  then $
        scimode=6 else $
        scimode=-1

    ;get mode dependent portion of hv table this mjf
      if scimode eq 2 then $
        vsteps=lz.mf(hkind(ghk(27).offs+indgen(ghk(27).ln))) $
      else if scimode eq 1 or scimode_ihk eq 1 then  $
        vsteps=lz.mf(hkm1(4).loc(indgen(16)))  $
      else if scimode eq 4 then  $
        vsteps=lz.mf(hkm1(4).loc(indgen(16)))  $
      else if scimode eq 6 then  begin
        veis_tbl_pri=lz.mf(hkm6(7).loc(indgen(16)))
        vsteps=veis_tbl_pri 
                                         endif else vsteps=0     
    
      ebias1=lz.mf(ihk(18).offs)
      ebias2=lz.mf(ihk(28).offs)
      
    if min(vsteps) gt 0 and max(vsteps) lt 64 then begin
      if printfile then $
      printf,lun,lzdate,seqno,recn,lz.dy,lz.ms, scimode,max(vsteps),$
      volt_en(max(vsteps),/en),$
      ebias1,ebias2,format='(i8,2x,a23,i6,i4,i9,2x,i2,2x,i3,2x,i5,2x,2i3)
      print,lzdate,seqno,recn,lz.dy,lz.ms,scimode,max(vsteps),$
      volt_en(max(vsteps),/en),$
      ebias1,ebias2,format='(i8,1x,a23,i6,i4,i9,2x,i2,2x,i3,2x,i5,2x,2i3)
    endif else scimode=-1

    if scimode ne -1 and allday ne 1 then goto,nxtfile 
    
  endfor   ;end record loop


nxtfile:
free_lun,lundat

endfileloop:

endfor    ;end file loop
if printfile then free_lun,lun

print,' ' & print,'lzscan finished'

end
