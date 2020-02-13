;scans lz file for mode
;pro lzscan,cdsearch=cdsearch

common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common m2stuff,hkind,ghk,vblhsp,sblhsp
common m1stuff,hkm1,vsm1,vdatc,sdatc

bpath=getenv('IDLSAV')
cdsearch=1

;scans swe lz files for mode, specie, energy range etc
;when search = 0, lz file selection is through a pickfile
;when search = 1, each file (hours 0-2) on CD is searched for backgdoound test
;and background test records are written to an ascii file

if keyword_set(cdsearch) eq 0 then search=0 else search=1

luns=11 & close,luns
lundat=22 & close,lundat

md=strarr(257)
md(1)='sci92' & md(3)='man92' & md(4)='con92' & md(5)='sci46' & md(7)='man46' 
md(8)='con46' & md(128)='trans' & md(256)='u'

tmm=[' u ','man','sci','eng']
specie_mode=['elecs','ions','bckg test','unknown']

;get indices of instrument housekeeping into mjf array, lz.mf   
  ihkmap,ihk 

;get mode1, mode1 tm map of science and genl hk data offsets into lz.mf
   mode1map,hkm1,fcblm1,vsm1,vdatc,sdatc
   mode2map,hkind,ghk,vblhsp,sblhsp

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
  nbckrec=-1

  lzfile=result(ifl) 
  lzdate=''
  flstr=strmid(lzfile,strlen(fdir)+1,strlen(lzfile)-strlen(fdir))
  if strlen(flstr) gt 12 then beginstr=12 else beginstr=0
  lzdate=strmid(lzfile,strlen(fdir)+beginstr,6)

  print,'data file to be scanned:'
  print,lzfile,'  ',lzdate
  

  openw,luns,getenv('IDLSAV')+'lzsearch.dat'

  ;open lz file
    openr,lundat,lzfile
      fh=read_lzhdr(lundat) ;fh=file header structure 

  print,' ' & print,'reading.....'
  print,lzfile

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
      if scimode_ihk eq 0 or scimode_ihk eq 1 then scimode=1
      if scimode_ihk eq 2 or scimode_ihk eq 11 then scimode=2

    ;get mode dependent portion of hv table this mjf
      if scimode eq 2 then $
        vsteps=lz.mf(hkind(ghk(27).offs+indgen(ghk(27).ln))) $
      else if scimode eq 1 or scimode_ihk eq 1 then  $
        vsteps=lz.mf(hkm1(4).loc(indgen(16)))

        ;vsteps=lz.mf(hkm1(24).offs+indgen(16))

    wele=where(vsteps lt 64,nwele)
    wion=where(vsteps ge 64 and vsteps le 127,nwion) 
 
    if scimode eq 1 then begin

      if nwele eq n_elements(vsteps) then elec_ion=0 $
      else if nwion eq n_elements(vsteps) and $
        total(long(vsteps)) ne 16*long(vsteps(0)) then elec_ion=1 $
      else if nwion eq n_elements(vsteps) and $
        total(long(vsteps)) eq 16*long(vsteps(0)) $
        then elec_ion=2 $
      else elec_ion=3
      if elec_ion le 1 then maxenstep=volt_en(max(vsteps),/en,ion=elec_ion) $
      else  maxenstep=0
    
      if elec_ion eq 2 then begin   ;background test mode
        nbckrec=nbckrec+1
        print,' '
        print,recn,' ',date_time,' ',md(lz.telmod),$
          ' ',tmm(tmmode_ihk),string(scimode_ihk,format='(i2)'),' ',$
          specie_mode(elec_ion),' ',string(vsteps(0),format='(i4)'),$
          '  ',lz.mf(ihk(18).offs),lz.mf(ihk(28).offs)
        if search and minday le 90 then $
          printf,luns,recn,' ',date_time,' ',md(lz.telmod),$
          ' ',tmm(tmmode_ihk),string(scimode_ihk,format='(i2)'),' ',$
          specie_mode(elec_ion),' ',string(vsteps(0),format='(i4)'),$
          '  ',lz.mf(ihk(18).offs),lz.mf(ihk(28).offs) $
        else if search and h ge 1 and m ge 30 then goto,nxtfile 
      
      endif else $
        print,recn,' ',date_time,' ',md(lz.telmod),$
          ' ',tmm(tmmode_ihk),string(scimode_ihk,format='(i2)'),' ',$
          specie_mode(elec_ion),' ',string(maxenstep,format='(i4)'),' ev',$
          '  ',lz.mf(ihk(18).offs),lz.mf(ihk(28).offs)

    endif else if scimode eq 2 then begin
      if wele(0) ne -1 then begin
        maxenstep=volt_en(max(vsteps(wele)),/en) 
        minenstep=volt_en(min(vsteps(wele)),/en)
      endif else if wion(0) ne -1 then begin
        maxenstep=volt_en(max(vsteps(wion)),/en,/ion)
        minenstep=volt_en(min(vsteps(wion)),/en,/ion)
      endif
      if maxenstep eq minenstep then begin    ;background test mode
        nbckrec=nbckrec+1
        print,' '
        print,recn,' ',date_time,' ',md(lz.telmod),$
          ' ',tmm(tmmode_ihk),string(scimode_ihk,format='(i2)'),' ',$
          specie_mode(2),' ',string(maxenstep,format='(i5)'),' ev',$
          '  ',lz.mf(ihk(18).offs),lz.mf(ihk(28).offs)
        if search and minday le 90 then $
           printf,luns,recn,' ',date_time,' ',md(lz.telmod),$
          ' ',tmm(tmmode_ihk),string(scimode_ihk,format='(i2)'),' ',$
          specie_mode(2),' ',string(maxenstep,format='(i5)'),' ev',$
          '  ',lz.mf(ihk(18).offs),lz.mf(ihk(28).offs)  $
        else if search and h ge 1 and m ge 30 then goto,nxtfile 

      endif else $
        print,recn,' ',date_time,' ',md(lz.telmod),$
          ' ',tmm(tmmode_ihk),string(scimode_ihk,format='(i2)'),' ',$
          ' ',string(maxenstep,format='(i5)'),' ev',$
          '  ',lz.mf(ihk(18).offs),lz.mf(ihk(28).offs)

    endif

    pg=50
    if pg*fix(recn/pg) eq recn and search eq 0 then begin
      answ='' & print,'hit return to continue' 
      read,answ & if answ ne '' then stop
    endif
      
  if search and minday gt 90 then goto,nxtfile 

  endfor   ;end record loop


nxtfile:
close,luns
close,lundat

;answ='' & print,'hit return to continue' & read,answ & if answ ne '' then stop
   
if nbckrec ne -1 then begin
 spawn,$
 'cp '+getenv('IDLSAV')+'lzsearch.dat '+bpath+lzdate+'.bckrec'
 print,'background test lzrecord file created ',$
  bpath+lzdate+'.bckrec'

 print,'hit return to continue' & answ='' & read,answ & if answ ne '' then stop
endif

endfileloop:
close,luns
close,lundat

endfor    ;end file loop

print,' ' & print,'lzscan finished'

end
