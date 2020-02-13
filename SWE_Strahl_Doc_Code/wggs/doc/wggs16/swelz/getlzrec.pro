function getlzrec,yymmdd,hhmmss

;give date (yymmdd) and time (hhmmss), find approximate record nymber of lz file

common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common m2stuff,hkind,ghk,vblhsp,sblhsp
common m1stuff,hkm1,vsm1,vdatc,sdatc
;get lz datapath
  lzpath=getenv('LZPATH')
  lzfltr='*.dat'


  ;select lz data file
    indate=yymmdd  
    arg=lzpath+'*'+string(indate,format='(i6)')+'*'+lzfltr+'*'
    result=findfile(arg,count=count)
    if count gt 1 then begin
      for i=0,count-1 do print,i,result(i)
      print,'multiple lz files: modify lz filter'
      stop
    endif else if result(0) eq '' then begin
      print,'lz file not found'
      stop
    endif
    lzfile=result(0)
                    
  ;open LZ data file and read header
    openr,lundat,lzfile,/get_lun
    fh=read_lzhdr(lundat) ;fh=file header structure 
 
  ;find record number of recoed header with nearest time
    inhms=hhmmss
    pb5=ymd_pb5(19000000+indate)
    pb5(2)=long(hms_hour(inhms)*3600000)
    lzrec=lztimrec(pb5)


return,lzrec

end



