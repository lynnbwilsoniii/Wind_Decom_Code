;=================== CALLING PROCEDURE: swelz =============================

PRO swelz, GROUP = GROUP

common lzstuff,infile,lundat,recn,fh,lz,ihk,hkm1,vsm1,vdatc,sdatc,sp,vsmjf
common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common m2stuff,hkind,ghk,vblhsp,sblhsp
common m1stuff,hkm1,vsm1,vdatc,sdatc
;common lzstuff,$
;infile,lundat,recn,fh,lz,scindx,ihk,hkm1,fcblm1,vsm1,vdatc,sdatc,sp,vsmjf
common log_delog,comp_tbl,dcomp_tbl


loadcolors,tbl=18

;set up structures
  sp={spinparams,mfrecn:0l,mfyr:0l,mfdy:0l,mfms:0l,$
      spincnt:0b,tjd:0l,sec:0d,mjfcnt:0b,spinp:0d,$
      old_spincnt:0b,old_tjd:0l,old_sec:0d,old_mjfcnt:0b,$
      lst_spinp:0d,lst_tjd:0l,lst_sec:0d,newmjf:1,datayes:0,lst_scimod:-1}

;read compress/decompress tables
decompress_tbl
     
;get indices of instrument housekeeping into mjf array, lz.mf   
  ihkmap,ihk 

;get mode1 tm map of science and genl hk data offsets into scindx
  mode1map,hkm1,fcblm1,vsm1,vdatc,sdatc  ;hkm1=genl hk offsets for mode1
                                         ;fcblm1=faraday cup offsets
                                         ;vsm1=veis/strl offsets
                                         ;vdatc=veis data index
                                         ;sdatc=strl data index
                    
;get input lz data file
  openr,lun,'swelzdatapath',/get_lun
  lzpath=''
  readf,lun,lzpath & free_lun,lun
  infile=pickfile(/read,get_path=lzpath,path=lzpath(0),filter='*.dat',$
  title='Level Zero Data Files')
  openw,lun,'swelzdatapath',/get_lun
  printf,lun,lzpath & free_lun,lun

;open level zero file
  openr,lundat,infile,/get_lun
  print, ' ' & print,'input data file name',infile

;call widget interface
  swelzw
END


