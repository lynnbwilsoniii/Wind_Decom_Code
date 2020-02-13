; This is a set of procedures to read and process SWE level zero data.
; Jim Byrnes provided the read procedures.
; A simple widget interface is used to select (direct access) data records and
; display a limited amount of housekeeping information.
; The calling procedure is SWELZ.PRO.

; R. J. Fitzenreiter, Dec 1993
; Modified April, 1994 (RJF)
; Modified October, 1994 (RJF)



;======================= fillwfields1 =======================================

pro fillwfields1

common sharewidgb,wb
common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common m2stuff,hkind,ghk,vblhsp,sblhsp
common m1stuff,hkm1,vsm1,vdatc,sdatc

;----------------- fill in (header) values of widget fields ------------------
  widget_control,set_value=string(lz.recn,format='(i5)'),wb.field1(2)
  widget_control,set_value=string(fh.mfcfst,format='(i5)'),wb.field1(15)
  widget_control,set_value=string(fh.mfclst,format='(i5)'),wb.field1(16)

  if fh.mfclst lt fh.mfcfst then $
    cntrdiff=1024+fh.mfclst-fh.mfcfst +1 else $
    cntrdiff=fh.mfclst-fh.mfcfst + 1
  widget_control,set_value=string(cntrdiff,format='(i5)'),wb.field1(17)
  widget_control,set_value=string(fh.nmf,format='(i5)'),wb.field1(18)
  widget_control,set_value=string(fh.ngap,format='(i5)'),wb.field1(19)

  widget_control,set_value=string(lz.mfc,format='(i5)'),wb.field1(30)
  widget_control,set_value=string(lz.nfill,format='(i5)'),wb.field1(31)
  widget_control,set_value=string(lz.nsync,format='(i5)'),wb.field1(32)
  
  md=strarr(257)
  md(1)='sci92' & md(3)='man92' & md(4)='con92' & md(5)='sci46' & md(7)='man46' 
  md(8)='con46' & md(128)='trans' & md(256)='u' 
  widget_control,set_value=string(lz.telmod,format='(i3)')+'  '+md(lz.telmod),$
    wb.field1(33)
  wqlty=where(lz.qlty ne 0,nwqlty)
  widget_control,set_value=string(nwqlty,format='(i5)'),wb.field1(34)


  time_utc,lz.mf(hkm1(1).offs:hkm1(1).offs+6),$
  tjd,sec,hour,min,isec,ms,hms,spincnt
  widget_control,set_value=hms,wb.field1(11)
  widget_control,set_value=string(sp.spincnt,format='(i3)'),wb.field1(12) 
  widget_control,set_value=string(sp.spinp,format='(f6.3)'),wb.field1(13)
 
  mjf_cntr=lz.mf(ihk(1).offs)
  widget_control,set_value=string(mjf_cntr,format='(i5)'),wb.field1(40)

  elec_ion=get_bits(lz.mf(ihk(6).offs),ihk(6).bv(0).p,ihk(6).bv(0).n)
  elecion=['electrons','ions']
  widget_control,set_value=string(elec_ion,format='(i1)')+'  '+$
    elecion(elec_ion),wb.field1(41)

  scimode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(0).p,ihk(2).bv(0).n)
  widget_control,set_value=string(scimode_ihk,format='(i5)'),wb.field1(42)

  tmmode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(1).p,ihk(2).bv(1).n)
  tmm=['u','man','sci','eng']    
  widget_control,set_value=string(tmmode_ihk,format='(i5)')+'  '+$
    tmm(tmmode_ihk),wb.field1(43)

  
  tmrate_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(2).p,ihk(2).bv(2).n)
  tmr=['92s','46s'] 
  widget_control,set_value=string(tmrate_ihk,format='(i5)')+'  '+$
    tmr(tmrate_ihk),wb.field1(44)

  for j=0,4 do begin
    tm_mode= get_bits(lz.mf(ihk(j*10).offs),ihk(j*10).bv(5).p,ihk(j*10).bv(5).n)
    widget_control,set_value=string(tm_mode,format='(i5)')+'  '+$
    tmm(tm_mode),wb.field1(50+j)
    tm_rate= get_bits(lz.mf(ihk(j*10).offs),ihk(j*10).bv(6).p,ihk(j*10).bv(6).n)
    widget_control,set_value=string(tm_rate,format='(i5)')+'  '+$
    tmr(tm_rate),wb.field1(60+j)
  endfor
end


;----------------- end filling values of widget fields ------------------------
;-----------------------------------------------------------------------------



;============================= swelzw_event ================================

PRO swelzw_event, event

common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common m2stuff,hkind,ghk,vblhsp,sblhsp
common m1stuff,hkm1,vsm1,vdatc,sdatc
common sharewidgb,wb
common shareswelzw,orig_recn
;common sharetimsel,timsel
common wstuff,wst

;help,event,/str
;help,wb,/str

CASE event.id OF
              
        wb.field1(0): begin  ;run with new input lz file
                widget_control,get_value=lzfile,wb.field1(0)
                WIDGET_CONTROL, event.top, /DESTROY
                swelzw
                endcase

	wb.field1(1): BEGIN  ;select mjf record
		
		WIDGET_CONTROL, wb.field1(1), GET_VALUE = val
                recn=val(0)
                print,' '		
		PRINT, 'mjf data recn selected = ' + STRING(recn)
                print,'physical record = ',fh.nphyrc - fh.nmf + recn
                read_rec,date_time ;read record and display time from header
                widget_control,set_value=date_time,wb.field1(10)
                proc_rec  ;process lz record 
                fillwfields1  ;fill widget fields
		END

        wb.button1(0): begin
          case event.value of
             'Parent' : begin
                   current_appl=xregistered('wa_drawf') 
                   if current_appl eq 0 then current_appl=xregistered('wanal')
                 endcase
             'Quit' : WIDGET_CONTROL, event.top, /DESTROY    
             'Veis counts' : plotcounts
             'Strahl counts' : plcnts_strl
             else:
          endcase
        endcase

        wb.button1(1): begin                             ;'Increment recn'
          case event.value of
             0 : recn_new=recn+1                             ;'+'

             1 : recn_new=recn-1                             ;'-_
          endcase
          if recn_new ge 1 and recn_new le fh.nmf then begin
            recn=recn_new
            widget_control,wb.field1(1),set_value=recn
            read_rec,date_time   ;read record and display time from header
            widget_control,set_value=date_time,wb.field1(10)
            proc_rec           ;process lz record
            fillwfields1  ;fill widget fields
          endif 
          wst.timsel='lz'
        endcase

        wb.button1(2): begin
           lunprt=00
           case event.value of
             0: prt_flhdr,lunprt,lzfile,fh          ;print file header
             1: begin
                  ms_hms,lz.ms,h,m,s           ;get hhmmss.ms from msec of day
                  prt_hdr,lunprt,lz,h,m,s           ;print major frame header
                endcase
             2: prt_mf,lunprt,lz.mf,lz.recn         ;print major frame data
             3: prt_ihk,lunprt                      ;print instr hk data
             4: prt_hk,lunprt                       ;print genl hk
             5: prt_fc,lunprt                       ;print faraday cup data
             6: prt_veis,lunprt                     ;print veis data
             7: prt_strahl,lunprt                   ;print strahl data

           endcase
                       endcase

       wb.button1(3) : begin
           case event.value of
             0: lzhdrs
             1: dataqlty,lpr=1,lfc=1,/scan
             2: notscimode
             3: scan_mode
             else:
           endcase
                      endcase

        else:
                
ENDCASE
END


;======================================swelzw =============================

PRO swelzw, GROUP = GROUP

;common lzstuff,$
;infile,lundat,recn,fh,lz,ihk,hkm1,vsm1,vdatc,sdatc,sp,vsmjf
common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common m2stuff,hkind,ghk,vblhsp,sblhsp
common m1stuff,hkm1,vsm1,vdatc,sdatc
common qual,q
common sharewidgb,wb
common shareswelzw,orig_recn

if xregistered('swelzw') then begin
  if recn ge 1 and recn le fh.nmf then begin
     widget_control,wb.field1(1),set_value=recn
     read_rec,date_time   ;read record and display time from header
     widget_control,set_value=date_time,wb.field1(10)
     proc_rec           ;process lz record
     fillwfields1  ;fill widget fields
  endif 
  return
endif

;set up structures
 
  wb={wb_widgets,base:0l,slider:0l,button1:lonarr(100),field1:lonarr(100),$
    menu1:0l,label:lonarr(100)}  


;read file header (Jim Byrnes' procedure)
  fh=read_lzhdr(lundat) ;fh=file header structure 

;for nrt set
  fh.rln=11552l
  
  prt_flhdr,0,lzfile,fh  ;print file header

;widget interface to select and read lz records
  wb.base = WIDGET_BASE(TITLE = 'SWE LZ Data', /COLUMN,xoffset=500,space=0)

  rbase=widget_base(wb.base,/row,space=25)
  wb.field1(0)=cw_field(rbase,title='lz data file',$
    value=lzfile,xsize=30,/string,/noedit)

  wb.button1(0)=cw_bgroup(rbase,$
    ['Parent','Quit','Veis counts','Strahl counts'],row=1,/return_name)
  
  listqinfo=['fh.mfcfst','fh.mfclst','lst-fst+1',' fh.nmf  ',$
             ' fh.ngap ']

  rbase=widget_base(wb.base,/row)
  wb.label(0)=widget_label(rbase,value=' ')
  rbase=widget_base(wb.base,/row)
  wb.label(1)=widget_label(rbase,value='File Header:')
  rbase=widget_base(wb.base,/row)
  for i=0,n_elements(listqinfo)-1 do $
    wb.field1(15+i)=cw_field(rbase,title=listqinfo(i),$
    value='',xsize=5,/string,/noedit,/column)


  rbase=widget_base(wb.base,/row)
  wb.label(2)=widget_label(rbase,value=' ')
  rbase=widget_base(wb.base,/row)
  wb.label(2)=widget_label(rbase,value='Record Header:')
  rbase=widget_base(wb.base,/row,space=25)
  wb.button1(1)=cw_bgroup(rbase,['+','-'],row=1,label_left='Increment recn',$
    ids=ids)
  wb.field1(1)=cw_field(rbase,title='recn',/return_events,/long,xsize=10)
  wb.field1(2)=cw_field(rbase,title='lz.recn',/noedit,/long,xsize=10)
  

  list1=['yr dy hms ','timetag   ','tagged spincount', 'spinperiod']
  rbase=widget_base(wb.base,/row,space=20)  
  cbase=widget_base(rbase,/column)
  for i=0,n_elements(list1)/2-1 do $
    wb.field1(10+i)=cw_field(cbase,title=list1(i),$
    value='',xsize=24,/string,/noedit,row=n_elements(list1)/2)

  cbase=widget_base(rbase,/column)
  for i=n_elements(list1)/2,n_elements(list1)-1 do $
    wb.field1(10+i)=cw_field(cbase,title=list1(i),$
    value='',xsize=10,/string,/noedit,$
    row=n_elements(list1)-n_elements(list1)/2)

  list2=['  lz.mfc  ',' lz.nfill ',' lz.nsync  ',' lz.telmod ','nmf qlty>0']
  rbase=widget_base(wb.base,/row)
  for i=0,n_elements(list2)-1 do $
    wb.field1(30+i)=cw_field(rbase,title=list2(i),$
    value='',xsize=7,/string,/noedit,/column)

  rbase=widget_base(wb.base,/row)
  wb.label(3)=widget_label(rbase,value=' ')
  rbase=widget_base(wb.base,/row)
  wb.label(4)=widget_label(rbase,value='Instrument Housekeeping:')
  rbase1=widget_base(wb.base,/row,space=25)

  cbase=widget_base(rbase1,/column)
  list3=['mjf_cntr','el_ion','scimode','tmmode','tmrate']
  rbase=widget_base(cbase,/row)
  for i=0,n_elements(list3)/2-1 do $
    wb.field1(40+i)=cw_field(rbase,title=list3(i),$
    value='',xsize=6,/string,/noedit,/column)
  rbase=widget_base(cbase,/row)
  for i=n_elements(list3)/2,n_elements(list3)-1 do $
    wb.field1(40+i)=cw_field(rbase,title=list3(i),$
    value='',xsize=5,/string,/noedit,/column)

  
  cbase=widget_base(rbase1,/column)
  rbase0=widget_base(cbase,/row)
  wb.label(6)=widget_label(rbase0,value='tm mode')
  for j=0,4  do wb.field1(50+j)=$
    cw_field(rbase0,title='status'+string(j+1,format='(i1)'),$
    value='',xsize=7,/string,/noedit,/column)

  rbase0=widget_base(cbase,/row)
  wb.label(7)=widget_label(rbase0,value='tm rate')
  for j=0,4  do wb.field1(60+j)=$
    cw_field(rbase0,title=' ',value='',xsize=7,/string,/noedit,/column)

  
  cbase=widget_base(wb.base,/column)
  wb.label(4)=widget_label(cbase,value=' ')
  listprt=$
  [ 'file header',    'record header',     'lz data',$ 
    'instr hk data',  'genl hk data', $
    'fc data',        'veis', 'strahl'  ]
  wb.button1(2)=cw_bgroup(cbase,listprt,row=2,label_top='Print Options')

 
  wb.label(5)=widget_label(cbase,value=' ')
  listappl=['records with qlty>0 (bad) frames',$
            'data blocks with qlty>0 (for selected recn)',$
            'record''s not in sci mode','scan modes']
  wb.button1(3)=cw_bgroup(cbase,listappl,row=2,$
    label_top='Search file applications')


WIDGET_CONTROL, wb.base, /REALIZE

;read and process current record in file
  if keyword_set(recn) eq 0 then recn=1
  orig_recn=recn
  widget_control,set_value=recn,wb.field1(1)
  read_rec,date_time   ;read record and display time from header
  widget_control,set_value=date_time,wb.field1(10)
  proc_rec    ;process lz record
  fillwfields1  ;fill widget fields

XMANAGER, "swelzw", wb.base, GROUP_LEADER = GROUP  ;hand off to manager

END


