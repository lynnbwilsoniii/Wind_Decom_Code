

;=========================== do_cts_average_event ============================

pro do_cts_average_event,event

common sharewidg,WDGT
;common sharewidgc,wc
common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common sums,sumcts,vlvls,instrhk,numspins
common log_delog,comp_tbl,dcomp_tbl
common magstuff,magfile,tpb5,bgse
common wstuff,wst
common swestuff,swest

;print,'pro do_cts_average_event'
;help,event,/str
;print,event.value

case event.id of 

WDGT.swelzav_nrmldet: swest.relgain_nmldet=event.index

WDGT.swelzav_ptchfitdets: swest.detslct_list_indx=event.index

WDGT.swelzav_vstep : swest.ivstep=event.index

else: begin
case event.value of
               
  'Do avg of slctd spins; save to file' : begin  ;(to get background)
     print,'sum over spins'
     ;initialize sums
       numspins=0
       sumcts=lonarr(swest.ndets,swest.nvsteps,swest.nsectors,500)
       vlvls=lonarr(swest.nvsteps,500)
       instrhk=$
         replicate({datim:'',elec_ion:0,tmmode:0,scimode:0,$
         ebias1:0,ibias1:0,ebias2:0,ibias2:0,calpwr:0},500)

     widget_control,WDGT.swelzav_bgnmjf,get_value=recn_begin
     widget_control,WDGT.swelzav_endmjf,get_value=recn_end
     widget_control,WDGT.swelzav_spn,get_value=spin_begin
     widget_control,WDGT.swelzav_spinbl,get_value=spin_end
     print,'recn_begin, spin_begin,  recn_end, spin_end ',$
            recn_begin, spin_begin,  recn_end, spin_end
   
     recn_curr=recn
     ispin_curr=swest.ispinbl
     for recn=recn_begin,recn_end do begin
       print,'recn ',recn
       proc_rec,date_time,tmmode_ihk=tmmode_ihk,elec_ion=elec_ion          
       scimode1 = vsmjf.scimode eq 0 or vsmjf.scimode eq 1 or vsmjf.scimode eq 4
       scimode2 = vsmjf.scimode eq 2 or vsmjf.scimode eq 11
       scimode6 = vsmjf.scimode eq 6
       
       if recn eq recn_begin then ispin1=spin_begin else ispin1=0
       if recn eq recn_end then ispin2=spin_end else ispin2=swest.nspins-1
       for ispin=ispin1,ispin2 do begin 
         ispinbl=ispin
         if (recn eq recn_begin and ispin eq ispin1) or $
            (recn eq recn_end and ispin eq ispin2)  then begin
           widget_control,WDGT.swelzc_recn,set_value=recn
           widget_control,WDGT.swelzc_spin,set_value=ispin
           isector=swest.isector & widget_control,WDGT.swelzc_sect,set_value=isector
           ivstep=swest.ivstep & widget_control,WDGT.swelzc_vstep,set_value=ivstep
           plotcts
         endif  
         sumcts(*,*,*,numspins)=abs(long(vsmjf.veis(*,*,*,ispin)) )
         if scimode1 then vlvls(*,numspins)=vsmjf.veistep
         if scimode2 then vlvls(*,numspins)=reform(vsmjf.veistep(*,0,0))
         if scimode6 then stop,'not yet operational for mode6'
         ;vlvls(*,numspins)=lz.mf(hkm1(24).offs+indgen(swest.nvsteps))

         tjd=long(fix(vsmjf.suntim_vsbl(ispin)/86400.d))
         sec=vsmjf.suntim_vsbl(ispin) - tjd*86400.d 
         hour_hms,sec/3600.d,hms           
         ;timpb5=tjd_pb5(long(tjd),long(1000*sec))  ;convert tjd,sec to pb5 time
         timpb5=reform(vsmjf.pb5tim_vsbl(*,ispin))
         dt=yrmoda(timpb5) + ' ' +string(timpb5(1),format='(i3)') + ' ' + hms
         instrhk(numspins).datim= dt(0)
         instrhk(numspins).elec_ion=$
           get_bits(lz.mf(ihk(6).offs),ihk(6).bv(0).p,ihk(6).bv(0).n)
         instrhk(numspins).tmmode=$
           get_bits(lz.mf(ihk(2).offs),ihk(2).bv(1).p,ihk(2).bv(1).n)
         instrhk(numspins).scimode=$
           get_bits(lz.mf(ihk(2).offs),ihk(2).bv(0).p,ihk(2).bv(0).n)
         instrhk(numspins).ebias1=lz.mf(ihk(18).offs)
         instrhk(numspins).ibias1=lz.mf(ihk(19).offs)
         instrhk(numspins).ebias2=lz.mf(ihk(28).offs)
         instrhk(numspins).ibias2=lz.mf(ihk(29).offs)
         instrhk(numspins).calpwr=$
           get_bits(lz.mf(ihk(3).offs),ihk(3).bv(0).p,ihk(3).bv(0).n)
         if recn eq recn_begin or recn eq recn_end then begin
           widget_control,WDGT.swelzav_spins,$
             set_value=string(numspins,format='(i3)')
           widget_control,WDGT.swelzav_eleion,$
             set_value=string(instrhk(numspins).elec_ion,format='(i2)')
           widget_control,WDGT.swelzav_ebias1,$
             set_value=string(instrhk(numspins).ebias1,format='(i2)')
           widget_control,WDGT.swelzav_ebias2,$
             set_value=string(instrhk(numspins).ebias2,format='(i2)')
           widget_control,WDGT.swelzav_calpwr,$
             set_value=string(instrhk(numspins).calpwr,format='(i2)')
         endif
         numspins=numspins+1
       endfor                    ;end ispin loop
     endfor                      ;end recn loop
     sumcts=sumcts(*,*,*,0:numspins-1)
     vlvls=vlvls(*,0:numspins-1)
     instrhk=instrhk(0:numspins-1)
     help,sumcts,vlvls,numspins
     recn=recn_curr
     swest.ispinbl=ispin_curr

 
     ;do average
     if numspins ge 1 then begin
       print,'average over spins'
       avgcts=fltarr(swest.ndets,swest.nvsteps,swest.nsectors)
       stdcts=fltarr(swest.ndets,swest.nvsteps,swest.nsectors)
       for jsect=0,swest.nsectors-1 do for jstep =0,swest.nvsteps-1 do $
       for jdet=0,swest.ndets-1 do begin
         result=stdev(float(sumcts(jdet,jstep,jsect,*)),mean)
         avgcts(jdet,jstep,jsect)=mean
         stdcts(jdet,jstep,jsect)=result
       endfor
       print,'voltage level ',vlvls(0)
       ;make sure just one voltage level
         w=where(vlvls ne vlvls(0))
         if w(0) eq -1 then begin
           savfile=getenv('BACKGPATH')+wst.lzdate+'_backg.idlsav'
           print,' '
           print,'Do you want to save background measurement file ',$
             savfile,' ? (y/n)'
           answ='' & read,answ 
           if answ eq 'y' then begin
             save,filename=savfile,sumcts,avgcts,stdcts,vlvls,instrhk
             print,'bkg file saved ',savfile
           endif else print,'no bkg file saved'
         endif else  print,'At least two different voltage levls ',$
           vlvls(0),vlvls(w(0))

     endif

                                   endcase
  
  'Detector pair rel gain' : relgain_detpairs
  

  'Accumulate slctd spins (YES glint rmvd)' : begin ;(to get relative gains)
     print,'accumulate over spins'
     ;initialize sums
       numspins=0
       sumcts=lonarr(swest.ndets,swest.nvsteps,swest.nsectors,500)
       vlvls=lonarr(swest.nvsteps,500)
       instrhk=$
         replicate({datim:'',sectjd:0.d,elec_ion:0,tmmode:0,scimode:0,$
         ebias1:0,ibias1:0,ebias2:0,ibias2:0,calpwr:0,bhat:fltarr(3)},500)

       widget_control,WDGT.swelzav_bgnmjf,get_value=recn_begin
       widget_control,WDGT.swelzav_endmjf,get_value=recn_end
       widget_control,WDGT.swelzav_spn,get_value=spin_begin
       widget_control,WDGT.swelzav_spinbl,get_value=spin_end
       print,'recn_begin, spin_begin,  recn_end, spin_end ',$
            recn_begin, spin_begin,  recn_end, spin_end
   
       recn_curr=recn
       ispin_curr=swest.ispinbl
       for recn=recn_begin,recn_end do begin
         print,'recn ',recn
         
         proc_rec,date_time,tmmode_ihk=tmmode_ihk,elec_ion=elec_ion
                    
         if recn eq recn_begin then ispin1=spin_begin else ispin1=0
         if recn eq recn_end then ispin2=spin_end else ispin2=swest.nspins-1
         for ispin=ispin1,ispin2 do begin 
           ispinbl=ispin
           if (recn eq recn_begin and ispin eq ispin1) or $
           (recn eq recn_end and ispin eq ispin2)  then begin
            widget_control,WDGT.swelzc_recn,set_value=recn
            widget_control,WDGT.swelzc_spin,set_value=ispin
            isector=swest.isector & widget_control,WDGT.swelzc_sect,set_value=isector
            ivstep=swest.ivstep & widget_control,WDGT.swelzc_vstep,set_value=ivstep
            plotcts
         endif
         ;set det 2 counts = det 3 counts in order to compute gains
         ;for period when vsmjf.set_det2_eq_det3=1
         if vsmjf.set_det2_eq_det3 then $
           vsmjf.veis(2,*,*,ispin)=vsmjf.veis(3,*,*,ispin)
           
         sumcts(*,*,*,numspins)=$
           long(abs(vsmjf.veis(*,*,*,ispin)) )
                    
         vlvls(*,numspins)=vsmjf.veistep(indgen(swest.nvsteps))

         tjd=long(fix(vsmjf.suntim_vsbl(ispin)/86400.d))
         sec=vsmjf.suntim_vsbl(ispin) - tjd*86400.d 
         hour_hms,sec/3600.d,hms           
         timpb5=tjd_pb5(long(tjd),long(1000*sec))  ;convert tjd,sec to pb5 time
         dt=yrmoda(timpb5) + ' ' +string(timpb5(1),format='(i3)') + ' ' + hms

         
;get mfi 3sec mag field
    ; timpb5=given pb5 time (year, day of year, millisec of day)  lonarr(3)
    ; tpb5 = pb5 time at center of minute for given record recn   lonarr(1440,3)
    ; bgse = mag vector at 3 sec intervals in minute tpb5     fltarr(1440,3,20)
    mindx=fix((timpb5(2))/60000)  ;magfile record number from minute index
    sindx=fix((timpb5(2)-mindx*60000 )/3000) ;3sec index in minute interval
    magfld=bgse(mindx,0:2,sindx)
    magtpb5=[tpb5(0),tpb5(1),tpb5(2) - (30000l - 1500l - long(sindx)*3000)]
    print,' '
    print,'given time ',timpb5
    print,'  mag time ',magtpb5
    print,'mag fld (gse) ',magfld
    if magfld(0) eq -1.e31 then begin
      print,'mag fld eq fill' & return
    endif
    bhat=magfld/sqrt(total(magfld^2))

         instrhk(numspins).datim= dt(0)
         instrhk(numspins).sectjd=vsmjf.suntim_vsbl(ispin)
         instrhk(numspins).elec_ion=$
           get_bits(lz.mf(ihk(6).offs),ihk(6).bv(0).p,ihk(6).bv(0).n)
         instrhk(numspins).tmmode=$
           get_bits(lz.mf(ihk(2).offs),ihk(2).bv(1).p,ihk(2).bv(1).n)
         instrhk(numspins).scimode=$
           get_bits(lz.mf(ihk(2).offs),ihk(2).bv(0).p,ihk(2).bv(0).n)
         instrhk(numspins).ebias1=lz.mf(ihk(18).offs)
         instrhk(numspins).ibias1=lz.mf(ihk(19).offs)
         instrhk(numspins).ebias2=lz.mf(ihk(28).offs)
         instrhk(numspins).ibias2=lz.mf(ihk(29).offs)
         instrhk(numspins).calpwr=$
           get_bits(lz.mf(ihk(3).offs),ihk(3).bv(0).p,ihk(3).bv(0).n)
         instrhk(numspins).bhat=bhat
         if recn eq recn_begin or recn eq recn_end then begin
           widget_control,WDGT.swelzav_spins,$
             set_value=string(numspins,format='(i3)')
           widget_control,WDGT.swelzav_eleion,$
             set_value=string(instrhk(numspins).elec_ion,format='(i2)')
           widget_control,WDGT.swelzav_ebias1,$
             set_value=string(instrhk(numspins).ebias1,format='(i2)')
           widget_control,WDGT.swelzav_ebias2,$
             set_value=string(instrhk(numspins).ebias2,format='(i2)')
           widget_control,WDGT.swelzav_calpwr,$
             set_value=string(instrhk(numspins).calpwr,format='(i2)')
         endif
         numspins=numspins+1
       endfor                    ;end ispin loop
     endfor                      ;end recn loop
     sumcts=sumcts(*,*,*,0:numspins-1)
     vlvls=vlvls(*,0:numspins-1)
     instrhk=instrhk(0:numspins-1)
     help,sumcts,vlvls,numspins
     recn=recn_curr
     swest.ispinbl=ispin_curr

                        endcase

  
  'Pitch fit: each detector, get rel gains' : if vsmjf.scimode eq 1 $
                        or vsmjf.scimode eq 4 or vsmjf.scimode eq 6 then $
                                            pitch_fit_det_spinsacc_m1 $
                                            else if vsmjf.scimode eq 2 then $
                                            pitch_fit_det_spinsacc_m2 
                                                   

  'Save relative gains                    ' : if vsmjf.scimode eq 1 $
                      or vsmjf.scimode eq 4 or vsmjf.scimode eq 6 then $
                                        pitch_fit_det_spinsacc_m1,savegains=1 $
                                            else if vsmjf.scimode eq 2 then $
                                        pitch_fit_det_spinsacc_m2,savegains=1
                                        
  'Pitch fit: each detector, get rel gains, det pair method' : begin
                        relgain_detpairs_avg
                        if vsmjf.scimode eq 1 $
                          or vsmjf.scimode eq 4 or vsmjf.scimode eq 6 then $
                          pitch_fit_det_spinsacc_m1,detpair=1
                                                               endcase                                         
  
  'Save relative gains, det pair method   ' : if vsmjf.scimode eq 1 $
                      or vsmjf.scimode eq 4 or vsmjf.scimode eq 6 then $
                      pitch_fit_det_spinsacc_m1,detpair=1,savegains=1 
                                                                       
  'Quit' : WIDGET_CONTROL, WDGT.swelzav_base, /DESTROY

  else: 
endcase
endcase
endcase
end



;=========================== do_cts_average =================================


pro do_cts_average

common sharewidg,WDGT
;common sharewidgc,wc
common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common sums,sumcts,vlvls,instrhk,numspins
common swestuff,swest

;get energy levels (ev) 
    if vsmjf.scimode eq 2 or vsmjf.scimode eq 11 then $
       wsteps=vsmjf.veistep(*,0,swest.ispinbl) $
    else if vsmjf.scimode eq 0 or vsmjf.scimode eq 1  or vsmjf.scimode eq 4 then $
       wsteps=vsmjf.veistep $
    else if vsmjf.scimode eq 6 then $
       wsteps=vsmjf.veistep(*,swest.ispinbl)  
    swest.ensteps_ev=volt_en(wsteps,/en,ion=swest.specie_selct)
    
if xregistered('do_cts_average') then begin
  WIDGET_CONTROL, WDGT.swelzav_base,/show
  widget_control,WDGT.swelzav_bgnmjf,set_value=recn
  ispin=swest.ispinbl
  widget_control,WDGT.swelzav_spn,set_value=ispin
  widget_control,WDGT.swelzav_nrmldet,set_droplist_select=swest.relgain_nmldet
  widget_control,WDGT.swelzav_vstep,set_droplist_select=swest.ivstep
  return
endif

    
WDGT.swelzav_base = WIDGET_BASE(TITLE = 'Do counts average', /COLUMN);main base

rbase1=widget_base(WDGT.swelzav_base,/column)
WDGT.swelzav_quit=cw_bgroup(rbase1,'Quit',/return_name)

WDGT.swelzav_detpair=cw_bgroup(rbase1,label_top='', $
  ['Detector pair rel gain'],$
  row=1,/return_name)

WDGT.swelzav_nrmldet=widget_droplist(rbase1,title='Normal detector',$
  value=string(indgen(6),format='(i1)'))
swest.relgain_nmldet=4
widget_control,WDGT.swelzav_nrmldet,set_droplist_select=swest.relgain_nmldet


WDGT.swelzav_vstep=widget_droplist(rbase1,title='Energy step (ev)',$
  value=string(swest.ensteps_ev,format='(i4)'))
;swest.ivstep=3
widget_control,WDGT.swelzav_vstep,set_droplist_select=swest.ivstep

WDGT.swelzav_ptchfitdets=widget_droplist(rbase1,title='Pitch fit: Det''s',$
  value=swest.detslct_list)
widget_control,WDGT.swelzav_ptchfitdets,$
  set_droplist_select=swest.detslct_list_indx  
     
WDGT.swelzav_getrelgn=cw_bgroup(rbase1,label_top='Rel gain determination', $
  ['Accumulate slctd spins (YES glint rmvd)',$
   'Pitch fit: each detector, get rel gains',$
   'Save relative gains                    ',$
   'Pitch fit: each detector, get rel gains, det pair method' ,$
   'Save relative gains, det pair method   '],$
  row=5,/return_name)

WDGT.swelzav_doavg=cw_bgroup(rbase1,label_top='Background determination', $
  ['Do avg of slctd spins; save to file'],$
  row=2,/return_name)
    
rbase=widget_base(WDGT.swelzav_base,/column)
WDGT.swelzav_set_d2_d3=cw_field(rbase,title='Set det2=det3',/string,xsize=5,/row)
  
rbase=widget_base(rbase1,/row)
WDGT.swelzav_bgnmjf=cw_field(rbase,title='begin: mjf',/long,xsize=5,/row)
widget_control,WDGT.swelzav_bgnmjf,set_value=recn

WDGT.swelzav_spn=cw_field(rbase,title='spinbl',/long,xsize=2,/row)

rbase=widget_base(rbase1,/row)
ispin=swest.ispinbl
widget_control,WDGT.swelzav_spn,set_value=ispin
WDGT.swelzav_endmjf=cw_field(rbase,title='end: mjf  ',/long,xsize=5,/row)
widget_control,WDGT.swelzav_endmjf,set_value=recn+30
WDGT.swelzav_spinbl=cw_field(rbase,title='spinbl',/long,xsize=2,/row)
WDGT.swelzav_spins=cw_field(rbase1,title='spins',/string,xsize=4,/row)
WDGT.swelzav_eleion=cw_field(rbase1,title='elec_ion',/string,xsize=2,/row)
WDGT.swelzav_ebias1=cw_field(rbase1,title='ebias1',/string,xsize=3,/row)
WDGT.swelzav_ebias2=cw_field(rbase1,title='ebias2',/string,xsize=3,/row)
WDGT.swelzav_calpwr=cw_field(rbase1,title='calpwr',/string,xsize=2,/row)

WIDGET_CONTROL, WDGT.swelzav_base, /REALIZE

XMANAGER, "do_cts_average", WDGT.swelzav_base, GROUP_LEADER = GROUP  ;hand off to manager

noyes=['No','Yes']
widget_control,WDGT.swelzav_set_d2_d3,set_value=noyes(vsmjf.set_det2_eq_det3)

END


