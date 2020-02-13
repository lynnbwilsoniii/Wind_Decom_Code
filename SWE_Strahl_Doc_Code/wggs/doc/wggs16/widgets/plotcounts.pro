
;=========================== increm =================================

pro increm,jstep,jmax,kstep

if jstep gt jmax-1 then begin
  jstep=0
  kstep=kstep+1
endif
if jstep lt 0 then begin
  jstep=jmax-1
  kstep=kstep-1
endif
end



;=========================== plotcts =================================

pro plotcts

common sharewidg,WDGT
common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common sharehk,tmmode_ihk,elec_ion
common wstuff,wst
common swestuff,swest   

if vsmjf.background_test then begin
  case swest.vplotsel of
    'all_steps_phi'  :veis_phi_16
    'all_steps_sang' :veis_sang_16
  endcase
endif else begin    
  case swest.vplotsel of
    'sectors'        :veis_sectors
    'phi'            :veis_phi
    'pitch'          :veis_pit
    'angles'         :vsangles
    'all_steps_phi'  :veis_phi_16
    'all_steps_sang' :veis_sang_16
    'vsteps'         :veis_steps
    'vsteps_backg'   :veis_steps,/backg
    'vsteps_crrctd'  :veis_steps,/crrctd
    'fvsteps'        :veis_steps,/ff
    'fvsteps_patch'  :veis_steps_patch
    'fvsteps_newpatch' :veis_steps_patchfit
    
    'pitch_fit_det_spin' :$
      if (vsmjf.scimode eq 1 or vsmjf.scimode eq 4 or vsmjf.scimode eq 6) $
      then pitch_fit_det_spin_m1 $
      else if  mode_ok and vsmjf.scimode eq 2 $
      then pitch_fit_det_spin_m2
                
    'pitch_fit_alldet_spin' : $
      if (vsmjf.scimode eq 1 or vsmjf.scimode eq 4 or vsmjf.scimode eq 6) $
      then pitch_fit_alldet_spin 
      
    else:      
  endcase
endelse
swest.vplotsel_last=swest.vplotsel
;widget_control,WDGT.swelzc_ptchfitdets,set_droplist_select=swest.detslct_list_indx  
end


;=========================== plotcounts_event =================================

pro plotcounts_event,event

common sharewidg,WDGT
common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common sums,sumcts,vlvls,instrhk,numspins
common sharehk,tmmode_ihk,elec_ion
common mpstuff,mflnm,pflnm,mdat,pdat,refsec
common shared,d
common wstuff,wst
common swestuff,swest
common sunglint,xveis_m1,xveis_m2,gdate
common sunglint_other,xveis_m1_other,gdate_other,xveis_m2_other



swpmd=['all elecs','all ions ','alt sects','alt spins', 'bckg test']
quit=''
specie=['elecs','ions']

CASE event.id OF

   WDGT.swelzc_recn : begin
     widget_control,WDGT.swelzc_recn,get_value=val
     recn=val(0) 
                 endcase
 
   WDGT.swelzc_spin : begin
     widget_control,WDGT.swelzc_spin,get_value=val
     swest.ispinbl=val(0) 
                 endcase

   WDGT.swelzc_sect : begin
     widget_control,WDGT.swelzc_sect,get_value=val
     swest.isector=val(0) 
                 endcase

   WDGT.swelzc_vstep : begin
     widget_control,WDGT.swelzc_vstep,get_value=val
     swest.ivstep=val(0) 
                 endcase
    

   WDGT.swelzc_quit: begin
    print,event.value
    case event.value of
      'Parent' :   begin
                print,event.value
                widget_control,WDGT.swelzc_recn,get_value=val & recn=val(0)
                widget_control,WDGT.swelzc_spin,get_value=val & swest.ispinbl=val(0)
                widget_control,WDGT.swelzc_sect,get_value=val & swest.isector=val(0)
                widget_control,WDGT.swelzc_vstep,get_value=val & swest.ivstep=val(0)
                widget_control,WDGT.swelz_base_main,/show
                return
                   endcase
      'Quit' :   begin
         WIDGET_CONTROL, event.top, /DESTROY
         return
                 endcase
      
      'Hardcopy_bw' : begin
                     wst.hardcopy=1
                     wst.printer=wst.printer_bw
                     wst.print_flnm=wst.print_flnm_bw
                     wst.print_cmd=wst.print_cmd_bw 
                     clrtbl_indx,/hardcopy
                   endcase

      'Hardcopy_clr' : begin
                     wst.hardcopy=1
                     wst.printer=wst.printer_clr
                     wst.print_flnm=wst.print_flnm_clr
                     wst.print_cmd=wst.print_cmd_clr 
                     clrtbl_indx,/hardcopy
                   endcase

    endcase
                 endcase

  WDGT.swelzc_relgain: begin
                do_cts_average
                swest.noshow=1
                ;swest.vplotsel='pitch'
                            endcase
                            
  WDGT.swelzc_pltype: begin
    case event.value of
      'Cts_sector       ' : swest.vplotsel='sectors' 
         
      'Cts_phi          ' : swest.vplotsel='phi'
          
      'Cts_pitch        ' : swest.vplotsel='pitch'
          
      'Det angles       ' : swest.vplotsel='angles'
          
      'Cts_phi all stps ' : swest.vplotsel='all_steps_phi'
          
      'Cts_sun all stps ' : swest.vplotsel='all_steps_sang'
         
      'Cts_step (def)   ' : swest.vplotsel='vsteps'
      
      'Cts_step /backg  ' : swest.vplotsel='vsteps_backg'

      'Cts_step /corrctd' : swest.vplotsel='vsteps_crrctd'
          
      'f_step           ' : swest.vplotsel='fvsteps'
      
      'f w/ patch, scpot' : swest.vplotsel='fvsteps_patch'

      'f w/ new patch   ' : swest.vplotsel='fvsteps_newpatch'
      
      
      else:
    endcase
                endcase

  WDGT.swelzc_bckgsubtr: begin
    case event.value of
      'BackgSubtr (Cts_step /backg)' : begin
          if swest.subtrbkg eq 'No' then swest.subtrbkg='Yes' $
          else swest.subtrbkg='No'
          widget_control, WDGT.swelzc_bckgsubtr_noyes, set_value = swest.subtrbkg
          swest.vplotsel='vsteps_backg'
                                         endcase
                endcase
                endcase
  
  WDGT.swelzc_glnt: begin
    swest.univgmask=event.index
    if swest.univgmask then widget_control,WDGT.swelzc_glntdate,set_value=gdate_other $
    else widget_control,WDGT.swelzc_glntdate,set_value=gdate
                endcase
  
  WDGT.swelzc_sect_incr: begin
    if swest.vplotsel eq 'vsteps' $
    or swest.vplotsel eq 'fvsteps' $
    or swest.vplotsel eq 'fvsteps_patch' $
    or swest.vplotsel eq 'fvsteps_newpatch' $
    or swest.vplotsel eq 'vsteps_crrctd' then begin
      case event.value of
        ' + ': swest.isector=swest.isector+1
        ' - ': swest.isector=swest.isector-1                 
      endcase
    endif  
                endcase

  WDGT.swelzc_vstep_incr: begin
    if swest.vplotsel eq 'sectors' $
    or swest.vplotsel eq 'phi' $
    or swest.vplotsel eq 'pitch' then begin
      case event.value of
         ' + ': swest.ivstep=swest.ivstep + 1
         ' - ': swest.ivstep=swest.ivstep - 1
      endcase
    endif
                endcase  

WDGT.swelzc_spin_incr: begin
    case event.value of
       ' + ': swest.ispinbl=swest.ispinbl + 1          
       ' - ': swest.ispinbl=swest.ispinbl - 1          
    endcase
                endcase 

WDGT.swelzc_recn_incr: begin
    case event.value of
             0 : recn=recn+1                             
             1 : recn=recn-1                            
    endcase          
              endcase

;WDGT.swelzc_ptchfitdets: swest.detslct_list_indx=event.index

;WDGT.swelzc_together: begin
;  case event.index of
;    0: swest.vplotsel='pitch_fit_alldet_spin'   ;'together'
;    1: swest.vplotsel='pitch_fit_det_spin'      ;'separately' 
;  endcase
;               endcase
  
              
WDGT.swelzc_mnmx: begin
  case event.value of
    'all det''s': swest.minmax=1       
    'each det': swest.minmax=0
    'each det (non-glint)': swest.minmax=-1
  endcase
              endcase

WDGT.swelzc_det : swest.chng_gain_det=event.index

WDGT.swelzc_rgincr : begin
    case event.value of
       ' + ': swest.relgain(swest.chng_gain_det)=$
         swest.relgain(swest.chng_gain_det) + swest.rel_gain_incre         
       ' - ': swest.relgain(swest.chng_gain_det)=$
         swest.relgain(swest.chng_gain_det) - swest.rel_gain_incre 
       'Restore' : swest.relgain = swest.relgain_old          
    endcase
    widget_control,WDGT.swelzc_rgnew,$
      set_value=string(swest.relgain,format='(6f7.2)')
                
endcase

WDGT.swelzc_vpotincr : begin
    if swest.vplotsel ne 'fvsteps_newpatch' then return
    case event.value of
       ' + ': swest.vpot=swest.vpot + swest.vpot_incre         
       ' - ': swest.vpot=swest.vpot - swest.vpot_incre 
       'Restore' : swest.vpot = swest.vpot_old
    endcase
endcase
    
WDGT.swelzc_lchanincr : begin
    if swest.vplotsel ne 'fvsteps_newpatch' then return
    case event.value of
       ' + ': swest.lchan=(swest.lchan + 1) < 15         
       ' - ': swest.lchan=(swest.lchan - 1) > 4 
       'Restore' : swest.lchan = swest.lchan_old
    endcase                
endcase
                
else:
endcase


ivstep=swest.ivstep
isector=swest.isector
increm,ivstep,swest.nvsteps,isector
swest.ivstep=ivstep
swest.isector=isector 
widget_control,WDGT.swelzc_vstep,set_value=ivstep
print,'ivstep,isector ',swest.ivstep,swest.isector

ispin=swest.ispinbl
increm,isector,swest.nsectors,ispin
swest.isector=isector 
swest.ispinbl=ispin
widget_control,WDGT.swelzc_sect,set_value=isector
print,'isector,ispin ',swest.isector,swest.ispinbl

increm,ispin,swest.nspins,recn
swest.ispinbl=ispin & widget_control,WDGT.swelzc_spin,set_value=ispin
print,'ispin,recn ',swest.ispinbl,recn

if recn ge 1 and recn le fh.nmf then begin

  widget_control,WDGT.swelzc_recn,set_value=recn
  ispin=swest.ispinbl & widget_control,WDGT.swelzc_spin,set_value=ispin
  isector=swest.isector & widget_control,WDGT.swelzc_sect,set_value=isector
  ivstep=swest.ivstep & widget_control,WDGT.swelzc_vstep,set_value=ivstep
  print,'recn ',recn,'  ispin ',swest.ispinbl,'  isector ',swest.isector,$
    '  ivstep ',swest.ivstep
  ;process lz record
     proc_rec,date_time,tmmode_ihk=tmmode_ihk,elec_ion=elec_ion
                  

  widget_control,WDGT.swelzc_mode,set_value=string(vsmjf.scimode,format='(i2)')
  if vsmjf.scimode eq 2 or vsmjf.scimode eq 11 then begin
    isector=swest.isector
    ispin=swest.ispinbl
    widget_control,WDGT.swelzc_eleion,set_value=specie(vsmjf.eleion(isector,ispin))
  endif
  if vsmjf.scimode eq 0 or vsmjf.scimode eq 1 or vsmjf.scimode eq 4 $
                    or vsmjf.scimode eq 6 and vsmjf.background_test eq 0 $
  then widget_control,WDGT.swelzc_eleion,set_value=specie(vsmjf.eleion(0))
  widget_control, WDGT.swelzc_eleionswp, set_value = swpmd(vsmjf.eleion_sweep)
  sz=size(vsmjf.cveis)
  swest.ndets=sz(1) & swest.nvsteps=sz(2) & swest.nsectors=sz(3)
  swest.nspins=sz(4)
  ;check incremented spin again with new mjf just processed
    if swest.ispinbl gt swest.nspins-1 then swest.ispinbl=swest.nspins-1  
   
  widget_control, WDGT.swelzc_bckgsubtr_noyes, set_value = swest.subtrbkg
  
 
  widget_control,WDGT.swelzc_glnt,set_droplist_select=swest.univgmask
  widget_control,WDGT.swelzc_glntnum,$
   set_value=string(n_elements(where(vsmjf.xveis(*,*,*,0) eq -1)),format='(i2)')
  if swest.univgmask then widget_control,WDGT.swelzc_glntdate,set_value=gdate_other $
  else widget_control,WDGT.swelzc_glntdate,set_value=gdate 
    
       
  wset,WDGT.swelzc_win & if swest.noshow ne 1 then wshow
  print,'vplotsel ',swest.vplotsel
  plotcts 
endif


end
   

;=========================== plotcounts =======================================

pro plotcounts

common sharewidg,WDGT
common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common sums,sumcts,vlvls,instrhk,numspins
common sharehk,tmmode_ihk,elec_ion
common mpstuff,mflnm,pflnm,mdat,pdat,refsec
common shared,d
common wstuff,wst
common swestuff,swest
common sunglint,xveis_m1,xveis_m2,gdate
common sunglint_other,xveis_m1_other,gdate_other,xveis_m2_other

if wst.lz_is_read eq 0 then return

swpmd=['all elecs','all ions ','alt sects','alt spins', 'bckg test']
specie=['elecs','ions']

clrtbl_indx

swest.relgain_old=vsmjf.relgain

     
if xregistered('plotcounts') then begin
  wset,WDGT.swelzc_win & wshow
  widget_control,WDGT.swelzc_recn,set_value=recn
  widget_control,WDGT.swelzc_spin,set_value=swest.ispinbl
  widget_control,WDGT.swelzc_sect,set_value=swest.isector
  widget_control,WDGT.swelzc_vstep,set_value=swest.ivstep

  ;process lz record  
    proc_rec,date_time,tmmode_ihk=tmmode_ihk,elec_ion=elec_ion
       
  widget_control,WDGT.swelzc_mode,set_value=string(vsmjf.scimode,format='(i2)')
  if vsmjf.scimode eq 2 or vsmjf.scimode eq 11 then $
    widget_control,WDGT.swelzc_eleion,$
    set_value=specie(vsmjf.eleion(swest.isector,swest.ispinbl))
  if vsmjf.scimode eq 0 or vsmjf.scimode eq 1 or vsmjf.scimode eq 4 $
     or vsmjf.scimode eq 6 and vsmjf.background_test eq 0 then $
     widget_control,WDGT.swelzc_eleion,set_value=specie(vsmjf.eleion(0))

  widget_control, WDGT.swelzc_eleionswp, set_value = swpmd(vsmjf.eleion_sweep)
  sz=size(vsmjf.cveis)
  swest.ndets=sz(1) & swest.nvsteps=sz(2) & swest.nsectors=sz(3)
  swest.nspins=sz(4)

  widget_control, WDGT.swelzc_bckgsubtr_noyes, set_value = swest.subtrbkg
  
  
  widget_control,WDGT.swelzc_glnt,set_droplist_select=swest.univgmask
  widget_control,WDGT.swelzc_glntnum,$
   set_value=string(n_elements(where(vsmjf.xveis(*,*,*,0) eq -1)),format='(i2)')
  if swest.univgmask then widget_control,WDGT.swelzc_glntdate,set_value=gdate_other $
  else widget_control,WDGT.swelzc_glntdate,set_value=gdate 
  
  widget_control,WDGT.swelzc_rgold,$
    set_value=string(swest.relgain_old,format='(6f7.2)')
  widget_control,WDGT.swelzc_rgnew,$
    set_value=string(swest.relgain,format='(6f7.2)')
               
  wset,WDGT.swelzc_win & if swest.noshow ne 1 then wshow
  print,'vplotsel ',swest.vplotsel
  plotcts 
  widget_control,WDGT.swelzc_base,/show
  return
endif

;process lz record
  proc_rec,date_time,tmmode_ihk=tmmode_ihk,elec_ion=elec_ion

;help,vsmjf.cveis
sz=size(vsmjf.cveis)
swest.ndets=sz(1) & swest.nvsteps=sz(2) & swest.nsectors=sz(3)
swest.nspins=sz(4)

swest.idet=0 
;swest.ivstep=0 
swest.isector=0     
swest.ispinbl=0

;define widget structure
   wc={base:0l,draw:0l,win:0l,quit:0l,pltype:0l,bckgsubtr:0l,bckgsubtr_noyes:0l,$
      mode:0l,eleion:0l,eleionswp:0l,mnmx:0l,sect_incr:0l,sect:0l,$
      vstep_incr:0l,vstep:0l,spin_incr:0l,spin:0l,recn_incr:0l,recn:0l,$
      ptchfitdets:0l,together:0l,glnt:0l,glntdate:0l,glntnum:0l,relgain:0l}
       
WDGT.swelzc_base = WIDGET_BASE(TITLE = 'SWE VEIS Counts Data  ', /COLUMN)  ;main base

rbase1=widget_base(WDGT.swelzc_base,/row)

cbase1=widget_base(rbase1,/column)
 

WDGT.swelzc_draw = WIDGET_DRAW(cbase1,/BUTTON_EVENTS,/FRAME, $
  UVALUE ='DRAW_WIN_EVENT',RETAIN = 2, XSIZE = 700, YSIZE = 750)

cbase2=widget_base(rbase1,/column)

WDGT.swelzc_quit=cw_bgroup(cbase2,['Parent','Quit',$
  'Hardcopy_bw','Hardcopy_clr'],row=1,/return_name)

WDGT.swelzc_relgain=cw_bgroup(cbase2,'Relative gain determination; Background',/return_name)

WDGT.swelzc_pltype=cw_bgroup(cbase2,$
  ['Cts_sector       ',$
   'Cts_phi          ',$
   'Cts_pitch        ',$
   'Det angles       ',$
   'Cts_phi all stps ',$
   'Cts_sun all stps ',$
   'Cts_step (def)   ',$
   'Cts_step /corrctd',$
   'f_step           ',$
   'f w/ patch, scpot',$
   'f w/ new patch   '$
   ],row=6,/return_name)

rbase=widget_base(cbase2,/row)
WDGT.swelzc_bckgsubtr=cw_bgroup(rbase,'BackgSubtr (Cts_step /backg)',$
     label_left='',row=1,/return_name)
WDGT.swelzc_bckgsubtr_noyes=cw_field(rbase,/string,xsize=3,ysize=1,title=' ',/row)


rbase=widget_base(cbase2,/row)
WDGT.swelzc_mode=cw_field(rbase,title='Mode',/integer,xsize=2,/row)
WDGT.swelzc_eleion=cw_field(rbase,title=' ',/string,xsize=5,/row)
WDGT.swelzc_eleionswp=cw_field(rbase,/string,xsize=9,ysize=1,title=' ',/row)

WDGT.swelzc_mnmx=cw_bgroup(cbase2,$
  label_top='Min-max y scale',$
  ['all det''s','each det','each det (non-glint)'],$
  row=1,/return_name)

rbase=widget_base(cbase2,/row)
WDGT.swelzc_sect_incr=cw_bgroup(rbase,$
  label_left='Sect',[' + ',' - '],row=1,/return_name)
WDGT.swelzc_sect=cw_field(rbase,title=' ',/long,xsize=1,/return_events)

WDGT.swelzc_vstep_incr=cw_bgroup(rbase,$
  label_left='Vstep',[' + ',' - '],row=1,/return_name)
WDGT.swelzc_vstep=cw_field(rbase,title=' ',/long,xsize=2,/return_events)

rbase=widget_base(cbase2,/row)
WDGT.swelzc_spin_incr=cw_bgroup(rbase,$
  label_left='Spin',[' + ',' - '],row=1,/return_name)
WDGT.swelzc_spin=cw_field(rbase,title=' ',/long,xsize=1,/return_events)

WDGT.swelzc_recn_incr=cw_bgroup(rbase,$
  label_left='Mjf',[' + ',' - '],row=1)
WDGT.swelzc_recn=cw_field(rbase,title=' ',/long,xsize=4,/return_events)

;rbase=widget_base(cbase2,/row)
;WDGT.swelzc_ptchfitdets=widget_droplist(rbase,title='Pitch fit: Det''s',$
;  value=swest.detslct_list)
  
;WDGT.swelzc_ptchfitdets=widget_droplist(rbase,value=['together','separately'])

rbase=widget_base(cbase2,/row)     
WDGT.swelzc_glnt=widget_droplist(rbase,title='Glint mask',$
  value=['Default','Other'])
WDGT.swelzc_glntdate=cw_field(rbase,title=' ',/string,xsize=10,/row)
WDGT.swelzc_glntnum=cw_field(rbase,title=' ',/string,xsize=2,/row)

;cbase=widget_base(cbase2,/column,/frame)
;wlblrg=widget_label(cbase,value='Relative gain adjustment')
;WDGT.swelzc_rgold=cw_field(cbase,title='Old',/string,xsize=42,/row)
;WDGT.swelzc_rgnew=cw_field(cbase,title='New',/string,xsize=42,/row)

;rbase=widget_base(cbase,/row)
;WDGT.swelzc_det=widget_droplist(rbase,title='Det',$
;  value=string(indgen(6),format='(i1)'))

;WDGT.swelzc_rgincr=cw_bgroup(rbase,label_left= $
;  string(swest.rel_gain_incre,format='(f3.1)')+$
;  ' incre ',[' + ',' - ','Restore'],row=1,/return_name)

;cbase=widget_base(cbase2,/column,/frame)
;wlblrg=widget_label(cbase,value='Spacecraft potential adjustment')

;rbase=widget_base(cbase,/row)
;WDGT.swelzc_vpotincr=cw_bgroup(rbase,label_left= $
;  string(swest.vpot_incre*1e-8,format='(f3.1)')+$
;  'e8 vpot incre',[' + ',' - ','Restore'],row=1,/return_name)

;rbase=widget_base(cbase,/row)
;WDGT.swelzc_lchanincr=cw_bgroup(rbase,label_left= $
;  'incre number of patch steps',[' + ',' - ','Restore'],row=1,/return_name)
        
WIDGET_CONTROL, WDGT.swelzc_base, /REALIZE


WIDGET_CONTROL, WDGT.swelzc_draw, GET_VALUE=windw
WDGT.swelzc_win=windw
wset,WDGT.swelzc_win
widget_control,WDGT.swelzc_recn,set_value=recn

widget_control,WDGT.swelzc_spin,set_value=swest.ispinbl
widget_control,WDGT.swelzc_sect,set_value=swest.isector
widget_control,WDGT.swelzc_vstep,set_value=swest.ivstep
widget_control,WDGT.swelzc_mode,set_value=string(vsmjf.scimode,format='(i2)')
if vsmjf.scimode eq 2 or vsmjf.scimode eq 11 then begin
    isector=swest.isector
    ispin=swest.ispinbl
    widget_control,WDGT.swelzc_eleion,$
      set_value=specie(vsmjf.eleion(swest.isector,swest.ispinbl))
endif
if vsmjf.scimode eq 0 or vsmjf.scimode eq 1 or vsmjf.scimode eq 4 $
                      or vsmjf.scimode eq 6 and vsmjf.background_test eq 0 $
then widget_control,WDGT.swelzc_eleion,set_value=specie(vsmjf.eleion(0))
widget_control, WDGT.swelzc_eleionswp, set_value = swpmd(vsmjf.eleion_sweep)

widget_control, WDGT.swelzc_bckgsubtr_noyes, set_value = swest.subtrbkg


widget_control,WDGT.swelzc_glnt,set_droplist_select=swest.univgmask
widget_control,WDGT.swelzc_glntnum,$
  set_value=string(n_elements(where(vsmjf.xveis(*,*,*,0) eq -1)),format='(i2)')
if swest.univgmask then widget_control,WDGT.swelzc_glntdate,$
  set_value=gdate_other else $
  widget_control,WDGT.swelzc_glntdate,set_value=gdate 

;widget_control,WDGT.swelzc_rgold,$
;  set_value=string(swest.relgain_old,format='(6f7.2)')
;widget_control,WDGT.swelzc_rgnew,$
;  set_value=string(swest.relgain,format='(6f7.2)')
;widget_control,WDGT.swelzc_det,set_droplist_select=swest.chng_gain_det
      
swest.minmax=1

if keyword_set(swest.vplotsel_last) eq 0 then swest.vplotsel='vsteps' else $
  swest.vplotsel=swest.vplotsel_last

swest.vplotsel_last=swest.vplotsel
plotcts

XMANAGER, "plotcounts", WDGT.swelzc_base, GROUP_LEADER = GROUP  

END
