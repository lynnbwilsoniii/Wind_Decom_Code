pro glintcts,savefile,prtfile=prtfile

common phasemod2,phiveis,theveis,phistrl,thestrl,vunit,vunitstrl

if keyword_set(prtfile) eq 0 then prtfile=0

if prtfile then begin
  arg=savefile+'_prt'
  result=findfile(arg,count=count)
    openw,lun,savefile+'_prt',/get_lun
    printf,lun,savefile
endif
print,savefile
restore,savefile
;help
help,sumcts,avgcts,stdcts,vlvls,instrhk


ndets=6 & nsectors=8 & nsteps=16 & ;nspins=7
numspins=n_elements(instrhk)


print,' ' & print,$
'instrhk={datim:'',elec_ion:0,tmmode:0,scimode:0,ebias1:0,ibias1:0,ebias2:0,ibias2:0,calpwr:0}'

if prtfile then printf,lun,' ' & if prtfile then printf,lun,$
'instrhk={datim:'',elec_ion:0,tmmode:0,scimode:0,ebias1:0,ibias1:0,ebias2:0,ibias2:0,calpwr:0}'


for i=0,numspins-1 do print,i,instrhk(i),format='(i4,a30,8i4)'

for i=0,numspins-1 do $
  if prtfile then printf,lun,i,instrhk(i),format='(i4,a30,8i4)'
  
for idet=0,ndets-1 do for isect=0,nsectors-1 do begin
    print,' ' & print,' '
    print,savefile
    print,'begin detector, sector ',idet,isect
    print,'uncompressed counts vs. step for each spin in averaging interval'
    if prtfile then printf,lun,' ' & if prtfile then printf,lun,' '
    if prtfile then printf,lun,'detector, sector ',idet,isect
    if prtfile then printf,lun,$
     'uncompressed counts vs. step for each spin in averaging interval'

    print,'numspins ',numspins
    for i=0,numspins-1 do $
      print,transpose(sumcts(idet,*,isect,i)),format='(16i5)'
    for i=0,numspins-1 do $
      if prtfile then $
      printf,lun,transpose(sumcts(idet,*,isect,i)),format='(16i5)'

    if prtfile then printf,lun,'detector, sector ',idet,isect    
    if prtfile then printf,lun,'istep, spin phase, mean, stdv, sqrt(mean) '    
    y=fltarr(nsteps) & stdy=fltarr(nsteps)
    for istep=0,nsteps-1 do begin
      y(istep)=avgcts(idet,istep,isect)
      stdy(istep)=stdcts(idet,istep,isect)
      print,'istep, mean, stdv, sqrt(mean) ',$
        istep, y(istep), stdy(istep), sqrt(y(istep))
      if prtfile then printf,lun,$
        istep, phiveis(idet,istep,isect), y(istep), stdy(istep), sqrt(y(istep))
    endfor
    print,'end detector, sector ',idet,isect
endfor



 
if prtfile then free_lun,lun
if prtfile then print,'print file created: ',savefile+'_prt'
if prtfile eq 1 then prtfile=0

end






;---------------- backg_m2_review_event -------------------------------------

pro backg_m2_review_event,event

common sharewidgb,wb
common stuff,ndets,nsteps,nsects,nspins,files,selectfiles
common phasemod2,phiveis,theveis,phistrl,thestrl,vunit,vunitstrl


;-----------------------------------------------------------------------------
;                   begin event response sections 
;-----------------------------------------------------------------------------
CASE event.id OF

wb.button(0): begin
  case event.value of
     'Quit' : WIDGET_CONTROL, event.top, /DESTROY
  endcase
endcase

wb.button(1): selectfiles(event.value)=1
  
wb.button(2): begin
  print,event.value
  case event.value of
    'Compare two spectra by det step sect   ': begin   

      wfiles=where(selectfiles eq 1)
      print,wfiles,files(wfiles)
      selectfiles=intarr(n_elements(files))
      nselect=n_elements(wfiles)
      if nselect eq 1 then stop,'two files must be selected'
      nselect=nselect < 2  ;only first two selected files will be compared

      ;read and compare selected background files
      bcts=fltarr(ndets,nsteps,nsects,n_elements(wfiles))
      stdbcts=fltarr(ndets,nsteps,nsects,n_elements(wfiles))
      vxunit_gse=dblarr(ndets,nsteps,nsects,n_elements(wfiles))
      vyunit_gse=dblarr(ndets,nsteps,nsects,n_elements(wfiles))
      vzunit_gse=dblarr(ndets,nsteps,nsects,n_elements(wfiles))
      spnaxis=dblarr(3,n_elements(wfiles))
      for i=0,nselect-1 do begin
        print,wfiles(i),'  ',files(wfiles(i))
 
        ;get attitude data file
          oapath=getenv('OAPATH')
          arg=oapath+'*'+'at_def_'+'*'+'_v*.cdf'
          atfiles=findfile(arg,count=count)
          atdate=strmid(atfiles,strlen(atfiles(0))-16,8)
        ;find att file date that matches backg file date
          bdate=strmid(files(wfiles(i)),strlen(getenv('BACKGPATH')),8)
          w=where(atdate eq bdate)
          atfile=atfiles(w(0))
          print,'att file, backg file ',atfile,'  ',files(wfiles(i))
          print,'att date, backg date ',atdate(w(0)),'  ',bdate
        ;open att file
          openr,lunat,atfile,/get_lun 
        ;get attitude 10 min data
          loadcdf,atfile,'Time_PB5',tpb5_at   
          loadcdf,atfile,'GSE_R_ASCENSION',gse_ra
          loadcdf,atfile,'GSE_DECLINATION',gse_dec
        atindx=6   ;ten min index of gse_ra & gse_dec corresponding to 0100 UT
        ;get unitvectors in gse
        for isect=0,nsects-1 do for istep=0,nsteps-1 do for idet=0,ndets-1 do $
        begin
           wx=-sin(theveis(idet)*!dtor)*$
                 cos(phiveis(idet,istep,isect)*!dtor)
           wy=-sin(theveis(idet)*!dtor)*$
                 sin(phiveis(idet,istep,isect)*!dtor)
           wz=-cos(theveis(idet)*!dtor)
           ;transform from payload to gse
           payload_to_gse,[wx,wy,wz],$
             [gse_ra(atindx),gse_dec(atindx)],pay_gse_mtx,wc_gse
           vxunit_gse(idet,istep,isect,i)=wc_gse(0)
           vyunit_gse(idet,istep,isect,i)=wc_gse(1)
           vzunit_gse(idet,istep,isect,i)=wc_gse(2)
        endfor
        ;get spnaxis in gse
        payload_to_gse,[0.,0.,1.0],$
             [gse_ra(atindx),gse_dec(atindx)],pay_gse_mtx,wc_gse
        spnaxis(*,i)=wc_gse
          
        restore,files(wfiles(i))
        for ilun=100,125 do free_lun,ilun
        ;help
        help,avgcts,stdcts,sumcts,vlvls,instrhk
        help,instrhk,/str
        s=size(avgcts)
        ndets=s(1)
        nsteps=s(2)
        nsects=s(3)
        bcts(*,*,*,i)=avgcts
        stdbcts(*,*,*,i)=stdcts
        avgcts=0 & stdcts=0 & sumcts=0 & vlvls=0
      endfor

        print,'comparing files ',files(wfiles(0)),files(wfiles(1))
        for idet=0,ndets-1 do for isect=0,nsects-1 do begin
          print,' ' & print,'detector ',idet,'sector ',isect,$
            '   cnts 0   cnts 1  avg rms   cnts diff',$
             format='(a10,i3,5x,a10,i3,a40)'
          for istep=0,nsteps-1 do  begin
           v0=[vxunit_gse(idet,istep,isect,0),$
             vyunit_gse(idet,istep,isect,0),vzunit_gse(idet,istep,isect,0)]
           v1=[vxunit_gse(idet,istep,isect,1),$
             vyunit_gse(idet,istep,isect,1),vzunit_gse(idet,istep,isect,1)]
           dotpv01=total(v0*v1)
           ;angle=$
           ;acos(vxunit_gse(idet,istep,isect,0)*vxunit_gse(idet,istep,isect,1)+$
                ;vyunit_gse(idet,istep,isect,0)*vyunit_gse(idet,istep,isect,1)+$
                ;vzunit_gse(idet,istep,isect,0)*vzunit_gse(idet,istep,isect,1))$
                ;/!dtor
            print,idet,istep,isect,$
              bcts(idet,istep,isect,0),bcts(idet,istep,isect,1),$
              (stdbcts(idet,istep,isect,0)+stdbcts(idet,istep,isect,1))/2,$
              bcts(idet,istep,isect,1)-bcts(idet,istep,isect,0),$
              format='(3I3,2f8.1,3x,f8.1,3x,f8.1)'
                     endfor
        endfor
        print,'comparing files ',files(wfiles(0)),files(wfiles(1))
      
    endcase

    'Single spectrum counts by det step sect' : begin
      wfiles=where(selectfiles eq 1)
      print,wfiles(0),files(wfiles(0))
      glintcts,files(wfiles(0))
      selectfiles=intarr(n_elements(files))
    endcase

    'Single spectrum....create print file   ': begin
      wfiles=where(selectfiles eq 1)
      print,wfiles(0),files(wfiles(0))
      glintcts,files(wfiles(0)),prtfile=1
      selectfiles=intarr(n_elements(files))
     endcase

    else : 
  endcase
endcase

endcase


end


pro backg_m2_review

common sharewidgb,wb
common stuff,ndets,nsteps,nsects,nspins,files,selectfiles
common phasemod2,phiveis,theveis,phistrl,thestrl,vunit,vunitstrl

;define widget structure
  wb={wb_widgets,base:lonarr(100),slider:lonarr(100),button:lonarr(100),$
    field:lonarr(100),menu:lonarr(100),draw:lonarr(100),win:lonarr(100),$
     win_xsize:lonarr(100),win_ysize:lonarr(100)}

prtfile=0
       
phasem2   ;spin phase angle in fixed payload coords

files=findfile(getenv('BACKGPATH')+'*_m2_backg.idlsav')
print,files

selectfiles=intarr(n_elements(files))

n=n_elements(files)
ndets=6
nsteps=16
nsects=8
;nspins=7
bcts=fltarr(ndets,nsteps,nsects,n)


wb.base(0) = WIDGET_BASE($
  TITLE = 'VEIS mode2 background files',$
  /COLUMN,xoffset=750);main base
rbase=widget_base(wb.base(0),/row)
cbase=widget_base(rbase,/column)

cbase=widget_base(rbase,/column)
wb.button(0)=cw_bgroup(cbase, ['Quit'],row=1,/return_name)

wb.button(1)=cw_bgroup(cbase,/nonexclusive,/frame,files,$
  row=n_elements(files),/return_index,$
  label_top='Select one or two files',$
  /scroll,y_scroll_size=500,x_scroll_size=300)

wb.button(2)=cw_bgroup(cbase,/return_name,$
  ['Single spectrum counts by det step sect',$
   'Single spectrum....create print file   ',$
   'Compare two spectra by det step sect   '],row=2)

WIDGET_CONTROL, wb.base(0), /REALIZE
;WIDGET_CONTROL, wb.draw(0), GET_VALUE=windw
;wb.win(0)=windw

XMANAGER, "backg_m2_review", wb.base(0), GROUP_LEADER = GROUP  ;hand off to manager

end
