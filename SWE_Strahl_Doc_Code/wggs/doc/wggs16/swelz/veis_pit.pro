pro veis_pit

;plots veis counts data

common sharewidg,wa
common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common magstuff,magfile,tpb5,bgse
common swestuff,swest
common wstuff,wst
common gsevunit,timpb5,vunit_gse

hardcopy=wst.hardcopy & ndets=swest.ndets & nvsteps=swest.nvsteps 
nsectors=swest.nsectors & nspins=swest.nspins & idet=swest.idet 
ivstep=swest.ivstep & isector=swest.isector & ispin=swest.ispinbl
delete=swest.delete & ndel=swest.ndel & minmax=swest.minmax 

wst.hardcopy=0

yn=['no','yes']
clr=[225,wst.clr_green]

start:

if keyword_set(hardcopy) eq 0 then hardcopy=0
if hardcopy then begin 
  pflnm=getenv('IDLSAV')+'idl.ps'
  print,' ' & print,'making hardcopy..... ' 
  set_plot,'ps'
  device,/inches,xoffset=1.0,yoffset=1.0,xsize=7.,ysize=8.,filename=pflnm
endif
 
!p.multi=[0,2,3,0,0]
charsize=2.25
 
xtitle=[' ',' ',' ',' ','pitch angle','pitch angle']
ytitle=['counts',' ','counts',' ','counts',' ']

timpb5=vsmjf.pb5tim_vsbl(*,ispin)
sec=double(timpb5(2)/1000)
hour_hms,sec/3600.d,hms
spndt= yrmoda(timpb5) + ' ' +string(timpb5(1),format='(i3)') + ' ' + hms
stitle=[' ',' ',' ',' ',spndt,' ']


;get mfi mag field
if swest.mag_3s_kp eq 1 then begin               ;use mfi 3s mag field
  get_3smag,timpb5,magtpb5,b,phi_bpos,the_bpos
endif else if swest.mag_3s_kp eq 2 then begin    ;use mfi kp mag field
  get_kpmag,timpb5,b,phi_bpos,the_bpos 
endif else begin                                 ;no mag data
  phi_bpos=0 & the_bpos=0
endelse  
bhat=b/sqrt(total(b^2))
phimag=phi_bpos
themag=the_bpos 

;get unit vectors corresponding to 180 degrees from look directions
  vhat=vsmjf.vunit

;approx transformation from payload to gse
  vhat(*,*,*,1:2)=-vhat(*,*,*,1:2)  

for jdet=0,ndets-1 do begin
  vparahat=vhat(jdet,ivstep,*,0)*bhat(0)+$
           vhat(jdet,ivstep,*,1)*bhat(1)+$
           vhat(jdet,ivstep,*,2)*bhat(2)
  x=acos(vparahat)/!dtor  ;pitch angle

  y=abs( vsmjf.veis(jdet,ivstep,*,ispin) )
  vlevel=-1
  if vsmjf.scimode eq 2 then begin
    if vsmjf.eleion_sweep eq 0 then vlevel=vsmjf.veistep(ivstep,0,0) 
  endif else $
 if vsmjf.scimode eq 1 or vsmjf.scimode eq 4 then vlevel=vsmjf.veistep(ivstep) $
 else if vsmjf.scimode eq 6 then vlevel=vsmjf.veistep(ivstep,ispin)
  xrange=[0,180]
  xticks=4
  xtickformat='(i3)'
  ;yrange=[0,4096]

  wnoglnt=where(vsmjf.xveis(jdet,ivstep,*,ispin) ne -1)  ;identify non-glint

  case minmax of
     1: yrange=[0,1.1*max(abs( vsmjf.veis(*,ivstep,*,ispin) ))] 
     0: yrange=[0,1.1*max(abs( vsmjf.veis(jdet,ivstep,*,ispin) ))] 
    -1: yrange=[0,1.1*max(abs( vsmjf.veis(jdet,ivstep,wnoglnt,ispin) ))]
  endcase 
;stop
  yticks=6
  ytickformat='(i4)'
  plot,x,y,/nodata,$
    title='det '+string(jdet,format='(i1)')+'    '+$
          '  step '+string(ivstep,format='(i2)')+$
          '  vlevel '+string(vlevel,format='(i3)')+$
          '  spin '+string(ispin,format='(i1)'),$          
    xrange=xrange,xticks=xticks,xstyle=1,xtickformat=xtickformat,$
    xtitle=xtitle(jdet),charsize=charsize,$
    yrange=yrange,yticks=yticks,ystyle=1,ytickformat=ytickformat,$
    ytitle=ytitle(jdet),subtitle=stitle(jdet)
  case minmax of
    -1: oplot,x(wnoglnt),y(wnoglnt),psym=4,symsize=0.85,color=clr(hardcopy)
    else: oplot,x,y,psym=4,symsize=0.85,color=clr(hardcopy)
  endcase
  wglnt=where(vsmjf.xveis(jdet,ivstep,*,ispin) eq -1)  ;identify glint
  if wglnt(0) ne -1 then begin
    if hardcopy eq 0 then oplot,[x(wglnt)],$
        [abs( vsmjf.veis(jdet,ivstep,wglnt,ispin))],$
        psym=7,symsize=0.85,color=wst.clr_green  $
     else oplot,[x(wglnt)],$
        [abs( vsmjf.veis(jdet,ivstep,wglnt,ispin))],$
        psym=7,symsize=0.85,color=clr(hardcopy)
  endif  
endfor


if hardcopy then begin   
  device,/close 
  set_plot,'x'
  print,' ' & print,'printing hardcopy file ',pflnm
  spawn, 'lp '+pflnm
  hardcopy=0
  clrtbl_indx
  goto,start
endif

!p.multi=0

end
