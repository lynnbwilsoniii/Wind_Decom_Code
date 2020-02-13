

;------------------- veis_steps_patchfit -------------------------------------
pro veis_steps_patchfit

;plots veis counts/f data and computes gaussian patch

common lzstuff,infile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common log_delog,comp_tbl,dcomp_tbl
common swestuff,swest
common wstuff,wst
common shared,d
common onecount,f1ct,v1ct

;This version computes a patch fit on veis data and does not depend on 
;the patch fit coefficients bn carried with the moments data.

hardcopy=wst.hardcopy & ndets=swest.ndets & nvsteps=swest.nvsteps 
nsectors=swest.nsectors & nspins=swest.nspins & idet=swest.idet 
ivstep=swest.ivstep & isector=swest.isector & ispin=swest.ispinbl
delete=swest.delete & ndel=swest.ndel & minmax=swest.minmax 

wst.hardcopy=0

print,'veis_steps_patchfit: isector ',isector

clr=[225,wst.clr_green]
oppdet=[5,4,3,2,1,0]
specie=['elecs','ions']

yn=['no','yes']
start:

if keyword_set(hardcopy) eq 0 then hardcopy=0
if hardcopy then begin
  pflnm=getenv('IDLSAV')+'idl.ps'
  print,' ' & print,'making hardcopy..... ' 
  set_plot,'ps'
  device,/inches,xoffset=1.0,yoffset=1.0,xsize=7.,ysize=8.,filename=pflnm
endif

pos,3,posn,ysep=0.1,xoff=0.1,xtop=0.9
noerase=[0,1,1]
charsize=1.15  ;2.25
   
timpb5=vsmjf.pb5tim_vsbl(*,ispin)
sec=double(timpb5(2)/1000)
hour_hms,sec/3600.d,hms
spndt= yrmoda(timpb5) + ' ' +string(timpb5(1),format='(i3)') + ' ' + hms

get_f_v_patch,ispin,vpot,vel,v_vpot,bncoef,fegaussfit,vgaussfit,$
  fblk,npatch,nvmin,nwsteps,wx,wy,wz,w,fe
  
enpot=2.85e-16*vpot*vpot
  
k1=nvmin
k2=nwsteps-1   

stitle=[' ',' ','adjstd rel gains  '+string(swest.relgain,format='(6f5.1)')]  
xtitle=[' ',' ',$
'mode'+string(vsmjf.scimode,format='(i2)')$
+'  '+spndt+'  '+' ']

ebias1=lz.mf(ihk(18).offs)
ebias2=lz.mf(ihk(28).offs)

            
for idet=0,2 do begin
  x=[-reverse(v_vpot(k1:k2)),v_vpot(k1:k2)]                       ;speed
  
  f=[[reverse(reform(fblk(idet,k1:k2,isector)))],$
      [reform(fblk(oppdet(idet),k1:k2,isector))]] ;phase densty corrected counts

  print,'det ',idet,oppdet(idet),'  k  speed  energy  f'
  for k=0,n_elements(x)-1  do print,k,x(k)/1e8,2.85e-16 * x(k)^2,f(k),$
    format='(i3,f10.3,f10.1,e12.4)'
        
  xfit=[[-reverse(reform(vgaussfit(idet,*)))],$
        [reform(vgaussfit(oppdet(idet),*))]]
  ffit=[[reverse(reform(fegaussfit(idet,*)))],$
        [reform(fegaussfit(oppdet(idet),*))]]
              
  yrange=[.1,1000]
   
    
  frange=[1.e-31,1.e-24]
  fticks=7
    
    if idet eq 0 then title='ebias1 '+string(ebias1,format='(i2)')+'     '+$
            'det '+string(idet,format='(i1)')+'         '+$
            'sector '+string(isector,format='(i1)')+'         '+$
            'det '+string(oppdet(idet),format='(i1)')+'      '+$
            'ebias2 '+string(ebias2,format='(i2)') $
    else  title=$
            'det '+string(idet,format='(i1)')+'                              '+$
            'det '+string(oppdet(idet),format='(i1)')     
                        
    plot_io,x,f,/nodata,yrange=frange,yticks=fticks,ystyle=1,$
      xrange=[-max(1.05*vel),max(1.05*vel)],xticks=6,xstyle=1,xminor=6,$
      ytitle='phase density',xtitle=xtitle(idet),$
      title=title,$
      subtitle=stitle(idet),charsize=charsize,$
      position=posn(*,idet),noerase=noerase(idet)
    oplot,[0.,0.],frange,linestyle=1   
    if hardcopy then oplot,x,f,psym=1,symsize=0.75 else $
      oplot,x,f,psym=1,symsize=0.75,color=wst.clr_orange
    
    ;plot patch fit obtained from idl version patch analysis
    oplot,xfit,ffit
    
    xyouts,posn(0,idet)+0.65*(posn(2,idet)-posn(0,idet)),$
    posn(1,idet)+0.9*(posn(3,idet)-posn(1,idet)),$
    /normal,'sc pot'
    
    xyouts,posn(0,idet)+0.65*(posn(2,idet)-posn(0,idet)),$
    posn(1,idet)+0.8*(posn(3,idet)-posn(1,idet)),$
    /normal,string(vpot*1e-8,format='(f3.1)')+'e8 cm/s'
    
    xyouts,posn(0,idet)+0.65*(posn(2,idet)-posn(0,idet)),$
    posn(1,idet)+0.7*(posn(3,idet)-posn(1,idet)),$
    /normal,string(enpot,format='(f4.1)')+'ev'
    
    xyouts,posn(0,idet)+0.65*(posn(2,idet)-posn(0,idet)),$
    posn(1,idet)+0.6*(posn(3,idet)-posn(1,idet)),$
    /normal,'(adjusted)'
    
    fcore,bncoef,dne_cld,te_cld,u_core
    ucore=sqrt(u_core(0)^2+u_core(1)^2+u_core(2)^2)
    thucore=asin(u_core(2)/ucore)/!dtor
    phucore=atan(u_core(1),u_core(0))/!dtor
    if phucore lt 0 then phucore=phucore+360.

    xyy=0.9 & yspce=0.075   
    xyouts,posn(0,idet)+0.8*(posn(2,idet)-posn(0,idet)),$
    posn(1,idet)+xyy*(posn(3,idet)-posn(1,idet)),$
    /normal,string(dne_cld,format='(f4.1)')+' cm^-3'
    
    xyouts,posn(0,idet)+0.8*(posn(2,idet)-posn(0,idet)),$
    posn(1,idet)+(xyy-yspce)*(posn(3,idet)-posn(1,idet)),$
    /normal,string(te_cld,format='(e8.2)')+' degK'

   xyouts,posn(0,idet)+0.8*(posn(2,idet)-posn(0,idet)),$
    posn(1,idet)+(xyy-2*yspce)*(posn(3,idet)-posn(1,idet)),$
    /normal,string(ucore*1e-5,format='(i4)')+' km/s'
    
   xyouts,posn(0,idet)+0.8*(posn(2,idet)-posn(0,idet)),$
    posn(1,idet)+(xyy-3*yspce)*(posn(3,idet)-posn(1,idet)),$
    /normal,string(thucore,format='(i3)')+' deg el'
    
   xyouts,posn(0,idet)+0.8*(posn(2,idet)-posn(0,idet)),$
    posn(1,idet)+(xyy-4*yspce)*(posn(3,idet)-posn(1,idet)),$
    /normal,string(phucore,format='(i3)')+' deg az'
     
   if hardcopy then begin
     oplot,-reverse(v1ct),reverse(f1ct(*)),linestyle=1
     oplot,v1ct,f1ct(*),linestyle=1
   endif else begin    
     oplot,-reverse(v1ct),reverse(f1ct(*)),linestyle=1,$
      color=wst.clr_orange
     oplot,v1ct,f1ct(*),linestyle=1,$
      color=wst.clr_orange 
   endelse   
  
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
