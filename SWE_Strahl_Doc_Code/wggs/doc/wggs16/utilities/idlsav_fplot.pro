;----------------- pre_fcntrs ------------------------------------------------
pro pre_fcntrs

common share,w,d

fdstr=d.fdstr
ispin=d.ispin

;combine patched and measured data and put in SOLAR WIND frame
ue=fdstr(ispin).ue         ;solar wind velocity

sz=size(fdstr(ispin).fe)
ndets=sz(1)
nvsteps=sz(2)
nsects=sz(3)
npatch=2      ;number of steps below lowest measured step to be patched
npatchover=0  ;number of lowest measured steps to be patched over

wx=dblarr(ndets,npatch+nvsteps,nsects)
wy=dblarr(ndets,npatch+nvsteps,nsects)
wz=dblarr(ndets,npatch+nvsteps,nsects)
f=dblarr(ndets,npatch+nvsteps,nsects)

wx(*,0:npatch,*)=fdstr(ispin).ve_patch(*,0:npatch,*)



stop
end


;----------------- p_fdistrib ------------------------------------------------

pro p_fdistrib

common share,w,d

if w.is_restored eq 0 then return

widget_control, w.draw1, GET_VALUE=windw
wset,windw

pos,3,posn,ysep=0.1,xoff=0.14,xtop=0.9
noerase=[0,1,1]
charsize=1.15  ;2.25
yrange=[.1,1000]
frange=[1.e-31,1.e-24]
fticks=7
oppdet=[5,4,3,2,1,0]
  
fdstr=d.fdstr
ispin=d.ispin
isector=d.isector
stitle=[' ',' ',$
  fdstr(ispin).datetime+'    '+$
  string(fdstr(ispin).timpb5,format='(i4,i4,i9)')]
vel=fdstr(ispin).vem
 
for idet=0,2 do begin
  ;plot measured f's
    vdet=reform(fdstr(ispin).vem(idet,*,isector))
    voppdet=reform((fdstr(ispin).vem(oppdet(idet),*,isector)))
    v=[-reverse(vdet),voppdet]                

    fdet=reform(fdstr(ispin).fe(idet,*,isector))
    foppdet=reform(fdstr(ispin).fe(oppdet(idet),*,isector))       
    f=[reverse(fdet),foppdet] 
   
    plot_io,v,f,/nodata,yrange=frange,yticks=fticks,ystyle=1,$
      xrange=[-max(1.05*vel),max(1.05*vel)],xticks=6,xstyle=1,xminor=6,$
      ytitle='phase density',xtitle='speed',$
      title= $
            'det '+string(idet,format='(i1)')+'         '+$
            'sector '+string(isector,format='(i1)')+'         '+$
            'det '+string(oppdet(idet),format='(i1)'),$
      position=posn(*,idet),noerase=noerase(idet),$
      subtitle=stitle(idet),charsize=charsize
    oplot,v,f,psym=1,symsize=0.75

    
  ;plot patch fit obtained from moments analysis          
    vfitdet=reform(fdstr(ispin).vem_patch(idet,*,isector))
    vfitoppdet=reform(fdstr(ispin).vem_patch(oppdet(idet),*,isector))
    vfit=[-reverse(vfitdet),vfitoppdet]
   
    ffitdet=reform(fdstr(ispin).fe_patch(idet,*,isector))
    ffitoppdet=reform(fdstr(ispin).fe_patch(oppdet(idet),*,isector)) 
    ffit=[reverse(ffitdet),ffitoppdet]                 
    oplot,vfit,ffit
    
  ;plot 1-count level
    f1ctdet=reform(fdstr(ispin).fe_1ct(idet,*))
    f1ctoppdet=reform(fdstr(ispin).fe_1ct(oppdet(idet),*))
    f1ct=[reverse(f1ctdet),f1ctoppdet]
    oplot,v,f1ct,linestyle=1
       
  oplot,[0.,0.],frange,linestyle=1
endfor

end

;---------------------- r_fdistrib -------------------------------------------
pro r_fdistrib

common share,w,d

w.is_restored=0

start:
;set the data path
  savpath_orig=getenv('IDLSAV')
  setenv,'IDLSAV='+'/data0/ftp/pub/exports/vinas/'

;the idlsav data filename
  savpath=getenv('IDLSAV')
  filename=pickfile(/read,get_path=savpath,path=savpath,$
               filter='*.fdistrib',$
               title='IDLsav fdistrb files')
  
;initialize the data structure
  fdstr=0  

;restore the idlsave file  
  restore,filename
  help,fdstr
  help,fdstr,/str
  d={fdstr:fdstr,ispin:0,isector:0}
  w.is_restored=1
 
widget_control,w.spinfld,set_value=d.ispin
widget_control,w.sectfld,set_value=d.isector
p_fdistrib
pre_fcntrs

setenv,'IDLSAV='+savpath_orig
end




;--------------- pos -------------------------------------------------------

pro pos,npnl,posn,yoff=yoff,ytop=ytop,xoff=xoff,xtop=xtop,ysep=ysep,$
  ysize=ysize,col=col,ypnl_relsiz=ypnl_relsiz,xsep=xsep

posn=fltarr(4,npnl)
if keyword_set(xoff) eq 0 then xoff=0.175     ;xoff=0.15
if keyword_set(xtop) eq 0 then xtop=0.80  ;0.84      ;xtop=0.875
if keyword_set(yoff) eq 0 then yoff=0.125       ;yoff=0.1
if keyword_set(ytop) eq 0 then ytop=1.0       ;ytop=1.0
if keyword_set(ysep) eq 0 then ysep=0.005;2
if keyword_set(col) eq 0 then col=0
if keyword_set(yoff1) eq 0 then yoff1=0.20
if keyword_set(xsep) eq 0 then xsep=0.05

if col ge 1 then col=1
if npnl-2*fix(npnl/2) ne 0 and col eq 1 then return

if npnl eq 1 then begin
  ;yoff=0.2 & ytop=0.7
  posn(1,0)=yoff
  posn(3,0)=ytop
  ;posn(3,0)=0.6*(ytop-1.5*yoff-ysep)
endif

yspace=ytop-1.5*yoff-( (npnl-1)/(col+1))*ysep
if keyword_set(ysize) eq 0 then begin
  if keyword_set(ypnl_relsiz) eq 0 then  $
    ypnl=replicate(yspace/(npnl/(col+1)),npnl) else $
    ypnl=ypnl_relsiz*yspace
endif else begin
  if keyword_set(ypnl_relsiz) eq 0 then  $
    ypnl=(ysize/total(ysize))*yspace  else $
    ypnl=ypnl_relsiz*yspace
endelse  

yl=fltarr(npnl)
yu=fltarr(npnl)

xl=fltarr(npnl)
xu=fltarr(npnl)

case col of
0: begin
     for i=0,npnl-1 do begin
       if i eq 0 then yl(i)=yoff else yl(i)=yl(i-1)+ypnl(i-1)+ysep
     endfor
     xl=replicate(xoff,npnl)
     xpnl=xtop-xoff
   endcase

1: begin
     xpnl=(xtop-xoff)/2-xsep
     for i=0,npnl-1,2 do begin
       if i le 1 then yl(i)=yoff else yl(i)=yl(i-2)+ypnl(i-2)+ysep
       yl(i+1)=yl(i)
       xl(i+1)=xoff
       xl(i)=xoff+xpnl+xsep
     endfor
   endcase
endcase

yu=yl+ypnl
xu=xl+xpnl

posn(1,*)=reverse(yl)
posn(3,*)=reverse(yu)
posn(0,*)=reverse(xl)
posn(2,*)=reverse(xu)


end



;----------------- idlsav_fplot_event -----------------------------------------
pro idlsav_fplot_event,event
common share,w,d


case event.id of

w.sect: begin
  if w.is_restored eq 0 then return
  case event.value of
    ' + ': if d.isector eq 5 then d.isector=0 else d.isector=d.isector+1
    ' - ': if d.isector eq 0 then d.isector=5 else d.isector=d.isector-1
  endcase
  widget_control,w.sectfld,set_value=d.isector
  p_fdistrib
  pre_fcntrs
endcase 

w.spin: begin
  if w.is_restored eq 0 then return
  case event.value of
    ' + ': d.ispin=(d.ispin+1) < (n_elements(d.fdstr)-1)
    ' - ': d.ispin=d.ispin-1 > 0
  endcase
  widget_control,w.spinfld,set_value=d.ispin
  p_fdistrib
  pre_fcntrs
endcase 

else: begin
  case event.value of
    'Restore SAVEfile': r_fdistrib
    'Quit': widget_control,event.top,/destroy
    'Hardcopy':
  endcase
endcase
endcase

end


;--------------------- MAIN: idlsav_fplot -----------------------------------
pro idlsav_fplot

common share,w,d

;reads and plots variables from an idlsave file of distribution functions

;define widget structure
w={mainbase:0l,buttns:0l,spin:0l,spinfld:0l,sect:0l,sectfld:0l,$
  draw1:0l,draw2:0l,is_restored:0}

widget_control,default_font='6x13'
  
w.mainbase = WIDGET_BASE(TITLE = 'SWE VEIS Counts Data  ', /COLUMN) 

rbase=widget_base(w.mainbase,/row)
w.buttns=cw_bgroup(rbase,['Restore SAVEfile','Quit','Hardcopy'],row=1,$
  /return_name)

w.spin=cw_bgroup(rbase,$
  label_left='Increment w.spin',[' + ',' - '],row=1,/return_name)
w.spinfld=cw_field(rbase,title=' ',/long,xsize=4)

w.sect=cw_bgroup(rbase,$
  label_left='Increment w.sector',[' + ',' - '],row=1,/return_name)
w.sectfld=cw_field(rbase,title=' ',/long,xsize=1)
   
x_size = 900        
y_size = 700
rbase=widget_base(w.mainbase,/row)
cbase=widget_base(rbase,/row)
w.draw1 = WIDGET_DRAW(rbase,/FRAME, RETAIN = 2,XSIZE = x_size/2, YSIZE = y_size)
cbase=widget_base(rbase,/row)
w.draw2 = WIDGET_DRAW(rbase,/FRAME, RETAIN = 2,XSIZE = x_size/2, YSIZE = y_size)

WIDGET_CONTROL, w.mainbase, /REALIZE

XMANAGER, "idlsav_fplot", w.mainbase, GROUP_LEADER = GROUP  ;hand off to manager



end