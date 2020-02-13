pro strl_steps

;plots strahl counts data

common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common log_delog,comp_tbl,dcomp_tbl
common detset,detseta,detsetb,detset
common swestuff,swest
common wstuff,wst

hardcopy=wst.hardcopy & nspins=swest.nspins & ispin=swest.ispinbl
nstrldets=swest.nstrldets & nstrlsteps=swest.nstrlsteps
istrldet=swest.istrldet & istrlstep=swest.istrlstep

wst.hardcopy=0

if keyword_set(strldet0) eq 0 then strldet0=0

yn=['no','yes']
clr=[125,125]

start:

if keyword_set(hardcopy) eq 0 then hardcopy=0
if hardcopy then begin 
  pflnm=getenv('IDLSAV')+'idl.ps'
  print,' ' & print,'making hardcopy..... ' 
  set_plot,'ps'
  device,/inches,xoffset=1.0,yoffset=1.0,xsize=7.,ysize=8.,filename=pflnm
endif


!p.multi=[0,2,3,0,0]

x=indgen(nstrlsteps)
xrange=[min(x)-1,max(x)+1]
xticks=7
xtickv=[0,3,7,11,15,19,23,27]
xtickn=strarr(xticks+1,nstrldets)
xtickn(*,nstrldets-2)=string(xtickv,format='(i2)')
xtickn(*,nstrldets-1)=string(xtickv,format='(i2)')

;yrange=[0,4096]
yrange=[0,1.1*max(dcomp_tbl( vsmjf.strl(*,*,ispin) ))]

timpb5=vsmjf.pb5tim_vsbl(*,ispin)
sec=double(timpb5(2)/1000)
hour_hms,sec/3600.d,hms

;get strahl hv step
ivstep=vsmjf.strlstep(ispin)

xtitle=strarr(nstrldets)
xtitle(nstrldets-4)=yrmoda(timpb5)+'  '+string(timpb5(1),format='(i3)') +' '+hms
xtitle(nstrldets-3)=yrmoda(timpb5)+'  '+string(timpb5(1),format='(i3)') +' '+hms
xtitle(nstrldets-2)='look phi, mag phi (gse)'
xtitle(nstrldets-1)='look phi, mag phi (gse)'
ytitle=replicate('counts',nstrldets)

;for jstrldet=strldet0,nstrldets-1,2 do begin 
for i=0,n_elements(detset)-1 do begin
  jstrldet=detset(i)
  y=dcomp_tbl( vsmjf.strl(jstrldet,*,ispin) ) 
  yticks=2
  ytickformat='(i4)'
  plot,x,y,/nodata,$
    title='det '+string(jstrldet,format='(i2)')+$
          '  levl '+string(ivstep,format='(i2)')+$
          string(volt_en_strl(ivstep,/en),$
          format='(i5)')+'ev'+$
          '  spn '+string(ispin,format='(i1)'),$          
    xrange=xrange,xticks=xticks,xstyle=1,xtitle=xtitle(jstrldet),$
    xtickv=xtickv,xtickname=xtickn(*,jstrldet),charsize=2.25,$    
    yrange=yrange,yticks=yticks,ystyle=1,ytickformat=ytickformat,$
    ytitle=ytitle(jstrldet);,subtitle=stitle(jstrldet)
  oplot,x,y,psym=4,symsize=0.50,color=clr(hardcopy)
endfor


if hardcopy then begin   
  device,/close 
  set_plot,'x'
  print,' ' & print,'printing hardcopy file ',pflnm
  spawn, 'lp '+pflnm
  ;spawn,'cp '+pflnm+' last_'+pflnm
  !p.color=125
  hcopy=0
  goto,start
endif

!p.multi=0

end
