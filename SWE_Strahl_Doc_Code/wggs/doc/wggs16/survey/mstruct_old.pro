pro mstruct,error=error

common shared,d
common wstuff,wst
common swestuff,swest

print, 'mstruct.pro'


;assign values to structure tags for time and ordinate axes parameters
 
;initialize some parameters for all plot panels
  d.pnl.ocolor=wst.clr_green  ;orange
  d.pnl.olinestyle=2
  d.pnl.plotio=0
  d.pnl.psym=0
  d.pnl.symsize=1.
  d.pnl.charthick=1.0
  d.pnl.charsize=1.15

;pnlsel = set of indices of selected set of variables
  wpnl=where(d.pnlsel ne -1)
  if wpnl(0) eq -1 then return
  pnlsel=d.pnlsel(wpnl)

  wchg=where(d.pnlsel-d.pnlsel_last ne 0)
  if wchg(0) ne -1 then swest.lzsurvey=0
  d.pnlsel_last=d.pnlsel 


;organize the pnlist indices selected by data type
    for i=0,d.ndvar-1 do begin
      d.wdatype(*,i)=-1
      w=where(d.pnlist.dtp(pnlsel) eq d.datype(i),nw)
      if nw gt 0 then d.wdatype(0:nw-1,i)=w
    endfor


;find range for time axis 
  refpb5=sec_pb5(d.refsec)
  hour_hms,refpb5(2)/3600000.d,hms1
            
  tmn=1.e30 & tmx=-1.e30               

  swe_electrons=0
  swe_moments=0
  for ipnl=0,n_elements(pnlsel)-1 do begin  
    idatyp=where(d.datype eq d.pnl(pnlsel(ipnl)).dtp)    
    call_procedure,d.pnl(pnlsel(ipnl)).dtp+'_timerange',tmn,tmx,idatyp
  
    if d.pnl(pnlsel(ipnl)).dtp eq 'isee_moments' then begin
      refpb5=isee_sec_pb5(d.refsec)
      d.pnl(pnlsel(ipnl)).tmlabl=$
        yrmoda(refpb5)+' '+string(refpb5(1),format='(i3)')
    endif 
  
    if d.pnl(pnlsel(ipnl)).dtp eq 'swe_moments' then swe_moments=1
  
    if d.pnl(pnlsel(ipnl)).dtp eq 'swe_moments' or $
       d.pnl(pnlsel(ipnl)).dtp eq 'swe_fpitch' or $
       d.pnl(pnlsel(ipnl)).dtp eq 'swe_fparaperp' then swe_electrons=1 
           
  endfor
  
  range_mins=(tmx-tmn)/60
  if range_mins le 0 then begin
    print,'mstruct: range_mins le 0 ',range_mins
    return
  endif
  
;set time scale for all panels
       
   timescale,range_mins,(tmn-d.refsec)/3600.d,(tmx-d.refsec)/3600.d,$
     xrange,xtickv,xtickname,xticks,minor_plot,refpb5,labelpb5
   
   if d.pnl(pnlsel(0)).dtp ne 'isee_moments' then $
     d.pnl(pnlsel).tmlabl=yrmoda(labelpb5)+' '+string(labelpb5(1),format='(i3)')
   d.pnl(pnlsel).subtitle=''
   d.pnl(pnlsel).tmrange=xrange 
     
   d.pnl(pnlsel).tmtickv=xtickv
   d.pnl(pnlsel).tmtickname=xtickname
   d.pnl(pnlsel).tmticks=xticks
   d.pnl(pnlsel).tminor=minor_plot  ;mnrtcks



;set other plot axes parameters 
  d.pnl(pnlsel).subtitle=''
  d.pnl(pnlsel).ypnlp=1./float(n_elements(pnlsel))
  
  for i=0,n_elements(pnlsel)-1 do begin
    k=pnlsel(i)
    d.pnl(k).ztitle='SWE electrons'
    
    idatyp=where(d.datype eq d.pnl(k).dtp)
    call_procedure,d.pnlist.dtp(k)+'_struct',d.pnlist.list(k),k,idatyp,$
      swe_electrons,swe_moments  
  endfor  


end

