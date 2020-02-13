pro mstruct_timeaxis
common shared,d
common wstuff,wst
common swestuff,swest

;pnlsel = set of indices of selected set of variables
  wpnl=where(d.pnlsel ne -1)
  if wpnl(0) eq -1 then return
  pnlsel=d.pnlsel(wpnl)
  
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




end