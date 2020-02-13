pro get_kpmag,timpb5,b,phi,theta

;common magkpstuff,magkpflnm,magkpdat
common wstuff,wst
common shared,d

magkpdat=d.mfi_magkpdat

;convert input time to sec from truncated julian day
t=pb5_sec(timpb5)

opt=1
case opt of
0: begin   ;find magkp data closest in time to input time
     mn=min(abs(magkpdat.ta - t),index)
     print,'input time ',timpb5
     print,'nearest magkp time ',magkpdat(index).tpb5
     print,'difference (seconds) ',t-magkpdat(index).ta
     b=magkpdat(index).bf
     phi=magkpdat(index).ph
     theta=magkpdat(index).th
   endcase

1: begin    ;find linear interpolated value at input time
     wlt=where(magkpdat.ta le t,nwlt)
     wgt=where(magkpdat.ta gt t,nwgt)
     if wlt(0) ne -1 and wgt(0) ne -1 then begin
       print,'input time ',timpb5
       print,'nearest magkp time le ',magkpdat(wlt(nwlt-1)).tpb5
       print,'difference (seconds) ',t-magkpdat(wlt(nwlt-1)).ta
       print,'nearest magkp time gt ',magkpdat(wgt(0)).tpb5
       print,'difference (seconds) ',magkpdat(wgt(0)).ta-t
       
       bfx=$
         interpol([magkpdat(wlt(nwlt-1)).bf(0),magkpdat(wgt(0)).bf(0)],$
         [magkpdat(wlt(nwlt-1)).ta,magkpdat(wgt(0)).ta],t)
       bfy=$
         interpol([magkpdat(wlt(nwlt-1)).bf(1),magkpdat(wgt(0)).bf(1)],$
         [magkpdat(wlt(nwlt-1)).ta,magkpdat(wgt(0)).ta],t)
       bfz=$
         interpol([magkpdat(wlt(nwlt-1)).bf(2),magkpdat(wgt(0)).bf(2)],$
         [magkpdat(wlt(nwlt-1)).ta,magkpdat(wgt(0)).ta],t)
       phi=atan(bfy(0),bfx(0))/!dtor      
       if phi lt 0 then phi=phi+360.
       theta=asin(bfz(0)/sqrt(bfx(0)^2+bfy(0)^2+bfz(0)^2))/!dtor
       b=[bfx,bfy,bfz]
     endif
   endcase
endcase

end