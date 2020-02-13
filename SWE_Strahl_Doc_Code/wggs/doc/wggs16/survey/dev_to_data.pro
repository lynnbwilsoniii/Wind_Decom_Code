function dev_to_data,ev_x,ev_y

common wstuff,wst

ev_pnl=where(ev_y ge wst.xysize(1,*) and ev_y le wst.xysize(3,*),nw)

if nw eq 1 then begin
  
  ydata=wst.xyrange(1,ev_pnl)+(float(ev_y)-wst.xysize(1,ev_pnl))*$
   (wst.xyrange(3,ev_pnl)-wst.xyrange(1,ev_pnl)) /$
   (wst.xysize(3,ev_pnl)-wst.xysize(1,ev_pnl))
  if wst.ylog(ev_pnl(0)) eq 1 and wst.minmax eq 0 then ydata=10.^ydata

  xdata=wst.xyrange(0,ev_pnl)+(float(ev_x)-wst.xysize(0,ev_pnl))*$
   (wst.xyrange(2,ev_pnl)-wst.xyrange(0,ev_pnl)) /$
   (wst.xysize(2,ev_pnl)-wst.xysize(0,ev_pnl))

  xdata= wst.xyrange(0,ev_pnl) > xdata < wst.xyrange(2,ev_pnl)

   ;print,wst.xyrange(0,ev_pnl),wst.xyrange(2,ev_pnl)
   print,'xdata,ydata,ev_pnl ',xdata,ydata,ev_pnl

  return,[xdata,ydata]

endif else begin
   print,'Selected y-coordinate is not within a plot panel'
   return,[0,0]
endelse

end
