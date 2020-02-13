pro xstrahlen_ev,event

common wstuff,wst

widget_control,event.id,get_uvalue=val


case val(0) of
  'strahlen0' : wst.strlen0=wst.strlenergies(event.index)
  'strahlen1' : wst.strlen1=wst.strlenergies(event.index)
endcase

  

  
end