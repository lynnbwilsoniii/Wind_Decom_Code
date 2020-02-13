pro xdata_versions_ev,event

common wstuff,wst

case strmid(event.value,0,13) of
  'swe_moments: ' : begin
      wst.mfilter=strmid(event.value,13,strlen(event.value)-13)
      print,'swe_moments version: ',wst.mfilter
                    endcase
                     
  'swe_pitch:   ' : begin
      wst.pfilter=strmid(event.value,13,strlen(event.value)-13)
      print,'swe_pitch version: ',wst.pfilter
                    endcase
  else:
endcase

end