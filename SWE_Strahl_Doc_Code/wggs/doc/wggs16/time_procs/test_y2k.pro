opt=1
case opt of
0: begin
     tjdays=[9353l + lindgen(10000l-9353l),lindgen(9353l)]
     for i=0,9000,50 do begin
       print,tjdays(i),tjd_pb5(tjdays(i),0l),tjd_pb5_y2k(tjdays(i),0l),$
       format='(7i6)'
     endfor
   endcase

1: begin
     y=1994l+lindgen(27)
     d=365l
     ms=0l
     for i=0,26 do begin
       print,y(i),d,pb5_tjd([y(i),d,ms]),pb5_tjd_y2k([y(i),d,ms]),$
       format='(6i8)'
     endfor
      
   endcase
endcase      
end