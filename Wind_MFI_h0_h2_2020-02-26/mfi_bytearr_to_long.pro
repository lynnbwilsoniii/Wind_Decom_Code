FUNCTION MFI_BYTEARR_TO_LONG, BYTEARR

;- convert one- (two-) dimensional bytearray to long number (one-dimensional long array).
;  in both cases conversion goes through first dimension

arr_info= size(bytearr)
case arr_info[0] of
   1: begin
         result = 0L
         for i=0, arr_info[1]-1 do result=(result + long(bytearr[i])*256L^i)
      end
   2: begin
         result = lonarr(1, arr_info[2])
         for i=0, arr_info[1]-1 do result=(result + long(bytearr[i,*])*256L^i)
      end
endcase

return, result
END