; @(#)read_lzrcd.pro  VERSION 1.3    11/15/94   14:26:58
FUNCTION read_lzrcd,ifnum,recn,recl,satid

;Jim Byrnes Dec 1993

ON_ERROR,1

ii = -1

case 1 of

     (satid eq 24): recqy = 512l 
     (satid eq 25): recqy = 252l 
     (satid eq 26): recqy = 252l 
     else: begin
         print,' Invalid spacecraft id number'
         return,ii
     end

endcase

recmf = recl - 48l - recqy

lz = {lzrcdr, intsn:0l, recn:0l, mfc:0l, spcclk:bytarr(8), yr:0l, dy:0l, $
      ms:0l, us:0l, nfill:0l, nsync:0l, telmod:0l, qlty:bytarr(recqy), $
      mf:bytarr(recmf)} 

point_lun, ifnum, recn*recl
if not (eof(ifnum)) then readu,ifnum,lz
;lz = conv_vax_unix(lz) ----- STEVENS 11/14: This conversions appears
;                             to be performed automatically in later
;                             versions of IDL. Verified that data read
;                             in w/o this procedure matches data post
;                             this procedure on the MIT LZ pipeline
return,lz
end


