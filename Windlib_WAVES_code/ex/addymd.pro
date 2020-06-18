pro addymd, iymd, iday

; IDL version of MLK's ADDYMD Fortran subroutine
; created and tested by R. MacDowall, 2/15/93
; To avoid all internal problems, convert IYMD and IDAY to LONG before using.
;   R. MacDowall, 2/18/94

MONTH = REFORM([0,31,60,91,121,152,182,213,244,274,305, $
335,366,0,31,59,90,120,151,181,212,243,273,304,334,365],13,2)

IYMD = LONG(IYMD)

IY=IYMD/10000
IK=IY*10000
IM=(IYMD-IK)/100
ID=IYMD-IK-IM*100
LY = ( 1 < (IY MOD 4)) ;(=0 or 1); in MLK's version this is MIN0(MOD(IY,4),1)+1
ID=(IY-1)*36525/100+MONTH(IM-1,LY)+ID+LONG(IDAY)
IY=(ID*100-1)/36525+1
ID=ID-36525*(IY-1)/100
LY = ( 1 < (IY MOD 4))
FOR I = 1, 12 DO BEGIN
  IM=I
  IF(ID LE MONTH(I,LY)) THEN GOTO, DONE
ENDFOR
DONE:
IYMD=IY*10000+IM*100+ID-MONTH(IM-1,LY)
RETURN
END

