

; EXISTING MODES 0, 1, 2, 3, 4, 11    as of 6/98
; Add MODE 6  11/3/98  JTS
;  CORRECT ERROR IN LOWEST_ML PARAMETER 4/14/99 JTS

pro fcff, fcffblk, mode, $
    year = year, $
    day = day, $
    sec = sec, $
    w_type = w_type, $
    windows = windows, $
    tracking = tracking, $
    lowest_ML = lowest_ML, $
    track_cup = track_cup, $
    FSRT = FSRT, $
    threshold = threshold, $ 
    max_ML = max_ML, $ 
    scan = scan

; Parameters passed to the routine:
;    fcffblk .... 122 byte block 
;    mode

; Parameters returned from the routine:
;    w_type
;    windows
;    tracking
;    scan


;****Test to see if we have a valid mode***
if(  (mode ne 0) and $
     (mode ne 1) and $
     (mode ne 2) and $
     (mode ne 3) and $
     (mode ne 4) and $
     (mode ne 6) and $
     (mode ne 11)) then message, $
  '    WARNING, invalid mode at FCFF. MODE = ' + string(mode, format = '(I4)'), /inform


; GET UTC HERE
utc_convert, fcffblk(2:7), year, day, sec 


; INITIALIZATIONS  ****************************

w_type = 2    ;  set this as default
tracking = 0  ;  assume not tracking as default
scan = 1      ;  assume full scan as default
windows = 31  ;  set this to the max number
lowest_ML = 3 ;  for mode 0, 1, 2, 11, the default is to begin with
              ;  the next to lowest double window (ML = 3, MH = 5)  
track_cup = 0 ;  0 = cup1, 1 = cup2
FSRT = 1800      ; Full Scan Repeat Time in seconds. prefer 15minx60sec = 900 sec. 
threshold = 1700 ; Threshold count necessary to keep tracking.  
max_ML = 51      ; Operational - 57 is next to last highest Possible. 
;**********************************************


if( (mode eq 3)or(mode eq 4)or(mode eq 6) ) then begin
     w_type = fcffblk(114) ; 115th byte (1=single step, 2=double step)
     tracking = fcffblk(98); 99th byte  (0=not tracking, 1=tracking)
     scan = fcffblk(115)   ; 116th byte (0=limited scan, 1 =full scan)
     ;lowest_ML = (fcffblk(118)+1) ; Replace this line with line below JTS 4/14/99
     lowest_ML = (fcffblk(118))
     track_cup = (fcffblk(91)) ;  
     FSRT = fcffblk(102)+256*fcffblk(103)+2^16*fcffblk(104)+2^24*fcffblk(105)
     threshold =  (fcffblk(110)+256*fcffblk(111)) ;
     max_ML =     (fcffblk(117))
endif


;****w_type should be 1 or 2.  otherwise trap error ****
if( (w_type ne 2) and (w_type ne 1) ) then w_type = 2

end


