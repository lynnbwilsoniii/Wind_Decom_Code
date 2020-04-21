;-----------------------------------------------------------------------------
; file  :   wind_ael.cmd 
;
; author:   Matthias R. Aellig 
;
; copyright:   
;
; purpose:  Command file with COMMON blocks for WIND data analysis 
;
; comment:  
; 
; history:  
;
;  02/19/99  M.A.  File creation

;
; RCS Info 
; 
; $Id$ 
;
; $Log$ 
;
; SCCS Info
; 
;     %Z%%M%   VERSION %I%    %G%  %U%
;
;----------------------------------------------------------------------------

COMMON cup_orientation, co_cup1_thet_deg, co_cup2_thet_deg

         co_cup1_thet_deg    =   -15.0d 
         co_cup2_thet_deg    =    15.0d 


COMMON cup_effarea, co_wovrs, co_area_cm2

         co_wovrs            =    0.022d
         co_area_cm2         =    50.26584d





       
