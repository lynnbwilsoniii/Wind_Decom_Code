;-----------------------------------------------------------------------------
; file  :   vbulk_norm.pro 
;
; author:   Matthias R. Aellig 
;
; copyright:   
;
; purpose:  Calculates bulk normal speed
;
; comment:  
; 
; history:  
;
;  02/19/99  M.A.  File creation
;  04/05/99  M.A.  Works also for theta_rad being a vector

;
; RCS Info 
; 
; $Id$ 
;
; $Log$ 
;
; SCCS Info
; 
;     @(#)vbulk_norm.pro   VERSION 1.2    04/05/99  12:54:01
;
;----------------------------------------------------------------------------

FUNCTION vbulk_norm, vel_vec, alpha_rad, theta_rad


;---------------------------------------------------------------------------
; Declare COMMON blocks
;----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; Check keywords 
;-----------------------------------------------------------------------------


;-----------------------------------------------------------------------------
; Do calculations 
;-----------------------------------------------------------------------------

vbulk_norm = dblarr( n_elements(alpha_rad) ) 

vbulk_norm = -  COS(theta_rad) * SIN(alpha_rad) * vel_vec(1)  $ 
             -  COS(theta_rad) * COS(alpha_rad) * vel_vec(0)  $ 
             -  SIN(theta_rad) * vel_vec(2) 

return, vbulk_norm

END 
