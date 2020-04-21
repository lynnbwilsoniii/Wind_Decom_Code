;-----------------------------------------------------------------------------
; file  :      po2cart.pro           ( Transformation Matrix ) 
; author:      Matthias R. Aellig
;
; copyright:   (c) Matthias R. Aellig, University of Bern, 1997
;
; 
; purpose:     
;              
; comment:     
;
;    phi_deg   vector with phi angles 
;
;  theta_deg   vector with theta angles 
;
; history:    
;              30/Oct/1997  M.A. File creation 
;              23/Dec/1997  M.A. KW RAD (radian) introduced 
;
; $Id:$ 
;
; $Log:$ 
;
;
;     @(#)pol2cart.pro   VERSION 1.1    02/08/00  13:14:36
;
; 
;----------------------------------------------------------------------------


FUNCTION  pol2cart, phi_deg, theta_deg, RAD=RAD 
 
  ; --------------------------------------------------------------------------
  ; 
  ; --------------------------------------------------------------------------


  
  IF (n_elements( phi_deg ) NE n_elements(theta_deg) ) THEN BEGIN 
    
    print, 'pol2cart: Input vectors have not the same size. Program terminates'
    return, -1L
    
  ENDIF 

  r_vec      = dblarr( 3, n_elements( phi_deg) ) 

  IF keyword_set( RAD ) THEN BEGIN 

    phi_rad    = phi_deg                               
    theta_rad  = theta_deg
  
  ENDIF ELSE BEGIN 

    phi_rad    = double( phi_deg   * 3.1415972d / 180.d  )                               
    theta_rad  = double( theta_deg * 3.1415972d / 180.d  )

  ENDELSE
 

  r_vec(0,*) = COS( phi_rad ) * COS( theta_rad ) 
  r_vec(1,*) = SIN( phi_rad ) * COS( theta_rad ) 
  r_vec(2,*) =                  SIN( theta_rad ) 
                          
  return, r_vec

  
END 
  
  
  
