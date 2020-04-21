;-----------------------------------------------------------------------------
; file  :     v_sc_2_gse.pro 
;
;
; author:     Matthias R. Aellig 
;
;
; copyright:  Matthias R Aellig, MIT/CSR, 2000 
;
;
; purpose:    Transforms velocities from S/C coordinates 
;             to GSE coordinates assuming the S/C were at rest
;             in the GSE frame of reference. 
;
;             NOTE: Velocity component have to be given in km/s
; 
;             0th component: x-coord in S/C 
;             1st component: y-coord in S/C 
;             2nd component: z-coord in S/C 
; 
; history:  
;
;  02/02/2000    M.A.     File creation 
; 
;
; SCCS Info
; 
;     @(#)v_sc_2_gse.pro   VERSION 1.2    02/02/00  12:29:25
;
; 
;----------------------------------------------------------------------------


FUNCTION v_sc_2_gse, v_sc_kms



;---------------------------------------------------------------------------
; Set constant values 
;----------------------------------------------------------------------------

  v_y_aberr_kms  =  - 29.9 

      ;--- revolution speed of Earth around Sun


;---------------------------------------------------------------------------
; Check Input 
;----------------------------------------------------------------------------

  IF n_elements( v_sc_kms(*, 0)  ) NE 3 THEN BEGIN 

     print, 'Warning: @(#)v_sc_2_gse.pro   VERSION 1.2    02/02/00  12:29:25: Wrong input dimensions. Routine terminates'
     return, -1L 

  ENDIF 

;---------------------------------------------------------------------------
; Create and reset return variable 
;----------------------------------------------------------------------------

  v_gse_kms     =   v_sc_kms

  v_gse_kms(*)  = 0.0d 


;-----------------------------------------------------------------------------
; Make transformation 
;-----------------------------------------------------------------------------

  v_gse_kms(0, *)    =  reform(   v_sc_kms(0, *)                  )
  v_gse_kms(1, *)    =  reform( - v_sc_kms(1, *) + v_y_aberr_kms  )
  v_gse_kms(2, *)    =  reform( - v_sc_kms(2, *)                  )


;-----------------------------------------------------------------------------
; That's it 
;-----------------------------------------------------------------------------

  return, v_gse_kms


END 


