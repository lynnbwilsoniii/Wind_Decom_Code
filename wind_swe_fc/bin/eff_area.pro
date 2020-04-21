;-----------------------------------------------------------------------------
; file  :   eff_aera.pro 
;
; author:   Matthias R. Aellig 
;
; copyright:   
;
; purpose:  Calculates effective aera of FC 
;
; comment:  
; 
; history:  
;
;  02/19/99  M.A.    File creation
;  03/23/99  M.A.    windsim results that give steeper 
;                    shoulder are incorporated
;  09/01/99  M.A.    The effective area is calculated taking 
;                    its value at the nearest supporting point. 
;                    The interpolated value is returned when 
;                    using the INTERPLO keyword. 
; 
;
; RCS Info 
; 
; $Id$ 
;
; $Log$ 
;
; SCCS Info
; 
;     @(#)eff_area.pro   VERSION 1.2    02/08/00  15:09:14
;
;----------------------------------------------------------------------------

FUNCTION eff_area, vel_vec, vb_norm, GRID_ONLY=GRID_ONLY, INTERPOL=INTERPOL 


;---------------------------------------------------------------------------
; Declare COMMON blocks
;----------------------------------------------------------------------------

   COMMON cup_effarea, co_wovrs, co_area_cm2 

   COMMON geometric, co_pi


;-----------------------------------------------------------------------------
; Create Variable with angualar response 
;-----------------------------------------------------------------------------

   eff_area_windsim_1 = [ $ 
       3382000.0, 3383000.0, 3383000.0, 3382000.0, 3381000.0, $ 
       3380000.0, 3378000.0, 3377000.0, 3376000.0, 3374000.0, $ 
       3372000.0, 3369000.0, 3368000.0, 3364000.0, 3362000.0, $ 
       3359000.0, 3355000.0, 3351000.0, 3347000.0, 3343000.0, $
       3338700.0, 3334100.0, 3329300.0, 3324300.0, 3318200.0, $
       3312800.0, 3306300.0, 3299600.0, 3292800.0, 3285900.0, $
       3277800.0, 3270700.0, 3261600.0, 3253500.0, 3244500.0, $
       3234600.0, 3224900.0, 3200100.0, 3161500.0, 3114000.0, $ 
       3058820.0, 2997170.0, 2930000.0, 2857000.0, 2779000.0     ] 

   eff_area_windsim_2 = [ $ 
       2694000.0, 2586999.7, 2465000.0, 2329999.6, 2183000.0, $
       2025999.6, 1859000.1, 1682999.6, 1497000.1, 1301999.6, $ 
       1099000.1, 887799.56, 668500.16, 452099.62, 257500.16, $ 
       96799.784, 539.96863, 0.0000000, 0.0000000, 0.0000000, $ 
       0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, $ 
       0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, $
       0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, $ 
       0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, $
       0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, $ 
       0.0000000  ]
                  

   eff_area_windsim      = [  eff_area_windsim_1, eff_area_windsim_2 ]

   angle_area_deg        = dindgen(91) 

;-----------------------------------------------------------------------------
; Do calculations 
;-----------------------------------------------------------------------------

; 02/02/2009 - JCK.  Change from norm to SQRT(TOTAL()) because of
;              double  precision problem
;   costheta  = vb_norm / norm(vel_vec) 
    costheta = (vb_norm/SQRT(TOTAL(vel_vec^2)) < 1.0d)

   costheta  = costheta > replicate(co_wovrs, n_elements(costheta)) 

        ;--- Takes costheta or co_wovrs where the latter is bigger


   IF keyword_set(GRID_ONLY) THEN BEGIN 

      eff_area  = 1d5 * co_area_cm2 *    $ 
               ( (1.d - co_wovrs) * ( 1.d - co_wovrs/costheta) )^9.d

   ENDIF

   IF keyword_set(INTERPOL) THEN BEGIN 

      angle_interpol_deg = ACOS(costheta) * 180./ co_pi

      eff_area = CALL_FUNCTION('interpol',  eff_area_windsim, $ 
                                            angle_area_deg,   $ 
                                            angle_interpol_deg  ) 

   ENDIF ELSE BEGIN 

      angle_round_deg = round( ACOS(costheta) * 180./ co_pi ) 

      eff_area =  eff_area_windsim( angle_round_deg ) 

             ; As eff_aera_windsim is calculated with a resolution 
             ; of one degree this way eff_aera is taken at the 
             ; closest degree value. 
           

   ENDELSE 


   return, eff_area

END 










