;+ 
;   $Id$ 
;
;   $Log$  
;
; ------------------------------------------------------------------------------
; Physics Math Constants and Variables 
; ------------------------------------------------------------------------------
;
; File   :  co_phyma.cmd
;
; Purpose:  This file defines COMMON blocks used in different IDL programs
;
; Author  : Matthias R. Aellig 
;
; Version history: 
;            
;            03/Feb/1997  M.A.  File creation 
;            01/Feb/1999  M.A.  COMMON block phys_const1 from cel_ael.cmd added
;
;@
;
; ------------------------------------------------------------------------------
; Mathematical constants 
; ------------------------------------------------------------------------------

COMMON geometric, co_pi

   co_pi = 3.14159265d

  
  
  
COMMON  phys_const1, co_charge, co_mnucl, co_kb

     ;---- Physical constants in SI units ( Formeln + Tafeln, 3rd ed. 1984 ) 
          
   co_charge = 1.6021892d-19 
   
     ;------ elementary charge in C 
     
   co_mnucl  =  1.6606d-27 
   
     ;------ nucleon mass (mean of proton and neutron mass) in kg 
     ;       changed to 1 amu, M.A. 13/Aug/1996

   co_kb     =  1.380662d-23 
   
     ;-----  Boltzmann's constant in J/K
   
