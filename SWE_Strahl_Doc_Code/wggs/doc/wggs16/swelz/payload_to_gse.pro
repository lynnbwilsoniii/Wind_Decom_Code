pro payload_to_gse, payload_pos, gse_att_pos, rotation_matrix, gse_pos

;PURPOSE:  PERFORM COORDINATE TRANSFORMATION FROM NON-SPINNING PAYLOAD 
;            COORDINATES TO GSE COORDINATES 

;  ARGUMENT LIST:

;  NAME	                 TYPE     USE	  DESCRIPTION
;  ----                  ----     ---     -----------
;  PAYLOAD_POS(3)        R*8      I       PAYLOAD COORDINATES TO BE TRANSFORMED
;  GSE_ATT_POS(2)        R*4      I       GSE ATTITUDE ANGLES (RA, DECL IN RAD)
;                                         AT THE EPOCH TIME OF THE SPACECRAFT
;                                         COORDINATES
;  ROTATION_MATRIX(9)    R*8      O       ROTATION MATRIX
;  GSE_POS(3)            R*8      O       TRANSFORMED GSE COORDINATES

;requires MULMAT    - ROUTINE THAT MULTIPLIES TWO MATRICES

;procedure :
;  BUILD the transformation matrix from GSE right ascension and GSE declination
;   angles
;  CALL MULMAT to multiply the transformation matrix by payload coordinates to
;   get GSE coordinates
;  RETURN



transform_matrix=dblarr(3,3)
rotation_matrix=dblarr(9)

rac=gse_att_pos(0)
dec=gse_att_pos(1)

epx=cos(dec) * cos(rac)
epy=cos(dec) * sin(rac)
epz=sin(dec)

cosa=sqrt(epy*epy + epz*epz)
sina=epx
cosd=epz/cosa
sind=epy/cosa

transform_matrix(0,0)=cosa
transform_matrix(0,1)=0.
transform_matrix(0,2)=sina
transform_matrix(1,0)=-sina * sind
transform_matrix(1,1)=cosd
transform_matrix(1,2)=cosa * sind
transform_matrix(2,0)=-sina * cosd
transform_matrix(2,1)=-sind
transform_matrix(2,2)=cosa * cosd

rotation_matrix(0)=transform_matrix(0,0)
rotation_matrix(1)=transform_matrix(0,1)
rotation_matrix(2)=transform_matrix(0,2)
rotation_matrix(3)=transform_matrix(1,0)
rotation_matrix(4)=transform_matrix(1,1)
rotation_matrix(5)=transform_matrix(1,2)
rotation_matrix(6)=transform_matrix(2,0)
rotation_matrix(7)=transform_matrix(2,1)
rotation_matrix(8)=transform_matrix(2,2)

mulmat, transform_matrix, payload_pos, 3, 3, 1, gse_pos

end
