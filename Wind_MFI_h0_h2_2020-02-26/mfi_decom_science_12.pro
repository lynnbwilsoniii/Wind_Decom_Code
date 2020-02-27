PRO MFI_DECOM_SCIENCE_12, LZ_REC=LZ_REC, CNTS_PR=CNTS_PR, CNTS_SC=CNTS_SC

;- define arrays
vector = {x:0d, y:0d, z:0d, q:0b}
cnts_pr = replicate(vector, 4, 250)
cnts_sc = replicate(vector, 2,  25)

;- decompose major frame
;- primary magnetometer
cnts_pr[0,*].x =  lz_rec.mafr[ 3, *]*16L + (lz_rec.mafr[ 4, *] and 240b)/16L
cnts_pr[0,*].y = (lz_rec.mafr[ 4, *] and 15b)*256L + lz_rec.mafr[5, *]
cnts_pr[0,*].z =  lz_rec.mafr[ 6, *]*16L + (lz_rec.mafr[ 7, *] and 240b)/16L
cnts_pr[1,*].x = (lz_rec.mafr[ 7, *] and 15b)*256L + lz_rec.mafr[8, *]
cnts_pr[1,*].y =  lz_rec.mafr[ 9, *]*16L + (lz_rec.mafr[10, *] and 240b)/16L
cnts_pr[1,*].z = (lz_rec.mafr[10, *] and 15b)*256L + lz_rec.mafr[11, *]
cnts_pr[2,*].x =  lz_rec.mafr[12, *]*16L + (lz_rec.mafr[13, *] and 240b)/16L
cnts_pr[2,*].y = (lz_rec.mafr[13, *] and 15b)*256L + lz_rec.mafr[14, *]
cnts_pr[2,*].z =  lz_rec.mafr[15, *]*16L + (lz_rec.mafr[16, *] and 240b)/16L
cnts_pr[3,*].x = (lz_rec.mafr[16, *] and 15b)*256L + lz_rec.mafr[17, *]
cnts_pr[3,*].y =  lz_rec.mafr[18, *]*16L + (lz_rec.mafr[19, *] and 240b)/16L
cnts_pr[3,*].z = (lz_rec.mafr[19, *] and 15b)*256L + lz_rec.mafr[20, *]
;- set flag equal to each minor frame flag; for first and last vectors in minor frame also check for double 0 bytes
cnts_pr[0,*].q = reform(lz_rec.q_mifr, 1, 250) or 4b*((lz_rec.mafr[ 2, *] eq   0) and (lz_rec.mafr[ 3, *] eq   0)) $
                                               or 4b*((lz_rec.mafr[ 2, *] eq 255) and (lz_rec.mafr[ 3, *] eq 255))
cnts_pr[1,*].q = reform(lz_rec.q_mifr, 1, 250)
cnts_pr[2,*].q = reform(lz_rec.q_mifr, 1, 250)
cnts_pr[3,*].q = reform(lz_rec.q_mifr, 1, 250) or 4b*((lz_rec.mafr[20, *] eq   0) and (lz_rec.mafr[21, *] eq   0)) $
                                               or 4b*((lz_rec.mafr[20, *] eq 255) and (lz_rec.mafr[21, *] eq 255))

;- secondary magnetometer
;- all secondary mag data in modes 1 and 2 are bad (not equaly spaced in time)
cnts_sc.q = 1
;- original code for secondary magnetometer
;sub = indgen(25)*10
;cnts_sc[0,*].x =  lz_rec.mafr[21, sub]*16L + (lz_rec.mafr[22, sub] and 240b)/16L
;cnts_sc[0,*].y = (lz_rec.mafr[22, sub] and 15b)*256L + lz_rec.mafr[23, sub]
;cnts_sc[0,*].z =  lz_rec.mafr[24, sub]*16L + (lz_rec.mafr[21, sub+1] and 240b)/16L
;cnts_sc[1,*].x = (lz_rec.mafr[21, sub+1] and 15b)*256L + lz_rec.mafr[22, sub+1]
;cnts_sc[1,*].y =  lz_rec.mafr[23, sub+1]*16L + (lz_rec.mafr[24, sub+1] and 240b)/16L
;cnts_sc[1,*].z = (lz_rec.mafr[24, sub+1] and 15b)*256L + lz_rec.mafr[2, sub+2]
;cnts_sc[0,*].q = reform(lz_rec.q_mifr[sub]   + lz_rec.q_mifr[sub+1], 1, 25)
;cnts_sc[1,*].q = reform(lz_rec.q_mifr[sub+1] + lz_rec.q_mifr[sub+2], 1, 25)

;- reform into one dimensional arrays
cnts_pr = reform(cnts_pr, 1000)
cnts_sc = reform(cnts_sc,   50)

END