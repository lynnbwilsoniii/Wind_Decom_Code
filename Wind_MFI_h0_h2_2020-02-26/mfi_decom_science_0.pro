PRO MFI_DECOM_SCIENCE_0, LZ_REC=LZ_REC, CNTS_PR=CNTS_PR, CNTS_SC=CNTS_SC

;- define arrays
vector = {x:0d, y:0d, z:0d, q:0b}
cnts_pr = replicate(vector, 2, 250)
cnts_sc = replicate(vector, 2, 250)

;- decompose major frame, primary and secondary magnetometers
cnts_pr[0,*].x =  lz_rec.mafr[ 3, *]*16L + (lz_rec.mafr[ 4, *] and 240b)/16L
cnts_pr[0,*].y = (lz_rec.mafr[ 4, *] and 15b)*256L + lz_rec.mafr[5, *]
cnts_pr[0,*].z =  lz_rec.mafr[ 6, *]*16L + (lz_rec.mafr[ 7, *] and 240b)/16L
cnts_sc[0,*].x = (lz_rec.mafr[ 7, *] and 15b)*256L + lz_rec.mafr[8, *]
cnts_sc[0,*].y =  lz_rec.mafr[ 9, *]*16L + (lz_rec.mafr[10, *] and 240b)/16L
cnts_sc[0,*].z = (lz_rec.mafr[10, *] and 15b)*256L + lz_rec.mafr[11, *]
cnts_pr[1,*].x =  lz_rec.mafr[12, *]*16L + (lz_rec.mafr[13, *] and 240b)/16L
cnts_pr[1,*].y = (lz_rec.mafr[13, *] and 15b)*256L + lz_rec.mafr[14, *]
cnts_pr[1,*].z =  lz_rec.mafr[15, *]*16L + (lz_rec.mafr[16, *] and 240b)/16L
cnts_sc[1,*].x = (lz_rec.mafr[16, *] and 15b)*256L + lz_rec.mafr[17, *]
cnts_sc[1,*].y =  lz_rec.mafr[18, *]*16L + (lz_rec.mafr[19, *] and 240b)/16L
cnts_sc[1,*].z = (lz_rec.mafr[19, *] and 15b)*256L + lz_rec.mafr[20, *]
;- set flag equal to each minor frame flag; for first and last vectors in minor frame also check for double 0 bytes
cnts_pr[0,*].q = reform(lz_rec.q_mifr, 1, 250) or 4b*((lz_rec.mafr[ 2, *] eq   0) and (lz_rec.mafr[ 3, *] eq   0)) $
                                               or 4b*((lz_rec.mafr[ 2, *] eq 255) and (lz_rec.mafr[ 3, *] eq 255))
cnts_sc[0,*].q = reform(lz_rec.q_mifr, 1, 250)
cnts_pr[1,*].q = reform(lz_rec.q_mifr, 1, 250)
cnts_sc[1,*].q = reform(lz_rec.q_mifr, 1, 250) or 4b*((lz_rec.mafr[20, *] eq   0) and (lz_rec.mafr[21, *] eq   0)) $
                                               or 4b*((lz_rec.mafr[20, *] eq 255) and (lz_rec.mafr[21, *] eq 255))
;- reform into one dimensional arrays
cnts_pr = reform(cnts_pr, 500)
cnts_sc = reform(cnts_sc, 500)

END