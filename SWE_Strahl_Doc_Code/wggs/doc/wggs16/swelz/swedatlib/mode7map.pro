; Reads telemetry map files and determines the offsets into each (mode7) LZ
;  data record.                                      Last modified: (07/24/02).

;  ============================== Ind_SciDat ==================================
; This routine get offsets (array indices) into mjf array of the science data, 
; i.e., the loose byte 19 allocated to SWE and the 43 fixed columns.
PRO ind_scidat,index ;                    Note: 'index' is an OUTPUT parameter.

n_mnf = 250 & l_mnf = 45 ;              Initialization for index calculation...
col_mnf = (indgen(n_mnf)*l_mnf)+2 & len_mnf = intarr(n_mnf)+l_mnf-2
w = where(((indgen(n_mnf)-(fix(indgen(n_mnf)/10)*10)) eq 2) or $
          ((indgen(n_mnf)-(fix(indgen(n_mnf)/10)*10)) eq 9))
col_mnf[w] = (w*l_mnf)+1 & len_mnf[w] = l_mnf-1

index = col_mnf[0]+indgen(len_mnf[0]) ;               Calculate index values...
for i=1,(n_mnf-1) do index = [index,col_mnf[i]+indgen(len_mnf[i])]

end


;  ============================== Mode7map ====================================
PRO mode7map ;         Common blocks and files provide both input and output...
 
common m7stuff,hkm7,sdatc7 ;  Common block containing shared mode7 information.

ind_scidat,scindx ;            Get offsets of science data into lz data record.

;---------------------- General housekeeping ----------------------------------
; General housekeeping byte offsets into sci_dat (structure field assignments).

btvr = {bv, bnm:' ', p:0, n:0} & n_hkm7 = 10 ;     # mode7 housekeeping fields.
hkm7 = replicate({genlhk, descr:' ', offs:0, ln:1, nbv:0,$
                             bv:replicate(btvr,8), loc:intarr(32)},n_hkm7)

hkm7[0].descr = '         mode_tm'
hkm7[0].offs = 0 & hkm7[0].ln = 1 & hkm7[0].nbv = 3
	hkm7[0].bv[0].bnm = '      scimode_hk'
	hkm7[0].bv[0].p = 4 & hkm7[0].bv[0].n = 5
	hkm7[0].bv[1].bnm = '      tmmode_hk'
	hkm7[0].bv[1].p = 6 & hkm7[0].bv[1].n = 2
	hkm7[0].bv[2].bnm = '      tmrate_hk'
	hkm7[0].bv[2].p = 7 & hkm7[0].bv[2].n = 1

hkm7[1].descr = '     utc_lstspn' & hkm7[1].offs = 1 & hkm7[1].ln = 4

hkm7[2].descr = '      ms_lstspn' & hkm7[2].offs = 5 & hkm7[2].ln = 2

hkm7[3].descr = 'spincount_tagged' & hkm7[3].offs = 7 & hkm7[3].ln = 1

hkm7[4].descr = ' mode7 indicator' & hkm7[4].offs = 15 & hkm7[4].ln = 1

hkm7[5].descr = '       fc_status' & hkm7[5].offs = 126 & hkm7[5].ln = 1
        
hkm7[6].descr = 'num_strahl_steps' & hkm7[6].offs = 127 & hkm7[6].ln = 1
	
hkm7[7].descr = ' pri_veis_hv_tbl' & hkm7[7].offs = 10134 & hkm7[7].ln = 17
hkm7[7].loc = 10134+indgen(17)
        
hkm7[8].descr = ' alt_veis_hv_tbl' & hkm7[8].offs = 10151 & hkm7[8].ln = 17
hkm7[8].loc = 10151+indgen(17)

hkm7[9].descr = '   strahl_hv_tbl' & hkm7[9].offs = 10168 & hkm7[9].ln = 32
hkm7[9].loc = 10168+indgen(32)

;                                  Get hk byte offsets into level zero array...
hkm7.offs = scindx[hkm7.offs] & hkm7.loc = scindx[hkm7.loc]

;----------------------------- Faraday Cup -----------------------------------
;   Faraday cup data block byte offset (sci_dat) structure field assignments...

n_fcblm6 = 31 & fcblm6 = replicate({farcupbl,descr:' ',offs:0,ln:0},n_fcblm6)

fcblm6[*].descr = 'fc block offsets' & fcblm6[*].ln = 122

fcblm6[0].offs = 128 & fcblm6[1].offs = 480 & fcblm6[2].offs = 830
fcblm6[3].offs = 1180

for i=4,27 do fcblm6[i].offs = fcblm6[i-4].offs+1412

fcblm6[28].offs = 10012 & fcblm6[29].offs = 10262 & fcblm6[30].offs = 10515

;                                  Get FC byte offsets into level zero array...
fcblm6.offs = scindx[fcblm6.offs]

;-------------------------------- STRAHL -------------------------------------
;                  Reading mode7 strahl map : offsets directly into lz array...

n_spectra = 7 & n_sdat = 720 ;     # of spectra and # of counts per spectrum...
sdatc7 = replicate({strlbl7,ind:intarr(n_sdat),spect_spncnt_offs:0l},n_spectra)

dir = getenv('SWEDATLIB') & lun = 1 & openr,lun,dir+'swemode7.prt'
null = '' & readf,lun,null ;         Open and begin reading telemetry map file.

for ispect=0,(n_spectra-1) do begin ;    Read header info. for each spectrum...
   spect = 0 & spect_spncnt_offs = 0 & readf,lun,spect,spect_spncnt_offs
   sdatc7[spect].spect_spncnt_offs = spect_spncnt_offs

   for ivstep=0,14 do begin ;   Read offsets for each velocity-step (energy)...
      vstep = 0 & vstepvar = 'step' & readf,lun,vstep,vstepvar

      for isect=0,7 do begin ;     Read all detector offsets for each sector...
         sect = 0 & offs = lonarr(18) & readf,lun,sect,offs
         sdatc7[spect].ind[(vstep*48)+(sect*6)+indgen(6)] = offs[3*indgen(6)]
      endfor
   endfor
endfor

free_lun,lun ;                             Release current logical-unit-number.

end
