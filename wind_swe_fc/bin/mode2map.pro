;     @(#)mode2map.pro   VERSION 1.1    6/20/96  14:39:57

; derived from code by Dick Fitzenreiter  

pro mode2map,ghk,fcblm2,vblhsp,sblhsp



;---------------------- science data -----------------------------------------

; get offsets (array index) into mjf array of the science data, 
; i.e.,  the 43 fixed columns (no loose byte 19 allocated to SWE)
  n_mnf=250
  n_col=45
  l_mnf=43
  scindx=intarr(n_mnf*l_mnf)
  col_mnf=indgen(n_mnf)*n_col + 2
  for i=0,n_mnf-1 do scindx(i*l_mnf+indgen(l_mnf))=col_mnf(i) + indgen(l_mnf)

;print,'col_mnf ',col_mnf
;print,'scindx ',scindx




;------------------ each block is a veis/strahl half spin -------------------
;veis header vblhsp.hdr(0:3):
;  byte 0   veis spin count (incremented when veis counts collected)
;  byte 1   sun pulse spin count 
;  byte 2   status flags
;  byte 3   compute byte 3 modulo f(hex) 16 to determine which half spin
;           (0 = 1st half, 1 = 2nd half)
;veis data vblhsp.ind(0:383):
;  bytes 4-387 (6 detectors X 16 steps X 4 sectors)

;strahl header sblhsp.hdr(0:3):
;  byte 0   spin count (all spins)
;  byte 1   strl step (points into HV table)
;  byte 2   status flags
;  byte 3   compute byte 3 modulo f(hex) 15 to determine which half spin
;           (0 = 1st half, 1 = 2nd half)
;strl data sblhsp(0:63):
;  bytes 4-67 (4 detectors X 16 steps X 1 sector) 


;----------- housekeeping/veis/strahl/faraday cup --------------------------

n_hspns=17
nhk_hsp=43
n_hkhsp=12
nhdr_hsp=4
nv_hsp=384
ns_hsp=64
nfc_hsp=146

hkblhsp=replicate({hskphsp,descr:'h',ind:intarr(nhk_hsp)},n_hkhsp)

vblhsp=$
  replicate({descr:'v',hdr:intarr(nhdr_hsp),ind:intarr(nv_hsp)},n_hspns)

sblhsp=$
  replicate({descr:'s',hdr:intarr(nhdr_hsp),ind:intarr(ns_hsp)},n_hspns)

;  JTS - the structure name farcupbl is used in mode1map.pro
;  here we changed the name to farcuplbk because the structure here is different. 
;fcblm2=replicate({farcupbl,descr:'f', offs:0, ln:146},n_hspns)
;fcblm2=replicate({farcupbl,descr:'f', ind:intarr(nfc_hsp)},n_hspns)
 fcblm2=replicate({farcupblk,descr:'f', ind:intarr(nfc_hsp)},n_hspns)

;offsets into science data array
  for ihspn=0,n_hspns-1 do begin
    if ihspn lt 12 then begin
      mnf_hsp=ihspn*15+1
      hkblhsp(ihspn).ind=(mnf_hsp-1)*l_mnf + indgen(nhk_hsp)
      ;print,ihspn
      ;print,mnf_hsp,mnf_hsp-1
      ;print,(mnf_hsp-1)*l_mnf
      ;print,indgen(nhk_hsp)
      ;print,hkblhsp(ihspn).ind
    endif else mnf_hsp= 180+(ihspn-12)*14
    vblhsp(ihspn).hdr=mnf_hsp*l_mnf + indgen(nhdr_hsp)
    vblhsp(ihspn).ind=mnf_hsp*l_mnf+ nhdr_hsp + indgen(nv_hsp) 
    sblhsp(ihspn).hdr=mnf_hsp*l_mnf + nhdr_hsp + nv_hsp + indgen(nhdr_hsp)
    sblhsp(ihspn).ind=$
      mnf_hsp*l_mnf + nhdr_hsp + nv_hsp + nhdr_hsp + indgen(ns_hsp)
    fcblm2(ihspn).ind=mnf_hsp*l_mnf + nhdr_hsp + nv_hsp + nhdr_hsp + ns_hsp +$
      indgen(nfc_hsp)
;  mnf_hsp 1 ... 
;  { ((0 to 15)*15+1) * 43 } + 4 + 384 + 4 + 64 + (0 to 145)
;  { (43) }  +4 +384 +4 +64 + (0 to 145) 
;  {  15*43 + 43 
 
  endfor


;offsets into lz data array
  hkblhsp.ind=scindx(hkblhsp.ind)
  vblhsp.hdr=scindx(vblhsp.hdr)
  vblhsp.ind=scindx(vblhsp.ind)
  sblhsp.hdr=scindx(sblhsp.hdr)
  sblhsp.ind=scindx(sblhsp.ind) 
  fcblm2.ind=scindx(fcblm2.ind)


;========================= test ===============================================
;testing science map (to run as a print test of tm map, 
;                     comment the first line and run as a main prog)
  ltest=0
  if ltest eq 1 then begin
    lz=strarr(n_col*n_mnf) + '0'
    lz(hkblhsp.ind)='h'
    lz(vblhsp.hdr)='V'
    lz(vblhsp.ind)='v'
    lz(sblhsp.hdr)='S'
    lz(sblhsp.ind)='s'
    lz(fcblm2.ind)='f'
    
    for i=0,n_mnf-1 do begin
      print,i,lz(i*n_col+indgen(n_col)),format='(z2,1x,45a1)'
    endfor
  endif 
;======================== end test ============================================


;---------------------- General housekeeping ----------------------------------
; general housekeeping byte offsets into sci_dat (structure field assignments)

btvr={bv, bnm:' ', p:0, n:0}
hkind=intarr(nhk_hsp*n_hkhsp)
hkind(*)=hkblhsp.ind

n_hkm2=41
ghk=replicate({descr:' ', offs:0, ln:0, nbv:0,$
                        bv:replicate(btvr,8)},n_hkm2)

	ghk(*).nbv=intarr(n_hkm2)

	ghk(0).descr='         mode_tm'
	ghk(0).offs=0
	ghk(0).ln=1
	ghk(0).nbv=3
  		ghk(0).bv(0).bnm='      scimode_hk'
  		ghk(0).bv(0).p=4
 	 	ghk(0).bv(0).n=5

 	 	ghk(0).bv(1).bnm='      tmmode_hk'
  		ghk(0).bv(1).p=6
  		ghk(0).bv(1).n=2

 	 	ghk(0).bv(2).bnm='      tmrate_hk'
 	 	ghk(0).bv(2).p=7
  		ghk(0).bv(2).n=1


	ghk(1).descr='     utc_lstspn'
	ghk(1).offs=1
	ghk(1).ln=4

	ghk(2).descr='      ms_lstspn'
	ghk(2).offs=5
	ghk(2).ln=2

	ghk(3).descr='spincount_tagged'
	ghk(3).offs=7
	ghk(3).ln=1

	ghk(4).descr='   opcode_lstcmd'
	ghk(4).offs=8
	ghk(4).ln=1

	ghk(5).descr='      lstcmd_rec'
	ghk(5).offs=9
	ghk(5).ln=4

	ghk(6).descr='            peek'
	ghk(6).offs=13
	ghk(6).ln=2

	ghk(7).descr='          unused'
	ghk(7).offs=15
	ghk(7).ln=1

	ghk(8).descr='    dpu_grnd_mon'
	ghk(8).offs=16
	ghk(8).ln=1

	ghk(9).descr='dpu_28v_pbus_mon'
	ghk(9).offs=17
	ghk(9).ln=1

	ghk(10).descr='dpu_28v_bus2_mon'
	ghk(10).offs=18
	ghk(10).ln=1

	ghk(11).descr='dpu_28v_bus1_mon'
	ghk(11).offs=19
	ghk(11).ln=1

	ghk(12).descr='  dpu_neg12v_mon'
	ghk(12).offs=20
	ghk(12).ln=1

	ghk(13).descr='  dpu_pos12v_mon'
	ghk(13).offs=21
	ghk(13).ln=1

	ghk(14).descr='   dpu_pos5v_mon'
	ghk(14).offs=22
	ghk(14).ln=1

	ghk(15).descr='          unused'
	ghk(15).offs=23
	ghk(15).ln=1

	ghk(16).descr='    lst_cmd_qued'
	ghk(16).offs=24
	ghk(16).ln=4

	ghk(17).descr=' next_cmd_in_que'
	ghk(17).offs=28
	ghk(17).ln=4

	ghk(18).descr='     dpuii_1_mon'
	ghk(18).offs=32
	ghk(18).ln=32

	ghk(19).descr='     dpuii_2_mon'
	ghk(19).offs=64
	ghk(19).ln=32

	ghk(20).descr='    task_err_tbl'
	ghk(20).offs=96
	ghk(20).ln=16

        ghk(21).descr='          unused'
	ghk(21).offs=112
	ghk(21).ln=8

        ghk(22).descr='  int_stack_pntr'
	ghk(22).offs=120
	ghk(22).ln=4

	ghk(23).descr='         rom_dmp'
	ghk(23).offs=124
	ghk(23).ln=1

	ghk(24).descr='         veis_st'
	ghk(24).offs=125
	ghk(24).ln=1

        ghk(25).descr='           fc_st'
	ghk(25).offs=126
	ghk(25).ln=1

        ghk(26).descr='          str_st'
	ghk(26).offs=127
	ghk(26).ln=1

	ghk(27).descr='     veis_hv_tbl'
	ghk(27).offs=128
	ghk(27).ln=64

	ghk(28).descr='   strahl_hv_tbl'
	ghk(28).offs=192
	ghk(28).ln=16

        ghk(29).descr='       fcmod_tbl'
	ghk(29).offs=208
	ghk(29).ln=8

        ghk(30).descr='       fccal_tbl'
	ghk(30).offs=216
	ghk(30).ln=4

        ghk(31).descr='     veis_tbl_id'
	ghk(31).offs=220
	ghk(31).ln=1

        ghk(32).descr='      str_tbl_id'
	ghk(32).offs=221
	ghk(32).ln=1
      
        ghk(33).descr='          man_hv'
	ghk(33).offs=222
	ghk(33).ln=1

	ghk(34).descr='          unused'
	ghk(34).offs=223
	ghk(34).ln=1

        ghk(35).descr='  current_limits'
	ghk(35).offs=224
	ghk(35).ln=6

       	ghk(36).descr='          unused'
	ghk(36).offs=230
	ghk(36).ln=10

        ghk(37).descr='burst_info_modeb'
	ghk(37).offs=240
	ghk(37).ln=16

	ghk(38).descr='    mem_addr_dmp'
	ghk(38).offs=256
	ghk(38).ln=3

	ghk(39).descr='       dump_walk'
	ghk(39).offs=259
	ghk(39).ln=1

        ghk(40).descr='     memory_dump'
	ghk(40).offs=260
	ghk(40).ln=256



end


