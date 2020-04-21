; @(#)mode1map.pro  VERSION 1.2    7/28/94   16:13:26
pro mode1map,hkm1,fcblm1,vsm1,vdatc,sdatc


btvr={bv, bnm:' ', p:0, n:0}


;---------------------- General housekeeping ----------------------------------
; general housekeeping byte offset (sci_dat) structure field assignments


n_hkm1=32
hkm1=replicate({genlhk, descr:' ', offs:0, ln:0, nbv:0,$
                        bv:replicate(btvr,8)},n_hkm1)

	hkm1(*).ln=intarr(n_hkm1)+1
	hkm1(*).nbv=intarr(n_hkm1)

	hkm1(0).descr='         mode_tm'
	hkm1(0).offs=0
	hkm1(0).ln=1
	hkm1(0).nbv=3
  		hkm1(0).bv(0).bnm='      scimode_hk'
  		hkm1(0).bv(0).p=4
 	 	hkm1(0).bv(0).n=5

 	 	hkm1(0).bv(1).bnm='      tmmode_hk'
  		hkm1(0).bv(1).p=6
  		hkm1(0).bv(1).n=2

 	 	hkm1(0).bv(2).bnm='      tmrate_hk'
 	 	hkm1(0).bv(2).p=7
  		hkm1(0).bv(2).n=1


	hkm1(1).descr='     utc_lstspn'
	hkm1(1).offs=1
	hkm1(1).ln=4

	hkm1(2).descr='      ms_lstspn'
	hkm1(2).offs=5
	hkm1(2).ln=2

	hkm1(3).descr='spincount_tagged'
	hkm1(3).offs=7
	hkm1(3).ln=1


	hkm1(4).descr='   opcode_lstcmd'
	hkm1(4).offs=8
	hkm1(4).ln=1

	hkm1(5).descr='      lstcmd_rec'
	hkm1(5).offs=9
	hkm1(5).ln=4

	hkm1(6).descr='            peek'
	hkm1(6).offs=13
	hkm1(6).ln=2

	hkm1(7).descr='          unused'
	hkm1(7).offs=15
	hkm1(7).ln=1

	hkm1(8).descr='    dpu_grnd_mon'
	hkm1(8).offs=16
	hkm1(8).ln=1

	hkm1(9).descr='dpu_28v_pbus_mon'
	hkm1(9).offs=17
	hkm1(9).ln=1

	hkm1(10).descr='dpu_28v_bus2_mon'
	hkm1(10).offs=18
	hkm1(10).ln=1

	hkm1(11).descr='dpu_28v_bus1_mon'
	hkm1(11).offs=19
	hkm1(11).ln=1

	hkm1(12).descr='  dpu_neg12v_mon'
	hkm1(12).offs=20
	hkm1(12).ln=1

	hkm1(13).descr='  dpu_pos12v_mon'
	hkm1(13).offs=21
	hkm1(13).ln=1

	hkm1(14).descr='   dpu_pos5v_mon'
	hkm1(14).offs=22
	hkm1(14).ln=1

	hkm1(15).descr='          unused'
	hkm1(15).offs=23
	hkm1(15).ln=1

	hkm1(16).descr='    lst_cmd_qued'
	hkm1(16).offs=24
	hkm1(16).ln=4

	hkm1(17).descr=' next_cmd_in_que'
	hkm1(17).offs=28
	hkm1(17).ln=4

	hkm1(18).descr='     dpuii_1_mon'
	hkm1(18).offs=32
	hkm1(18).ln=32

	hkm1(19).descr='     dpuii_2_mon'
	hkm1(19).offs=64
	hkm1(19).ln=32

	hkm1(20).descr='    task_err_tbl'
	hkm1(20).offs=96
	hkm1(20).ln=16

	hkm1(21).descr='  int_stack_pntr'
	hkm1(21).offs=120
	hkm1(21).ln=4

	hkm1(22).descr='         rom_dmp'
	hkm1(22).offs=124
	hkm1(22).ln=1

	hkm1(23).descr='          unused'
	hkm1(23).offs=125
	hkm1(23).ln=3

	hkm1(24).descr='     veis_hv_tbl'
	hkm1(24).offs=10134
	hkm1(24).ln=32

	hkm1(25).descr='   strahl_hv_tbl'
	hkm1(25).offs=10166
	hkm1(25).ln=32

	hkm1(26).descr='tsk_sch_addr_tbl'
	hkm1(26).offs=10198
	hkm1(26).ln=49

	hkm1(27).descr='          unused'
	hkm1(27).offs=10247
	hkm1(27).ln=15

	hkm1(28).descr='    mem_addr_dmp'
	hkm1(28).offs=10384
	hkm1(28).ln=3

	hkm1(29).descr='   mem_dmp_1_128'
	hkm1(29).offs=10387
	hkm1(29).ln=128

	hkm1(30).descr=' mem_dmp_129_256'
	hkm1(30).offs=10637
	hkm1(30).ln=128

	hkm1(31).descr='       dpuii_reg'
	hkm1(31).offs=10765
	hkm1(31).ln=32

	hkm1(31).descr='          unused'
	hkm1(31).offs=10797
	hkm1(31).ln=3



;------------------------- Faraday Cup --------------------------------------
; Faraday cup data block byte offset (sci_dat) structure field assignments

n_fcblm1=31
fcblm1=replicate({farcupbl, descr:' ', offs:0, ln:0},n_fcblm1)

fcblm1(*).descr='fc block offsets'
fcblm1(*).ln=122

fcblm1(0).offs=128	
fcblm1(1).offs=480
fcblm1(2).offs=830
fcblm1(3).offs=1180

for i=4,27 do fcblm1(i).offs=fcblm1(i-4).offs+1412

fcblm1(28).offs=10012
fcblm1(29).offs=10262
fcblm1(30).offs=10515

;----------------------- VEIS/STRAHL ---------------------------------------
; offsets into array sci_dat for the first veis/strahl block, vs
n_vsblks=4
n_spins=7
lvs=intarr(n_vsblks,n_spins)
lnvs=intarr(n_vsblks,n_spins)
lvs(*,0)=[250,602,952,1302]
lnvs(*,0)=[230,228,228,238]

; offsets into array sci_dat for remaining vs blocks
for j=1,n_spins-1 do begin
  lvs(*,j)=lvs(*,j-1)+1412
  lnvs(*,j)=lnvs(*,j-1)
endfor

; offsets into array sci_dat of veis/strahl data for each spin
n_vs=924
indvs=intarr(n_vs,n_spins)
for j=0,n_spins-1 do begin
  k=-1
  for i=0,n_vsblks-1 do for l=0,lnvs(i,j)-1 do begin
    k=k+1
    indvs(k,j)=lvs(i,j)+l 
  endfor
endfor

; offsets into ind_vs(0:n_vs-1,j) of veis and strahl data fragments each spin 
lnv=[114,intarr(13)+6,210,intarr(13)+6,96]
lv=intarr(29)
lns=intarr(28)+12
ls=intarr(28)
lv(0)=2
ls(0)=116
for i=1,27 do begin
  lv(i)=lv(i-1)+lnv(i-1)+lns(i-1)
  ls(i)=ls(i-1)+lns(i-1)+lnv(i)
endfor
lv(28)=lv(27)+lnv(27)+lns(27)

; offsets into array sci_dat of veis data for each spin
n_vdat=576
indveis=intarr(n_vdat,n_spins)
for j=0,n_spins-1 do begin
  k=-1
  for i=0,28 do for l=0,lnv(i)-1 do begin
  k=k+1
  indveis(k,j)=indvs(lv(i)+l,j)
  endfor
endfor

; offsets into array sci_dat of strahl data for each spin
n_sdat=336
indstrl=intarr(n_sdat,n_spins)
for j=0,n_spins-1 do begin
  k=-1
  for i=0,27 do for l=0,lns(i)-1 do begin
  k=k+1
  indstrl(k,j)=indvs(ls(i)+l,j)
  endfor
endfor

; veis and strahl log-compressed data indices in sci_dat array
vdatc=replicate({veisbl,ind:intarr(n_vdat)},n_spins)
sdatc=replicate({strlbl,ind:intarr(n_sdat)},n_spins)
for j=0,n_spins-1 do vdatc(j).ind(*)= indveis(*,j)
for j=0,n_spins-1 do sdatc(j).ind(*)= indstrl(*,j) 

; veis/strahl data block variables offset (sci_dat) structure field assignments
n_vsm1=9
vsm1=replicate({veis_strl,descr:replicate(' ',n_spins), offs:intarr(n_spins),$
   ln:intarr(n_spins), scimod:intarr(n_spins)},n_vsm1)
for j=0,n_spins-1 do begin
  vsm1(*).scimod(j)=indvs(0,j)+1
   
  vsm1(0).descr(j)='      spin count'
  vsm1(0).offs(j)=indvs(0,j) 
  vsm1(0).ln(j)= 1
	
  vsm1(1).descr(j)='     str hv step'
  vsm1(1).offs(j)=indvs(0,j) + 1
  vsm1(1).ln(j)=1

  vsm1(2).descr(j)=' logcomp vs data'
  vsm1(2).offs(j)=indvs(0,j) + 2
  vsm1(2).ln(j)=912

  vsm1(3).descr(j)='  pha channel hv'
  vsm1(3).offs(j)=indvs(0,j) + 914
  vsm1(3).ln(j)=1

  vsm1(4).descr(j)='  pha threshhold'
  vsm1(4).offs(j)=indvs(0,j) + 915
  vsm1(4).ln(j)=1

  vsm1(5).descr(j)='       pha acc 1'
  vsm1(5).offs(j)=indvs(0,j) + 916
  vsm1(5).ln(j)=2

  vsm1(6).descr(j)='       pha acc 2'
  vsm1(6).offs(j)=indvs(0,j) + 918
  vsm1(6).ln(j)=2

  vsm1(7).descr(j)='      pha diff 1'
  vsm1(7).offs(j)=indvs(0,j) + 920
  vsm1(7).ln(j)=2

  vsm1(8).descr(j)='      pha diff 2'
  vsm1(8).offs(j)=indvs(0,j) + 922
  vsm1(8).ln(j)=2
endfor


end
