pro ind_scidat,index

; get offsets (array index) into mjf array of the science data, 
; i.e., the loose byte 19 allocated to SWE + the 43 fixed columns

n_mnf=250
l_mnf=45
col_mnf=indgen(n_mnf)*l_mnf + 2
len_mnf=intarr(n_mnf)+l_mnf-2
w=where(indgen(n_mnf)-fix(indgen(n_mnf)/10)*10 eq 2 or $
        indgen(n_mnf)-fix(indgen(n_mnf)/10)*10 eq 9)
col_mnf(w)=w*l_mnf + 1
len_mnf(w)=l_mnf-1
;for i=0,n_mnf-1 do print,i,col_mnf(i)

index=col_mnf(0)+indgen(len_mnf(0))
for i=1,n_mnf-1 do index=[index,col_mnf(i)+indgen(len_mnf(i))]

lpr_indx=0
if lpr_indx then begin
  print,' '
  print,'offsets of scidat, i.e., loose byte 19 + 43 fixed columns'
  help,index
  print,index,format='(10i6)'
endif

end




pro mode1map,hkm1,fcblm1,vsm1,vdatc,sdatc

;get offsets of science data into lz data record
ind_scidat,scindx


btvr={bv, bnm:' ', p:0, n:0}


;---------------------- General housekeeping ----------------------------------
; general housekeeping byte offsets into sci_dat (structure field assignments)


n_hkm1=6   ;32
hkm1=replicate({genlhk, descr:' ', offs:0, ln:1, nbv:0,$
                        bv:replicate(btvr,8), loc:intarr(32)},n_hkm1)

	;hkm1(*).ln=intarr(n_hkm1)+1
	;hkm1(*).nbv=intarr(n_hkm1)

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


        
	hkm1(4).descr='     veis_hv_tbl'
	hkm1(4).offs=10134
	hkm1(4).ln=32
        hkm1(4).loc=10134+indgen(32)

	hkm1(5).descr='   strahl_hv_tbl'
	hkm1(5).offs=10166
	hkm1(5).ln=32
        hkm1(5).loc=10166+indgen(32)




	;hkm1(4).descr='   opcode_lstcmd'
	;hkm1(4).offs=8
	;hkm1(4).ln=1

	;hkm1(5).descr='      lstcmd_rec'
	;hkm1(5).offs=9
	;hkm1(5).ln=4

	;hkm1(6).descr='            peek'
	;hkm1(6).offs=13
	;hkm1(6).ln=2

	;hkm1(7).descr='          unused'
	;hkm1(7).offs=15
	;hkm1(7).ln=1

	;hkm1(8).descr='    dpu_grnd_mon'
	;hkm1(8).offs=16
	;hkm1(8).ln=1

	;hkm1(9).descr='dpu_28v_pbus_mon'
	;hkm1(9).offs=17
	;hkm1(9).ln=1

	;hkm1(10).descr='dpu_28v_bus2_mon'
	;hkm1(10).offs=18
	;hkm1(10).ln=1

	;hkm1(11).descr='dpu_28v_bus1_mon'
	;hkm1(11).offs=19
	;hkm1(11).ln=1

	;hkm1(12).descr='  dpu_neg12v_mon'
	;hkm1(12).offs=20
	;hkm1(12).ln=1

	;hkm1(13).descr='  dpu_pos12v_mon'
	;hkm1(13).offs=21
	;hkm1(13).ln=1

	;hkm1(14).descr='   dpu_pos5v_mon'
	;hkm1(14).offs=22
	;hkm1(14).ln=1

	;hkm1(15).descr='          unused'
	;hkm1(15).offs=23
	;hkm1(15).ln=1

	;hkm1(16).descr='    lst_cmd_qued'
	;hkm1(16).offs=24
	;hkm1(16).ln=4

	;hkm1(17).descr=' next_cmd_in_que'
	;hkm1(17).offs=28
	;hkm1(17).ln=4

	;hkm1(18).descr='     dpuii_1_mon'
	;hkm1(18).offs=32
	;hkm1(18).ln=32

	;hkm1(19).descr='     dpuii_2_mon'
	;hkm1(19).offs=64
	;hkm1(19).ln=32

	;hkm1(20).descr='    task_err_tbl'
	;hkm1(20).offs=96
	;hkm1(20).ln=16

	;hkm1(21).descr='  int_stack_pntr'
	;hkm1(21).offs=120
	;hkm1(21).ln=4

	;hkm1(22).descr='         rom_dmp'
	;hkm1(22).offs=124
	;hkm1(22).ln=1

	;hkm1(23).descr='          unused'
	;hkm1(23).offs=125
	;hkm1(23).ln=3

	;hkm1(24).descr='     veis_hv_tbl'
	;hkm1(24).offs=10134
	;hkm1(24).ln=32

	;hkm1(25).descr='   strahl_hv_tbl'
	;hkm1(25).offs=10166
	;hkm1(25).ln=32

	;hkm1(26).descr='tsk_sch_addr_tbl'
	;hkm1(26).offs=10198
	;hkm1(26).ln=49

	;hkm1(27).descr='          unused'
	;hkm1(27).offs=10247
	;hkm1(27).ln=15

	;hkm1(28).descr='    mem_addr_dmp'
	;hkm1(28).offs=10384
	;hkm1(28).ln=3

	;hkm1(29).descr='   mem_dmp_1_128'
	;hkm1(29).offs=10387
	;hkm1(29).ln=128

	;hkm1(30).descr=' mem_dmp_129_256'
	;hkm1(30).offs=10637
	;hkm1(30).ln=128

	;hkm1(31).descr='       dpuii_reg'
	;hkm1(31).offs=10765
	;hkm1(31).ln=32

	;hkm1(31).descr='          unused'
	;hkm1(31).offs=10797
	;hkm1(31).ln=3

;get hk byte offsets into level zero array 
hkm1.offs=scindx(hkm1.offs)
hkm1.loc=scindx(hkm1.loc)

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

;get fc byte offsets into level zero array 
fcblm1.offs=scindx(fcblm1.offs)


;----------------------- VEIS/STRAHL ---------------------------------------

n_spins=7

;vsm1 : offsets of (first two bytes) of beginning of veis/strahl data block  
n_vsm1=2
vsm1=replicate({veis_strl,descr:replicate(' ',n_spins), offs:intarr(n_spins),$
    ln:intarr(n_spins)},n_vsm1)

n_vdat=576
n_sdat=336
vdatc=replicate({veisbl,ind:intarr(n_vdat)},n_spins)
sdatc=replicate({strlbl,ind:intarr(n_sdat)},n_spins)

;new mode1 veis map : offsets directly into lz array

lpr=0
dir=getenv('SWEDATLIB')
openr,lun,dir+'veismap.prt',/get_lun
for ispin=0,n_spins-1 do begin
  spnbl=0 & strvar='block'  
  readf,lun,spnbl,strvar
  if lpr then print,spnbl,strvar
  for isector=0,5 do begin
    sector=0 & strvar='sector'
    readf,lun,sector,strvar
    if lpr then print,sector,strvar
    for istep=0,15 do begin
      step=0 & offs=lonarr(18)
      readf,lun,step,offs
      if lpr then print,step,offs,format='(i2,6(i6,i4,i3))'
      vdatc(spnbl).ind(sector*96+istep*6+indgen(6))=offs(3*indgen(6))
    endfor
  endfor
endfor
if lpr then print,'vdatc.ind'
if lpr then print,vdatc,format='(6i6)'
free_lun,lun


;new mode1 strahl map : offsets directly into lz array
lpr=0

openr,lun,dir+'strahlmap.prt',/get_lun
for ispin=0,n_spins-1 do begin
  spnbl=0 & vstep=0
  readf,lun,spnbl,vstep
  if lpr then print,spnbl,vstep
  spnbl=0 & strvar='block'  
  readf,lun,spnbl,strvar
  if lpr then print,spnbl,strvar
  for istep=0,27 do begin
    step=0 & offs=lonarr(36)
    readf,lun,step,offs
      if lpr then print,step,offs,format='(i2,12(i6,i4,i3))'
      ;vdatc(spnbl).ind(sector*96+istep*6+indgen(6))=offs(3*indgen(6))
      sdatc(spnbl).ind(istep*12+indgen(12))=offs(3*indgen(12))
  endfor

 vsm1(1).descr(ispin)='     str hv step'
 vsm1(1).offs(ispin)=vstep
 vsm1(1).ln(ispin)=1

 vsm1(0).descr(ispin)='      spin count'
 vsm1(0).offs(ispin)=vstep-1
 vsm1(0).ln(ispin)= 1

endfor
free_lun,lun

if lpr then print,'sdatc.ind'
if lpr then print,sdatc,format='(12i6)'



end

