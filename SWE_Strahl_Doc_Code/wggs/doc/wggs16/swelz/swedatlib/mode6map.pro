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




pro mode6map  
 
common m6stuff,hkm6,vsm6,vdatc6,sdatc6,bxyzdat6


;get offsets of science data into lz data record
ind_scidat,scindx


btvr={bv, bnm:' ', p:0, n:0}


;---------------------- General housekeeping ----------------------------------
; general housekeeping byte offsets into sci_dat (structure field assignments)


n_hkm6=10   
hkm6=replicate({genlhk, descr:' ', offs:0, ln:1, nbv:0,$
                        bv:replicate(btvr,8), loc:intarr(32)},n_hkm6)

	

	hkm6(0).descr='         mode_tm'
	hkm6(0).offs=0
	hkm6(0).ln=1
	hkm6(0).nbv=3
  		hkm6(0).bv(0).bnm='      scimode_hk'
  		hkm6(0).bv(0).p=4
 	 	hkm6(0).bv(0).n=5

 	 	hkm6(0).bv(1).bnm='      tmmode_hk'
  		hkm6(0).bv(1).p=6
  		hkm6(0).bv(1).n=2

 	 	hkm6(0).bv(2).bnm='      tmrate_hk'
 	 	hkm6(0).bv(2).p=7
  		hkm6(0).bv(2).n=1


	hkm6(1).descr='     utc_lstspn'
	hkm6(1).offs=1
	hkm6(1).ln=4

	hkm6(2).descr='      ms_lstspn'
	hkm6(2).offs=5
	hkm6(2).ln=2

	hkm6(3).descr='spincount_tagged'
	hkm6(3).offs=7
	hkm6(3).ln=1

        hkm6(4).descr='     veis_status'
	hkm6(4).offs=125
	hkm6(4).ln=1

        hkm6(5).descr='       fc_status'
	hkm6(5).offs=126
	hkm6(5).ln=1
        
        hkm6(6).descr='num_strahl_steps'
	hkm6(6).offs=127
	hkm6(6).ln=1
	
	hkm6(7).descr=' pri_veis_hv_tbl'
	hkm6(7).offs=10134
	hkm6(7).ln=17
        hkm6(7).loc=10134+indgen(17)
        
        hkm6(8).descr=' alt_veis_hv_tbl'
	hkm6(8).offs=10151
	hkm6(8).ln=17
        hkm6(8).loc=10151+indgen(17)

	hkm6(9).descr='   strahl_hv_tbl'
	hkm6(9).offs=10168
	hkm6(9).ln=32
        hkm6(9).loc=10168+indgen(32)


	
;get hk byte offsets into level zero array 
hkm6.offs=scindx(hkm6.offs)
hkm6.loc=scindx(hkm6.loc)

;------------------------- Faraday Cup --------------------------------------
; Faraday cup data block byte offset (sci_dat) structure field assignments

n_fcblm6=31
fcblm6=replicate({farcupbl, descr:' ', offs:0, ln:0},n_fcblm6)

fcblm6(*).descr='fc block offsets'
fcblm6(*).ln=122

fcblm6(0).offs=128	
fcblm6(1).offs=480
fcblm6(2).offs=830
fcblm6(3).offs=1180

for i=4,27 do fcblm6(i).offs=fcblm6(i-4).offs+1412

fcblm6(28).offs=10012
fcblm6(29).offs=10262
fcblm6(30).offs=10515

;get fc byte offsets into level zero array 
fcblm6.offs=scindx(fcblm6.offs)


;----------------------- VEIS/STRAHL ---------------------------------------

n_spins=7

n_vsm6=2
vsm6=replicate({veis_strl_m6, descr:replicate(' ',n_vsm6),offs:intarr(n_vsm6),$
   ln:intarr(n_vsm6),bv0:replicate(btvr,n_vsm6),bv1:replicate(btvr,n_vsm6),$
   bv2:replicate(btvr,n_vsm6) },n_spins)
                            
n_vdat=576
n_sdat=336
n_bdat=10
vdatc6=replicate({veisbl6,ind:intarr(n_vdat)},n_spins)
sdatc6=replicate({strlbl6,ind:intarr(n_sdat)},n_spins)
bxyzdat6=replicate({bxyzbl6,ind:intarr(n_bdat)},n_spins)

;mode6 veis map : offsets directly into lz array

lpr=0
dir=getenv('SWEDATLIB')
lun=1
openr,lun,dir+'swemode6_veis.prt';,/get_lun
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
      vdatc6(spnbl).ind(sector*96+istep*6+indgen(6))=offs(3*indgen(6))
    endfor
    if lpr then stop
  endfor
endfor
if lpr then print,'vdatc6.ind'
if lpr then print,vdatc6,format='(6i6)'
close,lun

;mode6 strahl map : offsets directly into lz array
lpr=0

lun=1
openr,lun,dir+'swemode6_strahl.prt';,/get_lun
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
      sdatc6(spnbl).ind(istep*12+indgen(12))=offs(3*indgen(12))
  endfor
 
 vsm6(ispin).descr(0)='      spin count'
 vsm6(ispin).offs(0)=vstep-1
 vsm6(ispin).ln(0)= 1
 
 vsm6(ispin).descr(1)='vtblflag_strlstp'                  
 vsm6(ispin).offs(1)=vstep
 vsm6(ispin).ln(1)=1
   vsm6(ispin).bv0(1).bnm='    strahl_step'
   vsm6(ispin).bv0(1).p=5;4
   vsm6(ispin).bv0(1).n=6;5

   vsm6(ispin).bv1(1).bnm=' altspns_enabld'
   vsm6(ispin).bv1(1).p=6
   vsm6(ispin).bv1(1).n=1;2

   vsm6(ispin).bv2(1).bnm='pri_alt_veistbl'
   vsm6(ispin).bv2(1).p=7
   vsm6(ispin).bv2(1).n=1
                                               
endfor
close,lun
if lpr then print,'sdatc6.ind'
if lpr then print,sdatc6,format='(12i6)'


lun=1
openr,lun,dir+'swemode6_bxyz.prt';,/get_lun
for ispin=0,n_spins-1 do begin
  spnbl=0 & vstep=0
  readf,lun,spnbl,vstep
  if lpr then print,spnbl,vstep
  spnbl=0 & strvar='block'  
  readf,lun,spnbl,strvar
  if lpr then print,spnbl,strvar
  for istep=0,9 do begin
    step=0 & offs=lonarr(3)
    readf,lun,step,offs
      if lpr then print,step,offs,format='(i2,3(i6,i4,i3))'
      bxyzdat6(spnbl).ind(istep)=offs(0)
  endfor 
endfor 
close,lun


end

