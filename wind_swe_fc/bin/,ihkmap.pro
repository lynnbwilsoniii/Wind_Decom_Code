pro ihkmap,ihk

n_ihk=45
btvr={bv, bnm:' ', p:0, n:0}
ihk=replicate({instrhk, descr:' ', offs:0, ln:0, nbv:0,$
                        bv:replicate(btvr,8)},n_ihk)

ihk(*).ln=intarr(n_ihk)+1
ihk(*).nbv=intarr(n_ihk)

		
 	ihk(0).descr='         status1'
	ihk(0).offs=27 * 45
	ihk(0).nbv=7
		ihk(0).bv(0).bnm='   checksum test'
		ihk(0).bv(0).p=0
		ihk(0).bv(0).n=1

		ihk(0).bv(1).bnm='   checksum done'
		ihk(0).bv(1).p=1
		ihk(0).bv(1).n=1

		ihk(0).bv(2).bnm='    eeprom write'
		ihk(0).bv(2).p=2
		ihk(0).bv(2).n=1

		ihk(0).bv(3).bnm='    eeprom power'
		ihk(0).bv(3).p=3
		ihk(0).bv(3).n=1

		ihk(0).bv(4).bnm='          unused'
		ihk(0).bv(4).p=4
		ihk(0).bv(4).n=1

		ihk(0).bv(5).bnm='         tm mode'
		ihk(0).bv(5).p=6
		ihk(0).bv(5).n=2

		ihk(0).bv(6).bnm='         tm rate'
		ihk(0).bv(6).p=7
		ihk(0).bv(6).n=1

	
		ihk(1).descr='        mjf_cntr'
		ihk(1).offs=33 * 45

		ihk(2).descr='         mode_tm'
		ihk(2).offs=37 * 45
		ihk(2).nbv=3
 			ihk(2).bv(0).bnm='     scimode_ihk'
  			ihk(2).bv(0).p=4
  			ihk(2).bv(0).n=5

  			ihk(2).bv(1).bnm='      tmmode_ihk'
  			ihk(2).bv(1).p=6
  			ihk(2).bv(1).n=2

  			ihk(2).bv(2).bnm='      tmrate_ihk'
  			ihk(2).bv(2).p=7
  			ihk(2).bv(2).n=1

 	ihk(3).descr='        dpu powr'
	ihk(3).offs=43 * 45
	ihk(3).nbv=8
		ihk(3).bv(0).bnm='        cal powr'
		ihk(3).bv(0).p=0
		ihk(3).bv(0).n=1

		ihk(3).bv(1).bnm='       lv dpuii1'
		ihk(3).bv(1).p=1
		ihk(3).bv(1).n=1

		ihk(3).bv(2).bnm='       lv dpuii2'
		ihk(3).bv(2).p=2
		ihk(3).bv(2).n=1

		ihk(3).bv(3).bnm='       hv dpuii1'
		ihk(3).bv(3).p=3
		ihk(3).bv(3).n=1

		ihk(3).bv(4).bnm='       hv dpuii2'
		ihk(3).bv(4).p=4
		ihk(3).bv(4).n=1

		ihk(3).bv(5).bnm='      hv1 enable'
		ihk(3).bv(5).p=5
		ihk(3).bv(5).n=1

		ihk(3).bv(6).bnm='      hv2 enable'
		ihk(3).bv(6).p=6
		ihk(3).bv(6).n=1

		ihk(3).bv(7).bnm='       a/d ready'
		ihk(3).bv(7).p=7
		ihk(3).bv(7).n=1


 	ihk(4).descr='    relay dpuii1'
	ihk(4).offs=47 * 45
	ihk(4).nbv=8
		ihk(4).bv(0).bnm='veis ebias & def'
		ihk(4).bv(0).p=0
		ihk(4).bv(0).n=1

		ihk(4).bv(1).bnm='veis ibias & def'
		ihk(4).bv(1).p=1
		ihk(4).bv(1).n=1

		ihk(4).bv(2).bnm=' veis a111 pos5v'
		ihk(4).bv(2).p=2
		ihk(4).bv(2).n=1

		ihk(4).bv(3).bnm=' strl bias & def'
		ihk(4).bv(3).p=3
		ihk(4).bv(3).n=1

		ihk(4).bv(4).bnm='        pha powr'
		ihk(4).bv(4).p=4
		ihk(4).bv(4).n=1

		ihk(4).bv(5).bnm='   fc mod pos28v'
		ihk(4).bv(5).p=5
		ihk(4).bv(5).n=1

		ihk(4).bv(6).bnm='    fc posneg15v'
		ihk(4).bv(6).p=6
		ihk(4).bv(6).n=1

		ihk(4).bv(7).bnm='        fc pos5v'
		ihk(4).bv(7).p=7
		ihk(4).bv(7).n=1


 	ihk(5).descr='    relay_dpuii2'
	ihk(5).offs=53 * 45
	ihk(5).nbv=8
		ihk(5).bv(0).bnm='veis ebias & def'
		ihk(5).bv(0).p=0
		ihk(5).bv(0).n=1

		ihk(5).bv(1).bnm='veis ibias & def'
		ihk(5).bv(1).p=1
		ihk(5).bv(1).n=1

		ihk(5).bv(2).bnm=' veis a111 pos5v'
		ihk(5).bv(2).p=2
		ihk(5).bv(2).n=1

		ihk(5).bv(3).bnm=' strl bias & def'
		ihk(4).bv(3).p=3
		ihk(5).bv(3).n=1

		ihk(5).bv(4).bnm='        pha powr'
		ihk(5).bv(4).p=4
		ihk(5).bv(4).n=1

		ihk(5).bv(5).bnm='   fc mod pos28v'
		ihk(5).bv(5).p=5
		ihk(5).bv(5).n=1

		ihk(5).bv(6).bnm='    fc posneg15v'
		ihk(5).bv(6).p=6
		ihk(5).bv(6).n=1

		ihk(5).bv(7).bnm='        fc pos5v'
		ihk(5).bv(7).p=7
		ihk(5).bv(7).n=1


 	ihk(6).descr=' cntl_reg_dpuii1'
	ihk(6).offs=57 * 45
	ihk(6).nbv=8
		ihk(6).bv(0).bnm=' elec/ion select'
		ihk(6).bv(0).p=0
		ihk(6).bv(0).n=1

		ihk(6).bv(1).bnm='          unused'
		ihk(6).bv(1).p=1
		ihk(6).bv(1).n=1

		ihk(6).bv(2).bnm='         pha mix'
		ihk(6).bv(2).p=2
		ihk(6).bv(2).n=1

		ihk(6).bv(3).bnm='   strahl output'
		ihk(6).bv(3).p=3
		ihk(6).bv(3).n=1

		ihk(6).bv(4).bnm='          unused'
		ihk(6).bv(4).p=4
		ihk(6).bv(4).n=1

		ihk(6).bv(5).bnm='       fc output'
		ihk(6).bv(5).p=5
		ihk(6).bv(5).n=1

		ihk(6).bv(6).bnm='          unused'
		ihk(6).bv(6).p=6
		ihk(6).bv(6).n=1

		ihk(6).bv(7).bnm='          fc mod'
		ihk(6).bv(7).p=7
		ihk(6).bv(7).n=1


 	ihk(7).descr=' cntl_reg_dpuii2'
	ihk(7).offs=63 * 45
	ihk(7).nbv=8
		ihk(7).bv(0).bnm=' elec/ion select'
		ihk(7).bv(0).p=0
		ihk(7).bv(0).n=1

		ihk(7).bv(1).bnm='          unused'
		ihk(7).bv(1).p=1
		ihk(7).bv(1).n=1

		ihk(7).bv(2).bnm='         pha mix'
		ihk(7).bv(2).p=2
		ihk(7).bv(2).n=1

		ihk(7).bv(3).bnm='   strahl output'
		ihk(7).bv(3).p=3
		ihk(7).bv(3).n=1

		ihk(7).bv(4).bnm='          unused'
		ihk(7).bv(4).p=4
		ihk(7).bv(4).n=1

		ihk(7).bv(5).bnm='       fc output'
		ihk(7).bv(5).p=5
		ihk(7).bv(5).n=1

		ihk(7).bv(6).bnm='          unused'
		ihk(7).bv(6).p=6
		ihk(7).bv(6).n=1

		ihk(7).bv(7).bnm='          fc mod'
		ihk(7).bv(7).p=7
		ihk(7).bv(7).n=1


 	ihk(8).descr='  dpu_pos5v_curr'
	ihk(8).offs=67 * 45
	ihk(8).nbv=0


 	ihk(9).descr='    dpu_cal_curr'
	ihk(9).offs=73 * 45
	ihk(9).nbv=0


 	ihk(10).descr='         status2'
	ihk(10).offs=77 * 45
	ihk(10).nbv=7
		ihk(10).bv(0).bnm='   checksum test'
		ihk(10).bv(0).p=0
		ihk(10).bv(0).n=1

		ihk(10).bv(1).bnm='   checksum done'
		ihk(10).bv(1).p=1
		ihk(10).bv(1).n=1

		ihk(10).bv(2).bnm='    eeprom write'
		ihk(10).bv(2).p=2
		ihk(10).bv(2).n=1

		ihk(10).bv(3).bnm='    eeprom power'
		ihk(10).bv(3).p=3
		ihk(10).bv(3).n=1

		ihk(10).bv(4).bnm='          unused'
		ihk(10).bv(4).p=4
		ihk(10).bv(4).n=1

		ihk(10).bv(5).bnm='         tm mode'
		ihk(10).bv(5).p=6
		ihk(10).bv(5).n=2

		ihk(10).bv(6).bnm='         tm rate'
		ihk(10).bv(6).p=7
		ihk(10).bv(6).n=1


 	ihk(11).descr='  dpuii2_hv_curr'
	ihk(11).offs=83 * 45
	ihk(11).nbv=0


 	ihk(12).descr='  dpuii1_hv_curr'
	ihk(12).offs=87 * 45
	ihk(12).nbv=0


 	ihk(13).descr='  dpuii2_lv_curr'
	ihk(13).offs=93 * 45
	ihk(13).nbv=0


 	ihk(14).descr='  dpuii1_lv_curr'
	ihk(14).offs=97 * 45
	ihk(14).nbv=0


 	ihk(15).descr='        dpu_curr'
	ihk(15).offs=103 * 45
	ihk(15).nbv=0


 	ihk(16).descr=' dpu_neg12v_curr'
	ihk(16).offs=107 * 45
	ihk(16).nbv=0


 	ihk(17).descr=' dpu_pos12v_curr'
	ihk(17).offs=113 * 45
	ihk(17).nbv=0


 	ihk(18).descr='    dpuii1_ebias'
	ihk(18).offs=117 * 45
	ihk(18).nbv=0  


 	ihk(19).descr='    dpuii1_ibias'
	ihk(19).offs=123 * 45
	ihk(19).nbv=0


 	ihk(20).descr='         status3'
	ihk(20).offs=127 * 45
	ihk(20).nbv=7
		ihk(20).bv(0).bnm='   checksum test'
		ihk(20).bv(0).p=0
		ihk(20).bv(0).n=1

		ihk(20).bv(1).bnm='   checksum done'
		ihk(20).bv(1).p=1
		ihk(20).bv(1).n=1

		ihk(20).bv(2).bnm='    eeprom write'
		ihk(20).bv(2).p=2
		ihk(20).bv(2).n=1

		ihk(20).bv(3).bnm='    eeprom power'
		ihk(20).bv(3).p=3
		ihk(20).bv(3).n=1

		ihk(20).bv(4).bnm='          unused'
		ihk(20).bv(4).p=4
		ihk(20).bv(4).n=1

		ihk(20).bv(5).bnm='         tm mode'
		ihk(20).bv(5).p=6
		ihk(20).bv(5).n=2

		ihk(20).bv(6).bnm='         tm rate'
		ihk(20).bv(6).p=7
		ihk(20).bv(6).n=1


 	ihk(21).descr='     dpuii1_vref'
	ihk(21).offs=133 * 45
	ihk(21).nbv=0


 	ihk(22).descr='dpuii1_strl_bias'
	ihk(22).offs=137 * 45
	ihk(22).nbv=0


 	ihk(23).descr='   dpuii1_fcsupp'
	ihk(23).offs=143 * 45
	ihk(23).nbv=0


 	ihk(24).descr='      dpuii1_pha'
	ihk(24).offs=147 * 45
	ihk(24).nbv=0


 	ihk(25).descr='   dpuii1_pos15v'
	ihk(25).offs=153 * 45
	ihk(25).nbv=0


 	ihk(26).descr='    dpuii1_pos5v'
	ihk(26).offs=157 * 45
	ihk(26).nbv=0


 	ihk(27).descr='   dpuii1_neg15v'
	ihk(27).offs=163 * 45
	ihk(27).nbv=0


 	ihk(28).descr='    dpuii2_ebias'
	ihk(28).offs=167 * 45
	ihk(28).nbv=0


 	ihk(29).descr='    dpuii2_ibias'
	ihk(29).offs=173 * 45
	ihk(29).nbv=0


 	ihk(30).descr='         status4'
	ihk(30).offs=177 * 45
	ihk(30).nbv=7
		ihk(30).bv(0).bnm='   checksum test'
		ihk(30).bv(0).p=0
		ihk(30).bv(0).n=1

		ihk(30).bv(1).bnm='   checksum done'
		ihk(30).bv(1).p=1
		ihk(30).bv(1).n=1

		ihk(30).bv(2).bnm='    eeprom write'
		ihk(30).bv(2).p=2
		ihk(30).bv(2).n=1

		ihk(30).bv(3).bnm='    eeprom power'
		ihk(30).bv(3).p=3
		ihk(30).bv(3).n=1

		ihk(30).bv(4).bnm='          unused'
		ihk(30).bv(4).p=4
		ihk(30).bv(4).n=1

		ihk(30).bv(5).bnm='         tm mode'
		ihk(30).bv(5).p=6
		ihk(30).bv(5).n=2

		ihk(30).bv(6).bnm='         tm rate'
		ihk(30).bv(6).p=7
		ihk(30).bv(6).n=1


 	ihk(31).descr='     dpuii2_vref'
	ihk(31).offs=183 * 45
	ihk(31).nbv=0


 	ihk(32).descr='          spare1'
	ihk(32).offs=187 * 45
	ihk(32).nbv=0


 	ihk(33).descr='   dpuii2_fcsupp'
	ihk(33).offs=193 * 45
	ihk(33).nbv=0


 	ihk(34).descr='      dpuii2_pha'
	ihk(34).offs=197 * 45
	ihk(34).nbv=0


 	ihk(35).descr='   dpuii2_pos15v'
	ihk(35).offs=203 * 45
	ihk(35).nbv=0


 	ihk(36).descr='    dpuii2_pos5v'
	ihk(36).offs=207 * 45
	ihk(36).nbv=0


 	ihk(37).descr='   dpuii2_neg15v'
	ihk(37).offs=213 * 45
	ihk(37).nbv=0


 	ihk(38).descr='       dpu fuses'
	ihk(38).offs=217 * 45
	ihk(38).nbv=8
		ihk(38).bv(0).bnm='dpuii1 lvcurrlim'
		ihk(38).bv(0).p=0
		ihk(38).bv(0).n=1

		ihk(38).bv(1).bnm='dpuii1 hvcurrlim'
		ihk(38).bv(1).p=1
		ihk(38).bv(1).n=1

		ihk(38).bv(2).bnm='dpuii2 lvcurrlim'
		ihk(38).bv(2).p=2
		ihk(38).bv(2).n=1

		ihk(38).bv(3).bnm='dpuii2 hvcurrlim'
		ihk(38).bv(3).p=3
		ihk(38).bv(3).n=1

		ihk(38).bv(4).bnm='          unused'
		ihk(38).bv(4).p=4
		ihk(38).bv(4).n=1

		ihk(38).bv(5).bnm='          unused'
		ihk(38).bv(5).p=6
		ihk(38).bv(5).n=1

		ihk(38).bv(6).bnm='          unused'
		ihk(38).bv(6).p=7
		ihk(38).bv(6).n=1

		ihk(38).bv(7).bnm='  dpu pos5v curr'
		ihk(38).bv(7).p=7
		ihk(38).bv(7).n=1


 	ihk(39).descr='          spare2'
	ihk(39).offs=223 * 45
	ihk(39).nbv=0


 	ihk(40).descr='         status4'
	ihk(40).offs=227 * 45
	ihk(40).nbv=7
		ihk(40).bv(0).bnm='   checksum test'
		ihk(40).bv(0).p=0
		ihk(40).bv(0).n=1

		ihk(40).bv(1).bnm='   checksum done'
		ihk(40).bv(1).p=1
		ihk(40).bv(1).n=1

		ihk(40).bv(2).bnm='    eeprom write'
		ihk(40).bv(2).p=2
		ihk(40).bv(2).n=1

		ihk(40).bv(3).bnm='    eeprom power'
		ihk(40).bv(3).p=3
		ihk(40).bv(3).n=1

		ihk(40).bv(4).bnm='          unused'
		ihk(40).bv(4).p=4
		ihk(40).bv(4).n=1

		ihk(40).bv(5).bnm='         tm mode'
		ihk(40).bv(5).p=6
		ihk(40).bv(5).n=2

		ihk(40).bv(6).bnm='         tm rate'
		ihk(40).bv(6).p=7
		ihk(40).bv(6).n=1


 	ihk(41).descr='          spare3'
	ihk(41).offs=233 * 45
	ihk(41).nbv=0


 	ihk(42).descr='      dpuii_test'
	ihk(42).offs=237 * 45
	ihk(42).nbv=6
		ihk(42).bv(0).bnm=' dpuii1 loopback'
		ihk(42).bv(0).p=0
		ihk(42).bv(0).n=1

		ihk(42).bv(1).bnm=' dpuii2 loopback'
		ihk(42).bv(1).p=1
		ihk(42).bv(1).n=1

		ihk(42).bv(2).bnm='          unused'
		ihk(42).bv(2).p=4
		ihk(42).bv(2).n=3

		ihk(42).bv(3).bnm='   dpuii2 tested'
		ihk(42).bv(3).p=5
		ihk(42).bv(3).n=1

		ihk(42).bv(4).bnm='   dpuii1 tested'
		ihk(42).bv(4).p=6
		ihk(42).bv(4).n=1

		ihk(42).bv(5).bnm='  test completed'
		ihk(42).bv(5).p=7
		ihk(42).bv(5).n=1



 	ihk(43).descr='     cmd_rec_cnt'
	ihk(43).offs=243 * 45
	ihk(43).nbv=0


 	ihk(44).descr='    cmd_proc_cnt'
	ihk(44).offs=247 * 45
	ihk(44).nbv=0

lpr_ihkmap=0
if lpr_ihkmap then begin
  print,' '
  print,'instrument housekeeping map of offsets'
  print,'                   name  offset  minor frame '
  for i=0,44 do print,i,ihk(i).descr,ihk(i).offs,ihk(i).offs/45
endif

end
