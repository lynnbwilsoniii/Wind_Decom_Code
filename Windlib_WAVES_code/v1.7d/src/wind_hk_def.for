! wind_hk_def.for - defines sizes/indexes of wind/waves housekeeping data
! (Note: this file is "included" by wind_hk_addr_def.for).

$IF ABSOFT_FORTRAN
	integer*4       w_first_core_hk_word, w_last_core_hk_word
	integer*4       w_last_pktid_hk_word, w_first_pktid_hk_word
	integer*4       w_sc_mode_hk_word, w_first_mfq_hk_word
	integer*4       w_last_mfq_hk_word, w_n_mf_w_fill_hk_word
	integer*4       w_n_mf_w_synch_err_hk_word, w_last_hk_idx

	parameter	(w_first_core_hk_word=0)		! "original" hk words
	parameter	(w_last_core_hk_word=80)
	parameter	(w_first_pktid_hk_word=81)	! packet id bytes
	parameter	(w_last_pktid_hk_word=105)
	parameter	(w_sc_mode_hk_word=106)		! sc mode (cdhf code)
	parameter	(w_first_mfq_hk_word=107)		! minor frame quality
	parameter	(w_last_mfq_hk_word=107+125-1)
	parameter	(w_n_mf_w_fill_hk_word = w_last_mfq_hk_word + 1)
	parameter	(w_n_mf_w_synch_err_hk_word = w_last_mfq_hk_word + 2)
	parameter	(w_last_hk_idx=w_n_mf_w_synch_err_hk_word)
$ELSE
	parameter	w_first_core_hk_word=0		! "original" hk words
	parameter	w_last_core_hk_word=80
	parameter	w_first_pktid_hk_word=81	! packet id bytes
	parameter	w_last_pktid_hk_word=105
	parameter	w_sc_mode_hk_word=106		! sc mode (cdhf code)
	parameter	w_first_mfq_hk_word=107		! minor frame quality
	parameter	w_last_mfq_hk_word=107+125-1
	parameter	w_n_mf_w_fill_hk_word = w_last_mfq_hk_word + 1
	parameter	w_n_mf_w_synch_err_hk_word = w_last_mfq_hk_word + 2
	parameter	w_last_hk_idx=w_n_mf_w_synch_err_hk_word
$ENDIF
