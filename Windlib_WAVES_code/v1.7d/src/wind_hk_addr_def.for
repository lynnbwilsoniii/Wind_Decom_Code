! wind_hk_addr_def.for - tables of wind/waves housekeeping addresses (minor
! frame numbers and associated word indexes)

	include		'wind_hk_def.for'

	! table of hk minor fr.addresses
	integer*4	hk_mf_addr(w_first_core_hk_word:w_last_core_hk_word)

	! table of hk word addresses
	integer*4	hk_word_addr(w_first_core_hk_word:w_last_core_hk_word)

	data		hk_mf_addr/
	1		  4,   4,   9,  14,  14,  19,  24,  24,  29,
	1		 34,  34,  39,  44,  44,  49,  54,  54,  59,
	1		 64,  64,  69,  74,  74,  79,  84,  84,  89,
	1		 94,  94,  99,
	1		104, 104, 109, 114, 114, 119, 124, 124, 129,
	1		134, 134, 139, 144, 144, 149, 154, 154, 159,
	1		164, 164, 169, 174, 174, 179, 184, 184, 189,
	1		194, 194, 199,
	1		204, 204, 209, 214, 214, 219, 224, 224, 229,
	1		234, 234, 239, 244, 244, 249,
	1		 66,  76,  86,  96, 106, 116/
	data		hk_word_addr/
	1		 17,  18,  18,  17,  18,  18,  17,  18,  18,
	1		 17,  18,  18,  17,  18,  18,  17,  18,  18,
	1		 17,  18,  18,  17,  18,  18,  17,  18,  18,
	1		 17,  18,  18,
	1		 17,  18,  18,  17,  18,  18,  17,  18,  18,
	1		 17,  18,  18,  17,  18,  18,  17,  18,  18,
	1		 17,  18,  18,  17,  18,  18,  17,  18,  18,
	1		 17,  18,  18,
	1		 17,  18,  18,  17,  18,  18,  17,  18,  18,
	1		 17,  18,  18,  17,  18,  18,
	1		 18,  18,  18,  18,  18,  18/
