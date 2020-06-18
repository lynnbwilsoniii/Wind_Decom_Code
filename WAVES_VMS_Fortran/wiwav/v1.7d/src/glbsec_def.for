! glbsec_def.for -- data definition and structure for the common global
! section storing WIND realtime telemetry data and associated indexes.

	parameter	wind_tm_glbsec_name='wind_tm_data'

	include		'wind_record_def.for'

	structure /wind_tm_glsec/
	   record /wind_record/ recs(500)		! TM ring buffer
	   integer*4	pointer				! pointer for recs
	   real*4	frame_rate			! secs 'tween minor frms
	   integer*4	status_bucket			! CIN AST writes on err
	   character	filename*80			! wrt_disk data file
	   integer*4	last_major_validated
	   integer*4	last_minor_validated
	   integer*4	last_major_written
	   union
	   map
	     integer*4	last_minor_written
	   end map
	   map
	     byte	last_minor_written_b
	     byte	%fill(3)
	   end map
	   end union
	   integer*4	number_of_records_written
	   integer*4	number_of_interrupts
	   integer*4	number_of_validations
	   integer*4	number_of_synch_errors
	   integer*4	num_tm_keeper_words
	   logical*1	tm_keeper_words(0:255)
	   integer*4	dummy_fill
	end structure

	record /wind_tm_glsec/ glsec

	parameter	size_of_fill=512-mod(sizeof(glsec),512)
	byte		glsec_fill(size_of_fill)

	common /wind_tm_glsec/ glsec, glsec_fill
	volatile /wind_tm_glsec/

!	parameter f_initial_frame_rate='ffff7fff'x / 100.0
	parameter f_initial_frame_rate=0.1876
