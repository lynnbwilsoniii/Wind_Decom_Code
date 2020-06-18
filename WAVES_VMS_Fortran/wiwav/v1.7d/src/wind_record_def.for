! wind_record_def.for - record structure for WIND global section and 
! wind_lib internal "record" (loosely defined) buffer.
!
! [Note: parm_def.for contains the vms_64bit_time record def.]

	integer*4	realtime_record_length
	parameter	(realtime_record_length=290)

	structure /wind_record/
	   union
	      map
	         byte	data(0:255)		! the minor frame
	      end map
	      map
	         byte	s1,s2,s3,minor_frame	! synch words 1-3, mf #
	      end map
	   end union
	   integer*4	major_frame		! major frame number
	   record /vms_64bit_time/ gathertime	! eather receive time
	   integer*4	sequence_number		! relative record #
	   record /vms_64bit_time/ scet		! spacecraft event time
	   integer*4	dpu_major_frame		! dpu major frame number
	   byte	minor_frame1			! 1st mf in TM sequence
	   byte	minor_frame3			! 3rd mf in TM sequence
	   byte	quality				! TM validater's flag
	   byte	written				! Disk Writer's flag
	   byte sp_test_number			! script player test number
	   byte sp_step_number			! script player step number
	   byte	d0				! dummy for alignment
	   byte	d1				! dummy for alignment
	end structure

