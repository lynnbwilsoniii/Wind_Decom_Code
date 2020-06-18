! low_byte_def.for - for looking at the low order byte of an integer*4
! Note that VAX/VMS and SunOS are different

	structure /low_byte/
	   union
	      map
	         integer*4	i4val
	      end map
	      map
		 ! vax
	         !byte		b 
	         !byte		b1
	         !byte		b2
	         !byte		b3
		 ! sun
	         !byte		b3
	         !byte		b2
	         !byte		b1
	         !byte		b
$IF LITTLE_ENDIAN_ARCH
	          byte		b 
	          byte		b1
	          byte		b2
	          byte		b3
$ELSE
	          byte		b3
	          byte		b2
	          byte		b1
	          byte		b
$ENDIF
	      end map
	   end union
	end structure
