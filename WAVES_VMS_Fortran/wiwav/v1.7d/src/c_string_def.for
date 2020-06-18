! c_string_def.for - a structure definition for working with strings
! between VAX/VMS FORTRAN and C (without descriptors) that will also
! work on UNIX machines.
!
	structure /c_string_4/
	   union
	   map
	      byte	b(4)
	   end map
	   map
	      character*4 c
	   end map
	   end union
	end structure

	structure /c_string_8/
	   union
	   map
	      byte	b(8)
	   end map
	   map
	      character*8 c
	   end map
	   end union
	end structure

	structure /c_string_16/
	   union
	   map
	      byte	b(16)
	   end map
	   map
	      character*16 c
	   end map
	   end union
	end structure

	structure /c_string_32/
	   union
	   map
	      byte	b(32)
	   end map
	   map
	      character*32 c
	   end map
	   end union
	end structure

	structure /c_string_36/
	   union
	   map
	      byte	b(36)
	   end map
	   map
	      character*36 c
	   end map
	   end union
	end structure

	structure /c_string_64/
	   union
	   map
	      byte	b(64)
	   end map
	   map
	      character*64 c
	   end map
	   end union
	end structure

	structure /c_string_128/
	   union
	   map
	      byte	b(128)
	   end map
	   map
	      character*128 c
	   end map
	   end union
	end structure

	structure /c_string_256/
	   union
	   map
	      byte	b(256)
	   end map
	   map
	      character*256 c
	   end map
	   end union
	end structure

	structure /c_string_512/
	   union
	   map
	      byte	b(512)
	   end map
	   map
	      character*512 c
	   end map
	   end union
	end structure

