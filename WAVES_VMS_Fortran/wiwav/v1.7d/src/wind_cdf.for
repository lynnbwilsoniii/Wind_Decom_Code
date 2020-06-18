! wind_cdf.for - wind_lib item interface to NSSDC CDF data elements
!
! About data typing...cdf rvariables map to wind_lib item buffers according
! to the following table:
!
!	CDF data type	  How handled
!	-------------	-------------------------------------------
!	8-byte real	- returned as is
!	8-byte epoch	- converted to ur8
!	4-byte real	- returned as is
!	4-byte int	- returned as is
!	2-byte int	- returned as integer*4 (zext for unsigned)
!	1-byte int	- returned as integer*4 (zext for unsigned)
!	character	- error returned
!
! Note that, with the exception of epoch-to-ur8 (small integer expansion
! is not considered a data type change here),
! this module does not convert the CDF rvariable data type to the wind_lib 
! data type.  Instead, the CDF values are coerced into the caller's item
! buffer without regard to the data type of the caller's buffer.  The entry
! value of the caller's ret_type argument is used only to properly align
! 8-byte and 4-byte quantities.  On return,
! the ret_type argument contains the wind_lib data type corresponding to the
! original CDF rvariable item type.  In this way higher level routines such
! as w_item_r8 and w_item_r4 may perform appropriate data type conversions.
! Also, the arthimetic evaluation module that parses certain item database
! clauses calls w_really_get_item directly and depends on the ret_type
! argument to identify the nature of the returned item buffer.  Anyway,
! this is how the data type conversion business works for now...jk
!

	integer*4	function	w_cdf_get( ch, 
	1				buf, size, ret_size, ret_type,
	1				fcdf, b_item_name, flags)
	implicit	none
	include		'c_string_def.for'
	include		'low_byte_def.for'
	include		'parm_def.for'
	include		'item_def.for'
	include		'as_mdt_def.for'
$IF ABSOFT_FORTRAN
!
! The Absoft Fortran compiler fails on
!
!	include		'CDF_INC:cdf.inc'
!
! I haven't yet determined how to use an environment variable in Fortran,
! so I just spec'ed out a relative-path.  I agree - not the best.
!
	include		'../cdf/include/cdf.inc'
!
! Also, "cdf_def.for" fails compilation with the message:
!   File = cdf_def.for, Line = 4, Column = 12
!   "CDF_VAR_NUM" has the INTEGER attribute.   It must not be given
!   the INTEGER attribute again.
! So it is commented out for Absoft.
!
!	include		'cdf_def.for'
$ELSE
	include		'CDF_INC:cdf.inc'
	include		'cdf_def.for'
$ENDIF
	integer*4	ch			! r, wind_lib channel number
	real*4		buf(*)			! w, caller's buffer
	integer*4	size			! r, size of buffer
	integer*4	ret_size		! w, number of elements written
	integer*4	ret_type		! w, return data type
	record /cdf_item/ fcdf
	byte		b_item_name(*)		! r, root wind_lib item name
	integer*4	flags			! r, item attribute mask

	integer*4	ios
	integer*4	ok, ok2
	integer*4	i,j,k,n
	integer*4	buf_sz
	integer*4	callers_type
	integer*4	k3len			! a function
	integer*4	w_cdf_to_wiwav_buf_put	! a function
	integer*4	w_cdf_get_id		! a function
	integer*4	w_cdf_type_to_w_type	! a function
	integer*4	w_synch_cdf_to_irvmatch	! a function
$IF ABSOFT_FORTRAN
$IF LITTLE_ENDIAN_ARCH
!	integer*4	cdf_lib			! a function
$ELSE
!	integer*4	cdf_lib			! a function
$ENDIF
$ELSE
	integer*4	cdf_lib			! a function
$ENDIF
	character*80	msg
	logical*4	got_independent_var
	logical*4	done
	logical*4	toggle


	! these are required for call to cdf_inquire
	integer*4	id
	integer*4	n_dim			! # of dimensions, rVariables
	integer*4	dim_sizes(0:cdf_max_dims) ! dimension sizes, ind zVars
	integer*4	ddim_sizes(0:cdf_max_dims)! dimension sizes, dep zVars
	integer*4	encoding		! data encoding
	integer*4	majority		! variable majority, col or row
	integer*4	max_rec			! last record number
	integer*4	n_rv			! # rVariables
	integer*4	n_attributes		! # attributes

	! dependent rVariable stuff
	character*32	drv_name
	integer*4	drv_n			! rVariable id number
	integer*4	drv_type		! rVariable data type
	integer*4	drv_n_el		! # elements of data type
	integer*4	drv_variance		! record variance
	integer*4	drv_dim_var(0:cdf_max_dims)	! dimension variance
	integer*4	idx_of_drv_var
	integer*4	w_drv_type
	integer*4	dindices(0:cdf_max_dims)

	record /multi_type/ fub

	structure /loop_index_tracker/
	   integer*4	first		! first loop position
	   integer*4	last		! last loop position
	   integer*4	inc		! increment
	   integer*4	cur		! current loop position holder
	   integer*4	idx		! cdf dimension to loop over
	end structure

	structure /drv_range_tracker/
	   integer*4	n		! number of ranges
	   integer*4	i		! current range
	   record /loop_index_tracker/ el(0:cdf_max_dims)
	end structure
	record /drv_range_tracker/ drt

	w_cdf_get = 0

! 	type *, '...inside w_cdf_get...'
!	type *, '...ch...', ch
!	type *, '...size...', size
!	type *, '...ret_size...', ret_size
!	type *, '...ret_type...', ret_type

	drv_name = fcdf.dependent_rv

	ok = w_cdf_get_id(ch, fcdf.b_files, fcdf.n_files, id)
	if (ok .ne. 1) goto 20

	! get the file info
!#	call cdf_inquire(id,
!#	1		n_dim,          ! not used
!#	1		dim_sizes(1),
!#	1		encoding,       ! not used
!#	1		majority,       ! not used
!#	1		max_rec,
!#	1		n_rv,		! not used
!#	1		n_attributes,   ! not used
!#	1		ok)
!#	if (ok .ne. cdf_ok) goto 30
!#	dim_sizes(0) = max_rec


	! get the dependent variable info
!#	drv_n = cdf_var_num(id, drv_name)
!#	if (drv_n .lt. 1) goto 70
!#	call cdf_var_inquire(id,
!#	1		drv_n,
!#	1		drv_name,
!#	1		drv_type,
!#	1		drv_n_el,	! not used
!#	1		drv_variance,   ! not used
!#	1		drv_dim_var(1), ! not used
!#	1		ok)
!#	if (ok .ne. cdf_ok) goto 50

$IF ABSOFT_FORTRAN
$IF LITTLE_ENDIAN_ARCH
	ok = cdf_lib( SELECT_, CDF_, id, 
$ELSE
	call cdf_lib_( SELECT_, CDF_, id, 
$ENDIF
$ELSE
	ok = cdf_lib( SELECT_, CDF_, id, 
$ENDIF
	1                      ZVAR_NAME_, drv_name,
	1             GET_,    
	1                      zVAR_DATATYPE_, drv_type,
	1                      zVAR_MAXREC_,   ddim_sizes(0),
	1                      zVAR_DIMSIZES_, ddim_sizes(1),
	1             NULL_,   ok2)
	if (ok .ne. cdf_ok) goto 50
	if (ok2 .ne. cdf_ok) goto 51


	! determine the caller's buffer size in terms of data type
	ok = w_cdf_type_to_w_type(drv_type, w_drv_type)
	callers_type = ret_type
	ret_type = w_drv_type
	buf_sz = size
	if (size .lt. 1) goto 52
	if (w_drv_type .eq. p_double_return) then
	   if (callers_type .eq. p_int_return .or. 
	1      callers_type .eq. p_float_return) then
	      buf_sz = buf_sz / 2
	      if (buf_sz .lt. 1) goto 54
	   end if
	end if

	! copy input indices and note which index varies
	idx_of_drv_var = -99
	do i=0,cdf_max_dims
	   if (fcdf.use_dependent_rv_ranges .ne. 0) then
	      dindices(i) = fcdf.dependent_rv_ranges(0,i)
	      if (dindices(i) .lt. -1) then
	         if (fcdf.dependent_rv_ranges(2,i) .gt. 0) then
	            ! positive increment
	            dindices(i) = 1 ! beginning of range flg
	         else
	            ! negative increment
	            dindices(i) = ddim_sizes(i) ! beginning of range flg
	         end if
	      end if
	   else
	      dindices(i) = fcdf.dependent_rv_indices(i)
	   end if
	   if (dindices(i) .eq. -1) idx_of_drv_var = i
	end do

	!
	! get the first (and possibly only) dependent variable value
	!
	got_independent_var = fcdf.independent_rv(1:1) .ge. ' '
	if (got_independent_var) then
	   ! get the independent variable info, if present, and
	   ! synch windlib stream to cdf file on irv_match
	   if (idx_of_drv_var .eq. -99) goto 103
	   ok = w_synch_cdf_to_irvmatch
	1		(ch, buf, buf_sz, ret_size, ret_type, fub, fcdf,
	1		 id, dim_sizes, 
	1                drv_n, dindices, idx_of_drv_var, drv_type, w_drv_type)
	   if (ok .ne. 1) goto 70
	   if (ret_size .gt. 0) then
	      ! an interpolate item, return value calculated, just return
	      w_cdf_get = ok
	      return
	   end if
	else
	   ! no independent variable, indices are completely specified
!#	   call cdf_var_get(id, drv_n, dindices(0), dindices(1), fub, ok)
!#	   if (ok .ne. cdf_ok) goto 60
$IF ABSOFT_FORTRAN
$IF LITTLE_ENDIAN_ARCH
	   ok = cdf_lib( SELECT_, CDF_, id, 
$ELSE
	   call cdf_lib_( SELECT_, CDF_, id, 
$ENDIF
$ELSE
	   ok = cdf_lib( SELECT_, CDF_, id, 
$ENDIF
	1                      zVAR_NAME_,       drv_name,
	1                      zVAR_RECNUMBER_,  dindices(0),
	1                      zVAR_DIMINDICES_, dindices(1),
	1             GET_,    zVAR_DATA_, fub,
	1             NULL_,   ok2)
	   if (ok .ne. cdf_ok) goto 60
	   if (ok2 .ne. cdf_ok) goto 62
	end if

	! convert cdf data type to caller's expected data type
	!xxxxxx
!	ret_type = w_drv_type ! [makes only ret_type .eq. w_drv_type valid]

	if (fcdf.use_dependent_rv_ranges .eq. 0) then
	   ! loop in one dimension (old pre Aug96 way)
	   i = 1
! following lines commented out so old item definition
! syntax returns only one value
!	   done = .false.
	   ret_size = 0
!	   do while (.not. done)
	      ! could include this routine inline for speed improvement...jk
	      ok = w_cdf_to_wiwav_buf_put(buf, i, w_drv_type, drv_type, fub)
	      if (ok .ne. 1) goto 110
!	      i = i + 1
!	      dindices(idx_of_drv_var) = dindices(idx_of_drv_var) + 1
	      ret_size = ret_size + 1
!	      done = ret_size .ge. buf_sz .or.
!	1            dindices(idx_of_drv_var) .gt. ddim_sizes(idx_of_drv_var)
!	      if (.not. done) then
!	         call cdf_var_get(id, drv_n, dindices(0), dindices(1), fub, ok)
!	         if (ok .ne. cdf_ok) goto 60
!	      end if
!	   end do
	else
	   ! initialize the looping index maintenance structure (post Aug96)
	   drt.n = 0
	   i = 0
	   do while (i .le. cdf_max_dims)
	      j = fcdf.dependent_rv_range_order(i)
	      if (j .ge. 0) then
	         drt.el(i).idx   = j
	         drt.el(i).first = fcdf.dependent_rv_ranges(0,j)
	         !
	         ! next if should catch the lone -1
	         if (drt.el(i).first .eq. -1) drt.el(i).first = dindices(j)
	         drt.el(i).last  = fcdf.dependent_rv_ranges(1,j)
	         !
	         ! supply a default high range as needed, negative increment
	         ! (the flag is a negative value other than -1)
	         if (drt.el(i).first .lt. 0) drt.el(i).first = ddim_sizes(j)
	         !
	         ! supply a default high range as needed, positive increment
	         ! (the flag is a negative value other than -1)
	         if (drt.el(i).last .lt. 0) drt.el(i).last = ddim_sizes(j)
	         !
	         drt.el(i).inc   = fcdf.dependent_rv_ranges(2,j)
	         drt.el(i).cur   = drt.el(i).first
	         drt.n = drt.n + 1
	      else
	         i = cdf_max_dims + 1
	      end if
	      i = i + 1
	   end do
	   drt.i = drt.n
	   !
	   i = 1
	   done = .false.
	   ret_size = 0
	   do while (.not. done)
	      ok = w_cdf_to_wiwav_buf_put(buf, i, w_drv_type, drv_type, fub)
	      if (ok .ne. 1) goto 110
	      i = i + 1
	      ret_size = ret_size + 1
	      ! reindex loop indexes as needed for all ranges.
	      ! always increment the inner most loop, the outer loops
	      ! are incremented only at inner most loop roll over times.
	      toggle = .true.
	      do j=drt.n-1,0,-1
	         if (toggle) then
	            drt.el(j).cur = drt.el(j).cur + drt.el(j).inc
	            if (drt.el(j).inc .gt. 0) then
	               if (drt.el(j).cur .le. drt.el(j).last) then
	                  toggle = .false.
	               else
	                  if (j .ne. 0) then
	                     drt.el(j).cur = drt.el(j).first
	                  else
	                     done = .true.
	                  end if
	               end if
	            else
	               if (drt.el(j).cur .ge. drt.el(j).last) then
	                  toggle = .false.
	               else
	                  if (j .ne. 0) then
	                     drt.el(j).cur = drt.el(j).first
	                  else
	                     done = .true.
	                  end if
	               end if
	            end if
	         end if
	         k = drt.el(j).idx
	         dindices(k) = drt.el(j).cur
	      end do
	      if (.not. done) done = ret_size .ge. buf_sz 
!	1            dindices(idx_of_drv_var) .gt. ddim_sizes(idx_of_drv_var)
	      if (.not. done) then
!#	         call cdf_var_get(id, drv_n, dindices(0), dindices(1), fub, ok)
!#	         if (ok .ne. cdf_ok) goto 60
$IF ABSOFT_FORTRAN
$IF LITTLE_ENDIAN_ARCH
	         ok = cdf_lib( SELECT_, CDF_, id, 
$ELSE
	         call cdf_lib_( SELECT_, CDF_, id, 
$ENDIF
$ELSE
	         ok = cdf_lib( SELECT_, CDF_, id, 
$ENDIF
	1                      zVAR_NAME_,       drv_name,
	1                      zVAR_RECNUMBER_,  dindices(0),
	1                      zVAR_DIMINDICES_, dindices(1),
	1             GET_,    zVAR_DATA_, fub,
	1             NULL_,   ok2)
	         if (ok .ne. cdf_ok) goto 60
	         if (ok2 .ne. cdf_ok) goto 62
	      end if
	   end do
	end if

	w_cdf_get = 1
	return
  1	format(1x,'W_CDF_GET: ', a, :, i2)
  2	format(1x,'W_CDF_GET: ', a, <cdf_max_dims>(i5))
 20	continue
	write(6,1,iostat=ios) 'cannot get CDF id number.'
	w_cdf_get = 0
	return
! 30	continue
!	write(6,1,iostat=ios) 'error calling cdf_inquire.'
!	call cdf_error(ok, msg)
!	i = k3len(msg)
!	write(6,1,iostat=ios) msg(1:i)
!	w_cdf_get = 0
!	return
 50	continue
	write(6,1,iostat=ios) 'cannot inquire dep.rV. (a) '//drv_name
$IF ABSOFT_FORTRAN
$IF LITTLE_ENDIAN_ARCH
	call cdf_error(ok, msg)
$ELSE
	call cdf_error_(ok, msg)
$ENDIF
$ELSE
	call cdf_error(ok, msg)
$ENDIF
	i = k3len(msg)
	write(6,1,iostat=ios) msg(1:i)
	w_cdf_get = 0
	return
 51	continue
	write(6,1,iostat=ios) 'cannot inquire dep.rV. (b) '//drv_name
$IF ABSOFT_FORTRAN
$IF LITTLE_ENDIAN_ARCH
	call cdf_error(ok2, msg)
$ELSE
	call cdf_error_(ok2, msg)
$ENDIF
$ELSE
	call cdf_error(ok2, msg)
$ENDIF
	i = k3len(msg)
	write(6,1,iostat=ios) msg(1:i)
	w_cdf_get = 0
	return
 52	continue
	write(6,1,iostat=ios) 'caller''s buffer size < 1 for '//drv_name
$IF ABSOFT_FORTRAN
$IF LITTLE_ENDIAN_ARCH
	call cdf_error(ok, msg)
$ELSE
	call cdf_error_(ok, msg)
$ENDIF
$ELSE
	call cdf_error(ok, msg)
$ENDIF
	i = k3len(msg)
	write(6,1,iostat=ios) msg(1:i)
	w_cdf_get = 0
	return
 54	continue
	write(6,1,iostat=ios) 'buffer size = 4bytes for (real*8) '//drv_name
$IF ABSOFT_FORTRAN
$IF LITTLE_ENDIAN_ARCH
	call cdf_error(ok, msg)
$ELSE
	call cdf_error_(ok, msg)
$ENDIF
$ELSE
	call cdf_error(ok, msg)
$ENDIF
	i = k3len(msg)
	write(6,1,iostat=ios) msg(1:i)
	w_cdf_get = 0
	return
 60	continue
	write(6,1,iostat=ios) 'cannot get dep.rV. (a) '//drv_name
	write(6,2,iostat=ios) 'indices are ', 
	1 (fcdf.dependent_rv_indices(n), n=1,cdf_max_dims)
$IF ABSOFT_FORTRAN
$IF LITTLE_ENDIAN_ARCH
	call cdf_error(ok, msg)
$ELSE
	call cdf_error_(ok, msg)
$ENDIF
$ELSE
	call cdf_error(ok, msg)
$ENDIF
	i = k3len(msg)
	write(6,1,iostat=ios) msg(1:i)
	w_cdf_get = 0
	return
 62	continue
	write(6,1,iostat=ios) 'cannot get dep.rV. (b) '//drv_name
	write(6,2,iostat=ios) 'indices are ', 
	1 (fcdf.dependent_rv_indices(n), n=1,cdf_max_dims)
$IF ABSOFT_FORTRAN
$IF LITTLE_ENDIAN_ARCH
	call cdf_error(ok2, msg)
$ELSE
	call cdf_error_(ok2, msg)
$ENDIF
$ELSE
	call cdf_error(ok2, msg)
$ENDIF
	i = k3len(msg)
	write(6,1,iostat=ios) msg(1:i)
	w_cdf_get = 0
	return
 70	continue
	i = k3len(drv_name)
	write(6,1,iostat=ios) 'cannot process independent rVar info for "'
	1	//drv_name(1:i)//'"'
	w_cdf_get = 0
	return
 103	continue
	i = k3len(drv_name)
	write(6,1,iostat=ios) 'no dim.variance specified: dep.rV "'
	1	//drv_name(1:i)//'"'
	w_cdf_get = 0
	return
 110	continue
	i = k3len(drv_name)
	write(6,1,iostat=ios) 
	1   '(a) cannot match CDF dvar type to Windlib item: '//
	1   '"'//drv_name(1:i)//'" ', drv_type
	w_cdf_get = 0
	return
	end
! This routine synchronizes the current windlib stream with a specified
! CDF file by matching a named CDF "independent" variable value to a named
! windlib event/item value (or "independent_variable_match").
! The CDF independent variable must be consistently increasing or decreasing
! over the specified range in order for the binary search routine to work.
! Once a bracket for the independent data entity has been established in
! the CDF file, the dependent value is taken from the indexes corresponding
! to the EARLIEST neighbor, LATEST neighbor, NEAREST neighbor, or LINEAR
! interpolation between bracket boundaries.
! If the caller has requested interpolation, the caller's buffer is filled
! here instead of passing back to the parent routine for further processing.
!
	integer*4	function	w_synch_cdf_to_irvmatch
	1		(ch, buf, buf_sz, ret_size, ret_type, fub, fcdf,
	1		id, dim_sizes, 
	1		drv_n, dindices, idx_of_drv_var, 
	1		drv_type, w_drv_type)
	implicit	none
	include		'c_string_def.for'
	include		'low_byte_def.for'
	include		'parm_def.for'
	include		'item_def.for'
	include		'as_mdt_def.for'
$IF ABSOFT_FORTRAN
!
! The Absoft Fortran compiler fails on
!
!	include		'CDF_INC:cdf.inc'
!
! I haven't yet determined how to use an environment variable in Fortran,
! so I just spec'ed out a relative-path.  I agree - not the best.
!
	include		'../cdf/include/cdf.inc'
!
! Also, "cdf_def.for" fails compilation with the message:
!   File = cdf_def.for, Line = 4, Column = 12
!   "CDF_VAR_NUM" has the INTEGER attribute.   It must not be given
!   the INTEGER attribute again.
! So it is commented out for Absoft.
!
!	include		'cdf_def.for'
$ELSE
	include		'CDF_INC:cdf.inc'
	include		'cdf_def.for'
$ENDIF
	integer*4	ch			! r, wind_lib channel number
	real*4		buf(*)			! w, caller's buffer
	integer*4	buf_sz			! r, size of buffer
	integer*4	ret_size		! w, number of elements written
	integer*4	ret_type		! w, return data type
	record /cdf_item/ fcdf
	integer*4	id			! r, CDF file/stream id#
	integer*4	dim_sizes(0:cdf_max_dims) ! r, dimension sizes, rVariables
	integer*4	drv_n			! r, (not used with zVars)
	integer*4	dindices(0:cdf_max_dims)! rw,
	integer*4	idx_of_drv_var		! r,
	integer*4	drv_type		! r, dependent rVar CDFdata type
	integer*4	w_drv_type		! r, dependent rVar Windata type

	integer*4	i,j,n
	integer*4	ok, ok2
	integer*4	ios
	integer*4	k3len			! a function
	character*80	msg
	integer*4	w_cdf_type_to_w_type	! a function
	integer*4	w_wiwav_to_cdf_cnvrt	! a function
	integer*4	w_cdf_to_wiwav_cnvrt	! a function
	integer*4	w_cdf_bracket_irv	! a function
	integer*4	w_linear_interpolate	! a function
	integer*4	indices(0:cdf_max_dims)
	integer*4	first			! index/rec#
	integer*4	second			! index/rec#
	integer*4	search_mode
	character*32	drv_name

	character*32	irv_name
!	integer*4	irv_n			! rVariable id number
	integer*4	irv_type		! rVariable data type
	integer*4	irv_n_el		! # elements of data type
	integer*4	irv_variance		! record variance
	integer*4	irv_dim_var(0:cdf_max_dims)	! dimension variance
	integer*4	idx_of_irv_var		! dimension in which the irv
						! varies (dim 0 = CDF record
						! dimension)
	record /multi_type/ fub, fub1, fub2, fub3, fub4, fub5, fub6, fub7
	integer*4	w_item_i4		!-xragma C (w_item_i4)
	integer*4	w_item_r4		!-xragma C (w_item_r4)
	integer*4	w_item_r8		!-xragma C (w_item_r8)
	integer*4	w_ur8_to_epoch		! a function
	integer*4	w_ur8_from_epoch	! a function
$IF ABSOFT_FORTRAN
!	integer*4	cdf_lib			! a function
$ELSE
	integer*4	cdf_lib			! a function
$ENDIF
	integer*4	w_irv_type
	real*4		r4a, r4b
	real*8		r8, r8a, r8b
	real*8		r8x, r8x1, r8x2
	real*8		r8y, r8y1, r8y2
	external	w_r8_compare
	external	w_r4_compare
	external	w_i4_compare
	external	w_ui4_compare
	external	w_i2_compare
	external	w_ui2_compare
	external	w_i1_compare
	external	w_ui1_compare
	integer*4	interp_situation

	w_synch_cdf_to_irvmatch = 1

	! key error messages to dependent variable name
	drv_name = fcdf.dependent_rv

	! copy input indices and note which index varies
	idx_of_irv_var = -99
	do i=0,cdf_max_dims
	   !!!!!!if (irv_dim_var(i) .ne. NOVARY) goto 102
	   indices(i)  = fcdf.independent_rv_indices(i)
	   if (indices(i)  .eq. -1) idx_of_irv_var = i
	end do
	if (idx_of_irv_var .eq. -99) goto 102

	irv_name = fcdf.independent_rv

!#	irv_n = cdf_var_num(id, irv_name)
!#	if (irv_n .lt. 1) goto 36
!#	call cdf_var_inquire(id,
!#	1		irv_n,
!#	1		irv_name,
!#	1		irv_type,
!#	1		irv_n_el,
!#	1		irv_variance,
!#	1		irv_dim_var(1),
!#	1		ok)
!#	if (ok .ne. cdf_ok) goto 40
$IF ABSOFT_FORTRAN
$IF LITTLE_ENDIAN_ARCH
	ok = cdf_lib( SELECT_, CDF_, id, 
$ELSE
	call cdf_lib_( SELECT_, CDF_, id, 
$ENDIF
$ELSE
	ok = cdf_lib( SELECT_, CDF_, id, 
$ENDIF
	1                      ZVAR_NAME_,     irv_name,
	1             GET_,    
	1                      zVAR_DATATYPE_, irv_type,
	1                      zVAR_MAXREC_,   dim_sizes(0),
	1                      zVAR_DIMSIZES_, dim_sizes(1),
	1             NULL_,   ok2)
	if (ok .ne. cdf_ok) goto 40
	if (ok2 .ne. cdf_ok) goto 41

!	type *, 'variance: ', irv_variance
!	type 9, 'dim var : ', irv_dim_var(1), irv_dim_var(2), irv_dim_var(3), 
!	1	irv_dim_var(4), irv_dim_var(5)

	ok = w_cdf_type_to_w_type(irv_type, w_irv_type)
	if (ok .ne. 1) goto 86

	! get the wind_lib version of the independent variable
	! Note that only specific data types are supported.
	if (w_irv_type .eq. p_double_return) then
	   ok = w_item_r8(ch,fcdf.independent_rv_match,fub1,1,j)
	   if (ok .ne. 1) goto 80
	   if (irv_type .eq. CDF_EPOCH) then
	      ! assume wind_lib item is of type ur8
	      ok = w_ur8_to_epoch(fub1, fub2)
	      if (ok .ne. 1) goto 90
	      fub1 = fub2
	   end if
	else if (w_irv_type .eq. p_float_return) then
	   ok = w_item_r4(ch,fcdf.independent_rv_match,fub1,1,j)
	   if (ok .ne. 1) goto 82
	else if (w_irv_type .eq. p_int_return) then
	   ok = w_item_i4(ch,fcdf.independent_rv_match,fub1,1,j)
	   if (ok .ne. 1) goto 84
	else
	   goto 86
	end if

	! When the caller requests more than one value we use the secondary
	! search mode if the primary search mode is interpolate (linear)
	search_mode = fcdf.search_mode
	if (search_mode .eq. W_CDF_SEARCH_INTERPOLATE .and. buf_sz.gt.1) then
	   search_mode = fcdf.search_mode_2nd
	end if

	! match the wind_lib item value to the cdf independent variable
	! value by positioning into the cdf file, matching 
	! values by one of the following
	! techniques:  nearest neighbor, floor, ceiling, or interpolation.
	first  = 0
	second = 0
	if (irv_type .eq. CDF_REAL8 .or.
	1   irv_type .eq. CDF_DOUBLE .or.
	1   irv_type .eq. CDF_EPOCH) then
	   ok = w_cdf_bracket_irv(id, irv_name, 
	1	indices, dim_sizes, idx_of_irv_var,
	1	w_r8_compare,
	1	fub1, first, second, fub2, fub3)
	else if (irv_type .eq. CDF_REAL4 .or.
	1        irv_type .eq. CDF_FLOAT) then
	   ok = w_cdf_bracket_irv(id, irv_name,
	1	indices, dim_sizes, idx_of_irv_var,
	1	w_r4_compare,
	1	fub1, first, second, fub2, fub3)
	else if (irv_type .eq. CDF_INT4) then
	   ok = w_cdf_bracket_irv(id, irv_name,
	1	indices, dim_sizes, idx_of_irv_var,
	1	w_i4_compare,
	1	fub1, first, second, fub2, fub3)
	else if (irv_type .eq. CDF_UINT4) then
	   ok = w_cdf_bracket_irv(id, irv_name,
	1	indices, dim_sizes, idx_of_irv_var,
	1	w_ui4_compare,
	1	fub1, first, second, fub2, fub3)
	else if (irv_type .eq. CDF_INT2) then
	   ok = w_cdf_bracket_irv(id, irv_name,
	1	indices, dim_sizes, idx_of_irv_var,
	1	w_i2_compare,
	1	fub1, first, second, fub2, fub3)
	else if (irv_type .eq. CDF_UINT2) then
	   ok = w_cdf_bracket_irv(id, irv_name,
	1	indices, dim_sizes, idx_of_irv_var,
	1	w_ui2_compare,
	1	fub1, first, second, fub2, fub3)
	else if (irv_type .eq. CDF_INT1 .or. 
	1        irv_type .eq. CDF_BYTE) then
	   ok = w_cdf_bracket_irv(id, irv_name,
	1	indices, dim_sizes, idx_of_irv_var,
	1	w_i1_compare,
	1	fub1, first, second, fub2, fub3)
	else if (irv_type .eq. CDF_UINT1) then
	   ok = w_cdf_bracket_irv(id, irv_name,
	1	indices, dim_sizes, idx_of_irv_var,
	1	w_ui1_compare,
	1	fub1, first, second, fub2, fub3)
	else
	   goto 96
	end if

	interp_situation = ok
	if (ok .eq. 0) then		! success
	   ! do nothing
	else if (ok .eq. -99) then	! error
	   ! some kind of error
	   goto 100
	else if (ok .eq. 1) then	! after eof
	   if (search_mode .eq. W_CDF_SEARCH_INTERPOLATE) then
	      first = dim_sizes(idx_of_irv_var) - 1
	      indices(idx_of_irv_var) = first
!#	      call cdf_var_get(id, irv_n, indices(0), indices(1), fub2, ok)
!#	      if (ok .ne. cdf_ok) goto 70
$IF ABSOFT_FORTRAN
$IF LITTLE_ENDIAN_ARCH
	      ok = cdf_lib( SELECT_, CDF_, id, 
$ELSE
	      call cdf_lib_( SELECT_, CDF_, id, 
$ENDIF
$ELSE
	      ok = cdf_lib( SELECT_, CDF_, id, 
$ENDIF
	1                      zVAR_NAME_,       irv_name,
	1                      zVAR_RECNUMBER_,  indices(0),
	1                      zVAR_DIMINDICES_, indices(1),
	1             GET_,    zVAR_DATA_, fub2,
	1             NULL_,   ok2)
	      if (ok .ne. cdf_ok) goto 70
	      if (ok2 .ne. cdf_ok) goto 71
	   end if
	else if (ok .eq. -1) then	! before bof
	   if (search_mode .eq. W_CDF_SEARCH_INTERPOLATE) then
	      second = 2
	      indices(idx_of_irv_var) = second
!#	      call cdf_var_get(id, irv_n, indices(0), indices(1), fub3, ok)
!#	      if (ok .ne. cdf_ok) goto 72
$IF ABSOFT_FORTRAN
$IF LITTLE_ENDIAN_ARCH
	      ok = cdf_lib( SELECT_, CDF_, id, 
$ELSE
	      call cdf_lib_( SELECT_, CDF_, id, 
$ENDIF
$ELSE
	      ok = cdf_lib( SELECT_, CDF_, id, 
$ENDIF
	1                      zVAR_NAME_,       irv_name,
	1                      zVAR_RECNUMBER_,  indices(0),
	1                      zVAR_DIMINDICES_, indices(1),
	1             GET_,    zVAR_DATA_, fub3,
	1             NULL_,   ok2)
	      if (ok .ne. cdf_ok) goto 72
	      if (ok2 .ne. cdf_ok) goto 73
	   end if
	end if
!	type *, '...interp_sit,first,second=', interp_situation, first, second
!	type *, '...fub2.r8,fub3.r8=', fub2.r8, fub3.r8

!	type *, '...drv...'
!	type *, 'drv #   : ', drv_n
!	type *, 'name    : ', drv_name.c
!	type *, 'type    : ', drv_type
!	type *, 'n elemen: ', drv_n_el
!	type *, 'variance: ', drv_variance
!	type 9, 'dim var : ', drv_dim_var(1), drv_dim_var(2), drv_dim_var(3), 
!	1	drv_dim_var(4), drv_dim_var(5)

	! Get the drv values at the bracket indexes.
	!
	! The indexes "first" and "second" obtained by bracketing the
	! independent rV are blindly applied to the dimension slot that
	! varies the dependent rV.
	!
	fub4.i4 = 0
	fub4.i4_2nd = 0
	dindices(idx_of_drv_var) = first
!#	call cdf_var_get(id, drv_n, dindices(0), dindices(1), fub4, ok)
!#	if (ok .ne. cdf_ok) goto 60
$IF ABSOFT_FORTRAN
$IF LITTLE_ENDIAN_ARCH
	ok = cdf_lib( SELECT_, CDF_, id, 
$ELSE
	call cdf_lib_( SELECT_, CDF_, id, 
$ENDIF
$ELSE
	ok = cdf_lib( SELECT_, CDF_, id, 
$ENDIF
	1                      zVAR_NAME_,       drv_name,
	1                      zVAR_RECNUMBER_,  dindices(0),
	1                      zVAR_DIMINDICES_, dindices(1),
	1             GET_,    zVAR_DATA_, fub4,
	1             NULL_,   ok2)
	if (ok .ne. cdf_ok) goto 60
	if (ok2 .ne. cdf_ok) goto 62
	!
	fub5.i4 = 0
	fub5.i4_2nd = 0
	dindices(idx_of_drv_var) = second
!#	call cdf_var_get(id, drv_n, dindices(0), dindices(1), fub5, ok)
!#	if (ok .ne. cdf_ok) goto 60
$IF ABSOFT_FORTRAN
$IF LITTLE_ENDIAN_ARCH
	ok = cdf_lib( SELECT_, CDF_, id, 
$ELSE
	call cdf_lib_( SELECT_, CDF_, id, 
$ENDIF
$ELSE
	ok = cdf_lib( SELECT_, CDF_, id, 
$ENDIF
	1                      zVAR_NAME_,       drv_name,
	1                      zVAR_RECNUMBER_,  dindices(0),
	1                      zVAR_DIMINDICES_, dindices(1),
	1             GET_,    zVAR_DATA_, fub5,
	1             NULL_,   ok2)
	if (ok .ne. cdf_ok) goto 60
	if (ok2 .ne. cdf_ok) goto 62
!	type *, '...fub4.r8,fub5.r8=', fub4.r8, fub5.r8

	! select/determine the target drv value by specified search mode
	! and set the index [ dindices(idx_of_drv_var) ] appropriately
	! for successive value retrievals (if needed)
	if (search_mode .eq. W_CDF_SEARCH_NEAREST) then
!	type *, '...nearest...',w_irv_type, irv_type
	   if (w_irv_type .eq. p_int_return) then
	      i = fub1.i4 - fub2.i4
	      j = fub3.i4 - fub1.i4
	      if (i .le. j) then
	         fub = fub4
	         dindices(idx_of_drv_var) = first
	      else
	         fub = fub5
	         dindices(idx_of_drv_var) = second
	      end if
	   else if (w_irv_type .eq. p_float_return) then
!	type *, '...fub1,2.r4=', fub1.r4, fub2.r4
	      r4a = fub1.r4 - fub2.r4
	      r4b = fub3.r4 - fub1.r4
	      if (r4a .le. r4b) then
	         fub = fub4
	         dindices(idx_of_drv_var) = first
	      else
	         fub = fub5
	         dindices(idx_of_drv_var) = second
	      end if
	   else if (w_irv_type .eq. p_double_return) then
!	type *, '...fub1,2,3.r8=', fub1.r8, fub2.r8, fub3.r8
	      r8a = fub1.r8 - fub2.r8
	      r8b = fub3.r8 - fub1.r8
	      if (r8a .le. r8b) then
	         fub = fub4
	         dindices(idx_of_drv_var) = first
	      else
	         fub = fub5
	         dindices(idx_of_drv_var) = second
	      end if
	   end if
	else if (search_mode .eq. W_CDF_SEARCH_INTERPOLATE) then
	   ! set index for successive value retrievals
!	   if (interp_situation .eq. 0) then		! bracketed ok
!	      dindices(idx_of_drv_var) = first
!	   else if (interp_situation .eq. 1) then	! eof interp
!	      dindices(idx_of_drv_var) = second
!	   else						! bof interp
!	      dindices(idx_of_drv_var) = first
!	   end if
	   !
	   ! convert the irv bracket boundry values to real*8
	   !
	   i = 1
	   ok = w_cdf_to_wiwav_cnvrt(r8x, i, 
	1          p_double_return, irv_type, fub1)
	   if (ok .ne. 1) goto 112
	   !
	   i = 1
	   ok = w_cdf_to_wiwav_cnvrt(r8x1, i, 
	1          p_double_return, irv_type, fub2)
	   if (ok .ne. 1) goto 112
	   !
	   i = 1
	   ok = w_cdf_to_wiwav_cnvrt(r8x2, i, 
	1          p_double_return, irv_type, fub3)
	   if (ok .ne. 1) goto 112
	   !
	   ! convert the drv bracket boundry values to real*8
	   !
	   r8y  = 0.0
	   i = 1
	   ok = w_cdf_to_wiwav_cnvrt(r8y1, i, 
	1          p_double_return, drv_type, fub4)
	   if (ok .ne. 1) goto 113
	   i = 1
	   !
	   ok = w_cdf_to_wiwav_cnvrt(r8y2, i, 
	1          p_double_return, drv_type, fub5)
	   if (ok .ne. 1) goto 113
	   !
	   ok = w_linear_interpolate(r8x,r8x1,r8x2,r8y,r8y1,r8y2)
	   if (ok .ne. 1) goto 120
!	type *, '...x,x1,x2=', r8x, r8x1, r8x2
!	type *, '...y,y1,y2=', r8y, r8y1, r8y2
!	   i = 1
	   ok = w_wiwav_to_cdf_cnvrt(r8y, p_double_return, buf, drv_type)
	   if (ok .ne. 1) goto 114
	   ret_size = 1
	   w_synch_cdf_to_irvmatch = 1
	   return
	else if (search_mode .eq. W_CDF_SEARCH_EARLIER) then
	   fub = fub4
	   dindices(idx_of_drv_var) = first
	else if (search_mode .eq. W_CDF_SEARCH_LATER) then
	   fub = fub5
	   dindices(idx_of_drv_var) = second
	else
	   goto 130
	end if

	return
  1	format(1x,'W_SYNCH_CDF_TO_IRVMATCH: ', a, :, i2)
  2	format(1x,'W_SYNCH_CDF_TO_IRVMATCH: ', a, <cdf_max_dims>(i5))
! 36	continue
!	i = k3len(irv_name)
!	write(6,1,iostat=ios) 'cannot get var num for inddep.rV. '//
!	1   irv_name(1:i)//'.'
!x	call cdf_error(ok, msg)
!x	i = k3len(msg)
!x	write(6,1,iostat=ios) msg(1:i)
!	w_synch_cdf_to_irvmatch = 0
!	return
 40	continue
	write(6,1,iostat=ios) 'cannot inquire ind.rV. (a) '//irv_name
$IF ABSOFT_FORTRAN
$IF LITTLE_ENDIAN_ARCH
	call cdf_error(ok, msg)
$ELSE
	call cdf_error_(ok, msg)
$ENDIF
$ELSE
	call cdf_error(ok, msg)
$ENDIF
	i = k3len(msg)
	write(6,1,iostat=ios) msg(1:i)
	w_synch_cdf_to_irvmatch = 0
	return
 41	continue
	write(6,1,iostat=ios) 'cannot inquire ind.rV. (b) '//irv_name
$IF ABSOFT_FORTRAN
$IF LITTLE_ENDIAN_ARCH
	call cdf_error(ok2, msg)
$ELSE
	call cdf_error_(ok2, msg)
$ENDIF
$ELSE
	call cdf_error(ok2, msg)
$ENDIF
	i = k3len(msg)
	write(6,1,iostat=ios) msg(1:i)
	w_synch_cdf_to_irvmatch = 0
	return
 60	continue
	write(6,1,iostat=ios) 'cannot get dep.rV. (a) '//drv_name
	write(6,2,iostat=ios) 'indices are ', 
	1 (fcdf.dependent_rv_indices(n), n=1,cdf_max_dims)
$IF ABSOFT_FORTRAN
$IF LITTLE_ENDIAN_ARCH
	call cdf_error(ok, msg)
$ELSE
	call cdf_error_(ok, msg)
$ENDIF
$ELSE
	call cdf_error(ok, msg)
$ENDIF
	i = k3len(msg)
	write(6,1,iostat=ios) msg(1:i)
	w_synch_cdf_to_irvmatch = 0
	return
 62	continue
	write(6,1,iostat=ios) 'cannot get dep.rV. (b) '//drv_name
	write(6,2,iostat=ios) 'indices are ', 
	1 (fcdf.dependent_rv_indices(n), n=1,cdf_max_dims)
$IF ABSOFT_FORTRAN
$IF LITTLE_ENDIAN_ARCH
	call cdf_error(ok2, msg)
$ELSE
	call cdf_error_(ok2, msg)
$ENDIF
$ELSE
	call cdf_error(ok2, msg)
$ENDIF
	i = k3len(msg)
	write(6,1,iostat=ios) msg(1:i)
	w_synch_cdf_to_irvmatch = 0
	return
 70	continue
	write(6,1,iostat=ios) '(EOF) cannot get indp.rV. (a) '//drv_name
	w_synch_cdf_to_irvmatch = 0
	return
 71	continue
	write(6,1,iostat=ios) '(EOF) cannot get indp.rV. (b) '//drv_name
	w_synch_cdf_to_irvmatch = 0
	return
 72	continue
	write(6,1,iostat=ios) '(BOF) cannot get indp.rV. (a) '//drv_name
	w_synch_cdf_to_irvmatch = 0
	return
 73	continue
	write(6,1,iostat=ios) '(BOF) cannot get indp.rV. (b) '//drv_name
	w_synch_cdf_to_irvmatch = 0
	return
 80	continue
	i = k3len(fcdf.independent_rv_match)
	write(6,1,iostat=ios) 'cannot get "'//fcdf.independent_rv_match(1:i)//
	1	'" as R8 item'
	w_synch_cdf_to_irvmatch = ok
	return
 82	continue
	i = k3len(fcdf.independent_rv_match)
	write(6,1,iostat=ios) 'cannot get "'//fcdf.independent_rv_match(1:i)//
	1	'" as R4 item'
	w_synch_cdf_to_irvmatch = ok
	return
 84	continue
	i = k3len(fcdf.independent_rv_match)
	write(6,1,iostat=ios) 'cannot get "'//fcdf.independent_rv_match(1:i)//
	1	'" as I4 item'
	w_synch_cdf_to_irvmatch = ok
	return
 86	continue
	i = k3len(fcdf.independent_rv_match)
	write(6,1,iostat=ios) 'item "'//fcdf.independent_rv_match(1:i)//
	1	'" is not I4, R4, or R8'
	w_synch_cdf_to_irvmatch = 0
	return
 90	continue
	i = k3len(fcdf.independent_rv_match)
$IF ABSOFT_FORTRAN
	write(6,1,iostat=ios)
	1       'cannot convert "'//fcdf.independent_rv_match(1:i)//
	1	'" from UR8 to CDF Epoch'
$ELSE
	write(6,1,iostat=ios) 'cannot convert "'//fcdf.independent_rv_match(1:i)//
	1	'" from UR8 to CDF Epoch'
$ENDIF
	w_synch_cdf_to_irvmatch = ok
	return
  96	continue
	write(6,1,iostat=ios) 'cannot bracket ind.rV. '//irv_name
	write(6,1,iostat=ios) '...no provision for CDF data type: ', irv_type
	w_synch_cdf_to_irvmatch = 0
	return
 100	continue
	write(6,1,iostat=ios) 'cannot get ind.rV. '//irv_name
	write(6,2,iostat=ios) 'indices are ', (indices(n), n=1,cdf_max_dims)
	write(6,*,iostat=ios) '   Err: Rec#,scet,irv:', first, fub1.r8, fub2.r8
	w_synch_cdf_to_irvmatch = 0
	return
 102	continue
	i = k3len(irv_name)
	write(6,1,iostat=ios) 'no dim.variance specified: ind.rV "'
	1	//irv_name(1:i)//'"'
	w_synch_cdf_to_irvmatch = 0
	return
 112	continue
	i = k3len(irv_name)
	write(6,1,iostat=ios) 
	1   '(b) cannot match CDF ivar type to Windlib item: '//
	1   '"'//irv_name(1:i)//'" ', irv_type
	w_synch_cdf_to_irvmatch = 0
	return
 113	continue
	i = k3len(drv_name)
	write(6,1,iostat=ios) 
	1   '(c) cannot match CDF dvar type to Windlib item: '//
	1   '"'//drv_name(1:i)//'" ', drv_type
	w_synch_cdf_to_irvmatch = 0
	return
 114	continue
	i = k3len(drv_name)
	write(6,1,iostat=ios) 
	1   '(d) cannot match CDF dvar type to Windlib item: '//
	1   '"'//drv_name(1:i)//'" ', drv_type
	w_synch_cdf_to_irvmatch = 0
	return
 120	continue
	write(6,1,iostat=ios) 'linear interpolation unsuccessful.'
	w_synch_cdf_to_irvmatch = 0
	return
 130	continue
	write(6,1,iostat=ios) 'invalid/unknown search mode specified.'
	w_synch_cdf_to_irvmatch = 0
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	w_cdf_get_id(ch, f, n_files, id)
	implicit	none
	include		'wind_os_def.for'
	include		'c_string_def.for'
	include		'item_def.for'
$IF ABSOFT_FORTRAN
!
! The Absoft Fortran compiler fails on
!
!	include		'CDF_INC:cdf.inc'
!
! I haven't yet determined how to use an environment variable in Fortran,
! so I just spec'ed out a relative-path.  I agree - not the best.
!
	include		'../cdf/include/cdf.inc'
$ELSE
	include		'CDF_INC:cdf.inc'
$ENDIF
	integer*4	ch
	record /c_string_256/ f(max_cdf_files)
	integer*4	n_files
	integer*4	id

	record /c_string_256/ g
!	record /c_string_256/ e
!	logical*4	first_time /.true./
	integer*4	i,k,m,o
	integer*4	ios
	integer*4	ok, ok2
	integer*4	w_cdf_cnvrt_fn		! a function
	character	msg*(cdf_statustext_len)
	integer*4	k3len
!	integer*4	w_cdf_fn_hash_get !$pragma c (w_cdf_fn_hash_get)
!	integer*4	w_cdf_fn_hash_put !$pragma c (w_cdf_fn_hash_put)
!	integer*4	w_cdf_fn_hash_clr !$pragma c (w_cdf_fn_hash_clr)
!	integer*4	w_cdf_fn_hash_deq_oldest !$pragma c (w_cdf_fn_hash_deq_oldest)
!	integer*4	w_cdf_get_fn_of_id !$pragma c (w_cdf_get_fn_of_id)
	integer*4	w_resolve_filename	! a function
	logical*4	exists
	integer*4	w_cdf_fn_match		! a function
	integer*4	w_cdf_fn_save 		! a function
	integer*4	w_cdf_fn_report		! a function
$IF ABSOFT_FORTRAN
!	integer*4	cdf_lib			! a function
$ELSE
	integer*4	cdf_lib			! a function
$ENDIF

	w_cdf_get_id = 0

!	if (first_time) then
!	   ok = w_cdf_fn_hash_clr()
!	   first_time = .false.
!	end if

!	call w_cdf_fn_report()

	! has this file been opened already?
	do k=1,n_files
	   ok = w_cdf_cnvrt_fn(ch, f(k).c, g.c, 0)
!	type *, '...f(k).c=', f(k).c(1:40)
!	type *, '...   g.c=', g.c(1:m)
	   if (ok .eq. 1) ok = w_cdf_fn_match(g.c, m)
	   if (ok .eq. 1) then
	      id = m
	      w_cdf_get_id = 1
	      return
	   end if
	end do

	! find the file in the file system
	k = 1
	ok = 0
	do while (k .le. n_files .and. ok .ne. 1)
	   ok = w_cdf_cnvrt_fn(ch, f(k).c, g.c, 2)
	   if (ok .eq. 1) then
	      ok = w_resolve_filename(g.c)
	      if (ok .eq. 1) then
	         if (index(g.c,'00.') .eq. 0) then ! skip bogus *v00.cdf files
	            m = k3len(g.c)
	            inquire(file=g.c(1:m), exist=exists)
	            if (.not. exists) ok = 0
	         else
	            ok = 0
	         end if
	      end if
	   end if
	   k = k + 1
	end do
	k = k - 1
	if (ok .ne. 1) goto 10

	! open the cdf file
!	call cdf_open(g.c(1:m), id, ok)
$IF ABSOFT_FORTRAN
$IF LITTLE_ENDIAN_ARCH
	ok = cdf_lib( OPEN_, CDF_, g.c(1:m), id,
$ELSE
	call cdf_lib_( OPEN_, CDF_, g.c(1:m), id,
$ENDIF
$ELSE
	ok = cdf_lib( OPEN_, CDF_, g.c(1:m), id,
$ENDIF
	1             SELECT_, CDF_zMODE_, zMODEon1,
	1             NULL_, ok2)
	if (ok .ne. ok2) then
	   write(6,1,iostat=o) 'received two different status codes:', ok, ok2
	end if
	if (ok .lt. cdf_ok) goto 20
	if (ok2 .lt. cdf_ok) goto 22
	if (ok .gt. cdf_ok)
	1  write(6,1,iostat=o) 'info on cdf_lib(open_,...), ok=', ok
	if (ok2 .gt. cdf_ok .and. ok .ne. ok2)
	1  write(6,1,iostat=o) 'info on cdf_lib(open_,...), ok2=', ok2

	! store the cdf id# and file name
	ok = w_cdf_cnvrt_fn(ch, f(k).c, g.c, 0)
	if (ok .ne. 1) goto 30
	ok = w_cdf_fn_save(g.c, id)

	w_cdf_get_id = 1

	return
  1	format(1x,'W_CDF_GET_ID: ', a, :, i, :, i)
 10	continue
	write(6,1,iostat=ios) 'cannot resolve CDF filename or file not found'
	write(6,1,iostat=ios) 'possible file format: '//f(1).c(1:40)
	return
 20	continue
	write(6,1,iostat=ios) 'error calling CDF_LIB (open,a), ok=', ok
	write(6,1,iostat=o) '   cdf_ok, cdf_warn:', cdf_ok, cdf_warn
$IF ABSOFT_FORTRAN
$IF LITTLE_ENDIAN_ARCH
	call cdf_error(ok, msg)
$ELSE
	call cdf_error_(ok, msg)
$ENDIF
$ELSE
	call cdf_error(ok, msg)
$ENDIF
	i = k3len(msg)
	write(6,1,iostat=ios) msg(1:i)
	write(6,1,iostat=ios) 'file='//g.c(1:60)//'.'
	return
 22	continue
	write(6,1,iostat=ios) 'error calling CDF_LIB (open,b), ok=', ok2
	write(6,1,iostat=o) '   cdf_ok, cdf_warn:', cdf_ok, cdf_warn
$IF ABSOFT_FORTRAN
$IF LITTLE_ENDIAN_ARCH
	call cdf_error(ok2, msg)
$ELSE
	call cdf_error_(ok2, msg)
$ENDIF
$ELSE
	call cdf_error(ok2, msg)
$ENDIF
	i = k3len(msg)
	write(6,1,iostat=ios) msg(1:i)
	write(6,1,iostat=ios) 'file='//g.c(1:60)//'.'
	return
 30	continue
	write(6,1,iostat=ios) 'cannot convert filename (2)'
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	w_cdf_cnvrt_fn( ch, f, g, do_ver)
	implicit	none
	include		'wind_os_def.for'
	include		'c_string_def.for'
	include		'parm_def.for'
!	include		'wind_tm_user_def.for'
	include		'wind_extra_info_def.for'
	integer*4	ch
	character*(*)	f		! input file spec
	character*(*)	g		! output file spec
	integer*4	do_ver		! 1=do version substitution, 0=don't
	integer*4	i,j,k,n,m,o
	character*8	c8
	character*2	c2(8) /'01','02','03','04','05','06','07','08'/
	integer*4	ios
	integer*4	k3len
	logical*4	got_ver
	logical*4	exists
	integer*4	highest_version /8/
	integer*4	last_version
	character*256	c256
	integer*4	w_resolve_filename
	integer*4	ok
	integer*4	ur8_to_dbms
	integer*4	yyyymmdd, hhmmss, msec

	w_cdf_cnvrt_fn = 0

!	type *, '...inside w_cdf_cnvrt...',f(1:44)

	m = 5
	i = index(f, '$DATE')
	if (i .eq. 0) then
	   m = 9
	   i = index(f, '$YYYYMMDD')
	end if
	if (i .eq. 0) then
	   m = 7
	   i = index(f, '$YYMMDD')
	end if
	if (i .ne. 0) then
	   ok = ur8_to_dbms(exi(ch).ur8_context, yyyymmdd, hhmmss, msec)
	   if (ok .ne. 1) goto 12
	   write(c8,'(i8.8)',iostat=ios) yyyymmdd
	   if (ios .ne. 0) goto 10
	   j = i + m
	   k = k3len(f)
	   n = max(j,k)
	   n = min(n,256)
	   o = 8
	   if (m .eq. 7) then ! eg.: omit the '19' in '1995'
	      o = 6
	      c8 = c8(3:8)
	   end if
	   if (i .gt. 1) then
	      g = f(1:i-1) // c8(1:o) // f(j:n)
	   else
	      g = c8 // f(j:n)
	   end if
	else
	   g = f
	end if

	if (sunos) then
	   j = index(g, ':')
	   if (j .gt. 1) then
	      call getenv( g(1:j-1), c256 )
	      k = k3len(c256)
              if (k .le. 1) goto 30
	      g = c256(1:k)//'/'//g(j+1:len(g))
	   end if
	end if
	if (macos_ppc) then
	   j = index(g, ':')
	   if (j .gt. 1) then
$IF ABSOFT_FORTRAN
	      call getenv( g(1:j-1), c256 )
$ELSE
	      call getenv( g(1:j-1), c256 )
$ENDIF
	      k = k3len(c256)
              if (k .le. 1) goto 30
	      g = c256(1:k)//'/'//g(j+1:len(g))
	   end if
	end if
	if (macos_intel) then
	   j = index(g, ':')
	   if (j .gt. 1) then
$IF ABSOFT_FORTRAN
	      call getenv( g(1:j-1), c256 )
$ELSE
	      call getenv( g(1:j-1), c256 )
$ENDIF
	      k = k3len(c256)
              if (k .le. 1) goto 30
	      g = c256(1:k)//'/'//g(j+1:len(g))
	   end if
	end if

	w_cdf_cnvrt_fn = 1
	if (do_ver .eq. 0) return
	if (do_ver .eq. 2) then
	   i = index(g, '$VER')
	   if (i .le. 0) return
	   j = i + 4
	   g = g(1:i-1)// '%%' // g(j:len(g))
	   return
	end if

	w_cdf_cnvrt_fn = 0
	i = index(g, '$VER')
	got_ver = i .ne. 0

	if (got_ver) then
	   j = i + 4
	   g = g(1:i-1) // '%%' // g(j:256)
	   n = k3len(g)
	   ok = w_resolve_filename(g(1:n))
	   if (ok .ne. 1) goto 40
!	   j = i + 4
!	   g = g(1:i-1) // 'xx' // g(j:256)
!	   n = k3len(g)
!	   exists = .false.
!	   last_version = 0
!	   k = highest_version
!	   do while (k .gt. 0 .and. .not. exists)
!	      g(i:i+1) = c2(k)
!	      inquire(file=g(1:n), exist=exists)
!	      if (exists) last_version = k
!	      k = k - 1
!	type *, g(1:n), k, exists
!	   end do
!	   if (last_version .eq. 0) goto 20
!	   g(i:i+1) = c2(last_version)
!	else if (vms) then
!	   inquire(file=g,exist=exists)
!	else if (unixos) then
!	   inquire(file=g,exist=exists)
!	else
!	   goto 40
	end if

	w_cdf_cnvrt_fn = 1

	return
  1	format(1x,'W_CDF_CNVRT_FN: ', a, :, i)
  2	format(1x,'W_CDF_CNVRT_FN: ', a, :, a)
 10	continue
	write(6,1,iostat=ios) 'bad context scet (1): ', exi(ch).ur8_context
	return
 12	continue
	write(6,1,iostat=ios) 'bad context scet (2): ', exi(ch).ur8_context
	return
! 20	continue
! this "error" will not be reported later, after the versioning strategy
! is finalized
!	write(6,1,iostat=ios) 'cannot determine latest CDF file version.'
!	k = k3len(g)
!	write(6,1,iostat=ios) 'File='//g(1:k)
!	return
 30	continue
	write(6,2,iostat=ios) 'environment symbol not defined: ', g(1:j-1)
	return
 40	continue
!	write(6,1,iostat=ios) 'unknown platform for file name resoltion.'
	write(6,1,iostat=ios) 'no match: ', g(1:n)
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	w_cdf_type_to_w_type
	1				(rv_type, w_rv_type)
	implicit	none
	integer*4	rv_type
	integer*4	w_rv_type
	include		'item_def.for'
$IF ABSOFT_FORTRAN
!
! The Absoft Fortran compiler fails on
!
!	include		'CDF_INC:cdf.inc'
!
! I haven't yet determined how to use an environment variable in Fortran,
! so I just spec'ed out a relative-path.  I agree - not the best.
!
	include		'../cdf/include/cdf.inc'
$ELSE
	include		'CDF_INC:cdf.inc'
$ENDIF

	w_cdf_type_to_w_type = 0

	if (rv_type .eq. CDF_REAL8 .or.
	1   rv_type .eq. CDF_DOUBLE .or.
	1   rv_type .eq. CDF_EPOCH) then
	   w_rv_type = p_double_return
	else if (rv_type .eq. CDF_REAL4 .or.
	1        rv_type .eq. CDF_FLOAT) then
	   w_rv_type = p_float_return
	else if (rv_type .eq. CDF_INT4 .or.
	1        rv_type .eq. CDF_UINT4) then
	   w_rv_type = p_int_return
	else if (rv_type .eq. CDF_INT2  .or.
	1        rv_type .eq. CDF_UINT2 .or.
	1        rv_type .eq. CDF_BYTE  .or.
	1        rv_type .eq. CDF_INT1  .or.
	1        rv_type .eq. CDF_UINT1      ) then
	   w_rv_type = p_int_return
	else
	   w_rv_type = 0
	   return
	end if

	w_cdf_type_to_w_type = 1
	return
	end
!------------------------------------------------------------------------------
! Converts CDF data type to Wind/Waves wind_lib data type
!
	integer*4	function	w_cdf_to_wiwav_cnvrt
	1		(buf, i, w_type, rv_type, fub)
	implicit	none
	include		'low_byte_def.for'
	include		'as_mdt_def.for'
	include		'item_def.for'
$IF ABSOFT_FORTRAN
!
! The Absoft Fortran compiler fails on
!
!	include		'CDF_INC:cdf.inc'
!
! I haven't yet determined how to use an environment variable in Fortran,
! so I just spec'ed out a relative-path.  I agree - not the best.
!
	include		'../cdf/include/cdf.inc'
$ELSE
	include		'CDF_INC:cdf.inc'
$ENDIF
	real*4		buf(*)			! w, caller's buffer
	integer*4	i
	integer*4	w_type
	integer*4	rv_type
	record /multi_type/ fub, x, cfub, wfub
	byte		i1
	integer*2	i2
	integer*4	i4
	real*4		r4
	real*8		r8
	integer*4	ok
	integer*4	w_ur8_from_epoch
	integer*4	o
	integer*4	w_wiwav_to_cdf_cnvrt	! an entry point
	integer*4	w_cdf_to_wiwav_buf_put	! an entry point

!	type 7, '...',w_type, rv_type
!  7	format(1x,a,2(2x,z8.8))

	!----------------------------------------------------------------------
	entry	w_cdf_to_wiwav_buf_put(buf, i, w_type, rv_type, fub)
!	w_cdf_to_wiwav_buf_put = 0
!	w_cdf_to_wiwav_buf_put = 1
!	return

	w_cdf_to_wiwav_cnvrt = 0

	if (rv_type .eq. CDF_REAL8 .or.
	1   rv_type .eq. CDF_DOUBLE) then
	!
	   if (w_type .eq. p_double_return) then
	      buf(i) = fub.r4
	      i = i + 1
	      buf(i) = fub.r4_2nd
	   else if (w_type .eq. p_float_return) then
	      buf(i) = fub.r8
	   else if (w_type .eq. p_int_return) then
	      x.i4 = fub.r8
	      buf(i) = x.r4
	   else
	      return
	   end if
	!
	else if (rv_type .eq. CDF_EPOCH) then
	   if (w_type .ne. p_double_return) return
	   ok = w_ur8_from_epoch(buf(i), fub.r8)
	   i = i + 1
	   if (ok .ne. 1) goto 20
	else if (rv_type .eq. CDF_REAL4 .or.
	1        rv_type .eq. CDF_FLOAT) then
	!
	   if (w_type .eq. p_float_return) then
	      buf(i) = fub.r4
	   else if (w_type .eq. p_double_return) then
	      x.r8 = fub.r4
	      buf(i) = x.r4
	      i = i + 1
	      buf(i) = x.r4_2nd
	   else if (w_type .eq. p_int_return) then
	      x.i4 = fub.r4
	      buf(i) = x.r4
	   else
	      return
	   end if
	!
	else
	   !
	   ! first convert CDF i*1 and i*2 to i*4, then to caller's type
	   !
	   if (rv_type .eq. CDF_INT4) then
	      i4 = fub.i4
	   else if (rv_type .eq. CDF_UINT4) then ! xxxxx expand for r*8
	      i4 = fub.i4
	   else if (rv_type .eq. CDF_INT2) then
	      i4 = fub.i2
	   else if (rv_type .eq. CDF_UINT2) then
	      i4 = zext(fub.i2)
	   else if (rv_type .eq. CDF_UINT1) then
	      i4 = zext(fub.b1)
	   else if (rv_type .eq. CDF_INT1) then
	      i4 = fub.b1
	   else if (rv_type .eq. CDF_BYTE) then
	      i4 = fub.b1
	   else
	      return
	   end if
	   if (w_type .eq. p_int_return) then
	      x.i4 = i4
	      buf(i) = x.r4
	   else if (w_type .eq. p_float_return) then
	      buf(i) = i4
	   else if (w_type .eq. p_double_return) then
	      x.r8 = i4
	      buf(i) = x.r4
	      i = i + 1
	      buf(i) = x.r4_2nd
	   else
	      return
	   end if
	end if

	w_cdf_to_wiwav_cnvrt = 1
	return
  1	format(1x, 'W_CDF_TO_WIWAV_CNVRT: ', a)
 20	continue
	write(6,1,iostat=o) 'cannot convert from CDF Epoch to UR8'
	w_cdf_to_wiwav_cnvrt = ok
	return

	!----------------------------------------------------------------------
	entry	w_wiwav_to_cdf_cnvrt(wfub, w_type, cfub, rv_type)
	! -never write to the high order elements unless required
	! -the cdf data type result is really intended for a w_item_* buffer,
	!  so 4-byte or 8-byte alignment is forced
	!...there may be need of an additional parameter to take care
	!...of endian ordering
	!- rewrite in C to handle to unsigned data types
	!- some of the following conversions may generate integer overflows

	w_wiwav_to_cdf_cnvrt = 0

	if (rv_type .eq. CDF_REAL8 .or.
	1   rv_type .eq. CDF_DOUBLE .or.
	1   rv_type .eq. CDF_EPOCH) then
	   ! we ignore ur8_to_epoch conversion...?
	   if (w_type .eq. p_double_return) then
	      cfub.r8 = wfub.r8
	   else if (w_type .eq. p_float_return) then
	      cfub.r8 = wfub.r4
	   else if (w_type .eq. p_int_return) then
	      cfub.r8 = wfub.i4
	   else
	      return
	   end if
	else if (rv_type .eq. CDF_REAL4 .or.
	1        rv_type .eq. CDF_FLOAT) then
	   if (w_type .eq. p_double_return) then
	      cfub.r4 = wfub.r8
	   else if (w_type .eq. p_float_return) then
	      cfub.r4 = wfub.r4
	   else if (w_type .eq. p_int_return) then
	      cfub.r4 = wfub.i4
	   else
	      return
	   end if
	else if (rv_type .eq. CDF_INT4  .or.
	1        rv_type .eq. CDF_UINT2 .or.
	1        rv_type .eq. CDF_UINT1 .or.
	1        rv_type .eq. CDF_UINT4) then
	   if (w_type .eq. p_double_return) then
	      cfub.i4 = wfub.r8
	   else if (w_type .eq. p_float_return) then
	      cfub.i4 = wfub.r4
	   else if (w_type .eq. p_int_return) then
	      cfub.i4 = wfub.i4
	   else
	      return
	   end if
	else if (rv_type .eq. CDF_INT2) then
	   if (w_type .eq. p_double_return) then
	      i2 = wfub.r8
	   else if (w_type .eq. p_float_return) then
	      i2 = wfub.r4
	   else if (w_type .eq. p_int_return) then
	      i2 = wfub.i4
	   else
	      return
	   end if
	   cfub.i4 = i2
	else if (rv_type .eq. CDF_BYTE  .or.
	1        rv_type .eq. CDF_INT1  ) then
	   if (w_type .eq. p_double_return) then
	      i1 = wfub.r8
	   else if (w_type .eq. p_float_return) then
	      i1 = wfub.r4
	   else if (w_type .eq. p_int_return) then
	      i1 = wfub.i4
	   else
	      return
	   end if
	   cfub.i4 = i1
	else
	   return
	end if
	w_wiwav_to_cdf_cnvrt = 1
	return
	end
!------------------------------------------------------------------------------
! This function performs a binary search of an open cdf file.  Argument rv_n
! is the cdf variable number.  This rVariable should have no variance, that is,
! rv_n represents an "independent" variable whose values change in a 
! monotonically increasing manner with increasing cdf record number.
! This subprogram may hang or produce incorrect results if the specified cdf 
! rVariable does not meet the above conditions.
! This function returns -1 if the target is prior to BOF, 0 if the target
! is equal to or between the rV values at the file boundries, 1 if the
! target is after EOF, or -99 on error.
!------------------------------------------------------------------------------
	integer*4	function	w_cdf_bracket_irv
	1		(id, irv_name, indices, dim_sizes, idx, 
	1		w_compare, trg, rb,re, flr,ceil)
	implicit	none
	include		'low_byte_def.for'
	include		'as_mdt_def.for'
$IF ABSOFT_FORTRAN
!
! The Absoft Fortran compiler fails on
!
!	include		'CDF_INC:cdf.inc'
!
! I haven't yet determined how to use an environment variable in Fortran,
! so I just spec'ed out a relative-path.  I agree - not the best.
!
	include		'../cdf/include/cdf.inc'
$ELSE
	include		'CDF_INC:cdf.inc'
$ENDIF
	integer*4	id			! r, cdf id number
	character*(*)	irv_name		! r, cdf zVariable name
	integer*4	indices(0:*)		! r, cdf rV indices
	integer*4	dim_sizes(0:*)		! r, cdf sizes of each dimens.
	integer*4	idx			! r, dim index that varies
	integer*4	w_compare		! r, comparison function
	external	w_compare
	record /multi_type/ trg			! r, target value
	integer*4	rb			! rw, rec# begin = lower limit
	integer*4	re			! rw, rec# end = upper limit
	record /multi_type/ flr			! w, rV value for bracket floor
	record /multi_type/ ceil		! w, rV value for bracket ceilng

	integer*4	ios
	integer*4	ok, ok2
	integer*4	mid
	record /multi_type/ x
	character*60	msg
	integer*4	i,j
$IF ABSOFT_FORTRAN
!	integer*4	cdf_lib
$ELSE
	integer*4	cdf_lib
$ENDIF

	integer*4	target_lt
	integer*4	target_eq
	integer*4	target_gt
	parameter	(target_lt = -1)
	parameter	(target_eq = 0)
	parameter	(target_gt = 1)

	! get the boundary conditions from the dimension sizes array
	rb = 1
	re = dim_sizes(idx)

	! check the lower boundry condition
	indices(idx) = rb
!#	call cdf_var_get(id, irv_n, indices(0), indices(1), flr, ok)
!#	if (ok .ne. cdf_ok) goto 100
$IF ABSOFT_FORTRAN
$IF LITTLE_ENDIAN_ARCH
	ok = cdf_lib( SELECT_, CDF_, id, 
$ELSE
	call cdf_lib_( SELECT_, CDF_, id, 
$ENDIF
$ELSE
	ok = cdf_lib( SELECT_, CDF_, id, 
$ENDIF
	1                      zVAR_NAME_,       irv_name,
	1                      zVAR_RECNUMBER_,  indices(0),
	1                      zVAR_DIMINDICES_, indices(1),
	1             GET_,    zVAR_DATA_, flr,
	1             NULL_,   ok2)
	if (ok .ne. cdf_ok) goto 100
	if (ok2 .ne. cdf_ok) goto 101
	ok = w_compare(flr, trg)
	if (ok .eq. target_lt .or. ok .eq. target_eq) then
	   re = rb
	   ceil = flr
	   w_cdf_bracket_irv = -1
	   if (ok .eq. target_eq) w_cdf_bracket_irv = 0
	   return
	end if

	! check the upper boundry condition
	indices(idx) = re
!#	call cdf_var_get(id, irv_n, indices(0), indices(1), ceil, ok)
!#	if (ok .ne. cdf_ok) goto 100
$IF ABSOFT_FORTRAN
$IF LITTLE_ENDIAN_ARCH
	ok = cdf_lib( SELECT_, CDF_, id, 
$ELSE
	call cdf_lib_( SELECT_, CDF_, id, 
$ENDIF
$ELSE
	ok = cdf_lib( SELECT_, CDF_, id, 
$ENDIF
	1                      zVAR_NAME_,       irv_name,
	1                      zVAR_RECNUMBER_,  indices(0),
	1                      zVAR_DIMINDICES_, indices(1),
	1             GET_,    zVAR_DATA_, ceil,
	1             NULL_,   ok2)
	if (ok .ne. cdf_ok) goto 100
	if (ok2 .ne. cdf_ok) goto 101
	ok = w_compare(ceil, trg)
	if (ok .eq. target_gt .or. ok .eq. target_eq) then
	   rb = re
	   flr = ceil
	   w_cdf_bracket_irv = 1
	   if (ok .eq. target_eq) w_cdf_bracket_irv = 0
	   return
	end if

	! do the binary search
	i = 0
	j = re-rb+1 ! really want log base 2 of (re-rb), but this is ok
	do while ((re-rb) .gt. 1)
	   i = i + 1
	   if (i .gt. j) goto 200
	   mid = rb + (re-rb) / 2
	   indices(idx) = mid
!#	   call cdf_var_get(id, irv_n, indices(0), indices(1), x, ok)
!#	   if (ok .ne. cdf_ok) goto 100
$IF ABSOFT_FORTRAN
$IF LITTLE_ENDIAN_ARCH
	   ok = cdf_lib( SELECT_, CDF_, id, 
$ELSE
	   call cdf_lib_( SELECT_, CDF_, id, 
$ENDIF
$ELSE
	   ok = cdf_lib( SELECT_, CDF_, id, 
$ENDIF
	1                      zVAR_NAME_,       irv_name,
	1                      zVAR_RECNUMBER_,  indices(0),
	1                      zVAR_DIMINDICES_, indices(1),
	1             GET_,    zVAR_DATA_, x,
	1             NULL_,   ok2)
	   if (ok .ne. cdf_ok) goto 100
	   if (ok2 .ne. cdf_ok) goto 101
!	type *, '...bsearch, got rec#', mid
	   ok = w_compare(x, trg)
	   if (ok .eq. target_lt) then
	      ceil = x
	      re = mid
	   else if (ok .eq. target_gt) then
	      flr = x
	      rb = mid
	   else
	      ceil = x
	      re = mid
	      flr = x
	      rb = mid
	   end if
	end do

	w_cdf_bracket_irv = 0		! zero is success
	return
   1	format(1x,'W_CDF_BRACKET_IRV: ', a, :, i5)
 100	continue
	write(6,1,iostat=ios) 'cannot do cdf_var_get (a).'
$IF ABSOFT_FORTRAN
$IF LITTLE_ENDIAN_ARCH
	call cdf_error(ok, msg)
$ELSE
	call cdf_error_(ok, msg)
$ENDIF
$ELSE
	call cdf_error(ok, msg)
$ENDIF
	write(6,1,iostat=ios) msg
	w_cdf_bracket_irv = -99
	return
 101	continue
	write(6,1,iostat=ios) 'cannot do cdf_var_get (b).'
$IF ABSOFT_FORTRAN
$IF LITTLE_ENDIAN_ARCH
	call cdf_error(ok2, msg)
$ELSE
	call cdf_error_(ok2, msg)
$ENDIF
$ELSE
	call cdf_error(ok2, msg)
$ENDIF
	write(6,1,iostat=ios) msg
	w_cdf_bracket_irv = -99
	return
 200	continue
	write(6,1,iostat=ios) 'too many iterations, rV not increasing:', j
	w_cdf_bracket_irv = -99
	return
	end
!------------------------------------------------------------------------------
! wind cdf filename and id buffer functions
!------------------------------------------------------------------------------
	integer*4	function	w_cdf_fn_id_buf_fun()
	implicit	none
	character*(*)	f
	integer*4	id
	integer*4	prev_key /0/
	integer*4	sequence /0/
	integer*4	max_files
	parameter	(max_files=10)
	structure /saved_file_info/
	   integer*4	id
	   integer*4	sequence
	   character*256 f
	end structure
	record /saved_file_info/ sfi(max_files)
$IF ABSOFT_FORTRAN
	character*256	cmp_f
$ENDIF
	integer*4	i,j,k,n,o
	integer*4	status
	integer*4	w_cdf_fn_match		! an entry point
	integer*4	w_cdf_fn_save		! an entry point
	integer*4	w_cdf_fn_report		! an entry point
	integer*4	n_quick /0/
	integer*4	n_calls /0/
$IF ABSOFT_FORTRAN
!
! The Absoft Fortran compiler fails on
!
!	include		'CDF_INC:cdf.inc'
!
! I haven't yet determined how to use an environment variable in Fortran,
! so I just spec'ed out a relative-path.  I agree - not the best.
!
	include		'../cdf/include/cdf.inc'
$ELSE
	include		'CDF_INC:cdf.inc'
$ENDIF
	character	msg*(cdf_statustext_len)
	integer*4	k3len
	integer*4	ok

	!----------------------------------------------------------------------
	entry w_cdf_fn_report()
	write(6,*) 'w_cdf_fn_report: n_calls, n_quick = ', n_calls, n_quick
	w_cdf_fn_report = 1
	return

	!----------------------------------------------------------------------
	entry	w_cdf_fn_match(f, id)
	w_cdf_fn_match = 1
	n_calls = n_calls + 1
	if (prev_key .ne. 0) then
	   ! test the most recently used file name for a match
$IF ABSOFT_FORTRAN
           cmp_f = sfi(prev_key).f
	   if (cmp_f .eq. f) then
$ELSE
	   if (sfi(prev_key).f .eq. f) then
$ENDIF
	      id = sfi(prev_key).id
	      n_quick = n_quick + 1
	      return
	   end if
	end if
	do i=1,max_files
$IF ABSOFT_FORTRAN
           cmp_f = sfi(prev_key).f
	   if (cmp_f .eq. f) then
$ELSE
	   if (sfi(i).f .eq. f) then
$ENDIF
	      id = sfi(i).id
	      prev_key = i
	      return
	   end if
	end do
	w_cdf_fn_match = 0
	return

	!----------------------------------------------------------------------
	entry	w_cdf_fn_save(f, id)
	w_cdf_fn_save = 1

	sequence = sequence + 1
	j = '7ffffff'x
	do i=1,max_files
	   if (sfi(i).sequence .eq. 0) then
	      prev_key = i
	      sfi(i).sequence = sequence
	      sfi(i).id = id
	      sfi(i).f = f
	      return
	   end if
	   if (sfi(i).sequence .lt. j) then
	      j = sfi(i).sequence
	      k = sfi(i).id
	      n = i
	   end if
	end do

$IF ABSOFT_FORTRAN
$IF LITTLE_ENDIAN_ARCH
	call cdf_close(k, ok)
$ELSE
	call cdf_close_(k, ok)
$ENDIF
$ELSE
	call cdf_close(k, ok)
$ENDIF
	if (ok .ne. CDF_OK) goto 40
	prev_key  = n
	sfi(n).id = id
	sfi(n).f  = f
	sfi(n).sequence = sequence

	return
  1	format(1x,'W_CDF_FN_SAVE: ', a, :, i)
 40	continue
	write(6,1,iostat=o) 'error calling CDF_CLOSE, ok=', ok
$IF ABSOFT_FORTRAN
$IF LITTLE_ENDIAN_ARCH
	call cdf_error(ok, msg)
$ELSE
	call cdf_error_(ok, msg)
$ENDIF
$ELSE
	call cdf_error(ok, msg)
$ENDIF
	i = k3len(msg)
	write(6,1,iostat=o) msg(1:i)
	return
	end
