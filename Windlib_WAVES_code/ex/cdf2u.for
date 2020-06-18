! cdf2.for - just lists some data

	program	cdf2
	implicit	none
	include		'CDF_INC:cdf.inc'
	character*60	file /'wind_mfi:wi_k0_mfi_19950214_v01.cdf'/
	integer*4	indices(cdf_max_dims)

	integer*4	id
	integer*4	n_dim			! # of dimensions, rVariables
	integer*4	dim_sizes(cdf_max_dims)	! dimension sizes, rVariables
	integer*4	encoding		! data encoding
	integer*4	majority		! variable majority, col or row
	integer*4	max_rec			! last record number
	integer*4	n_rv			! # rVariables
	integer*4	n_attributes		! # attributes

	integer*4	irv_n			! rVariable id number
	integer*4	irv_type		! rVariable data type
	integer*4	irv_n_el		! # elements of data type
	integer*4	irv_variance		! record variance
	integer*4	irv_dim_var(cdf_max_dims)	! dimension variance
	character*32	irv_name		! rVariable name
	real*8		r8

	integer*4	drv_n			! rVariable id number
	integer*4	drv_type		! rVariable data type
	integer*4	drv_n_el		! # elements of data type
	integer*4	drv_variance		! record variance
	integer*4	drv_dim_var(cdf_max_dims)	! dimension variance
	character*32	drv_name		! rVariable name
	real*4		r4(3)

	integer*4	i,j,k,m,n
	integer*4	ok
	character*80	msg
	real*8		scet
!	integer*4	w_ur8_from_epoch

	irv_name = 'Epoch'
	drv_name = 'BGSEc'

	call cdf_open(file, id, ok)
	if (ok .ne. cdf_ok) then
	   call cdf_error(ok, msg)
	   type *, msg
	   stop
	end if

	! get the file info
	call cdf_inquire(id,
	1		n_dim,
	1		dim_sizes,
	1		encoding,
	1		majority,
	1		max_rec,
	1		n_rv,
	1		n_attributes,
	1		ok)
	if (ok .ne. cdf_ok) then
	   call cdf_error(ok, msg)
	   type *, msg
	   stop
	end if

	type *, file
	type *, '...Data set...'
	type *, 'Encoding: ', encoding
	type *, 'Majority: ', majority
	type *, 'Max Rec : ', max_rec
	type *, 'N dim   : ', n_dim
	type 9, 'dim_sizes:', dim_sizes(1), dim_sizes(2), dim_sizes(3),
	1	dim_sizes(4), dim_sizes(5)
  9	format(1x,a, 5(i5))


	! get the independent variable info
	irv_n = cdf_var_num(id, irv_name)
	if (irv_n .lt. 1) then
	   call cdf_error(ok, msg)
	   type *, msg
	   stop
	end if
	call cdf_var_inquire(id,
	1		irv_n,
	1		irv_name,
	1		irv_type,
	1		irv_n_el,
	1		irv_variance,
	1		irv_dim_var,
	1		ok)
	if (ok .ne. cdf_ok) then
	   call cdf_error(ok, msg)
	   type *, msg
	   stop
	end if

	type *, '...irv...'
	type *, 'irv #   : ', irv_n
	type *, 'name    : ', irv_name
	type *, 'type    : ', irv_type
	type *, 'n elemen: ', irv_n_el
	type *, 'variance: ', irv_variance
	type 9, 'dim var : ', irv_dim_var(1), irv_dim_var(2), irv_dim_var(3), 
	1	irv_dim_var(4), irv_dim_var(5)

	! get the dependent variable info
	drv_n = cdf_var_num(id, drv_name)
	if (drv_n .lt. 1) then
	   call cdf_error(ok, msg)
	   type *, msg
	   stop
	end if
	call cdf_var_inquire(id,
	1		drv_n,
	1		drv_name,
	1		drv_type,
	1		drv_n_el,
	1		drv_variance,
	1		drv_dim_var,
	1		ok)
	if (ok .ne. cdf_ok) then
	   call cdf_error(ok, msg)
	   type *, msg
	   stop
	end if

	type *, '...drv...'
	type *, 'drv #   : ', drv_n
	type *, 'name    : ', drv_name
	type *, 'type    : ', drv_type
	type *, 'n elemen: ', drv_n_el
	type *, 'variance: ', drv_variance
	type 9, 'dim var : ', drv_dim_var(1), drv_dim_var(2), drv_dim_var(3), 
	1	drv_dim_var(4), drv_dim_var(5)


	type 1, 'Rec#', 'Epoch', 'BGSEc 1', 'BGSEc 2', 'BGSEc 3', 'SCET'
	type 1, '----',
	1	'------------------------',
	1	'------------',
	1	'------------',
	1	'------------',
	1	'--------------'

	do i=1,20 ! ,max_rec
	   indices(1) = 1
	   call cdf_var_get(id, irv_n, i, indices, r8, ok)
	   if (ok .ne. cdf_ok) then
	      type *, '...cannot get value for irv...', i
	      call cdf_error(ok, msg)
	      type *, msg
	      stop
	   end if
!	   ok = w_ur8_from_epoch(scet, r8)
	   do j=1,3
	      indices(1) = j
	      call cdf_var_get(id, drv_n, i, indices, r4(j), ok)
	      if (ok .ne. cdf_ok) then
	         type *, '...cannot get value for drv...', i, j
	         call cdf_error(ok, msg)
	         type *, msg
	         stop
	      end if
	   end do
	   type 2, i, r8, r4(1), r4(2), r4(3), scet
	end do

 1	format(1x,a4, 1x, a18,   3(1x,a10),   1x, a14)
 2	format(1x,i4, 1x, f18.2, 3(1x,f10.5), 1x, f14.8)
	end
