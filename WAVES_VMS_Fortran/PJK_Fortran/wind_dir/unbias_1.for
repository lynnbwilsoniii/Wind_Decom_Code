	subroutine unbias(fft_chan,mantissa,exponent,raw_voltages)
*	Purpose;
*	To remove bias from raw data that is not 32-bit integers

	implicit none
 
	real*4		offsets(40)
	real*4		temp
	real*4		raw_voltages(1024)
	integer*4	mantissa(1024)
	integer*4	fft_chan
	integer*4	exponent(1024)
*	integer*4	w_item_r4
	integer*4	iloop
	integer*4	nex
	integer*4	ntemp


	data offsets/	2048., 2050., 2046., 2048.,
	1		2048., 2050., 2046., 2051.,
	1		2048., 2082., 2041., 2043.,
	1		2048., 2078., 2042., 2046.,
	1		2048., 2048., 2048., 2047.,
	1		2048., 2075., 2043., 2045.,
	1		2048., 2048., 2048., 2037.,
	1		2048., 2048., 2048., 2040.,
	1		2048., 2048., 2048., 2036.,
	1		2048., 2048., 2287., 2048./

*	USE THESE TO CALCULATE THE RAW VOLTAGES
*	ok       = w_item_r4(ch,'OFFSETS',offsets,size40,size_out)
	   
	do iloop = 1,1024
	  ntemp  = mantissa(iloop) .and. 4095
	  nex    = exponent(iloop) .and. 3
	  temp   = -(ntemp - offsets((fft_chan-1)*4 + nex+1))
	  raw_voltages(iloop) = temp * (-16.3**nex)
	enddo
	return
	end

