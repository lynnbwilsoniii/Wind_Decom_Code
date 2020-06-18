! wind_string_lib.for - some string manipulating routines used by several
! programs.  These routines are intended to be linked to via the shareable
! library userdir:wind_shr.
!
! Jon Kappler, 1990


*-------------------------------------------------------------------------------
* sundry string routines
*-------------------------------------------------------------------------------

*-------------------------------------------------------------------------------
	integer*4	function	k2len(text)
! returns "useable" string length by subtracting the number of trailing spaces
! and/or nulls and/or tabs from the the allocated string length.
	character*(*)	text
!	character	null*1, tab*1
!	parameter	(null=char(0))
!	parameter	(tab=char(9))
	k2len = len(text)
	do while(k2len.ge.1 .and. (text(k2len:k2len) .le. ' '))
	   k2len = k2len - 1
	end do
!	do while(k2len.ge.1 .and. 
!	1	(text(k2len:k2len) .eq. ' ' .or. 
!	1	text(k2len:k2len)  .eq. tab .or.
!	1	text(k2len:k2len)  .eq. null))
!	   k2len = k2len - 1
!	end do
	return
	end

*-------------------------------------------------------------------------------
	integer*4	function	k3len(text)
! returns "useable" string length by subtracting the number of trailing
! non-printable control characters and/or white space characters.
	character*(*)	text
	k3len = len(text)
	do while(k3len.ge.1 .and. (text(k3len:k3len) .le. ' '))
	   k3len = k3len - 1
	end do
	return
	end

*-------------------------------------------------------------------------------
	subroutine translate(string,old_char,new_char, i_start, i_stop)
! Replaces all occurances of character old_char with character new_char
! in string string over rang i_start to i_stop.  Pass i_start and i_stop
! as zero to process the full length of string string.
	character*(*)	string
	character*1	new_char, old_char
	integer*4	istart, istop, i
	istart = i_start
	istop = i_stop
	if (istart .le. 0) istart = 1
	if (istop  .le. 0) istop  = len(string)
	do i=istart,istop
	   if (string(i:i) .eq. old_char) string(i:i) = new_char
	end do
	return
	end

*-------------------------------------------------------------------------------
	subroutine to_lower(string, i_start, i_stop)
* Replace upper case letters with lower case over range string(i_start:i_stop).
* Pass i_start and i_stop as zero to process the full length of string string.
	character*(*)	string
	integer*4	istart, istop
	integer*4	i
	istart = i_start
	istop = i_stop
	if (istart .le. 0) istart = 1
	if (istop  .le. 0) istop  = len(string)
	do i=istart,istop
           if (string(i:i) .ge. 'A' .and. string(i:i) .le. 'Z') 
	1      string(i:i) = char(ichar(string(i:i)) + 32)
	end do
	return
	end

*-------------------------------------------------------------------------------
	subroutine to_upper(string, i_start, i_stop)
* Replace lower case letters with upper case over range string(i_start:i_stop).
* Pass i_start and i_stop as zero to process the full length of string string.
	character*(*)	string
	integer*4	istart, istop, i
	istart = i_start
	istop = i_stop
	if (istart .le. 0) istart = 1
	if (istop  .le. 0) istop  = len(string)
	do i=istart,istop
           if (string(i:i) .ge. 'a' .and. string(i:i) .le. 'z') 
	1      string(i:i) = char(ichar(string(i:i)) - 32)
	end do
	return
	end

*-------------------------------------------------------------------------------
	subroutine collapse(source, slen, delim)
! -removes multiple occurances of delim over range in source
!  by collapsing the string toward the lower indecies.
! -modifies argument slen
	character*(*)	source
	character*1	delim
	integer*4	slen
	character*2	double_delim
	integer*4	i, shift
	integer*4	ok

	if (slen .eq. 0) return
	double_delim = delim//delim

	! get the leading shift (number of leadings delim chars)
	ok = 1
	i = 1
	do while(ok)
	   if (source(i:i) .eq. delim) then
	      i = i + 1
	      if (i .gt. len(source)) then
	         i = 0
	         ok = 0
	      end if
	   else
	      ok = 0
	   end if
	end do
	if (i .eq. 0) then
	   slen = 0
	   return
	end if
	shift = i - 1

	! move everything left shift number of chars and check for more shift
	if (shift .gt. 0) source(i-shift:i-shift) = source(i:i)
	i = i + 1
	do i=i,slen
	   if (source(i-1:i) .eq. double_delim) shift = shift + 1
	   if (shift .gt. 0) source(i-shift:i-shift) = source(i:i)
	end do

	! blank out the last portion of source if shifted
	if (shift .gt. 0) then
	   source(slen-shift+1:) = delim
	end if

	! evaluate the source string's length
	do while(source(slen:slen) .eq. delim)
	   slen = slen - 1
	end do

	return
	end

*-----------------------------------------------------------------------------
	integer*4	function	next_word(s,i,j)
! Returns indexes i and j to single space delimited words in string s.
! Indexes i and j should be zero on first call (first parse of a particular
! string).  Subsequent calls to next_word increment i and j (i = j + 2, for
! j > 0 on entry).
	implicit	integer*4 (a-z)
	character*(*)	s

	! assumes one space between each word
	next_word = 0

	if (i.lt.1 .or. j.lt.1) j = -1
	i = j + 2
	if (i .gt. len(s)) return
	if (s(i:i) .eq. ' ') return
	j = index(s(i:),' ')
	if (j .gt. 0) then
	   j = i + j - 2
	else
	  j = len(s)
	end if

	next_word = 1

	return
	end


*-------------------------------------------------------------------------------
	integer*4	function	count(big, small)
! count occurances of small string in big string
	character*(*)	big, small
	integer*4	lb, ls, i, j
	count=0
	lb=len(big)
	ls=len(small)
	i=1
	j=1
	do while(i .gt. 0 .and. j .le. lb)
	   i = index(big(j:),small)
	   if (i .gt. 0) then
	     count = count + 1
	     j = j + i + ls -1
	   end if
	end do
	return
	end


!------------------------------------------------------------------------------
	logical*4	function	there_exists_a(head,tail,str,i,j)
! This function returns indexes to a substring (field) of argument str bounded
! by the substrings head and tail.  Immediate return value of function 
! indicates success/failure of field location operation.
! Trailing whitespace in arguments head and tail is significant.
	implicit	none
	character*(*)	head		! a search string, beginning of field
	character*(*)	tail		! a search string, end of field
	character*(*)	str		! string to search for head and tail
	integer*4	i		! index of 1st char after head
	integer*4	j		! index of 1st char before tail

	i = 0
	j = 0
	there_exists_a = .false.
	if (len(str) .lt. 1) return
	if (len(head) .lt. 1) return
	if (len(tail) .lt. 1) return
	if (len(head) + len(tail) .ge. len(str)) return

	i = index(str,head)
	if (i .eq. 0) return
	i = i + len(head)

	j = index(str(i:), tail)
	if (j .eq. 0) then
	   i = 0
	   return
	end if
	j = i + j - 2

	there_exists_a = .true.
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	wind_bsearch
	1				(a,size1a,size,key,answer,compare)
!
! Uses a binary search to find the specified key in a sorted array of fixed-
! length data items.
! For ascending data elements compare function should return:
!	-1 when the key is less than the data item value
!	0  when the key is equal to the data item value
!	1  when the key is greater than the data item value
! Argument answer is the array index (adjusted for size of data items).
! Answer will index the earliest key of a duplicate key series.
!
	implicit	none
	byte		a(*)		! data area (address of) to search
	integer*4	size1a		! size of one element of a
	integer*4	size		! size of area to search (in elements)
	integer*4	key		! address of key to find
	integer*4	answer		! index into a that matches key
	integer*4	compare		! address of a comparison function
	external	compare
	integer*4	hi,lo,status,i
$IF ABSOFT_FORTRAN
	integer*4	key_less_than,key_equal_to,key_greater_than
	parameter	(key_less_than    = -1)
	parameter	(key_equal_to     = 0)
	parameter	(key_greater_than = 1)
$ELSE
	parameter	key_less_than    = -1
	parameter	key_equal_to     = 0
	parameter	key_greater_than = 1
$ENDIF
	integer*4	midpoint, get_byte_index
	midpoint(lo,hi) = max(lo, ((hi+lo)/2) )
	get_byte_index(size1a, i) = (i*size1a) - size1a + 1

	answer = 0
	wind_bsearch = 0
	if (size .lt. 1) return

	lo = 1
	hi = size
	answer = midpoint(lo,hi)
	i      = get_byte_index(size1a,answer)
	status = compare(a(i),key)
	do while(status .ne. key_equal_to)
	   if (hi .le. lo) return
	   if (status .eq. key_greater_than) then
	      lo = answer + 1
	   else
	      hi = answer
	   end if
	   answer = midpoint(lo,hi)
	   i      = get_byte_index(size1a,answer)
	   status = compare(a(i),key)
	end do

	wind_bsearch = 1
	return
	end


!------------------------------------------------------------------------------
	integer*4	function	i4_heapsort(a,sz)
	implicit	none
	integer*4	a(*)
	integer*4	sz
	integer*4	i,j,k,n
	integer*4	x,y
	integer*4	largest, heapsize
	integer*4	tmp
	integer*4	parent, left, right
!	parent(i) = i / 2
	left(i) = 2 * i
	right(i) = (2 * i) + 1

	i4_heapsort = 1
	if (sz .le. 1) return

  9	format(1x,'H=', 12(i4))

	n = sz
	heapsize = sz

	! heapify
	do j=n/2,1,-1
	   i = j
 200	   continue
	   x = left(i)
	   y = right(i)
	   if (x .le. heapsize .and. a(x) .gt. a(i)) then
	      largest = x
	   else
	      largest = i
	   end if
	   if (y .le. heapsize .and. a(y) .gt. a(largest)) then
	      largest = y
	   end if
	   if (largest .ne. i) then
	      tmp = a(i)
	      a(i) = a(largest)
	      a(largest) = tmp
	      i = largest
              goto 200
	   end if
	end do

	! sort
	do j=n,2,-1
	   i = j
	   tmp = a(1)
	   a(1) = a(i)
	   a(i) = tmp
	   heapsize = heapsize - 1
	   i = 1
 400	   continue
	   x = left(i)
	   y = right(i)
	   if (x .le. heapsize .and. a(x) .gt. a(i)) then
	      largest = x
	   else
	      largest = i
	   end if
	   if (y .le. heapsize .and. a(y) .gt. a(largest)) then
	      largest = y
	   end if
	   if (largest .ne. i) then
	      tmp = a(i)
	      a(i) = a(largest)
	      a(largest) = tmp
	      i = largest
              goto 400
	   end if
	end do

	end

!------------------------------------------------------------------------------
	integer*4	function	ch_heapsort(a,sz,tmp)
	implicit	none
	character*(*)	a(*)
	integer*4	sz
	character*(*)	tmp
	integer*4	i,j,k,n
	integer*4	x,y
	integer*4	largest, heapsize
	integer*4	parent, left, right
!	parent(i) = i / 2
	left(i) = 2 * i
	right(i) = (2 * i) + 1

	ch_heapsort = 1
	if (sz .le. 1) return

  9	format(1x,'H=', 12(i4))

	n = sz
	heapsize = sz

	! heapify
	do j=n/2,1,-1
	   i = j
 200	   continue
	   x = left(i)
	   y = right(i)
	   if (x .le. heapsize .and. a(x) .gt. a(i)) then
	      largest = x
	   else
	      largest = i
	   end if
	   if (y .le. heapsize .and. a(y) .gt. a(largest)) then
	      largest = y
	   end if
	   if (largest .ne. i) then
	      tmp = a(i)
	      a(i) = a(largest)
	      a(largest) = tmp
	      i = largest
              goto 200
	   end if
	end do

	! sort
	do j=n,2,-1
	   i = j
	   tmp = a(1)
	   a(1) = a(i)
	   a(i) = tmp
	   heapsize = heapsize - 1
	   i = 1
 400	   continue
	   x = left(i)
	   y = right(i)
	   if (x .le. heapsize .and. a(x) .gt. a(i)) then
	      largest = x
	   else
	      largest = i
	   end if
	   if (y .le. heapsize .and. a(y) .gt. a(largest)) then
	      largest = y
	   end if
	   if (largest .ne. i) then
	      tmp = a(i)
	      a(i) = a(largest)
	      a(largest) = tmp
	      i = largest
              goto 400
	   end if
	end do

	end

!------------------------------------------------------------------------------
	integer*4	function	i4_heapsort_bad(ac,n)
! Uses the heapsort algorithm to sort the array ac of n integers.
	implicit	none
	integer*4	ac(*)
	integer*4	n
	integer*4	i, ir, j, l, cac

	i4_heapsort_bad = 0
	if (n.eq.0) return
	i4_heapsort_bad = 1

	l  = (n/2) + 1
	ir = n

 100	if (l .gt. 1) then
	   l = l - 1
	   cac = ac(l)
	else
	   cac = ac(ir)
	   ac(ir) = ac(1)
	   ir = ir - 1
	   if (ir .eq. 1) then
	      ac(1) = cac
	      return					! return
	   end if
	end if
	i = l
	j = l + l

 200	if (j .le. ir) then
	   if (j .lt. ir) then
	      if (ac(j) .lt. ac(j+1)) j = j + 1
	   end if
	   if (cac .lt. ac(j)) then
	      ac(i) = ac(j)
	      i = j
	      j = j + 1
	   else
	      j = ir + 1
	   end if
	   goto 200
	end if
	ac(i) = cac
	goto 100

	end

!------------------------------------------------------------------------------
	integer*4	function	ch_heapsort_bad(ac,n,cac)
! Uses the heapsort algorithm to sort the n strings represented by array ac.
! Argument cac is a buffer large enough to hold any element of ac.
	implicit	none
	character*(*)	ac(*), cac
	integer*4	n
	integer*4	i, ir, j, l

	ch_heapsort_bad = 1
	if (n.eq.0) return

	l  = (n/2) + 1
	ir = n

 100	if (l .gt. 1) then
	   l = l - 1
	   cac = ac(l)
	else
	   cac = ac(ir)
	   ac(ir) = ac(1)
	   ir = ir - 1
	   if (ir .eq. 1) then
	      ac(1) = cac
	      return					! return
	   end if
	end if
	i = l
	j = l + l

 200	if (j .le. ir) then
	   if (j .lt. ir) then
	      if (ac(j) .lt. ac(j+1)) j = j + 1
	   end if
	   if (cac .lt. ac(j)) then
	      ac(i) = ac(j)
	      i = j
	      j = j + 1
	   else
	      j = ir + 1
	   end if
	   goto 200
	end if
	ac(i) = cac
	goto 100

	end

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! routines from script player
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
	integer*4	function	remove_comments(s,n,is_continued)
! Comments begin with a semicolon or exclamation mark.  Two adjacent
! semicolons or '!;' indicates
! a continuation line.  Constant character text is between double quote
! marks.  Semicolons and exclamation marks within character constants are
! ignored.
	character	s*(*)
	integer*4	n
	integer*4	is_continued
	integer*4	in_quotes
	integer*4	looking_for_first_quote
	integer*4	k
	integer*4	i

	remove_comments = 1

	i = 0
	k = n
	is_continued = 0
	in_quotes    = 0
	looking_for_first_quote = 1
	if (n.lt.1) return
	do while(i.lt.k)
	   i = i + 1
	   if (looking_for_first_quote .and. s(i:i) .eq. '"') then
	      in_quotes = 1
	      looking_for_first_quote = 0
	   else if (s(i:i) .eq. '"') then
	      in_quotes = 0
	      looking_for_first_quote = 1
	   end if
	   if (.not. in_quotes .and.
	1     ( s(i:i) .eq. ';' .or. s(i:i) .eq. '!') ) then
	      n = i - 1
	      if (i.lt.k) is_continued = s(i:i+1) .eq. ';;'
	      return
	   end if
	end do

	return
	end

!------------------------------------------------------------------------------
	logical*4	function	valid_characters(s)
! This routine trues if all characters of strings are from the set:
!
!	{0..9, a..z, A..Z, underscore}
!
	implicit	none
	character	s*(*)		! input string argument
	integer*4	i		! loop index
	integer*4	k2len		! a string len function
	logical*4	is_digit		! a statement function
	logical*4	is_alpha		! a statement function
	logical*4	is_underscore		! a statement function
	character*1	c			! statement function argument
	is_digit(c) = c .ge. '0' .and. c .le. '9'
	is_alpha(c) = (c .ge. 'A' .and. c .le. 'Z') .or.
	1		(c .ge. 'a' .and. c .le. 'z')
	is_underscore(c) = c .eq. '_'

	valid_characters = .false.

	do i=1,k2len(s)
	   if (.not. (
	1	is_digit(s(i:i)) .or.
	1	is_alpha(s(i:i)) .or.
	1	is_underscore(s(i:i))  )  ) return
	end do

	valid_characters = .true.

	return
	end

!------------------------------------------------------------------------------
	logical*4	function	is_an_integer_constant(s)
! Returns true if all characters in are in the set: {0..9}.
	implicit	none
	character	s*(*)			! a whitespace free string
	integer*4	i			! index into arg1
	logical*4	is_digit		! statement function
	character	c*1			! statement function argument
	is_digit(c) = c .ge. '0' .and. c .le. '9'

	is_an_integer_constant = .false.

	do i=1,len(s)
	   is_an_integer_constant = is_digit(s(i:i))
	   if (.not. is_an_integer_constant) return
	end do

	return
	end

$IF ABSOFT_FORTRAN
!	options/extend_source
$ELSE
	options/extend_source
$ENDIF
!------------------------------------------------------------------------------
	logical*4	function	is_a_real_constant(str,i,j,length)
! Determines the indexes to a formatted real number in a character string.
! On entry indexes i and j may only bracket a portion of the real number.
! Expected forms are 1.0, .001, 45.e-9, .7e4, 3.14d8, etc.
! Real constants must have decimal points, unaccepted forms are 1e7, 2e-1, etc.
	implicit	none
	character	str*(*)			! a whitespace free string
	integer*4	i			! an index into arg1
	integer*4	j			! an index into arg1
	integer*4	length			! length of arg1
	integer*4	saved_j			! save variable for j
	logical*4	is_an_integer_constant	! a function
	logical*4	is_digit		! statement function
	character	c*1			! statement function argument
	is_digit(c) = c .ge. '0' .and. c .le. '9'
	
	is_a_real_constant = .false.
	saved_j	= j

	if (str(i:j).eq.'.') goto 100		! no digits to left of dec. pt.

	! we know there is something to the left of the decimal point, it
	! must be an "integer"
	if (.not. is_an_integer_constant(str(i:j))) return
	if ((j+1) .gt. length) return		! no decimal point, not real
	if (str(j+1:j+1) .ne. '.') return	! no decimal point, not real
	j = j + 1				! j points to decimal point

	! next examine characters right of the decimal point, the only
	! acceptable patterns are [.digits], [.digit(s)-E-digit(s)],
	! [.E-digits], [.next-token], and [.end-of-string]
	j = j + 1				! j points 1 char right of deci.
	if (j.gt.length) goto 102		! end-of-string, is real
	if (is_digit(str(j:j))) then		! [.digit(s)], is real
	   j = j - 1
	   goto 100
	end if
	if (str(j:j) .eq. 'D') goto 101		! is real
	if (str(j:j) .eq. 'E') then
	   j = j + 1
	   if (j.gt.length) goto 103		! undefined, but not real
	   if (str(j:j) .ne. 'Q') then		! checking for .EQ.
	      j = j - 1
	      goto 101
	   else
	      goto 103
	   end if
	else
	   goto 103
	end if


 100	j = j + 1				! get digits right of decimal pt
	if (j.le.length) then
	   if (is_digit(str(j:j))) then
	         goto 100
	   end if
	else
	   goto 102
	end if

! XXXXXX handles 1.E and 1.D differently xxxxxx

 101	if (str(j:j) .eq. 'E' .or. str(j:j) .eq. 'D') then ! exponent exists?
	   j = j + 1
	else
	   goto 102
	end if
	if (j.gt.length) goto 104		! E or D requires an exponent

	if ((str(j:j).eq.'+') .or. (str(j:j).eq.'-')) then ! sign of exp
	   j = j + 1
	end if
	if (j.gt.length) goto 104

	if (is_digit(str(j:j))) then		! 1st digit of exponent
	   j = j + 1
	else
	   goto 104
	end if
	if (j.gt.length) goto 102
	if (is_digit(str(j:j))) then		! 2nd digit of exponent
	   j = j + 1
	end if
 102	j = j - 1

	is_a_real_constant = .true.
	return

 103	j = saved_j
	return
 104	j = j - 1
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	remove_whitespace(s,length)
! Removes spaces, tabs, formfeeds and any control chars from a character string.
	implicit	none
	character*(*)	s		! arg1
	integer*4	length		! arg2, length of arg1
	integer*4	place		! destination index for character copy
	integer*4	i		! index into s
	integer*4	shift		! count of white characters
$IF ABSOFT_FORTRAN
	character*(*)	space
	parameter	(space=' ')
$ELSE
	parameter	space=' '
$ENDIF

	remove_whitespace = 0
	place = 0
	shift = 0
	if (length .eq. 0) return

	do i=1,length
	   if (s(i:i) .le. space) then			! skip this char
	      shift = shift + 1
	   else
	      place = place + 1
	      if (shift .gt. 0) s(place:place) = s(i:i)
	   end if
	end do

	if (place .lt. length) s(place+1:) = ' '
	length = place

	remove_whitespace = 1

	return
	end

*-------------------------------------------------------------------------------
	subroutine to_upper_not_quoted(string, i_start, i_stop)
! Tranforms lower case letters to uppercase, except if the characters fall
! between double quotation marks, over string(i_start:i_stop).
! Pass i_start and i_stop as zero to process the full length of string string.
	character*(*)	string
	integer*4	istart, istop
	integer*4	i
	logical*4	in_quotes
	logical*4	looking_for_first_quote

	in_quotes    = 0
	looking_for_first_quote = 1
	istart = i_start
	istop = i_stop
	if (istart .le. 0) istart = 1
	if (istop  .le. 0) istop  = len(string)
	do i=istart,istop
	   if (looking_for_first_quote .and. string(i:i) .eq. '"') then
	      in_quotes = 1
	      looking_for_first_quote = 0
	   else if (string(i:i) .eq. '"') then
	      in_quotes = 0
	      looking_for_first_quote = 1
	   end if
	   if (.not. in_quotes .and.
	1     (string(i:i) .ge. 'a' .and. string(i:i) .le. 'z') )
	1     string(i:i) = char(ichar(string(i:i)) - 32)
	end do
	return
	end

*-------------------------------------------------------------------------------
	subroutine left_justify(s,k)
! Left justifies string s(:k), where k > 0.  Argument k is modified to indicate
! the position of the rightmost non-whitespace charater in the entry substring
! s(:k).
	character*(*)	s
	integer*4	k
	integer*4	shift
	if (k.lt.1) return
	i = 1
	shift = 0
	do while(s(i:i) .lt. '!')
	   shift = shift + 1
	   i = i + 1
	   if (i.gt.k) then
	      k = 0
	      return
	   end if
	end do

	if (shift .gt. 0) then
	   s = s(shift+1:)
	   k = k - shift
	end if

	return
	end

!------------------------------------------------------------------------------
	integer*4	function	init_area(area,sz)
! This routine use succisive calls to lib$movc5 to initialize large
! areas of memory with nulls.  This processes initializes the global section
! each time it starts up because other processes having mapped it keep the
! section active in memory.
	implicit	none
	integer*4	sz		! size in bytes of data area
	byte		area(sz)	! data area to initialize
	byte		null /0/	! value to fill blocks with
	integer*4	max /65535/	! size in bytes of a block
	integer*4	num_blocks	! number of blocks of size max
	integer*4	partial_block	! logical indicating a partial block
	integer*4	i		! index variable
	integer*4	addr		! address of data area

	num_blocks = sz/max
	partial_block = mod(sz,max)
$IF ABSOFT_FORTRAN
	addr = loc(area)
$ELSE
	addr = %loc(area)
$ENDIF

	do i=1,num_blocks
	   call lib$movc5(0,0,null,max,%val(addr))
	   addr = addr + max
	end do

	if (partial_block .gt. 0)
	1	call lib$movc5(0,0,null,partial_block,%val(addr))

	init_area = 1

	return
	end

$IF ABSOFT_FORTRAN
!	options/extend_source
$ELSE
	options/extend_source
$ENDIF
!------------------------------------------------------------------------------
	integer*4	function	replace_all(line,len_line,old,new)
! Replaces all occurances of string old with string new in string line.
! Argument len_line must be > 0 on entry and is modified to show new length
! (that is, relative to the passed len_line length) of string line after
! replacements.
	implicit	none
	character	old*(*), new*(*)
	character	line*(*)
	integer*4	len_line
	integer*4	len_old, len_new, adder, i, my_eol

	replace_all = 0
	if (len_line .eq. 0) return
	my_eol  = len(line)
	len_new = len(new)
	len_old = len(old)
	adder   = len_new - len_old
	i = index(line(:len_line), old)

	do while(i .gt. 0 .and. i .lt. my_eol)
	   if (i.gt.1) then
	      line = line(:i-1)//new//line(i+1:)
	   else
	      line = new//line(2:)
	   end if
	   len_line = len_line + adder
	   i = index(line(:len_line), old)
	end do

	replace_all = 1
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	index_paren_expression(s,i,j,p)
! Returns the indexes of an expression in a statement.  Example, for the
! statement
!		if (a.gt.b .or. (.not. c)) sendtc dpureset
! the expression is the string between the first left paren and the last
! right paren. Argument p is the statements prefix, which for the example
! is 'if ' (note trailing space).
! Argument s is read forwards until the right paren matching the initial
! left paren is found in this situation.  No checking is done for extra
! right parens.
!

	implicit	none
	character	s*(*)				! arg1, a statement
	integer*4	i,j				! indexes for s
	character	p*(*)				! expression prefix in s

	integer*4	k				! string s length
	integer*4	k2len				! string length function
	integer*4	nleft				! # of left parens
	integer*4	nright				! # of right parens

	index_paren_expression = 0			! indicate failure

	k = k2len(s)					! get useable length
	i = index(s,p)					! locate keyword in s
	if (i.eq.0) return				! ret on error
	i = len(p) + 1					! index first left paren
	if (i .gt. k) return				! ret on error
	if (s(i:i) .ne. '(') return			! ret on error
	j = i + 1					! point 1 char forward
	if (j.ge.k) return				! ret on error
	nleft  = 1					! have found 1 '('
	nright = 0					! have found 0 ')'

	do while(j.le.k .and. nleft.ne.nright)		! 
	   if (s(j:j) .eq. '(') nleft  = nleft + 1	! accumulate total '('
	   if (s(j:j) .eq. ')') nright = nright + 1	! accumulate total ')'
	   j = j + 1					! advance char pointer
	end do

	j = j - 1					! correct final pointer
	if (nleft .ne. nright) return			! balanced parens?
	index_paren_expression = 1			! indicate success
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	remove_whitespace_not_quoted(s,length)
! Removes spaces, tabs, formfeeds and any control chars from a character string
! not between double quotes.
	implicit	none
	character*(*)	s		! arg1
	integer*4	length		! arg2, length of arg1
	integer*4	place		! destination index for character copy
	integer*4	i		! index into s
	integer*4	shift		! count of white characters
	logical*4	in_quotes	! logical
	logical*4	looking_for_first_quote	! logical
$IF ABSOFT_FORTRAN
	character*(*)	space
	parameter	(space=' ')
$ELSE
	parameter	space=' '
$ENDIF

	remove_whitespace_not_quoted = 0
	place = 0
	shift = 0
	in_quotes = 0
	looking_for_first_quote = 1
	if (length .eq. 0) return

	do i=1,length
	   if (looking_for_first_quote .and. s(i:i) .eq. '"') then
	      in_quotes = 1
	      looking_for_first_quote = 0
	   else if (s(i:i) .eq. '"') then
	      in_quotes = 0
	      looking_for_first_quote = 1
	   end if
	   if ((.not. in_quotes) .and. (s(i:i) .le. space)) then ! skip char
	      shift = shift + 1
	   else
	      place = place + 1
	      if (shift .gt. 0) s(place:place) = s(i:i)
	   end if
	end do

	if (place .lt. length) s(place+1:) = ' '
	length = place

	remove_whitespace_not_quoted = 1

	return
	end

!------------------------------------------------------------------------------
	integer*4	function	copy_string(source,destination)
! Copy contents of string source to string destination.
	character*(*)	source,destination
	destination = source
	copy_string = 1
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	c_strncpy(d,s,n)
! Copy at most n bytes from array s to array d.  A zero in source s flags
! end of copy sequence.
	byte	d(*), s(*)
	integer*4	n
	integer*4	i

	i = 1
	d(1) = s(1)
	do while (s(i) .ne. 0 .and. i .le. n)
	   d(i) = s(i)
	   i = i + 1
	end do
	if (i .le. n) d(i) = s(i)

	c_strncpy = 1
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	copy_string_removing_quotes
	1				(source,destination,quote,length)
! Copies string from sorce to destination removing leading and trailing quote
! marks, and replacing occurances of adjacent quote marks with a single quote.
! Note that character argument quote must be specified and may actually be
! any character.  If leading or trailing characters of source are not the
! quote character (specified in call list) no copying is performed.
	character*(*)	source,destination
	character*1	quote
	integer*4	length
	integer*4	ks,kd
	character*2	double
	integer*4	ok
	integer*4	replace_all2
	integer*4	k2len

	copy_string_removing_quotes = 0
	ks = k2len(source)
	kd = len(destination)
	if (ks .lt. 2 .or. kd .lt. 1) return
	if (source(1:1) .ne. quote .or. source(ks:ks) .ne. quote) return

	if (ks .eq. 2) then
	   destination = ' '
	else
	   destination = source(2:ks-1)
	end if

	! now take care of the double occurances
	double = quote//quote
	ok = replace_all2(destination,destination,double,quote,length,1)
	if (ok .ne. 1) return

	copy_string_removing_quotes = 1
	return
	end

$IF ABSOFT_FORTRAN
!	options/extend_source
$ELSE

	options/extend_source
$ENDIF
!------------------------------------------------------------------------------
	integer*4	function	replace_all2
	1				(s,dest,old,new,my_eol,no_case)
! Replace occurances of string old with string new in source string s and
! store the results in string dest.  When no_case is true the replacement
! is case sensitive (uses the FORTRAN INDEX function).
	implicit	none
	character*(*)	s				! source string
	character*(*)	dest				! destination string
	character	old*(*)				! string to be replaced
	character	new*(*)				! replacement string
	integer*4	my_eol				! useable string length
	integer*4	len_dest			! descriptor length
	integer*4	no_case				! case sensitivity flag
	integer*4	k2len				! a function
	integer*4	jndex				! a function
	integer*4	len_new				! string length
	integer*4	len_old				! string length
	integer*4	adder				! string size diff
	integer*4	i,j				! string index

	replace_all2 = 0
	dest        = s
	len_dest    = k2len(dest)
	my_eol      = len(dest)
	if (len_dest .eq. 0) return
	len_new     = len(new)
	len_old     = len(old)
	adder       = len_new - len_old
	if (no_case) then
	   i = index(dest, old)
	else
	   i = jndex(dest, old)
	end if

	do while(i .gt. 0)
	   if (i.gt.1) then
	      dest = dest(:i-1)//new//dest(i+len_old:)
	   else
	      dest = new//dest(len_old+1:)
	   end if
	   i = i + len_new
	   j = 0
	   if (no_case .and. (i.le.my_eol) ) then
	      j = index(dest(i:), old)
	   else if (i.le.my_eol) then
	      j = jndex(dest(i:), old)
	   end if
	   if (j.eq.0) then
	      i = 0
	   else
	      i = i + j - 1
	   end if
	end do

	my_eol = k2len(dest)
	replace_all2 = 1
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	jndex(str, sub)
! This routine performs case insensitive indexing.  String length < 512.
	implicit	integer*4 (a-z)
	character	str*(*), sub*(*)
	character	cstr*512, csub*512
	integer*4	k2len, i,j

	i = k2len(str)
	cstr = str
	call to_upper(cstr, 1, i)
	j = k2len(sub)
	csub = sub
	call to_upper(csub, 1, j)

	len_str = len(str)
	len_sub = len(sub)

	jndex = index(cstr(:len_str), csub(:len_sub))
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	jndex2(str, sub)
! This routine performs case insensitive indexing.  String length < 1024.
	implicit	none
	character	str*(*), sub*(*)
	character	cstr*1024, csub*1024
	integer*4	k3len, i,j
	integer*4	len_str, len_sub

	i = k3len(str)
	cstr = str
	call to_upper(cstr, 1, i)
	j = k3len(sub)
	csub = sub
	call to_upper(csub, 1, j)

	len_str = len(str)
	len_sub = len(sub)

	jndex2 = index(cstr(:len_str), csub(:len_sub))
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	ncstrcmp(s,t)
! No-Case-STRing-CoMPare - returns 0 for s==t, <0 for s<t, >0 for s>t
	implicit	none
	character*(*)	s, t
	integer*4	i,j
	integer*4	d
	parameter	(d=ichar('A') - ichar('a'))	! ASCII only
	integer*4	xs, xt
	character*1	u, v

	xs = len(s)
	xt = len(t)

	j=min(xs,xt)
	do i=1,j
	   if (s(i:i) .eq. t(i:i)) then
	      ! match so far
	   else
	      u = s(i:i)
	      v = t(i:i)
	      if (u .ge. 'A' .and. u .le. 'Z') u = char( ichar(u) - d )
	      if (v .ge. 'A' .and. v .le. 'Z') v = char( ichar(v) - d )
	      if (u .eq. v) then
	         ! match so far
	      else
	         ncstrcmp = ichar(s) - ichar(t)
	         return
	      end if
	   end if
	end do

	i = j + 1
	if (xs .gt. xt) then
	   ncstrcmp = 1
	else if (xs .lt. xt) then
	   ncstrcmp = -1
	else
	   ncstrcmp = 0
	end if

	return
	end

!------------------------------------------------------------------------------
	integer*4	function	index_last(s,sub)
! Indexes last occurance of string sub in string s.
	implicit	none
	character*(*)	s, sub
	integer*4	ks, ku, j

	index_last = 0
	ks = len(s)
	ku = len(sub)
	if (ks .lt. 1) return
	if (ku .lt. 1) return
	if (ks .lt. ku) return
	j = ks - ku + 1 

	do while(s(j:ks) .ne. sub)
	   if (s(j:j) .ne. sub(1:1)) then
	      ks = j - 1
	      j = ks - ku + 1
	   else
	      j = j - 1
	      ks = ks - 1
	   end if
	   if (j.lt.1) return
	end do

	index_last = j
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	indexes_to_field (str,delim,fnum,i,j)
! This function returns indexes (i and j) to the substring between
! the fnum-th minus 1 and fnum-th occurances of delim in string str.
! Arguments:
! str - string to scan
! delim - character delimiting fields in string str
! fnum - field number to select
! i - beginning index of field
! j - ending index of field
	implicit	none
	character	str*(*), delim*1
	integer*4	fnum
	integer*4	i,j
	integer*4	dnum,len_str

	indexes_to_field = 0		! initialize result to false
	i                = 0
	j                = 0
	dnum             = 0		! delimiter number
	len_str          = len(str)

	! get the beginning index by finding the fnum-1th delimiter
	do while(i .lt. len_str .and. dnum .lt. (fnum-1))
	   i = i + 1
	   if (str(i:i) .eq. delim) dnum = dnum+1
	end do

	! check for errors
	if (.not. (dnum .eq. (fnum-1))) goto 10 ! too few delimiters in str
	i = i + 1
	if (i .gt. len_str) goto 10		! too few delimiters in str
	if (str(i:i) .eq. delim) goto 10	! adjacent delimiters

	! get the ending index by finding the fnum delimiter or end of string
	j = i
	do while(j .lt. len_str .and. str(j:j) .ne. delim)
	   j = j + 1
	end do

	! make an end of string adjustment
	if (j .eq. len_str) j = j + 1
	j = j - 1
	indexes_to_field = 1

	return
 10	i = 0
	j = 0
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	make_box(r1,c1,r2,c2,box,len_box)
! Assumes 24x80 screen, 7-bit sequences.  Rn are rows, Cn are columns.
! On return string box contains escape sequences to draw a box whose
! opposite corners are (r1,c1) and (r2,c2) in (row,column) coordinates.
	implicit	none
	integer*4	r1,r2,c1,c2,len_box
	character	box*(*)
	integer*4	i,j,k,n
	integer*4	ios
$IF ABSOFT_FORTRAN
	character*(*)	csi,on,off,lf,cr,bs,vc,hc,ulc,urc,llc,lrc
	parameter	(csi=char(27)//'[')
	parameter	(on=char(14))
	parameter	(off=char(15))
	parameter	(lf=char(10), cr=char(13), bs=char(8))
	parameter	(vc='x', hc='q', ulc='l', urc='k', llc='m', lrc='j')
$ELSE
	parameter	csi=char(27)//'['
	parameter	on=char(14)
	parameter	off=char(15)
	parameter	lf=char(10), cr=char(13), bs=char(8)
	parameter	vc='x', hc='q', ulc='l', urc='k', llc='m', lrc='j'
$ENDIF
	character*1	hbar*132
$IF ABSOFT_FORTRAN
!	data		(hbar(i:i), i=1,132) /132*hc/
$ELSE
	data		(hbar(i:i), i=1,132) /132*hc/
$ENDIF
	character	p1*8, p2*8
	integer*4	make_line
$IF ABSOFT_FORTRAN
	character*(*)	lend,rend
	parameter	(lend='t', rend='u')
$ELSE
	parameter	lend='t', rend='u'
$ENDIF

$IF ABSOFT_FORTRAN
!
!  2007/07/17:  The Absoft Fortran compiler fails on
!
!     data      (hbar(i:i), i=1,132) /132*hc/
!
!  so just explicitly set "hbar" with a simple do-loop.
!
	do i=1,32
           hbar(i:i) = hc
	end do
$ENDIF

	make_box = 1

 1	format(a2,i2.2,';',i2.2,'f')
 2	format(a2,i2.2,';',i2.2,'f',a1,a2,i2.2,';',i2.2,'f',a1)
	write(p1,1) csi, r1, c1
	write(p2,1) csi, r2, c1

	! do the top
	k = c2 - c1 - 1
	i   = 8  + 1 + 1  + k       + 1
	box = p1//on//ulc//hbar(:k)//urc

	! do the sides
	j = i
	do n=r1+1,r2-1
	   i = j + 1
	   j = i + 18 - 1
	   write(box(i:j),2,iostat=ios) csi, n, c1, vc, csi, n, c2, vc
	end do

	! do the bottom
	i = j + 1
	j =   i  + 8  + 1  + k       + 1  + 1 - 1
	box(i:j) = p2//llc//hbar(:k)//lrc//off

	len_box = j
	return
	!----------------------------------------------------------------------
	entry		make_line(r1,c1,r2,c2,box,len_box)
! On return string box contains escape sequences to draw a line whose
! opposite points are (r1,c1) and (r2,c2) in (row,column) coordinates.
	make_line = 1
	write(box(:8),1) csi, r1, c1
	box(9:10) = on//lend
	i = c2-c1-1
	box(11:11+i-1) = hbar(:i)
	i = 11 + i
	box(i:i+1) = rend//off
	len_box = i + 1
	return
	end

$IF ABSOFT_FORTRAN
!	options/extend_source
$ELSE
	options/extend_source
$ENDIF
!------------------------------------------------------------------------------
	integer*4	function	first_whitespace(s)
! Returns index to first occurance of whitespace in string s.
	implicit	none
	character*(*)	s
	integer*4	i,j

	i = 1
	j = len(s)
	do while(i.le.j .and. s(i:i) .gt. ' ')
	   i = i + 1
	end do

	if (i.gt.j) i = 0
	first_whitespace = i

	return
	end

$IF ABSOFT_FORTRAN
!	options/extend_source
$ELSE
	options/extend_source
$ENDIF
!------------------------------------------------------------------------------
	integer*4	function	right_justify(s)
! Right justifies non-whitespace within string s.
	implicit	none
	character*(*)	s
	integer*4	i,j,k
	integer*4	k2len

	right_justify = 0

	i = k2len(s)
	j = len(s)

	if (i.le.0) then
	else if (i.eq.j) then
	else
	   k = j - i + 1
	   s(k:) = s
	   s(:k-1) = ' '
	end if

	right_justify = 1

	return
	end

!------------------------------------------------------------------------------
	integer*4	function	brev(a,v,x)
! this routine returns arg A with bits V through X reversed
	implicit	none
	integer*4	a,v,x
	integer*4	b,i,j

	b = a
	j = x
	do i=v,x
	   if (btest(a,i)) then
	      b = ibset(b,j)
	   else
	      b = ibclr(b,j)
	   end if
	   j = j - 1
	end do

	brev = b
	return
	end

*-------------------------------------------------------------------------------
        integer*4       function        null_terminate(text)
! converts trailing whitespace to nulls.
! if no trailing whitespace exists the last character is converted to null.
	implicit	none
	integer*4	i
        character*(*)   text
$IF ABSOFT_FORTRAN
        character*(*)   null,space
	parameter	(null=char(0))
	parameter	(space=' ')
$ELSE
	parameter	null=char(0)
	parameter	space=' '
$ENDIF

	null_terminate = 0

	i = len(text)
	if (i.eq.0) return

        do while((i .ge. 1) .and. (text(i:i) .le. space))
	   text(i:i) = null
           i = i - 1
        end do

	if (i .eq. len(text)) text(i:i) = null

	null_terminate = 1

	return
	end

!------------------------------------------------------------------------------
$IF ABSOFT_FORTRAN
!	options/extend_source
$ELSE
	options/extend_source
$ENDIF
	integer*4	function	copy_without_white(s,t)
	implicit	none
	character*(*)	s,t
	integer*4	i,j,k,n

	copy_without_white = 0
	i = len(s)
	j = len(t)
	if (i.lt.1 .or. j.lt.1) return
	t = ' '

	k = 1
	n = 1
	do while(k .le. i .and. n .le. j)
	   if (s(k:k) .gt. ' ') then
	      t(n:n) = s(k:k)
	      n = n + 1
	   end if
	   k = k + 1
	end do

	copy_without_white = 1
	return
	end

!------------------------------------------------------------------------------
$IF ABSOFT_FORTRAN
!	options/extend_source
$ELSE
	options/extend_source
$ENDIF
	integer*4	function	copy_until_white(s,t)
	implicit	none
	character*(*)	s,t
	integer*4	i,j,k,n
	integer*4	not_white

	copy_until_white = 0
	i = len(s)
	j = len(t)
	if (i.lt.1 .or. j.lt.1) return
	t = ' '

	k = 1
	n = 1
	not_white = 1
	do while( (k .le. i) .and. (n .le. j) .and. not_white)
	   not_white = s(k:k) .gt. ' '
	   if (not_white) then
	      t(n:n) = s(k:k)
	      n = n + 1
	   end if
	   k = k + 1
	end do

	copy_until_white = 1
	return
	end

!------------------------------------------------------------------------------
$IF ABSOFT_FORTRAN
!	options/extend_source
$ELSE
	options/extend_source
$ENDIF
	integer*4	function	find_first_nonwhite(s,i)
	implicit	none
	character*(*)	s
	integer*4	i,k
	integer*4	got_white

	find_first_nonwhite = 0

	i = 1
	k = len(s)
	if (k.lt.1) return

	got_white = 1
	do while( (i .le. k) .and. got_white)
	   got_white = s(i:i) .le. ' '
	   if (got_white) i = i + 1
	end do

	if (got_white) then
	   i = 0
	   return
	end if

	find_first_nonwhite = 1
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	rm_dups_in_sorted_array(list,sz)
! list members must be
	implicit	none
	character*(*)	list(*)
	integer*4	sz
	integer*4	i,j,k

	rm_dups_in_sorted_array = 1

	i = 2
	j = i - 1
	k = sz
	do while (i .le. k)
	   if (list(i) .ne. list(j)) then
	      j = j + 1
	      if (j .ne. i) list(j) = list(i)
	      i = i + 1
	   else
	      i = i + 1
	      sz = sz - 1
	   end if
	end do

	return
	end

!------------------------------------------------------------------------------
! Called from qsort-like routine, sorts ascending.
! returns -1 for a < b, 0 for a == b, or 1 for a > b.
!------------------------------------------------------------------------------
	integer*2	function	q_i2_ch_cmp_asc(a,b)
	implicit	none
	character*(*)	a
	character*(*)	b
	integer*4	i,j,k,n,val

	i = len(a)
	j = len(b)
	k = min(i,j)

	n = 1
	do while (n .le. k .and. a(n:n) .eq. b(n:n))
	   n = n + 1
	end do

	if (n .gt. k) then
	   if (i .eq. j) then
	      val = 0
	   else if (i .gt. j) then
	      val = 1
	   else
	      val = -1
	   end if
	else
	   if (a(n:n) .lt. b(n:n)) then
	      val = -1
	   else if (a(n:n) .gt. b(n:n)) then
	      val = 1
	   else ! should never get here
	      val = 0
	   end if
	end if

	q_i2_ch_cmp_asc = val
	return
	end

!------------------------------------------------------------------------------
! Called from qsort-like routine, sorts descending.
! returns -1 for a < b, 0 for a == b, or 1 for a > b.
!------------------------------------------------------------------------------
	integer*2	function	q_i2_ch_cmp_des(b,a)
	implicit	none
	character*(*)	a
	character*(*)	b
	integer*4	i,j,k,n,val

	i = len(a)
	j = len(b)
	k = min(i,j)

	n = 1
	do while (n .le. k .and. a(n:n) .eq. b(n:n))
	   n = n + 1
	end do

	if (n .gt. k) then
	   if (i .eq. j) then
	      val = 0
	   else if (i .gt. j) then
	      val = 1
	   else
	      val = -1
	   end if
	else
	   if (a(n:n) .lt. b(n:n)) then
	      val = -1
	   else if (a(n:n) .gt. b(n:n)) then
	      val = 1
	   else ! should never get here
	      val = 0
	   end if
	end if

	q_i2_ch_cmp_des = val
	return
	end
