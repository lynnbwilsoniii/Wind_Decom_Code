! scr_manager_def.for - data structure definitions for calling the full screen
! field edit manager and associated routines

	parameter	edit_at_type=1
	parameter	scroll_this_type=3
	parameter	readonly_type=4

	parameter	bottom_rolls_to_top        = '0001'x
	parameter	bottom_moves_to_next_field = '0002'x
	parameter	bottom_is_sticky           = '0004'x
	parameter	top_rolls_to_bottom        = '0010'x
	parameter	top_moves_to_prev_field    = '0020'x
	parameter	top_is_sticky              = '0040'x

! c*16 retkey should be first item in each structure

	structure /scr_mgr_object/
	   character*16 retkey	! w: name of key that terminated edit of field
	   integer*4	type	! r: field type: edit, scroll, etc.
	   integer*4	before	! r: address of routine called prior to edit
	   integer*4	proc	! r: address of object processing routine
	   integer*4	after	! r: address of routine called after edit
	   integer*4	show	! r: address of show object routine
	   integer*4	errhdlr	! r: address of error handling routine
	   integer*4	auxkey	! r: address of auxiliary key handling routine
	   integer*4	obj	! r: address of object data structure
	end structure

	structure /tv_field/
	   character*16 retkey	! w: name of key that terminated edit of field
	   integer*4	col	! r: terminal column coordinate of field
	   integer*4	row	! r: terminal row coordinate of field
	   integer*4	siz	! r: width of field
	   character*132 str	!rw: (string) value of field
	   integer*4	l_str	! w:  return length of field (string)
	   character*6	iva	! r: initial video attributes (during field edit)
	   character*6	fva	! r: final video attributes (refresh after edit)
	   integer*4	readonly! r: readonly flag, true for readonly field
	   integer*4	chgflg	! w: true if field value changed, 0 otherwise
	   integer*4	before	! r: address of routine called prior to edit
	   integer*4	after	! r: address of routine called after edit
	end structure

	structure /tv_on_key/
	   character*16	key
	   integer*4	routine
	end structure

	structure /tv_scroll/
	   character*16 retkey	! w: name of key that terminated scroll
	   integer*4	flags   ! r: behaviorial flags
	   integer*4	r1	! r: start row for scrolling region
	   integer*4	r2	! r: end row for scrolling region
	   integer*4	c1	! r: starting column for scrolling region
	   integer*4	c2	! r: ending column for scrolling region
	   integer*4	xr	! w: current row in scrolling region
	   integer*4	idw	! w: data index of 1st line in scroll region
	   integer*4	idx	! w: data index of current line in scroll region
	   integer*4	idy	! w: data index of last line in scroll region
	   integer*4	idz	! r: data index of last available scroll item
	   integer*4	before	! r: address of routine called prior to scroll
	   integer*4	after	! r: address of routine called after scroll
	   integer*4	gtscrldt! r: address of routine returning data to scroll
	   integer*4	on_key	! r: address of secondary keystroke handler
	   union
	   map
	      character	pstr*264
	   end map
	   map
	      character	pos*9	! w: screen address escape sequence
	      character	str*255	! r: buffer written by gtscrldt routine
	   end map
	   end union
	   integer*4	l_str	! r: number of chars put in buffer
	end structure
