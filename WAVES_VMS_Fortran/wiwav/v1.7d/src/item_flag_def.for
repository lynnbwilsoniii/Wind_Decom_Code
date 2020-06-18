! item_flag_def.for

	integer*4	p_extract
	integer*4	p_fixed_value
	integer*4	p_composite
	integer*4	p_file_name
	integer*4	p_validation
	integer*4	p_function_number
	integer*4	p_area
	integer*4	p_count_item
	integer*4	p_text_item
	integer*4	p_xlate_present
	integer*4	p_description
	integer*4	p_author_date
	integer*4	p_int_return
	integer*4	p_float_return
	integer*4	p_double_return
	integer*4	p_char_return
	integer*4	p_lookup_table
	integer*4	p_format
	integer*4	p_procedure_number
	integer*4	p_attribute_flags
	integer*4	p_cdf_item
	integer*4	p_pchar_return
	integer*4	p_ur8_convert
	integer*4	p_type_return_mask

	parameter	(p_extract	    = '00000001'x)
	parameter	(p_fixed_value	    = '00000002'x)
	parameter	(p_composite	    = '00000004'x)
	parameter	(p_file_name	    = '00000008'x)
	parameter	(p_validation	    = '00000010'x)
	parameter	(p_function_number  = '00000020'x)
	parameter	(p_area		    = '00000040'x)
	parameter	(p_count_item	    = '00000080'x)
	parameter	(p_text_item        = '00000100'x)
	parameter	(p_xlate_present    = '00000200'x)
	parameter	(p_description      = '00000400'x)
	parameter	(p_author_date      = '00000800'x)
	parameter	(p_int_return	    = '00001000'x)
	parameter	(p_float_return	    = '00002000'x)
	parameter	(p_double_return    = '00004000'x)
	parameter	(p_char_return	    = '00008000'x)
	parameter	(p_type_return_mask = '0000f000'x)
	parameter	(p_lookup_table	    = '00010000'x)
	parameter	(p_format      	    = '00020000'x)
	parameter	(p_procedure_number = '00040000'x)
	parameter	(p_cdf_item         = '00080000'x)
	parameter	(p_pchar_return     = '00100000'x)
	parameter	(p_ur8_convert      = '00200000'x)
	parameter	(p_attribute_flags  = 'ffffffff'x)

