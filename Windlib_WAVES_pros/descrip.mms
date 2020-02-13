#$Id: descrip.mms,v 1.2 2000/01/21 00:26:24 scottm Exp $
#
#  Copyright (c) 1988-2000, Research Systems Inc.  All rights reserved.
#  This software includes information which is proprietary to and a
#  trade secret of Research Systems, Inc.  It is not to be disclosed
#  to anyone outside of this organization. Reproduction by any means
#  whatsoever is  prohibited without express written permission.
#
#  makefile CALL_EXTERNAL examples, Alpha VMS
#  To build these examples you should:
#  1)Make sure the logical IDL_DIR is set to the IDL installation
#    directory.  For example:
#    $DEFINE IDL_DIR ALPHA$DKA100:[IDL.]
#  2)Run MMS with this file by doing:
#    $MMS/DESC=MAKEFILE_VMS.MMS 
#
SO_EXT=exe
CC=cc
LD=link
CFLAGS=/float=IEEE_FLOAT/IEEE_MODE=DENORM/INCLUDE=IDL_DIR:[EXTERNAL]
LD_FLAGS= /EXE=$* /SHARE
LD_POST= $*/OPT

objs = simple_vars.obj string_array.obj incr_struct.obj sum_2d_array.obj
ld_objs = simple_vars.obj,string_array.obj,incr_struct.obj,sum_2d_array.obj

call_examples.$(SO_EXT) : $(objs)
	$(LD) $(LD_FLAGS) $(ld_objs),$(LD_POST)

tidy :
	purge/nolog *.obj
	delete/nolog *.obj;

clean : tidy
	purge/nolog *.exe
	delete/nolog *.exe;
