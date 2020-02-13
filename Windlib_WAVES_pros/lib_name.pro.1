;$Id: lib_name.pro,v 1.2 2000/06/22 18:17:48 ali Exp $
FUNCTION LIB_NAME,name_arg
    name = name_arg
    ON_ERROR,2
    prefix = ''
    CASE !VERSION.OS_FAMILY OF 
    'unix'  :BEGIN
         CASE !VERSION.OS OF
         'hp-ux': lib_ext = 'sl'
         'AIX': begin
            ;AIX won't find a shared lib in the current dir
            ; unless the name is preceded with a ./
            lib_ext = 'a'
            prefix = './'
          end
	 'sunos': begin
	    lib_ext = 'so'
	    if (!version.memory_bits eq 64) then name = name + '_64'
	  end
         else: lib_ext = 'so'
         ENDCASE 
    END
    'vms'   :  lib_ext = 'EXE'
    'Windows' : lib_ext = 'DLL'
    'MacOS' : lib_ext = 'shlb'
    ELSE: MESSAGE,"Don't know what to do with: " + !VERSION.OS_FAMILY
    ENDCASE
    return,prefix + name + '.' + lib_ext
END
