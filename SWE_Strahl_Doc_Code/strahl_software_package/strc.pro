; combine string and strcompress

FUNCTION strc, str, strmid0, strmid1

   IF n_params() eq 3 THEN RETURN, strmid(strcompress(string(str), /remove), strmid0, strmid1) $
   ELSE RETURN, strcompress(string(str), /remove)

END
