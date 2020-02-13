FUNCTION swe_strahl_read_all_vs, date

   file_test4 = file_test(getenv('STRAHL_DATA') + strmid(date, 2, 2) + strmid(date, 5, 2) + strmid(date, 8, 2) + '_v4.strahl')
   file_test6 = file_test(getenv('STRAHL_DATA') + strmid(date, 0, 4) + strmid(date, 5, 2) + strmid(date, 8, 2) + '_v6.strahl')
   file_test7 = file_test(getenv('STRAHL_DATA') + strmid(date, 0, 4) + strmid(date, 5, 2) + strmid(date, 8, 2) + '_v7.strahl')

   IF file_test4 OR file_test6 OR file_test7 THEN BEGIN

      IF file_test4 THEN a = swe_strahl_read_kosta(date)
      IF file_test6 THEN a = swe_strahl_read_v7_kosta(date)
      IF file_test7 THEN a = swe_strahl_read_v7_kosta(date)

      RETURN, a

   ENDIF ELSE BEGIN

      RETURN, {status:-1}

   ENDELSE

END
