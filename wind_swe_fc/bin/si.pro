


FUNCTION si, s, i, j
format = '(I' + strtrim(string(i), 2) + '.' + $
  strtrim(string(j), 2) + ')'
return, string(s, format = format)
END
