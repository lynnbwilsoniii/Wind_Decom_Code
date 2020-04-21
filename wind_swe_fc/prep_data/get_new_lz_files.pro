
;Stevens Wind SWE LZ auto pipeline implimentation, 12/14

function get_new_lz_files_old, n_new_lzfiles = n_new_lzfiles

restore, '/crater/observatories/wind/processing_state.idl'

lzfiles = file_search('/crater/observatories/wind/swe/lz/*/*.dat*')
zz = strmid(lzfiles, 1, 2, /reverse)
zipped = where(zz eq 'gz')
unzipped = where(zz ne 'gz')

lbl = strmid(lzfiles, 28, 9, /reverse)
lbl[unzipped] = strmid(lzfiles[unzipped], 25, 9, /reverse)
ok =  where( (lbl eq 'wi_lz_swe') or (lbl eq 'WI_LZ_SWE'), nok)
lzfiles = lzfiles[ok]
lbl=lbl[ok]

zz = strmid(lzfiles, 1, 2, /reverse)
zipped = where(zz eq 'gz')
unzipped = where(zz ne 'gz')

vv = strmid(lzfiles, 8, 2, /reverse)
vv[unzipped] = strmid(lzfiles[unzipped], 5, 2, /reverse)

dstr = strmid(lzfiles, 18, 8, /reverse)
dstr[unzipped] = strmid(lzfiles[unzipped], 15, 8, /reverse)

lbl = strmid(lzfiles, 28, 9, /reverse)
lbl[unzipped] = strmid(lzfiles[unzipped], 25, 9, /reverse)



yr = fix(strmid(dstr, 0, 4))

to_process = intarr(n_elements(lzfiles))
for i = 0, n_elements(lzfiles) - 1 do begin &$
   match = where(dstr[i] eq proc_state.date, nmatch) &$
   if nmatch eq 1 then to_process[i] = ( (yr[i] gt 2004) and ( (proc_state[match].version lt fix(vv[i])) or (proc_state[match].processed eq 0))) &$
   if (nmatch eq 0 and (yr[i] ge 2014)) then to_process[i] = 1 &$
   endfor

tknew = where(to_process eq 1, n_new_lzfiles)
if n_new_lzfiles gt 0 then return, lzfiles[tknew] else return, ''

end




; This needs to be calatogued more systematically.
;
pro catalogue_lz

lz_record = {julday:-99d, year:-9999, doy:-999, fullname:'', shortname:'', highest_ver:-9, $
             specfile:'', proc_date:-99d, version_date:-99d, udata:ptr_new(), frozen: 0B}
lz_records = [lz_record]

jd0 = julday(1, 1, 1995, 0, 0, 0)
this_jd = jd0
today_jd = systime(/julian)

lz_path = '/crater/observatories/wind/swe/lz/'

while this_jd le today_jd do begin &$

   caldat, this_jd, month, day, year &$
   mm = string(month, format = '(I02)') &$
   dd = string(day, format = '(I02)') &$
   yyyy = string(year, format = '(I4)') &$
   doy = this_jd - julday(1, 1, year, 0, 0) + 1
   dstr = yyyy+mm+dd &$
   shortname = 'wi_lz_swe_'+dstr &$
   thisrec = lz_record
   thisrec.julday = this_jd
   thisrec.year = year
   thisrec.doy = doy
   files = file_search(lz_path + yyyy + '/' + shortname + '*', count=count)
 
  if count gt 0 then begin
     print, 'cataloguing ' + files[0]
     p0 = strpos(files[0], 'wi_lz')
     vv = strmid(files, p0+20, 2)
     version = fix(vv)
     thisrec.highest_ver = max(version, which)
     thisrec.shortname = shortname
     thisrec.fullname = files[which]
     spawn, 'ls -l --time-style=iso ' + files[which], detail
     spaces = strsplit(detail, ' ')
     vdate = strmid(detail, spaces[5], spaces[6]-spaces[5])
     if strlen(vdate) le 6 then begin; handle case of mm-dd formatting
        vy = yyyy
        vm = strmid(vdate, 0, 2)
        vd = strmid(vdate, 3, 5)
     endif else begin
        vy = strmid(vdate, 0, 4)
        vm = strmid(vdate, 5, 2)
        vd = strmid(vdate, 8, 2)
     endelse
     vjd = julday(vm, vd, vy)
     thisrec.version_date = vjd
     thisrec.udata = ptr_new(files)

     specfiles = file_search('/crater/observatories/wind/ionspec/ionspec' + dstr + '.idl*', count=count)
     if count gt 0 then begin
        thisrec.specfile = specfiles[0]
        spawn, 'ls -l --time-style=iso ' + specfiles[0], detail
        spaces = strsplit(detail, ' ')
        pdate = strmid(detail, spaces[5], spaces[6]-spaces[5])
        if strlen(pdate) le 6 then begin ; handle case of mm-dd formatting
           vy = yyyy
           vm = strmid(pdate, 0, 2)
           vd = strmid(pdate, 3, 5)
        endif else begin
           py = strmid(pdate, 0, 4)
           pm = strmid(pdate, 5, 2)
           pd = strmid(pdate, 8, 2)
        endelse
        pjd = julday(pm, pd, py)
        thisrec.proc_date = pjd
     endif

   if thisrec.year lt 2016 then thisrec.frozen = 1

endif

this_jd = this_jd + 1
lz_records = [lz_records, thisrec]

endwhile

stop
  
end

; Now we can update anything where the version date is later than the
; processed date

pro update_lz_catalogue_after_jd, jd_min; is there a better way here than just regenerating?

restore,  '/crater/observatories/wind/lz_catalogue.idl'
lz_record = {julday:-99d, year:-9999, doy:-999, fullname:'', shortname:'', highest_ver:-9, $
             specfile:'', proc_date:-99d, version_date:-99d, udata:ptr_new(), frozen: 0B}
this_jd = jd_min
today_jd = systime(/julian)
caldat, today_jd, today_mo, today_dy, today_yr
today_yyyy = string(today_yr, format = "(I4)")
lz_path = '/crater/observatories/wind/swe/lz/'

; update the catalogue from jd_min to present
tk = where(lz_records.julday lt jd_min, ntk)
if ntk gt 0 then lz_records = lz_records[tk]

; now cycle from jd_min through present
while this_jd le today_jd do begin &$

   caldat, this_jd, month, day, year &$
   mm = string(month, format = '(I02)') &$
   dd = string(day, format = '(I02)') &$
   yyyy = string(year, format = '(I4)') &$
   doy = this_jd - julday(1, 1, year, 0, 0) + 1
   dstr = yyyy+mm+dd &$
   shortname = 'wi_lz_swe_'+dstr &$
   thisrec = lz_record
   thisrec.julday = this_jd
   thisrec.year = year
   thisrec.doy = doy
   files = file_search(lz_path + yyyy + '/' + shortname + '*', count=count)
 
  if count gt 0 then begin
     print, 'cataloguing ' + files[0]
     p0 = strpos(files[0], 'wi_lz')
     vv = strmid(files, p0+20, 2)
     version = fix(vv)
     thisrec.highest_ver = max(version, which)
     thisrec.shortname = shortname
     thisrec.fullname = files[which]
     spawn, 'ls -l --time-style=iso ' + files[which], detail
     spaces = strsplit(detail, ' ')
     vdate = strmid(detail, spaces[5], spaces[6]-spaces[5])
     if strlen(vdate) le 6 then begin
        vy = today_yyyy
        vm = strmid(vdate, 0, 2)
        vd = strmid(vdate, 3, 2)
     endif else begin
        vy = strmid(vdate, 0, 4)
        vm = strmid(vdate, 5, 2)
        vd = strmid(vdate, 8, 2)
     endelse
     vjd = julday(vm, vd, vy)
     thisrec.version_date = vjd
     thisrec.udata = ptr_new(files)

     specfiles = file_search('/crater/observatories/wind/ionspec/ionspec' + dstr + '.idl*', count=count)
     if count gt 0 then begin thisrec.specfile = specfiles[0]
        spawn, 'ls -l --time-style=iso ' + specfiles[0], detail
        spaces = strsplit(detail, ' ')
        pdate = strmid(detail, spaces[5], spaces[6]-spaces[5])
        if strlen(pdate) le 6 then begin
           py = today_yyyy
           pm = strmid(pdate, 0, 2)
           pd = strmid(pdate, 3, 2)
        endif else begin
           py = strmid(pdate, 0, 4)
           pm = strmid(pdate, 5, 2)
           pd = strmid(pdate, 8, 2)
        endelse
        pjd = julday(pm, pd, py)
        thisrec.proc_date = pjd
     endif
     
     if thisrec.year lt 2016 then thisrec.frozen = 1
     
  endif
  
this_jd = this_jd + 1
lz_records = [lz_records, thisrec]

endwhile

save, lz_records, filename =   '/crater/observatories/wind/lz_catalogue.idl'

end


function get_new_lz_files, n_new_lzfiles = n_new_lzfiles

restore,  '/crater/observatories/wind/lz_catalogue.idl'
tk = where((lz_records.version_date gt lz_records.proc_date) and (lz_records.frozen ne 1), n_new_lzfiles)
if n_new_lzfiles gt 0 then return, lz_records[tk].fullname else return, ''

end

; Some robust pipeline management is required here
