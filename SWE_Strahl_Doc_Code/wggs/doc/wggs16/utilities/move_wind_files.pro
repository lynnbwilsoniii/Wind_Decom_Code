;moves wind files from ftp/pub/imports to appropriate directories

indir='/data2/ftp/pub/imports/'
swelz='wi_lz_swe_*.dat' & swelzdir='/data2/swe/lz/'
swekp='wi_k0_swe_*.cdf' & swekpdir='/data5/swe/swekp/'
wiorb='wi_or_*.cdf'     & wiorbdir='/data5/swe/oa/'
wiatt='wi_at_*.cdf'     & wiattdir='/data5/swe/oa/'
sfdu='.sfdu'

print,' ' & print,'List all files :'
spawn,'ls '+indir+'* | more'
print,'Hit return to continue' & r='' & read,r & if r ne '' then stop

data_type=['swelz','swekp','windorbit','windattitude']
filter=[swelz,swekp,wiorb,wiatt]
outdir=[swelzdir,swekpdir,wiorbdir,wiattdir]

for i=0,n_elements(data_type)-1 do begin
  result=findfile(indir+filter(i),count=count)
  if count ne 0 then begin
    for j=0,count-1 do begin
      print,' ' & print,data_type(i)+' files found: '
      for j=0,count-1 do print,result(j)
      print,'Hit return to continue' & r='' & read,r & if r ne '' then stop
      for j=0,count-1 do begin
        filename=strmid(result(j),strlen(indir),$
          strlen(result(j))-strlen(indir))
        oldflnm=result(j)
        newflnm=outdir(i)+filename 
        print,' '       
        print,'old filename ',oldflnm
        print,'new filename ',newflnm 
        mvcmd='mv '+oldflnm+' '+newflnm
        print,mvcmd
        print,'Hit return to move the file' & r='' & read,r
        if r ne '' then stop        
        spawn,mvcmd
        newres=findfile(newflnm)
        if newres(0) ne '' then print,'file move confirmed...',newres(0) $
        else stop,'file move NOT confirmed'        
      endfor
    endfor  
  endif else begin
    print,'no '+data_type(i)+' files found
    print,'Hit return to continue' & r='' & read,r & if r ne '' then stop
  endelse    
endfor

result=findfile(indir+'*.sfdu',count=count)
if count ne 0 then for i=0,count-1 do spawn,'rm '+result(i)

end
