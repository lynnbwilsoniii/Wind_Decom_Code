This version of panelist is not to be compiled or executed but is intended to 
illustrate the changes required for init/panelist.pro 
when adding a new survey data set.

The only changes to be made are to arrays 
"datype", "wilabl", "xscroll","yscroll","bttn","witype", "fltr", and optionally, 
"datype_multiday". The changes are the addition of one array element as 
indicated below by the dummy data type name "exp_dtyp".
       
USER NOTE: 
Search on "exp_dtyp" to find lines in the code 
that require changes of additions for the new data type.


;====================== panelist ==========================================

pro panelist

common shared,d

;<<<<<<<<<<<< variable names and parameters must be          >>>>>>>>>>
;<<<<<<<<<<<< explicitly referred to by name in this section >>>>>>>>>>


;these datatype are the possible survey data type accessed and displayed
datype = [        $
  'swe_moments',      $
  'swe_fpitch',       $
  'mfi_mag3s',        $
  'swe_ionkp',        $
  'mfi_magkp',        $
  'swe_strahl',       $
  'isee_moments', $
  'wav_tnr',     $
  'swe_redfcuts',     $
  'wav_hrtnr',   $
  'swe_strahlen',     $
  'wind_orbit',    $
  'swe_fparaperp',     $
  'wav_nekp',  $
  'wav_nehr',  $
  'exp_dtyp'  $           This change is required           
  ]


;only this subset of  datatypes are permitted to be plotted in 
;multi-day plots greater than maxnumberdays
maxnumberdays=4
datype_multiday = [        $
  'swe_moments',      $
  'mfi_mag3s',        $
  'swe_ionkp',        $
  'mfi_magkp',        $
  'swe_strahlen',     $
  'wind_orbit',       $
  'swe_fparaperp',    $
  'wav_nekp',  $
  'exp_dtyp'  $           This change is optional
  ]
  
;compare datype names with data type names in structure "datapaths" that are
;survey types of data, not levelzero, as defined in the startup file
  restore,getenv('IDLSAV')+'datapaths'
  wsurvey=where(datapaths.ifsurvey eq 1)
  wdiff=where(datype ne datapaths(wsurvey).name)
  if wdiff(0) ne -1 then stop,'panelist: datatype inconsistency'

        
wilabl = $
[ 'SWE moments',$
  'SWE pitch angle',$
  'WIND 3sec mag',$
  'SWE ion KP',$
  'WIND mag KP',$
  'Strahl spectrum',$
  'ISEE moments',$
  'Waves 1m TNR',$
  'SWE f & redf cuts',$
  'Waves HR TNR',$
  'Strahl@en', $
  'WIND orbit', $
  'SWE fpara fperp',$
  'Waves 1m Ne', $
  'Waves HR Ne',$
  'exp_dtyp name'$       This change is required
  ]

Add element for "exp_dtyp" to each of the following four arrays following the 
existing pattern.

xscroll=$   horizontal dimension of plot variable scrolling widget field    
[160,100, 100, 160, 100, 175, 135, 150,  75, 150, 150, 75,  75, 75, 75]

yscroll=$  vertical dimension of plot variable scrolling widget field
[600,500, 200, 400, 200, 200, 450, 175, 100, 175, 175,100, 100,100,100]

bttn= $    a data type index 
[0,  2,   3,   4,   5,   6,   7,   8,   9,  10,  11, 12,  13, 14, 15]

witype=   $  see procedure widgets/select_pltvar.pro
[0,  2,   0,   0,   0,   0,   0,   1,   0,   1,   0,  0,   0,  0,  0]
 
fltr=  ['*',$
        '*',$
        '*.cdf',$
        '*.cdf',$
        '*.cdf',$
        '*',$
        '*.cdf',$
        '*.tnr',$
        '*redfcuts',$
        '*.hrtnr',  $
        '*', $
        '*.cdf',$
        '*.pitavg',$
        '*.cdf',$
        '*.cdf',$
        'exp_dtyp file name filter and extension'  $ see survey/input.pro
          ]

;The above arrays become tags for the structure d below

         
;<<<<<<<<<<<< variable names and parameters no longer  >>>>>>>>>>
;<<<<<<<<<<<< referred to by name below this point     >>>>>>>>>>



pathenv=datapaths(wsurvey).envar

dir=strarr(n_elements(datype))
for i=0,n_elements(datype)-1 do dir(i)=getenv(pathenv(i))


offs=lonarr(n_elements(datype))
len=lonarr(n_elements(datype))

for i=0,n_elements(datype)-1 do begin 
  call_procedure,datype(i)+'_list',listi
  len(i)=n_elements(listi)
  typi=replicate(datype(i),n_elements(listi))
  if i eq 0 then begin
    list=listi
    type=typi
    offs(i)=0
  endif else begin
    offs(i)=n_elements(list)
    list=[list,listi]
    type=[type,typi]
  endelse    
endfor


pnlist=$

  {dtp:type,$
            
   list:list,$

   ev_val:intarr(n_elements(list)),$

   offs:offs,$
   
   len:len}


pnlstr={dtp:'', varname:'',ypnlp:0.,$
    ztitle:'',$
    labl:'',range:fltarr(2),ticks:2,minor:5,$
    tickv:fltarr(30),tickname:strarr(30), subtitle:'',$
    tmlabl:'',tmrange:dblarr(2),tmticks:2,tminor:5,fill:-1.e31,$
    tmtickv:dblarr(30),tmtickname:strarr(30), $
    plotio:0,psym:0,symsize:1.,oplot:0,oplotvar:'',olinestyle:0,ocolor:225,$
    pltype:'',enindx:0, indx:0, xory:0,ev_val:0,step:0.,$
    lzrange:[[0.,4032.],[-32.,-24.]],charthick:1.0,charsize:1.15,horizlin:-1.e31}


ndvar=n_elements(datype)
d={ndvar:ndvar,$
   datype:datype,$
   dir:dir,$
   pathenv:pathenv,$
   fltr:fltr,$
   flnm:strarr(ndvar),$
   datype_input:intarr(ndvar),$   
   wilabl:wilabl,$
   pnlist:pnlist,$
   ndx:lonarr(2,ndvar),$
   ndx_orig:lonarr(2,ndvar),$
   ndx_last:lonarr(2,ndvar),$
   ndx_buff:lonarr(2,ndvar),$
   ndx_buff2:lonarr(2,ndvar),$
   ndx_stored:lonarr(2,ndvar),$
   pnl:replicate(pnlstr,n_elements(pnlist.list)),$
   pnlsel:-1+intarr(n_elements(pnlist.list)),$
   pnlsel_last:-1+intarr(n_elements(pnlist.list)),$
   wdatype:-1+lonarr(n_elements(pnlist.list),ndvar),$
   timsel:'lz',$
   xscroll:xscroll,yscroll:yscroll,$
   bttn:bttn,$
   witype:witype,$
   refsec:0.d, $
   maxnumberdays:maxnumberdays, $
   datype_multiday:datype_multiday $
 }



end
