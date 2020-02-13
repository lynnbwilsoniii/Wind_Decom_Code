This version of the startup file is not to be compiled or executed 
but is intended to illustrate the changes required in startup file 
for data paths and environment variable when adding a new survey data set.

USER NOTE: 
Search on "exp_dtyp" and "EXP_DTYP" and substitute the new data type name.
Then put the new startup file in the directory in which the IDL session is run.
To run the SWEDAT tool and tool environment:
% idl startup_filename



;==============================================================================
;======= The following are REQUIRED user settings (null '' NOT allowed) =======

 
;<<<<<<<< Directory path settings >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
wggs_version='swedat01'     ;current version

wggs_dir='/export/home/rjf/'  ;directory path name for the tool

sav_dir='/data1/'        ;scratch subdirectory with user write priviledges.

;<<<<<<<< Variables to control font,widget, and plot window size >>>>>>>>>>>>
default_font='6x13'   ;'7x14'     ;'5x8'
xsize_main='650'
ysize_main='750'     
xsize_lzsrvy='950'    ;'825'    ;'700'
ysize_lzsrvy='165'    ;'140'
xsize_lz='950'  ;'1075'        ;'915'
ysize_lz='500'  ;'565'        ;'480'

device,decomposed=0

;<<<<<<<<<<<<< Printers >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;set default printer_name and print_filename
printer_name_bw=' '              ;default black&white printer name
;printer_name_bw='-d ljps130'
print_filename_bw='idl_bw.ps'    ;black&white ps plot file name

printer_name_clr='-d lephpc114'  ;'-d leptek'      ;default color printer name
print_filename_clr='idl_clr.ps'  ;color ps plot file name


;<<<<<<<<<<<<<<<<<< Color Table >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;set default color table index for black & white and for color 
ctbl = '18'        ;GSFC for set_plot,'x'
;ctbl = '13'       ;RAINBOW for set_plot,'x'

ctbl_hc = '18'      ;GSFC for set_plot,'ps'       hardcopy
;ctbl_hc = '13'     ;RAINBOW for set_plot,'ps'    hardcopy
;ctbl_hc = '23'     ;IOWA for set_plot,'ps'      hardcopy
   

;========== The following are OPTIONAL settings ==============================
;=== If a path is not used, then null string '' is required (NOT ' '). =======

local_usr='/export/home/rjf/wggs_local/'  ;A private directory (optional)

sav_dir_2='/data1/idlsav2/'            ;A second scratch direcory (optional)

mpnew='/data1/swe/mpnew/'  ;Staging directory for creation of survey files

momavg='/mnt/leprjf_data7/swe/momavg/'

;Optional, secondary moments directory
  swe_moments2_path=['/mnt/leprjf_data5/swe/moments2/']
  setenv,'SWE_MOMPATH2='+swe_moments2_path(0) 


;Special user application
;Note: The string to the right of the equals sign is the name of the 
;       user-supplied procedure which can be located in local_usr directory
  setenv,'SPECIAL_APPL=special_appl_rjf'
  setenv,'SPECIAL_APPL_SWELZ=special_appl_swelz_rjf'
  
;================ INPUT DATA PATH SETTINGS ====================================
;========== The following are OPTIONAL data path settings =====================
;----------If datatype not used, set data type path name='' -------------------

;data type names
  datype = [$
  'swe_levelzero',    $
  'swe_moments',      $
  'swe_fpitch',       $
  'mfi_mag3s',        $
  'swe_ionkp',        $
  'mfi_magkp',        $
  'swe_strahl',       $
  'isee_moments',     $
  'wav_tnr',        $
  'swe_redfcuts',     $
  'wav_hrtnr',      $
  'swe_strahlen',     $
  'wind_orbit',       $
  'swe_fparaperp',    $
  'wav_nekp',         $
  'wav_nehr' ,        $
  'exp_dtyp'  $
  ]


;data type path name     possible data paths for each data type
;NOTE: The default path will be the first path in the string array.
 
swe_levelzero_path   = ['/cdrom/cdrom0/data/wi/swe/lz/',$
                        '/data1/swe/lz/',$
                        '/mnt/leprjf_data2/swe/lz/']

swe_moments_path     = ['/mnt/leprjf_data7/swe/moments/',$
                        '/data1/swe/mpnew/',$
                        '/mnt/leprjf_data6/swe/mpnew/',$
                        '/mnt/leprjf_data7/swe/temp/',$
                        '/mnt/leprjf_data5/swe/moments2/',$
                        '/mnt/tinman_data3/swe/mpnew/',$
                        '/data1/swe/moments_cdf/',$
                        '/data1/swe/tmp_moments_cdf/'] 
                  
swe_fpitch_path      = ['/mnt/leprjf_data9/swe/ptch/',$
                        '/data1/swe/mpnew/',$
                        '/mnt/leprjf_data6/swe/mpnew/',$
                        '/mnt/tinman_data3/swe/mpnew/',$
                        '/cdrom/cdrom0/swetest/'] 
                 
mfi_mag3s_path       = ['/mnt/lepmfi_mfi/']        ;see NOTE1

mfi_magkp_path       = ['/mnt/lepmfi_kp/']         ;see NOTE1

swe_ionkp_path       = ['/mnt/leprjf_data5/swe/swekp/']

swe_strahl_path      = ['/mnt/leprjf_data8/swe/strl/',$
                        '/data1/swe/mpnew/',$
                        '/mnt/tinman_data3/swe/mpnew/',$
                        '/mnt/leprjf_data6/swe/mpnew/']   

isee_moments         = ['/data0/ftp/pub/imports/',$
                        '/cdrom/cdrom0/',$
                        '/data1/isee/iseecdf/',$
                        '/mnt/leprjf_data2/iseecdf']
                        ;ISEE-1 moments AND distrib functions

wav_tnr_path       = ['/mnt/lepmlk_tnr/']

wav_hrtnr_path     = ['/mnt/leprjf_data5/waves/']

swe_redfcuts_path    = ['/mnt/leprjf_data5/swe/redfc/',$
                        '/data1/swe/mpnew/',$
                        '/mnt/tinman_data3/swe/mpnew/',$
                        '/mnt/leprjf_data6/swe/mpnew/'] 

wind_orbit_path      = ['/mnt/leprjf_data5/swe/oa/',$
                        '/mnt/lepmfi_oa/']       ;see NOTE2

swe_fparaperp_path   = ['/mnt/leprjf_data5/swe/ptchav/',$
                        '/mnt/tinman_data3/swe/mpnew/',$
                        '/data1/swe/mpnew/']

wav_nekp_path        = ['/mnt/leprjf_data5/waves_nekp/']

wav_nehr_path        = ['/mnt/leprjf_data5/waves_nehr/'] 

exp_dtyp_path        = ['exp_dtyp data path(s)']
                                                                
;NOTE1
;  When reading SWE levelzero data, the program also reads mag3sec data, 
;  automatically.  If mag3sec data is unavailable, then magkp data is used, but
;  you MUST first read magkp as one of the selected types of survey data. 
;  If both mag3sec and magkp data are unavailable, then the program stops.

;NOTE2
;  If orbatt_path set to '' (not ' '), then spacecraft to GSE coordinate 
;  transformation done by simple rotation agout spacecraft X-axis


;define datapaths structure                                                           
  npaths=n_elements(datype)  
  datapaths=replicate({name:'',envar:'',list:strarr(10),ifsurvey:0},npaths)


;---- enter data type name, environment variable for data path, 
;---- and list of data paths as shown
 
;ifsurvey = 1 if datype is survey data, otherwise ifsurvey = 0 if levelzero data
                          
datapaths(0).name='swe_levelzero'
datapaths(0).envar='SWE_LZPATH'
datapaths(0).list(where(swe_levelzero_path ne ' '))=swe_levelzero_path
datapaths(0).ifsurvey=0

datapaths(1).list(where(swe_moments_path ne ' '))=swe_moments_path
datapaths(1).name='swe_moments'
datapaths(1).envar='SWE_MOMPATH'
datapaths(1).ifsurvey=1

datapaths(2).list(where(swe_fpitch_path ne ' '))=swe_fpitch_path
datapaths(2).name='swe_fpitch'
datapaths(2).envar='SWE_PITPATH'
datapaths(2).ifsurvey=1

datapaths(3).list(where(mfi_mag3s_path ne ' '))=mfi_mag3s_path
datapaths(3).name='mfi_mag3s'
datapaths(3).envar='MFI_MAGPATH'
datapaths(3).ifsurvey=1

datapaths(4).list(where(swe_ionkp_path ne ' '))=swe_ionkp_path
datapaths(4).name='swe_ionkp'
datapaths(4).envar='SWE_IONKPPATH'
datapaths(4).ifsurvey=1

datapaths(5).list(where(mfi_magkp_path ne ' '))=mfi_magkp_path
datapaths(5).name='mfi_magkp'
datapaths(5).envar='MFI_MAGKPPATH'
datapaths(5).ifsurvey=1

datapaths(6).list(where(swe_strahl_path ne ' '))=swe_strahl_path
datapaths(6).name='swe_strahl'
datapaths(6).envar='SWE_STRPATH'
datapaths(6).ifsurvey=1

datapaths(7).list(where(isee_moments ne ' '))=isee_moments
datapaths(7).name='isee_moments'
datapaths(7).envar='ISEE_PATH'
datapaths(7).ifsurvey=1

datapaths(8).list(where(wav_tnr_path ne ' '))=wav_tnr_path
datapaths(8).name='wav_tnr'
datapaths(8).envar='WAVES_TNR'
datapaths(8).ifsurvey=1

datapaths(9).list(where(swe_redfcuts_path ne ' '))=swe_redfcuts_path
datapaths(9).name='swe_redfcuts'
datapaths(9).envar='SWE_REDFCUTSPATH'
datapaths(9).ifsurvey=1

datapaths(10).list(where(wav_hrtnr_path ne ' '))=wav_hrtnr_path
datapaths(10).name='wav_hrtnr'
datapaths(10).envar='WAVES_HRTNR'
datapaths(10).ifsurvey=1

datapaths(11).list(where(swe_strahl_path ne ' '))=swe_strahl_path
datapaths(11).name='swe_strahlen'
datapaths(11).envar='SWE_STRAHLEN'
datapaths(11).ifsurvey=1

datapaths(12).list(where(wind_orbit_path ne ' '))=wind_orbit_path
datapaths(12).name='wind_orbit'
datapaths(12).envar='WIND_OAPATH'
datapaths(12).ifsurvey=1

datapaths(13).list(where(swe_fparaperp_path ne ' '))=swe_fparaperp_path
datapaths(13).name='swe_fparaperp'
datapaths(13).envar='SWE_PARAPERP'
datapaths(13).ifsurvey=1

datapaths(14).list(where(wav_nekp_path ne ' '))=wav_nekp_path
datapaths(14).name='wav_nekp'
datapaths(14).envar='WAV_NEKP'
datapaths(14).ifsurvey=1

datapaths(15).list(where(wav_nehr_path ne ' '))=wav_nehr_path
datapaths(15).name='wav_nehr'
datapaths(15).envar='WAV_NEHR'
datapaths(15).ifsurvey=1

datapaths(16).list(where(exp_dtyp_path ne ' '))=exp_dtyp_path
datapaths(16).name='exp_dtyp'
datapaths(16).envar='EXP_DTYP'
datapaths(16).ifsurvey=1
  
;=========== End of optional data path settings ===============================
;==============================================================================

;===== End of all user settings. No changes to be made below this point =======  
;=========== The following lines are required. No changes allowed!  ===========

wggs_base=wggs_dir+wggs_version+'/'  ;Full pathname of current version of WGGS.

setenv,'WGGSVERSION='+wggs_version
setenv,'WGGSBASE='+wggs_base
setenv,'SWEDATLIB='+wggs_base+'swelz/swedatlib/'     
setenv,'IDLSAV='+sav_dir+'idlsav/'        
setenv,'LOCALUSR='+local_usr
setenv,'DEFAULT_FONT='+default_font
setenv,'XSIZE_MAIN='+xsize_main
setenv,'YSIZE_MAIN='+ysize_main
setenv,'XSIZE_LZSRVY='+xsize_lzsrvy
setenv,'YSIZE_LZSRVY='+ysize_lzsrvy
setenv,'XSIZE_LZ='+xsize_lz
setenv,'YSIZE_LZ='+ysize_lz

!path=!path+':'+expand_path('+'+wggs_base)+':'+expand_path('+'+getenv('IDLSAV'))
!path=!path+':'+expand_path('+'+getenv('LOCALUSR'))

setenv,'GLINTPATH='+wggs_base+'swelz/swedatlib/glint/' ;glint mask
setenv,'BACKGPATH='+wggs_base+'swelz/swedatlib/backg/' ;VEIS background
setenv,'MPNEW='+mpnew       ;moments/pitch/strahl staging directory 
setenv,'MOMAVGPATH='+momavg ;VEIS moment averages
setenv,'IDLSAV2='+sav_dir_2 
setenv,'FSAVE_EXT=.fsav'
setenv,'FEVENTTYPE=_fshck'
setenv,'LOGICAL_UNITLZ=10'  ;set logical unit numbers
  
setenv,'COLORTABLE='+ctbl
setenv,'COLORTABLE_HARDCOPY='+ctbl_hc
loadct,fix(ctbl)   ;set default color table

;set default pitch angle file filter
pfilter=''       
setenv,'SPECIAL_PITLABEL=' 
 
;set default moments file filter
mfilter=''       
setenv,'SPECIAL_MOMLABEL='+'0potmod' 

setenv,'LZNOMAG=' ;ok'
setenv,'CREATSVFL=';/data1/swe/tmp_moments_savfil/'

;The idlsav scratch subdirectory will be created if it does not already exist.
  result=findfile(getenv('IDLSAV')+'mfilter',count=count)
  if count eq 0 then print,'Making subdirectory idlsav'
  if count eq 0 then spawn,'mkdir '+ getenv('IDLSAV')
  if count eq 0 then save,filename=getenv('IDLSAV')+'mfilter',mfilter
  if count eq 0 then save,filename=getenv('IDLSAV')+'pfilter',pfilter

  i=0
  n=' '
  result=findfile(getenv('IDLSAV')+'selection0',count=count)
  if count eq 0 then save,filename=getenv('IDLSAV')+'selection0',i,i,n,n

  result=findfile(getenv('IDLSAV')+'selection1',count=count)
  if count eq 0 then save,filename=getenv('IDLSAV')+'selection1',i,i,n,n

  result=findfile(getenv('IDLSAV')+'selection2',count=count)
  if count eq 0 then save,filename=getenv('IDLSAV')+'selection2',i,i,n,n

  result=findfile(getenv('IDLSAV')+'selection3',count=count)
  if count eq 0 then save,filename=getenv('IDLSAV')+'selection3',i,i,n,n

  save,filename=getenv('IDLSAV')+'printer_def_bw',$
    printer_name_bw,print_filename_bw

  save,filename=getenv('IDLSAV')+'printer_def_clr',$
    printer_name_clr,print_filename_clr

print,!path
print,'   WGGSBASE ',getenv('WGGSBASE')
print,'   SWEDATLIB ',getenv('SWEDATLIB')
print,'   IDLSAV ',getenv('IDLSAV')
print,'   LOCALUSR ',getenv('LOCALUSR')
print,'   GLINTPATH ',getenv('GLINTPATH')
print,'   BACKGPATH ',getenv('BACKGPATH')


;save input data type names and paths
  save,filename=getenv('IDLSAV')+'datapaths',datapaths
    
;set input data path environment variables  
  setpaths,/init
  
@compile_wi_h0_swe
  
;====== End of required user settings and non-changeable lines of code ========
;==============================================================================