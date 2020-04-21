; %Z%%M%  VERSION %I%    %G%   %U%
PRO cal_event,event
on_error,2

common widget_id, title, opt1, opt2, opt3, optq
common display, spnum, filename, unit, sppoint
widget_control, get_uvalue = i, event.id
case i of
1000:	print,' '
2000:	print,' '
    2100:	BEGIN
		  widget_control, opt1, get_value = val
		  filename=val(0)
		  spnum=1
		  widget_control, opt2, set_value = spnum
		  spawn,string('wc -l ',filename),result	;find # of lines 
		  reads,result,lines,format='(f)'	;convert str to long int
		  sppoint=lonarr(fix(lines/3289.)+5)

;pointer array of (lines in file / lines per spectrum ) elements (+5 for safety)
;this'll give an idea of how many spectra the file contains
;the array will contain pointers to the begining of spectra in the file

		  erase
		  openr,unit,filename,/get_lun
		  showcal,1,1,sppoint,sp=spnum,file=filename,/screen,/show4,unit=unit
		END

    2200:	begin
		  widget_control, opt2, get_value = val
		  spnum=val(0)
		  erase
		  showcal,1,1,sppoint,sp=spnum,file=filename,/screen,/show4,unit=unit
		end

    2300:	case event.value of
             	    0 : begin			;'+'
			erase
			spnum = spnum + 1
			widget_control,opt2,set_value = spnum
			showcal,1,1,sppoint,sp=spnum,file=filename,/screen,/show4,unit=unit
			end	
		    1 : begin			;'-'
			erase
			spnum = spnum - 1
			widget_control,opt2,set_value = spnum
			showcal,1,1,sppoint,sp=spnum,file=filename,/screen,/show4,unit=unit
			end
		endcase
 
    2400:       begin
		startplot
		if (!D.NAME eq 'PS') then BEGIN
		  print,'Creating file for printing...'
		  showcal,1,1,sppoint,sp=spnum,file=filename,/screen,/show4,unit=unit
		  hardplot
		  print,'Printing...'
		ENDIF
		print,'Done'
		end

    9999:	BEGIN
		  widget_control,event.top,/destroy
		  ;print,sppoint,format='(6i9)'
		  free_lun,unit 
		END
else:		begin & end	;this doesn't really belong here
endcase
end
;****************************************************************
;****************************************************************
;****************************************************************
PRO calgraph,FILE=file,SP=sp
on_error,2

;display paramater declaration
common display, spnum, filename, unit, sppoint
;graphic interface declaration
common widget_id, title, opt1, opt2, opt3, optq

if not keyword_set(FILE) then file = 'caldata'
if not keyword_set(SP) then sp = 1
filename = file
spnum = sp

base = widget_base(title = 'SWE Faraday Cup Calibration Data Display',/column)
;title = widget_label(base,uvalue=500,value=file)
graphs = widget_draw(base,/frame,uvalue=1000,xsize=750,ysize=650,colors=5)
options = widget_base(base,uvalue=2000,space=30,xoffset=0,/row)
	opt1 = cw_field(options,uvalue=2100,title='File:',$
				value=filename,/string,/return_events)
	opt2 = cw_field(options,uvalue=2200,title='Sp num',$
				value = spnum,xsize=3,/integer,/return_events)
        opt3 = cw_bgroup(options,['Next','Prev'],uvalue=2300,row=1,ids=ids)
	opt4 = widget_button(options,uvalu=2400,value='Print')
	optq = widget_button(options,uvalue=9999,value='Quit')
widget_control, /realize,base
widget_control, get_value = win, graphs
wset, win
!P.MULTI = [0,2,2,0,0]


spawn,string('wc -l ',filename),result	;find # of lines 
reads,result,lines,format='(f)'	;convert str to long int
sppoint=lonarr(fix(lines/3289.)+5)
print,'Approx number of spectra: ',fix(lines/3289.),format='(a,i3)'

;pointer array of (lines in file / lines per spectrum ) elements (+5 for safety)
;this'll give an idea of how many spectra the file contains
;the array will contain pointers to the spectra in the file

openr,unit,filename,/get_lun
showcal,1,1,sppoint,sp=spnum,file=filename,/screen,/show4,unit=unit

xmanager, 'cal', base
end
;****************************************************************
;****************************************************************
;****************************************************************
