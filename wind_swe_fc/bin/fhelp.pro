PRO display, filename, append=append	
			;typically called with a string instead of variable
on_error, 2		;so common and file=file(0) exist to make file=string
 if not KEYWORD_SET(append) then append = 0
 common sharef, file, file_lun 	;file is shared to preserve the filename memory
 common sharew, w
 file=filename(0)      	;it converts string arrays to strings
 input=readfile(file)
 WIDGET_CONTROL,w.text,set_value=input,append=append
END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO fhelp_event, event
on_error, 2

common sharef, file, file_lun
common sharel, nextrab, window, last_option, topic, topic_list, quit_list, anal_list
common sharew, w
;print,topic
help,event,/stru	;terribly useful for development purposes
CASE event.id OF

    w.base:  BEGIN
                                ;print,' nextrab,event.y'
                                ;print, nextrab, event.y
                                ;print, 35+(event.y-(640+46*nextrab))/15
        IF (event.y gt (640+46*nextrab)) then BEGIN
            WIDGET_CONTROL,w.text,ysize=35+(event.y-(640+46*nextrab))/15
        ENDIF
    ENDCASE
    w.quitb.button:    BEGIN    ;Quit, Close, or Help
        CASE event.value OF
            0:	WIDGET_CONTROL, event.top, /DESTROY
            1:  WIDGET_CONTROL, event.top, /ICONIFY
            2:  BEGIN
                IF window ne 97 then BEGIN
                    WIDGET_CONTROL, w.text, set_value='Help Help.  We''re sorry.'
                    WIDGET_CONTROL, w.text, /append, $
                      set_value='The Option you have just hit is imaginary.'
                    WIDGET_CONTROL, w.text, /append, set_value=['Please rotate '+$
                                                                'your head by 90 degrees and try again.']
                    window=97
                ENDIF ELSE BEGIN
                    WIDGET_CONTROL, w.text, set_value=['Help Help Help.  '+$
                                                       'Thank you for flying fhelp helpways...']
                    display,'~wind/help/General.fhelp',/append
                    window=-1
                ENDELSE
            END
        ELSE: BEGIN & END
        ENDCASE
    ENDCASE

    w.button(1):	BEGIN   ;Help Options
                                ;Begin Clean up of mess from last Help Option
        CASE last_option OF
            -1:
            1:  BEGIN
                WIDGET_CONTROL, w.extrab(1), /DESTROY
                nextrab=nextrab-1
            END
            5:                  ;WIDGET_CONTROL, w.extrab(5), /DESTROY
            6:                  ;WIDGET_CONTROL, w.extrab(6), /DESTROY
            7:                  ;WIDGET_CONTROL, w.extrab(7), /DESTROY
            else:  
        ENDCASE
                                ;End Clean up of mess from last Help Option
                                ;Announce new topic
        topic='Topic: '+topic_list(event.value)
        WIDGET_CONTROL, w.quitb.field, set_value=topic
                                ;Execute Help option
        CASE event.value OF
            0:  display,'~wind/help/General.fhelp'
            1:	BEGIN
                IF ( not WIDGET_INFO(w.extrab(0),/VALID_ID) ) then BEGIN
                    w.extrab(1)=WIDGET_BASE(w.base, /row, map=1,space=20)
                    w.button(2)=cw_bgroup(w.extrab(1), anal_list, /row,$
                                          label_left='Key Param Analysis')
                    nextrab=nextrab+1
                ENDIF
            END
            2:  display,'~wind/help/SIM.outline'
            3:  display,'~wind/help/VTTOOL_LZ_RECOVERY'
            4:  display,'~wind/help/DMP.FHELP'
            5:  IF ( not WIDGET_INFO(w.extrab(5),/VALID_ID) ) then BEGIN
                w.extrab(5)=WIDGET_BASE(w.base, /row, space=0)
                w.field(5)=cw_field(w.extrab(5), /string, /return_events,$
                                    xsize=30, value='', title='Enter file path/name:')
                w.extrab(8)=WIDGET_BASE(w.extrab(5),/row,/NONEXCLUSIVE)
                w.button(5)=widget_button(w.extrab(8),value='Read Only')
                w.button(8)=widget_button(w.extrab(5),value='Dismiss')
                nextrab=nextrab+1
            ENDIF
            6:  BEGIN
            END
            7:  IF ( not WIDGET_INFO(w.extrab(7),/VALID_ID) ) then BEGIN
                w.extrab(7)=WIDGET_BASE(w.base, /row, space=20)
                w.field(1)=cw_field(w.extrab(7), /string, /return_events,$
                                    xsize=30, value='', title='Enter command:       ')
                w.button(7)=widget_button(w.extrab(7),value='Dismiss')
                WIDGET_CONTROL,w.field(1),/input_focus
                nextrab=nextrab+1
            ENDIF
        ENDCASE
        last_option = event.value
    ENDCASE
    
    w.button(2):    BEGIN       ;Key Param Buttons
        CASE event.value OF
            0:	display,'/plasma/h1/wind/source/analysis/key_p/README'
            1:  display,'/plasma/h1/wind/source/analysis/key_p/Makefile'
            
            2:  BEGIN           ;Run testkp (fortran shell version)
                widget_control,w.text,set_value='Running testkp'
                widget_control,w.text,set_value='Please wait...',/APPEND
                spawn,['/plasma/h1/wind/source/analysis/key_p/testkp'+$
                       ' >& /tmp/fhelp_kp']
                display,'/tmp/fhelp_kp'
            END
            3:  display,'~fvm/GUIDE'
        ENDCASE
    ENDCASE
    
    w.button(5):	BEGIN   ;Read Only/Save File toggle button
        CASE event.select OF    ;This is frank's little file editor...
            0:	BEGIN           ;0 = you released the button
                WIDGET_CONTROL,w.button(5),get_value=val
                IF (val(0) eq 'Save File??') then dismiss=1 else dismiss=0
                WIDGET_CONTROL,w.button(5),set_value='Read Only'
                WIDGET_CONTROL,w.field(5),get_value=savefile
                WIDGET_CONTROL,w.field(5),set_value=string('Saving file ',savefile(0))
                WIDGET_CONTROL,w.text,editable=0
                WIDGET_CONTROL,w.text,get_value=output
                IF (not writefile(savefile(0),output)) then 	BEGIN
                    WIDGET_CONTROL,w.field(5),$
                      set_value=['Error saving file: '+savefile(0)]
                    wait,5
                ENDIF
                file=savefile(0)
                WIDGET_CONTROL,w.field(5),set_value=file
                IF (dismiss) then BEGIN
                    WIDGET_CONTROL,w.extrab(5),/DESTROY
                    dismiss=0
                    nextrab=nextrab-1
                    last_option = -1
                ENDIF
                WIDGET_CONTROL,w.button(1),/sensitive
            END
            1:  BEGIN           ;1=you pushed the button down
                WIDGET_CONTROL,w.button(1),sensitive=0
                WIDGET_CONTROL,w.field(5),get_value=file
                WIDGET_CONTROL,w.button(5),set_value='Save File'
                WIDGET_CONTROL,w.text,/editable
                savefile=string(file(0),'.fhelp')
                WIDGET_CONTROL,w.field(5),set_value=savefile(0)
            END
        ENDCASE
    ENDCASE
    
    w.button(8):	BEGIN	;Dismiss button: gets rid of extra widget base
        WIDGET_CONTROL,w.text,editable=0
        WIDGET_CONTROL,w.button(5),get_value=mode
        mode = mode(0)
        CASE mode OF
            'Read Only':BEGIN	;normal Dismiss
                WIDGET_CONTROL,w.button(1),/sensitive
                WIDGET_CONTROL,w.extrab(5),/DESTROY
                nextrab=nextrab-1
            ENDCASE
            'Save File':BEGIN	;Dismiss if file still editable
                WIDGET_CONTROL,w.button(5),set_value='Save File??'
                WIDGET_CONTROL,w.button(5),set_value='Ignore Edit'
            ENDCASE
            'Save File??': BEGIN ;Dismiss but don't save edits
                display,file
                WIDGET_CONTROL,w.button(1),/sensitive
                WIDGET_CONTROL,w.extrab(5),/DESTROY
                nextrab=nextrab-1
            ENDCASE
        ENDCASE
        last_option = -1	      
    ENDCASE
    w.button(7):	BEGIN	;Dismiss button: gets rid of extra widget base
        WIDGET_CONTROL,w.extrab(7),/DESTROY
        nextrab=nextrab-1
        last_option = -1
    ENDCASE
    
    w.field(1):	BEGIN		;gets command and passes it to a C shell
        WIDGET_CONTROL, w.field(1), get_value=val
        command=val(0)
        WIDGET_CONTROL, w.text, set_value=string(command,':')
        command = string('(',command,') |& cat')
                                ;print,command
        spawn,command,result
                                ;print,result
        WIDGET_CONTROL, w.text, set_value=result,/APPEND
        WIDGET_CONTROL, w.field(1), set_value=''
    ENDCASE
    
    w.field(5):	BEGIN		;gets filename and displays file in w.text
        WIDGET_CONTROL, w.button(5), get_value=mode
        mode = mode(0)
        WIDGET_CONTROL, w.field(5), get_value=val
        print,mode,' ',val(0)
        IF (mode ne 'Save File') then BEGIN
            input=readfile(val(0))
            WIDGET_CONTROL, w.text, set_value=input
        ENDIF ELSE savefile=val(0)
    ENDCASE
    
ELSE:	BEGIN & END
ENDCASE
END
;		  input='You have entered a unix shell.'
;		  WIDGET_CONTROL,w.text,set_value=input
;		  input='Type "exit" to return to program'
;		  WIDGET_CONTROL,w.text,set_value=input,/APPEND
;		  spawn 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO fhelp, GROUP = GROUP
on_error,2

common sharef, file, file_lun
common sharel, nextrab, window, last_option, topic, topic_list, quit_list, anal_list
common sharew, w

nextrab=0                       ;how many extra bases are open
window = -1			;top row buttons
last_option  = -1		;last_option stores index to last extrab used
topic = 'Select a topic'        ;initial topic string
file  = ''                      ;declare file as a null string
;file = '~wind/help/Welcome'		;startup welcome file
                                        ;set up widget structure
  w={w_widgets,base:0l,$			;base widget
	       quitb:0l,topicb:0l,text:0l,$	;3 major components of base
	       extrab:lonarr(10),$		;an array of extra bases to pop up
	       button:lonarr(10),$		;you guessed it: buttons
	       field:lonarr(10)}		;display and input fields

  w.base=widget_base(title='HELP!', /column, /tlb_size_events)

  w.quitb=widget_base(w.base, /row, /frame, space=20)
  quit_list=['Quit','Close','Help']
  w.button(0)=cw_bgroup(w.quitb, quit_list, column=3)
  w.field(0)=widget_label(w.quitb, value = topic)

  w.topicb=WIDGET_BASE(w.base,/row,/frame)
  topic_list=$
  [ 'General Help',   $
    'Key Param Anal', $
    'FC Data Sim',    $
    'lz  Files',      $
    '.dmp Files',     $
    'Display File',   $
    'Edit Text',      $
    'fmcsh'	      ]
  w.button(1)=cw_bgroup(w.topicb, topic_list, row=2,$
		        label_top='Help Options', /exclusive)
;;;;;;;;;;;;;;;;;;;;;;;;;;
;this is where we keep the unmapped WIDGET_BASEs
;they can vary any way and be mapped and unmapped at will (i hope)
;the first one will be the analysis widget heirarchy
;  w.extrab(0)=WIDGET_BASE(w.base, /row, /frame, map=0, title='ANALYSIS')
  anal_list=$
  [ 'README',$
    'Making testkp',$
    'Run testkp',   $
    'Extra notes'   ]
;  w.button(2)=cw_bgroup(w.extrab(0), anal_list,/row, label_top='testkp analysis')
;  i decided to realize extra bases later
;  as soon as a child is declared in an existing base, the child is realized
;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;input=readfile(file(0))	;this is for the startup text
  w.text=widget_text(w.base,/scroll,ysize=35,xsize=81,$
    value='                              Welcome to HELP!')

WIDGET_CONTROL, w.base, /REALIZE
XMANAGER, "fhelp", w.base, GROUP_LEADER = GROUP  ;hand off to manager 
END
