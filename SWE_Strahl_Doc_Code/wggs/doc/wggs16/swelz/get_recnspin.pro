

;====================== get_recnspin ====================================
 
pro get_recnspin

common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common mpstuff,mflnm,pflnm,mdat,pdat,refsec
common shared,d
common wstuff,wst
common swestuff,swest
common sharelevelzero,pltwin_frmt,oplot_sec

;-------------------------------------------------------------
print,'get_recnspin: wst.lzdate ',wst.lzdate,' wst.timsel ',wst.timsel
case wst.timsel of
 'survey' : begin
          
       tpoint1=refsec+wst.xydata(0)*3600.d ;secs of selctd point from tjd epoch 
       print,' ' & print,'selected point in time ',$
         wst.xydata(0),'  ',wst.hms,'  ',tpoint1,'  ',wst.pb5
       ;spn is the index of mdat.ta or pdat.ta corresponding to tpoint

       case 1 of

       d.wdatype(0,0) ne -1 : begin                 ;moments yes
         swest.spn=long(indx_begin(mdat.ta,tpoint1 ))
         hour_hms,(mdat(swest.spn).ta - refsec)/3600.d,hmsspn
         recn=mdat(swest.spn).mfrec     ;+1
         swest.ispinbl=mdat(swest.spn).mfspinbl
         print,'spn,mdat(spn).ta,hmsspn,recn,swest.ispinbl',$
           swest.spn,mdat(swest.spn).ta,hmsspn,recn,swest.ispinbl

                              endcase

       d.wdatype(0,0) eq -1 and $
       (d.wdatype(0,1) ne -1 or d.wdatype(0,2) ne -1): begin  ;mom no, pitch yes
          swest.spn=long(indx_begin(pdat.ta,tpoint1 ))
          hour_hms,(pdat(swest.spn).ta - refsec)/3600.d,hmsspn
          recn=pdat(swest.spn).mfrec    ;+1
          swest.ispinbl=pdat(swest.spn).mfspinbl
          print,'spn,pdat(spn).ta,hmsspn,recn,swest.ispinbl',$
          swest.spn,pdat(swest.spn).ta,hmsspn,recn,swest.ispinbl
                                                        endcase
       else : begin    ;moments and pitch no
          recn=lztimrec(sec_pb5(tpoint1))
          swest.ispinbl=0
          swest.isector=0
          ;swest.ivstep=0
              endcase
       endcase
             endcase

 'lz' : print,'rec, swest.ispinbl ',recn,swest.ispinbl, 'selected'

 'lztm' : begin
            ;case pltwin_frmt of
            ;0: begin
            ;     pb5=ymd_pb5(long(wst.lzdate))
            ;     pb5(2)=wst.xydata(0)*3600000
            ;     recn=lztimrec(pb5)
            ;     swest.ispinbl=0
            ;     print,'selected point in time ',$
            ;     wst.xydata(0),'  ',wst.hms,'  ',wst.pb5
            ;   endcase

            ;1: begin
                recn=lztimrec(wst.pb5)
                ; swest.ispinbl=0
                print,'selected point in time ',wst.pb5
               ;endcase
            ;endcase
          endcase

 else : begin
   recn=1 & swest.ispinbl=0
        endcase

endcase

wst.timsel='lz'    ;reset

end   




