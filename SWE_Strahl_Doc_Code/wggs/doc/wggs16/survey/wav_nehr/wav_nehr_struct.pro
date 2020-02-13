pro wav_nehr_struct,varbl,k,idatyp,swe_electrons,swe_moments

common shared,d
common wstuff,wst

if swe_electrons eq 0 then d.pnl(k).ztitle='WIND/WAVES'

case varbl of
  
  'wav_nehr': begin
      d.pnl(k).labl='Ne wavhr'  ;(/cm^3)
      wlt1=where(d.wav_nehrdat.n lt 1.)
      if wlt1(0) eq -1 then $
        d.pnl(k).range=[1.,100.] else $
        d.pnl(k).range=[0.1,100.]
      d.pnl(k).plotio=1
      d.pnl(k).ticks=2
      d.pnl(k).psym=0;3
      d.pnl(k).oplot=1
      d.pnl(k).oplotvar='Ni density'
   endcase
   
  'wav_qnehr': begin
      d.pnl(k).labl='Q wavhr'  ;(/cm^3)
      d.pnl(k).range=[1.,1000.]
      d.pnl(k).plotio=1
      d.pnl(k).ticks=2
      d.pnl(k).psym=0;3
   endcase
endcase

d.pnl(k).pltype='y(x)'
   
end

