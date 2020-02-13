pro wav_nekp_struct,varbl,k,idatyp,swe_electrons,swe_moments

common shared,d
common wstuff,wst

if swe_electrons eq 0 then d.pnl(k).ztitle='WIND/WAVES'

case varbl of
  
  'wav_nekp': begin
      d.pnl(k).labl='Ne wavkp'  ;(/cm^3)
      wlt1=where(d.wav_nekpdat.n lt 1.)
      if wlt1(0) eq -1 then $
        d.pnl(k).range=[1.,100.] else $
        d.pnl(k).range=[0.1,100.]
      d.pnl(k).plotio=1
      d.pnl(k).ticks=2
      d.pnl(k).psym=4;3
      d.pnl(k).symsize=0.2
      d.pnl(k).oplot=1
      d.pnl(k).oplotvar='Ni density'
   endcase
   
  'wav_qnekp': begin
      d.pnl(k).labl='Q wavkp'  ;(/cm^3)
      d.pnl(k).range=[1.,1000.]
      d.pnl(k).plotio=1
      d.pnl(k).ticks=2
      d.pnl(k).psym=4;3
      d.pnl(k).symsize=0.2
   endcase
endcase

d.pnl(k).pltype='y(x)'
   
end

