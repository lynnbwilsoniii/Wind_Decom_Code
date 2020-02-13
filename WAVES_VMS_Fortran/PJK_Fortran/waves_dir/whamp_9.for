      program whamp
C
C  Modification par Gerard  Belmont pour que 
C                            le calcul de "xsi" soit plus precis (01/91)
C        CHANGEMENT DU SIGNE DE XSI(5) (Belmont, 02/91).
C  Double precision, indispensable sur VAX, dans le calcul des fonctions
C  Z par RYLA, dans le calcul de la susceptibilite par KSI, et dans DIFU
C  pour le tenseur dielectrique, l'equation d'onde et la polarisation.
C
C Changed by Jim Crumley to pull out the flat file stuff.  Just
C the pure, ugly ASCII for me.  (ALL of the removed lines start with cjpc).
C
C	CALLS:	
C	INIGENE, USES BLOCK DATA TO INITIALIZE
C		INIMAXW      	CALCULATES TEMPERATURE FOR BIMAXWELLIAN
C		ECRILIS		WRITES THESE INITIAL VALUES TO OUTPUT
C	IF(IPLAS.EQ.1) CALL DENOFPE	NORMALIZE TO FP?
C	NEWINPT
C		TYPIN		READ NEW INPUT FROM .COM FILE
C		INIZP		?
C	IF(IPLAS.EQ.1) CALL FRQPLAS			
C	ZPINIT
C	IF(IPLAS.EQ.1) CALL FRQPLAS			
C	INITXZP
C	ITERAT
C		CALLS DIFU 20 TIMES
C		AND KSI(XSI,J,IB,KOL,IERR)
C
      double complex eps(6,4),dir,dix,diz,dip,efl(3),bfl(3),ZPL,ZMO
      complex x,xo,xvo,xx(6),xp(6),dx,ome,fpx,dox,doz,dop,ri
      complex compze,polbk,compz,compy,compye,vp,ZPSM,comte1,comte2
Cqi   character*2 itime(3),idate(3)
      character*72 icarp(5),icarpe
      character*5 stext
      character stex4*10
      common /infile/ ifile
      common /reinit/ dop,dox,doz,kv,dx,xo,xvo,po,zo,zlg,plg,pvo,zvo
      common /xpz/ xx,pp(6),zz(6),a(6),b(6),d(6),ass(6),vd(6),
     +   dn(6),ta(6),xp,cv,pm(3),zm(3),xoi,xc,pzl,aixoi
      common /disfunc/ ren(6),isp(6),jma,red,den,st(6),px,rn,t(6)
      common /cout/ x,p,z,efl,bfl,dir,dix,diz,dip,eps,vg(2),sg(2),ri
      common/be/ibern,ipol,iplas,pn,gyr,itrin
      common /iniv/ ome,fpx,pfq,xa,dek
Cqi   common /entsor/ listing,stex(7),stext(7),wfreq,niou
      common /entsor/ listing,wfreq,niou,stex(7),stext(7)
      common /polar/ compz(6),polbk,compze,compy(6),compye,vp(6,3),
     +  ZPL(3),ZMO(3),ZPSM(3),zp2sm2,finebp,fineby,
     +  finbt1,finbt2,comte1,comte2
      common /entex/ stex4
      common /initra/ iabso
      dimension vref(6)
      ifile=5
      listing=25
      open(unit=25,file='reswhamp.lis',status='new')
      do 62 i=1,6
  62  vref(i)=0.
c
   51 format(/,25x,'too heavily damped :')
  125 format(/,25x,'no convergence :')
  112 format(1x,a3,f6.4,1x,a3,f8.5,1x,a3,e11.4,1x,e11.4,a10,2e10.2)
  113 format(1x, (a3,f7.4,1x),a3,e10.3,1x,e10.3,2x,a10,2e10.2)
c
c             initialisations
      ibel=0
      iabso=0
      ibelo=0
      npl=0
      wfreq=0.0
      itrin=0
    1 continue
c
c	inigene initializes all parameters, and writes to reswhamp.
c		this happens before new values are read in from typin.
c
      call inigene(listing,vref)
c
      if(npl.eq.1) goto 6
c                  ****  ask for input  ****
    5 continue
c     if(itrin.eq.1)go to 605
c     itrin=0
c 601 continue
      if(iplas.eq.1)call denofpe
  605 continue
      call newinpt(npl,kfs,icarp,icarpe)
      if(npl.eq.1) goto 1
    6 npl=0
      k=0
      l=0
      kv=1
      if(iplas.eq.1) call frqplas
      call zpinit
    7 continue
      if(wfreq.ne.0)call trinit(xoi,wfreq,*600,*605)
  600 continue
      if(xoi.eq.0.)xoi=1e-6
      x=cmplx(xoi,aixoi)
	write(listing,*) 'after main 600, freq=',x
   10 continue
c
c	inixzp calculates appropriate k for each component
c
      call inixzp
	write(listing,*) 'after inixzp,zz=',zz
	write(listing,*) 'after inixzp,xx=',xx
	write(listing,*) 'after inixzp,pp=',pp
c
c                  ****  start of iteration.  ****
      call iterat(k,l,*30,*255,*7,*10)
c      30 : convergence
c     255 : non convergence
c       7 : nouveau x,z,p
c      10 : nouvel x
c
  255 continue
      stex4='no convgce'
      write(listing,125)
      call outpt
      if(niou.eq.1)type 113,stext(1),stex(2),stext(5),
     1 (stex(i),i=6,7),stex4,polbk
      if(niou.eq.2)type 112,stext(1),stex(2),stext(3),stex(4),
     1 stext(5),stex(6),stex(7),stex4,polbk
      if(wfreq.ne.0)call trinit(xoi,wfreq,*600,*605)
      ibel=1
      if(kv.eq.1) ibelo=1
CLacombe  calcul de la polar meme si non convergence :
CLacombe      goto 344
      go to 30
c                  ****  convergence  **********************************
   30 continue
      k=0
      call difu(4,jma,ierr)
      if(ierr.ne.0) goto 50
      x=x-dir/dix
c
      call compol(jma)
      xi=aimag(x)
c
c    delta vitesse groupe:vg-vderive
      vg(1)=-dip/dix-vref(2)
      vg(2)=-diz/dix-vref(2)
c
      ri=sqrt(p**2+z**2)*cv/x
      if(ipol.eq.1) ri=p*cv/x
      if(vg(1).ne.0.) sg(1)=xi/vg(1)
      if(vg(2).ne.0.) sg(2)=xi/vg(2)
c          ****  type the results  *************************************
      stex4=' '
      call outpt
c
      if(niou.eq.1)type 113,stext(1),stex(2),stext(5),
     1 (stex(i),i=6,7),stex4,polbk
      if(niou.eq.2)type 112,stext(1),stex(2),stext(3),stex(4),
     1 stext(5),stex(6),stex(7),stex4,polbk
      if(wfreq.ne.0)call trinit(xoi,wfreq,*600,*5)
  344 call updatpz(listing,kfs,ibel,ibelo,*5,*7,*140)
c
  140 continue
      call nwstrtf
      go to 10
   50 continue
      stex4='too damped'
      write(listing,51)
CLacombe :  
C    calcul de la polar meme si amortissement trop grand (ierr.ne.0) :
      call compol(jma)
      call outpt
      if(niou.eq.1)type 113,stext(1),stex(2),stext(5),
     1 (stex(i),i=6,7),stex4,polbk
      if(niou.eq.2)type 112,stext(1),stex(2),stext(3),stex(4),
     1 stext(5),stex(6),stex(7),stex4,polbk
      if(wfreq.ne.0)call trinit(xoi,wfreq,*600,*605)
      goto 344
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine newinpt(npl,kfs,icarp,icarpe)
c
c     ce s/p demande les parametres pour un nouveau balayage
c
      complex xx(6),xp(6)
      character*72 icarp(5),icarpe
      common /xpz/ xx,pp(6),zz(6),a(6),b(6),d(6),ass(6),vd(6),
     +   dn(6),ta(6),xp,cv,pm(3),zm(3),xoi,xc,pzl,aixoi
      common/be/ibern,ipol,iplas,pn,gyr,itrin
      common /entsor/ listing,wfreq,niou,stex(7),stext(7)
      character*5 stext
      character stex4*10
      common /entex/ stex4
      common /initra/ iabso
      call typin(npl,kfs,icarpe,icarp)
      call inizp
      if(ipol.eq.1) write(listing,55)
      if(iplas.eq.1) write(listing,56)
      if(ipol.eq.1)type 55
      if(iplas.eq.1) type 56
      gyr=float(ifix(xoi))
      if(ibern.eq.1) xoi=gyr+.5
   55 format( '  option r:  representation polaire.'/
     + '  p=module(k), z=angle(k)/perp.(radians)'/)
   56 format( '  option y:  z,p,f normalises a fpe '/)
      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine typin(npl,kfs,icarpe,icarp)
c        arguments: npl    new plasma. when a plasma parameter is
c                                      changed, npl is set to 1.
c                   kfs    =1  p specified last.
c                          =2  z specified last.
      common /ffdata/ ffdopen, id1, id2, lrecl
      common /ftimes/ tstrt,tfin
      common /infile/ ifile
      real*8       tstrt,tfin,tdelta
      character    cc*1,ic*1
      character    ltrs1*26,ltrs2*26,digits*10
      character*72 icarpe,icarp(5),inp
      parameter( cc = ' ' )
      parameter( ltrs1 = 'abcdefghijklmnopqrstuvwxyz' )
      parameter( ltrs2 = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' )
      parameter( digits = '0123456789' )
c
      common /xpz/ array(89)
      common/be/ibern,ipol,iplas,pn,gyr,itrin
      common /entsor/ listing,wfreq,niou,stex(7),stext(7)
      character*5 stext
      common /initra/ iabso
      character stex4*10
      common /entex/ stex4
      dimension tv(2)
      data iout/2/,kp,kz/1,1/
c
c **** correspondence between array and names in whamp:  ****
c  array(iof+ 0 12 18 24 30 36 42  48 54 60 66 78 79 82 85  86  87 88
c            xx pp zz a  b  d  ass vd dn ta xp cv pm zm xoi xc  pzl aixo
c
c
    1 iv=1000
c
      ios = 1
   70 continue
      if (ios .ne. 0 ) then
         type '( / 2a )',  cc, 'input: '
         write(listing,*) cc, 'input: '
         read( ifile, '( a )', iostat = ios )  inp
         if (ios .ne. 0) rewind ifile
         goto 70
      endif
c
      type*,inp
      write(listing,*)inp
      icarpe=inp
c
      nc=0
    4 nc=nc+1
      if (nc.le.72) then
        ic=inp(nc:nc)
        if(ic.eq.' ') goto 4
        if (iv.le.88) then
          if ((index( ltrs1, ic ) .le. 0 ).and.
     +        (index( ltrs2, ic ) .le. 0 )) then
c* 'ic' n'est pas une lettre
            if( index( digits, ic ) .ne. 0 ) then
c* 'ic' est un chiffre
              tv(ie)=tv(ie)*dek+( index( digits, ic ) - 1 )*dec
              dec=dec*dek/10.
              nv=1
              goto 4
            elseif (ic.ne.',') then
              if (ic.eq.'-') then
c* 'ic' est le signe moins
                if (tv(ie).ne.0.) then
                  type 61,ic
                  type 62
                  goto 1
                endif
                dec=-dec
              elseif (ic.eq.'.') then
c* 'ic' est le point d'un flottant
                if (abs(dec).ne.1.) then
                  type 61,ic
                  type 62
                  goto 1
                endif
                dek=dek/10.
                dec=dec/10.
              elseif(ic.eq.')') then
c* 'ic' est une parenthese fermante
                if ((dec.ne.1.).or.
     +              (ie.ne.1.or.tv(1).le.0.)) then
                  type 61,ic
                  type 62
                  goto 1
                endif
                iof=tv(1)+.1
                tv(1)=0.
                nv=0
              endif
              goto 4
            endif
          endif
        endif
      endif
      if (iv.le.88) then
        if (nv.eq.1) then
          if ((iof.gt.6).or.
     +        (iof.gt.3.and.iv.gt.66).or.
     +        (iof.gt.1.and.iv.gt.85)) then
            tv(1)=tv(1)*10.**tv(2)
            type 63,tv(1)
            type 62
            goto 1
          endif
          if (ic.eq.'e'.or.ic.eq.'E') then
            ie=2
            dek=10.
            dec=1.
            goto 4
          endif
          array(iv+iof)=tv(1)*10.**tv(2)
          iof=iof+1
          if(iv.eq.79) kp=iof
          if(iv.eq.82) kz=iof
        endif
      endif
      if (nc.le.72) then
        dek=10.
        dec=1.
        tv(1)=0.
        tv(2)=0.
        nv=0
        ie=1
        if(ic.eq.',') goto 4
        iv=1000
        iof=1
        if(ic.eq.'a'.or.ic.eq.'A') then
          iv=24
        elseif(ic.eq.'b'.or.ic.eq.'B') then
          iv=30
        elseif(ic.eq.'c'.or.ic.eq.'C') then
          iv=86
        elseif(ic.eq.'d'.or.ic.eq.'D') then
          iv=36
        elseif(ic.eq.'f'.or.ic.eq.'F') then
          iv=85
        elseif(ic.eq.'i'.or.ic.eq.'I') then
          iv=88
        elseif(ic.eq.'h'.or.ic.eq.'H') then
          type 65
          type 66
          type 67
          goto 1
        elseif(ic.eq.'l'.or.ic.eq.'L') then
          iv=87
        elseif(ic.eq.'m'.or.ic.eq.'M') then
          iv=42
        elseif(ic.eq.'n'.or.ic.eq.'N') then
          iv=54
        elseif(ic.eq.'o'.or.ic.eq.'O') then
          iout=0
          goto 4
        elseif(ic.eq.'p'.or.ic.eq.'P') then
          iv=79
        elseif(ic.eq.'q'.or.ic.eq.'Q') then
          iv=100
        elseif(ic.eq.'r'.or.ic.eq.'R') then
          iv=101
        elseif(ic.eq.'s'.or.ic.eq.'S') then
          if(ffdopen.eq.1) then
            tdelta = 0.0
cjpc	    call fhputinfo(id1,ierr,tstrt,tfin,'DARIUS',1.0e34,tdelta,0,0)
cjpc	    call ffclose(id1, id2, ierr)
	  endif
          stop
        elseif(ic.eq.'t'.or.ic.eq.'T') then
          iv=60
        elseif(ic.eq.'v'.or.ic.eq.'V') then
          iv=48
        elseif(ic.eq.'x'.or.ic.eq.'X') then
          goto 1
        elseif(ic.eq.'y'.or.ic.eq.'Y') then
          iv=102
        elseif(ic.eq.'w'.or.ic.eq.'W') then
          iv=103
        elseif(ic.eq.'g'.or.ic.eq.'G') then
          iv=104
        elseif(ic.eq.'z'.or.ic.eq.'Z') then
          iv=82
        endif
c
        if (iv.lt.1000) then
          if(iv.lt.66.or.iv.eq.86) then
            npl=1
          elseif(iv.eq.79) then
            kp=1
            kfs=1
          elseif(iv.eq.82) then
            kz=1
            kfs=2
          elseif(iv.eq.100) then
            ibern=1-ibern
          elseif(iv.eq.101) then
            ipol=1-ipol
          elseif(iv.eq.102) then
            iplas=1-iplas
          elseif(iv.eq.103) then
            wfreq=1
          elseif(iv.eq.104) then
            iabso=1
          endif
          goto 4
        endif
c
        ios = 1
   80   continue
        if (ios .ne. 0) then
           type '( / 2a )',  cc,  ' help, yes or no?'
           read( ifile, '( a )', iostat = ios )  ic
           if (ios .ne. 0) rewind ifile
           goto 80
        endif
        if(ic.eq.'n'.or.ic.eq.'N') goto 1
        type 65
        type 66
        type 67
        goto 1
      endif
c
   50 goto (52,53,54,55) kp
   53 array(81)=array(80)
   54 array(82)=array(81)-array(80)
   55 if(array(82).eq.0.) array(82)=10.
   52 goto (56,57,58,59) kz
   57 array(84)=array(83)
   58 array(85)=array(84)-array(83)
   59 if(array(85).eq.0.) array(85)=10.
   56 if(iout.ne.1) call inout
      iout=1
c
   61 format(' ambiguity caused by the character "',a1,'"')
   62 format(' the rest of the line is ignored. please try again]')
   63 format(' the value',e11.3,' will not fit in the variable field')
   65 format(' an input line may consist of up to 72 characters.'/
     +  ' the format is:'/' name1=v11,v12,v13,...name2=v21,v22,...name'/
     +  ' the names are chosen from the list:'//
     +  ' name              parameter'/
     +  ' a(i)              the alpha1 parameter in the distribution.'/
     +  '                   (i) is the component number, i=1 - 6.'/
     +  ' b(i)              the alpha2 parameter in the distribution.'/
     +  ' c                 the electron cyclotron freq. in khz.'/
     +  ' d(i)              the delta parameter in the distribution'/
     +  ' f                 frequency, start value for iteration.'/
     +  ' i                 imaginary part of frequency start value'/
     +  ' l            l=1  the p and z parameters are interpreted'/
     +  '                   as logarithms of the wave numbers. this'/
     +  '                   option allows for logarithmic steps.'/
     +  '              l=0  default value. linear steps.'/
     +  ' m(i)              mass in units of proton mass.'/
     +  ' n(i)              number density in part./cubic meter'/
     +  ' p(i)              perpendicular wave vector components.'/
     +  '                   p(1) is the smallest value, p(2) the'/
     +  '                   largest value, and p(3) the increment.')
   66 format(  ' s          stop:terminates the program.'/
     +  ' t(i)                  temperature in kev'/
     +  ' v(i)              drift velocity / thermal velocity.'/
     +  ' z(i)              z-component of wave vector. i has the'/
     +  '                   same meaning as for p(i).'/
     +  ' a name without index refers to the first element, "a" is '/
     +	' thus equivalent to "a(1)". the values v11,v12,.. may be '/
     +  ' specified in i-, f-, or e-format, separated by comma(,).'/
     +  ' the "=" is optional, but makes the input more readable.'/
     +  ' example: input:a1.,2. b(3).5,p=.1,.2,1.e-2'/
     +  ' this sets a(1)=1., a(2)=2., b(3)=.5, p(1)=.1, p(2)=.2,'/
     +  ' and p(3)=.01. if the increment p(3)/z(3) is negative, p/z'/
     +	' will first be set to p(2)/z(2) and then stepped down to'/
     +  ' p(1)/z(1)'/
     +  ' the last specified of p and z will vary first.'/
     +  ' if the letter "o" (without value) is included, you will'/
     +	' be asked to specify a new output format.'//
     +	'  pour coincer la racine dans 1 gyrointervalle (modes'/
     +	'  de bernstein),taper la lettre q.'/)
   67 format( '  pour une representation polaire,'/
     +  '  taper la lettre r.  p est alors module(k),'/
     +  '  et z designe angle(k)/perp.(en radians)'//
     +  '  pour normaliser z,p,f a fpe au lieu de fc'/
     +  '  (en input et en output),taper la lettre y dans l input '//
     +  '  pour faire les operations inverses(retour au mode'/
     +  '  initial),taper a nouveau les lettres q,r ou y.'//
     +  '  en cas d input de plus de 72 caracteres '/
     +  '  taper la lettre x a la fin de chaque ligne '/
     +  '  pour annoncer la suite. '/)
c
      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine outpt
c
      character cc*1
      parameter( cc = ' ' )
      character ltrs1*26,ltrs2*26
      parameter( ltrs1 = 'abcdefghijklmnopqrstuvwxyz' )
      parameter( ltrs2 = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' )
      character iou*20
      character ofil*20
      character ic*1
      character stex4*10
      real*8    tstrt,tfin
      double complex eps(6,4),di(4),efl(3),bfl(3),ZPL,ZMO
      complex compze,polbk,compz,compy,compye,vp,ZPSM,comte1,comte2
      complex x,xx,xp,ri
c
      common /ffdata/ ffdopen,id1,id2,lrecl
      common /ftimes/ tstrt,tfin
      common /infile/ ifile
      common /xpz/ xx(6),pp(6),zz(6),aa(6,2),dd(6),ass(6),vd(6),
     +dn(6),ta(6),xp(6),cv,pm(3),zm(3),xoi,xc,pzl,aixoi
      common /cout/ x,p,z,efl,bfl,di,eps,vg(2),sg(2),ri
      common /disfunc/ ren(6),isp(6),jma,red,den,st(6),px,rn,t(6)
      common itest
      common /polar/ compz(6),polbk,compze,compy(6),compye,vp(6,3),
     +  ZPL(3),ZMO(3),ZPSM(3),zp2sm2,finebp,fineby,
     +  finbt1,finbt2,comte1,comte2
      common /entsor/ listing,wfreq,niou,stex(7),stext(7)
      character*5 stext
      common /initra/ iabso
      common /entex/ stex4
      common/be/ibern,ipol,iplas,pn,gyr,itrin
c
c      data 	   ffdopen/0/
      data ierr /0/      
c
      integer*4	   nrows, ncols, ierr, nbuf, loc
      character	   type
      character*10 names
      real*4       ffarray
      real*8	   fftime
      equivalence(ffarray, fftime)
      dimension    names(143)
      dimension    ffarray(145)
c
c	added by PJK to compare with DISPER, 2 June, 2006
c
	PRINT*,'OUTPT CALLED,IC=,EPS=',IC
	DO I = 1,6
	  PRINT*,I,EPS(I,1)
	ENDDO	
	PRINT*,'CHECK,PX,XC,X,X(KHZ)',PX,XC,X,X*XC
	PRINT*,'CHECK, IPLAS,W/WP=',IPLAS,X*XC/PX
	print*,'outpt,ta(1),xc,rn,cv',ta(1),xc,rn,cv
c	cv seems to be c/(thermal vel(1))
	PRINT*,'FOR OSCARS, MULTIPLY BY (W/WC)**2, W^2 EPS=',IC
	print*,'w/wc is',x
	print*,'px,xc are fp,fc in kHz',px,xc
	PRINT*,' AND THEN BY (Wc/Wp)**2'
	DO I = 1,6
	  PRINT*,I,(X*XC/PX)**2*EPS(I,1)
	ENDDO
	PRINT*,' AND ADD (k C / Wp)**2 AS APPROPRIATE'
c	oscfact is fc(Hz)/fp(Hz)/(vt/c) squared
	OSCFACT = (CV*XC/PX)**2
C	DO I = 1,6
C	  PRINT*,I,(X*XC/PX)**2*EPS(I,1)
c	  PRINT*,I,(X)**2*EPS(I,1)/CV**2
	I=1
	  PRINT*,I,(X*XC/PX)**2*EPS(I,1) - OSCFACT*Z**2
	  PRINT*,2,(X*XC/PX)**2*EPS(2,1)
	I = 3
	  PRINT*,I,(X*XC/PX)**2*EPS(I,1) + OSCFACT*P*Z
	I  = 4
	  PRINT*,I,(X*XC/PX)**2*EPS(I,1) - OSCFACT*(P**2+Z**2)
	  PRINT*,5,(X*XC/PX)**2*EPS(5,1) 
	I = 6
	  PRINT*,I,(X*XC/PX)**2*EPS(I,1) - OSCFACT*P**2
c
      if(ffdopen.eq.0) then
	names( 0)  = 'REC NO'
        names( 1) = 'PI/2-AN'
        names( 2) = 'K.Rg'
        names( 3) = 'F RE'
        names( 4) = 'F IM'
        names( 5) = 'ex r'
        names( 6) = 'ex i'
        names( 7) = 'ey r'
        names( 8) = 'ey i'
        names( 9) = 'ez r'
        names(10) = 'ez i'
        names(11) = 'bx r'
        names(12) = 'bx i'
        names(13) = 'by r'
        names(14) = 'by i'
        names(15) = 'bz r'
        names(16) = 'bz i'
        names(17) = 'vgp'
        names(18) = 'vgz'
        names(19) = 'sgp'
        names(20) = 'sgz'
        names(21) = 'd r'
        names(22) = 'd i'
        names(23) = 'dx r'
        names(24) = 'dx i'
        names(25) = 'dz r'
        names(26) = 'dz i'
        names(27) = 'dp r'
        names(28) = 'dp i'
        names(29) = 'ri r'
        names(30) = 'ri i'

        names(31) = 'e11 r'
        names(32) = 'e11 i'
        names(33) = 'ex11 r'
        names(34) = 'ex11 i'
        names(35) = 'ez11 r'
        names(36) = 'ez11 i'
        names(37) = 'ep11 r'
        names(38) = 'ep11 i'
        names(39) = 'e12 r'
        names(40) = 'e12 i'
        names(41) = 'ex12 r'
        names(42) = 'ex12 i'
        names(43) = 'ez12 r'
        names(44) = 'ez12 i'
        names(45) = 'ep12 r'
        names(46) = 'ep12 i'
        names(47) = 'e13 r'
        names(48) = 'e13 i'
        names(49) = 'ex13 r'
        names(50) = 'ex13 i'
        names(51) = 'ez13 r'
        names(52) = 'ez13 i'
        names(53) = 'ep13 r'
        names(54) = 'ep13 i'
        names(55) = 'e22 r'
        names(56) = 'e22 i'
        names(57) = 'ex22 r'
        names(58) = 'ex22 i'
        names(59) = 'ez22 r'
        names(60) = 'ez22 i'
        names(61) = 'ep22 r'
        names(62) = 'ep22 i'
        names(63) = 'e23 r'
        names(64) = 'e23 i'
        names(65) = 'ex23 r'
        names(66) = 'ex23 i'
        names(67) = 'ez23 r'
        names(68) = 'ez23 i'
        names(69) = 'ep23 r'
        names(70) = 'ep23 i'
        names(71) = 'e33 r'
        names(72) = 'e33 i'
        names(73) = 'ex33 r'
        names(74) = 'ex33 i'
        names(75) = 'ez33 r'
        names(76) = 'ez33 i'
        names(77) = 'ep33 r'
        names(78) = 'ep33 i'

        names(79) = 'xx1 r'
        names(80) = 'xx1 i'
        names(81) = 'xx2 r'
        names(82) = 'xx2 i'
        names(83) = 'xx3 r'
        names(84) = 'xx3 i'
        names(85) = 'xx4 r'
        names(86) = 'xx4 i'
        names(87) = 'xx5 r'
        names(88) = 'xx5 i'
	names(89) = 'xx6 r'
        names(90) = 'xx6 i'

        names(91) = 'pp1'
        names(92) = 'pp2'
        names(93) = 'pp3'
        names(94) = 'pp4'
        names(95) = 'pp5'
        names(96) = 'pp6'
        names(97) = 'zz1'
        names(98) = 'zz2'
        names(99) = 'zz3'
        names(100) = 'zz4'
        names(101) = 'zz5'
        names(102) = 'zz6'
        names(103) = 'cc'
        names(104) = 'czeR'
        names(105) = 'czeI'
        names(106) = 'cyeR'
        names(107) = 'cyeI'
        names(108) = 'POLBK Real'
        names(109) = 'POLBK Imag'
        names(110) = 'PH Ne-Bp'
        names(111) = 'PH Ne-By'
        names(112) = 'cte1R'
        names(113) = 'empty'
        names(114) = 'PH N-BT1'
        names(115) = 'compte2'
        names(116) = 'empty'
        names(117) = 'PH NT-BT2'
        names(118) = 'rcze(i)'
        names(119) = 'icze(i)'
        names(120) = 'rcze(i)'
        names(121) = 'icze(i)'
        names(122) = 'rcze(i)'
        names(123) = 'icze(i)'
        names(124) = 'rcze(i)'
        names(125) = 'icze(i)'
        names(126) = 'rcze(i)'
        names(127) = 'icze(i)'
        names(128) = 'rcze(i)'
        names(129) = 'icze(i)'
        names(130) = 'rcye(i)'
        names(131) = 'icye(i)'
        names(132) = 'rcye(i)'
        names(133) = 'icye(i)'
        names(134) = 'rcye(i)'
        names(135) = 'icye(i)'
        names(136) = 'rcye(i)'
        names(137) = 'icye(i)'
        names(138) = 'rcye(i)'
        names(139) = 'icye(i)'
        names(140) = 'rcye(i)'
        names(141) = 'icye(i)'
	names(142) = 'fobs'
c
        fftime  = 1.0
        tstart  = 1.0
        tfin    = 1.0
        ncols   = 144 
        lrecl   = (ncols+1) * 4
        type    = 'D'
        nbuf    = 4
        loc     = 0
        nrows   = 0
        if((ic.ne.'a').and.(ic.ne.'A')) then
	  ffdopen = 1
cjpc          call ffcreate('reswhampff', lrecl, ncols, nrows, id1, id2,
cjpc     +                  ierr, nbuf)
	  if(ierr.lt.0) stop 
          do i=1,144 
cjpc	     call fhput(id1, ierr, i, names(i-1), ' ', 'WHAMP',
cjpc     +			type, loc)
	     if(ierr.lt.0) stop 
	     type = 'R'	! After 1st line, switch it to 'R'
          enddo
        endif
      endif
c
      if(abs(aimag(x)/x).lt.1.e-7) x=cmplx(real(x),0.)
      itest=0
      isor=0
      write(listing,6)
      if (itest.eq.1) write(listing,8)
c
      k=0
    1 k=k+1
      if(k.gt.kmx) return
      ic = iou( k : k )
      if( ic .eq. '/' )then
        write(listing,6)
      else
        iind = max(index(ltrs1,ic),index(ltrs2,ic))
c
c           ( a   b   c   d   e   f   g   h  i  j  k  l  m  n
c
        goto( 10, 18, 34, 24, 16, 10, 20, 1, 1, 1, 1, 1,38, 1,
     +         1, 12, 1, 26, 22, 28, 1, 36, 32, 1, 1, 14 ) iind         
c
c              o  p   q  r   s   t   u  v  w   x  y  z  )
c
      end if
      goto 1
c
   10 continue
      if(iplas.eq.1) x=x/pn
      write(listing,11)x
      if(ffdopen.eq.1) then
        ffarray(3+2) = real(x)
        ffarray(4+2) = aimag(x)
      endif
      stext(5)='f='
      stex(6)=real(x)
      stex(7)=aimag(x)
      if(iplas.eq.1) x=x*pn
      if ((ic.ne.'a').and.(ic.ne.'A')) goto 1
c
   12 continue
      if(iplas.eq.1) p=p/pn
      write(listing,13)p
      if(ffdopen.eq.1) then
        ffarray(2+2) = p
      endif
      if (ic.ne.'a'.and.ic.ne.'A') then
         isor=isor+1
         stext(isor)='p='
         isor=isor+1
         stex(isor)=p
      end if
      if(iplas.eq.1) p=p*pn
      if ((ic.ne.'a').and.(ic.ne.'A')) goto 1
c
   14 continue
      if(iplas.eq.1.and.ipol.ne.1) z=z/pn
      write(listing,15)z
      if(ffdopen.eq.1) then
	ffarray(1+2) = z
      endif
      if (ic.ne.'a'.and.ic.ne.'A') then
         isor=isor+1
         stext(isor)='z='
         isor=isor+1
         stex(isor)=z
      end if
      if(iplas.eq.1.and.ipol.ne.1) z=z*pn
      if ((ic.ne.'a').and.(ic.ne.'A')) goto 1
      write(listing,6)
c
   16 continue
      write(listing,17)  cc,  efl
      if(ffdopen.eq.1) then
        ffarray(5+2)  = dreal(efl(1))	! ex real & img
        ffarray(6+2)  = dimag(efl(1))	! ex real & img
        ffarray(7+2)  = dreal(efl(2))	! ey real & img
        ffarray(8+2)  = dimag(efl(2))	! ey real & img
        ffarray(9+2)  = dreal(efl(3))	! ez real & img
        ffarray(10+2) = dimag(efl(3))	! ez real & img
      endif
      if ((ic.ne.'a').and.(ic.ne.'A')) goto 1
      write(listing,6)
c
   18 continue
      write(listing,19)  cc,  bfl
      if(ffdopen.eq.1) then
        ffarray(11+2) = dreal(bfl(1))	! bx real & img
        ffarray(12+2) = dimag(bfl(1))	! bx real & img
        ffarray(13+2) = dreal(bfl(2))	! by real & img
        ffarray(14+2) = dimag(bfl(2))	! by real & img
        ffarray(15+2) = dreal(bfl(3))	! bz real & img
        ffarray(16+2) = dimag(bfl(3))	! bz real & img
      endif
      if ((ic.ne.'a').and.(ic.ne.'A')) goto 1
      write(listing,6)
c
   20 continue
      write(listing,21)  cc,  vg
      if(ffdopen.eq.1) then
        ffarray(17+2) = vg(1)
        ffarray(18+2) = vg(2)
      endif
      if ((ic.ne.'a').and.(ic.ne.'A')) goto 1
c
   22 continue
      write(listing,23)  cc,   sg
      if(ffdopen.eq.1) then
        ffarray(19+2) = sg(1)
        ffarray(20+2) = sg(2)
      endif
      if ((ic.ne.'a').and.(ic.ne.'A')) goto 1
      write(listing,6)
c
   24 continue
      write(listing,25)  cc,  di
      if(ffdopen.eq.1) then
        ffarray(21+2) = dreal(di(1))	! d  real & img
        ffarray(22+2) = dimag(di(1))	! d  real & img
        ffarray(23+2) = dreal(di(2))	! dx real & img
        ffarray(24+2) = dimag(di(2))	! dx real & img
        ffarray(25+2) = dreal(di(3))	! dy real & img
        ffarray(26+2) = dimag(di(3))	! dy real & img
        ffarray(27+2) = dreal(di(4))	! dz real & img
        ffarray(28+2) = dimag(di(4))	! dz real & img
      endif
      if ((ic.ne.'a').and.(ic.ne.'A')) goto 1
c
   26 continue
      write(listing,27)  cc,  ri
      if(ffdopen.eq.1) then
        ffarray(29+2) = real (ri)
        ffarray(30+2) = aimag(ri)
      endif
      if ((ic.ne.'a').and.(ic.ne.'A')) goto 1
      write(listing,6)
c
   28 continue
      do 30 j=1,6
           n=1+j/4+j/6
           m=j-j/4*2-j/6
           write(listing,29)cc,n,m,(eps(j,i),i=1,4)
           if(ffdopen.eq.1) then
              ffarray(31+2+(j-1)*8) = dreal(eps(j,1))
              ffarray(32+2+(j-1)*8) = dimag(eps(j,1))
              ffarray(33+2+(j-1)*8) = dreal(eps(j,2))
              ffarray(34+2+(j-1)*8) = dimag(eps(j,2))
              ffarray(35+2+(j-1)*8) = dreal(eps(j,3))
              ffarray(36+2+(j-1)*8) = dimag(eps(j,3))
              ffarray(37+2+(j-1)*8) = dreal(eps(j,4))
              ffarray(38+2+(j-1)*8) = dimag(eps(j,4))
           endif
   30 continue
      write(listing,6)
      if ((ic.ne.'a').and.(ic.ne.'A')) goto 1
      write(listing,31)  cc,(xx(j),j=1,11,2),pp,zz
      if(ffdopen.eq.1) then
        do j=1,6
           ffarray(79+2+(j-1)*2) = real (xx(j))
           ffarray(79+2+(j*2)-1) = aimag(xx(j))
        end do
        do j=1,6
           ffarray(91+2+(j-1)) = pp(j)
           ffarray(97+2+(j-1)) = zz(j)
        end do
              ffarray(102+2) = 1.0 
              ffarray(103+2) = 1.0 
cpjk              ffarray(104+2) = dreal(compze)
              ffarray(104+2) = real(compze)
              ffarray(105+2) = aimag(compze)
cpjk              ffarray(106+2) = dreal(compye)
              ffarray(106+2) = real(compye)
              ffarray(107+2) = aimag(compye)
cpjk              ffarray(108+2) = dreal(polbk)
              ffarray(108+2) = real(polbk)
cpjk              if(dreal(polbk).gt.1) ffarray(108+2) = 1/ffarray(108+2)
cpjk              if(dreal(polbk).lt.-1) ffarray(108+2) = 1/ffarray(108+2)
              if(real(polbk).gt.1) ffarray(108+2) = 1/ffarray(108+2)
              if(real(polbk).lt.-1) ffarray(108+2) = 1/ffarray(108+2)
cpjk              ffarray(108+2) = dreal(polbk)
              ffarray(108+2) = real(polbk)
	      ffarray(109+2) = aimag(polbk)
              ffarray(110+2) = finebp
              ffarray(111+2) = fineby
cpjk	      ffarray(112+2) = dreal(compte1)
	      ffarray(112+2) = real(compte1)
              ffarray(113+2) = 1.0 
	      ffarray(114+2) = finbt1
cpjk	      ffarray(115+2) = dreal(compte2)
	      ffarray(115+2) = real(compte2)
              ffarray(116+2) = 1.0 
	      ffarray(117+2) = finbt2 

         do j=1,6  
cpjk        ffarray(117+2+(j-1)*2+1)=dreal(compz(j)) 
        ffarray(117+2+(j-1)*2+1)=real(compz(j)) 
        ffarray(117+2+(j-1)*2+2)=imag(compz(j)) 
         end do
         do j=1,6  
cpjk        ffarray(129+2+(j-1)*2+1)=dreal(compy(j)) 
        ffarray(129+2+(j-1)*2+1)=real(compy(j)) 
        ffarray(129+2+(j-1)*2+2)=imag(compy(j)) 
         end do
	 ffarray(144) = real(x)*(1+6*p/real(x))
cjpc	call fput(id2, ffarray, lrecl, ierr)
        if(ierr.lt.0) stop 
	fftime = fftime  + 1
        tfin   = fftime
      endif
      goto 1
c
   32 continue
      y=real(x)
      write(10,33) y,p
      goto 1
c
   34 continue
      write(listing,35) cc,compze,compye,polbk
      write(listing,352) finebp
      write(listing,353) fineby
      write(listing,354) comte1,finbt1
C      write(listing,354) comte2,finbt2
         do j=1,jma
         write(listing,351) cc, compz(j),compy(j)
         end do
      if ((ic.ne.'a').and.(ic.ne.'A')) goto 1
      write(listing,6)
c
   36 continue
         do j=1,jma
         write(listing,37) cc,j,(vp(j,i),i=1,3)
         end do
      if ((ic.ne.'a').and.(ic.ne.'A')) goto 1
      write(listing,6)
c
   38 continue
	 write(25,39)
         write(listing,391) zpl(1),zmo(1),zpsm(1)
         write(listing,392) zpl(2),zmo(2),zpsm(2)
         write(listing,393) zpl(3),zmo(3),zpsm(3)
	 write(25,394)zp2sm2
      if ((ic.ne.'a').and.(ic.ne.'A')) goto 1
      write(listing,6)
c
      entry inout
      niou=0
  101 type*,  cc,  ' output:'
      write(listing,*)  cc, ' output:'
      read( ifile, '( a )', iostat = ios )  iou
      if( ios .ne. 0 )then
           rewind ifile
           iou = ' '
      end if
      type*,iou
      write(listing,*)iou
      do 103 k=1,20
           ic = iou( k : k )
           if(ic.eq.'z')niou=niou+1
           if(ic.eq.'p')niou=niou+1
           if(ic.eq.' ') goto 103
           if (ic.eq.'h') then
             type 105
             goto 101
           endif
           kmx=k
           if(ic.ne.'w') goto 103
           ios = 1
c
   40      continue
           if (ios .ne. 0 ) then
             type '( / 2a )',  cc,    'output file:'
             write(listing,*) cc,' output file:'
             read( ifile, '( a )', iostat = ios ) ofil
             if (ios .ne. 0 ) rewind ifile
             goto 40
           endif
           open(10,file=ofil,status='unknown',access='sequential',
     +         iostat = ios )
           if (ios .ne. 0 ) call errmsg( ios )
  103 continue
c
    8 format(' ',t65,'valeur trop petite dans rint')
    6 format('  ')
   11 format('+',t28,'freq=',e12.4,e12.4)
   13 format('+',t15,'p=',f9.5)
   15 format('+','z=',f8.5)
   17 format(44x,a,'ex=',e12.4,e12.4,'  ey=',e12.4,e12.4,
     +       '  ez=',e12.4,e12.4)
   19 format(44x,a,'bx=',e12.4,e12.4,'  by=',e12.4,e12.4,
     +       '  bz=',e12.4,e12.4)
   21 format(a,' vgp=',e9.2,'  vgz=',e9.2,'  ')
  23  format(a,' sgp=',e9.2,'  sgz=',e9.2,'  ')
  25  format(a,' d=',2e10.2,'  dx=',2e10.2,'  dz=',2e10.2,
     +       ' dp=',2e10.2,/)
  27  format(a,' ri=',2e10.2)
  29  format(a,' e',2i1,'=',2e10.2,'  ex=',2e10.2,
     +       'ez=',2e10.2,'  ep=',2e10.2,/)
  31  format(a,' xx=',12e12.3,/,' pp=',6e12.3,/,' zz=',6e12.3,/)
  33  format(2x,'x=',e15.7,'  p=',e15.7)
  35  format(a,37x,'compz e =',2e11.3,2x,'compy e =',2e11.3,2x,
     + 'polbk=',2e11.3)
  37  format(a,40x,'j=',i2,' vp =',3(e11.3,e11.3,2x))
 351  format(a,37x,'compz(j)=',2e11.3,2x,'compy(j)=',2e11.3) 
 352  format(103x,'phaseNe - phaseBp = ',f7.2) 
 353  format(103x,'phaseNe - phaseBy = ',f7.2) 
 354  format(39x,'competot =',2e11.3,3x,'phaseNe - phaseBtot = ',f7.2) 
 391  format(31x,'x',3x,3(2e12.3,3x))
 392  format(31x,'y',3x,3(2e12.3,3x))
 393  format(31x,'z',3x,3(2e12.3,3x))
 394  format(117x,e12.3)
  39  format(46x,'Zplus',21x,'Zmoins',19x,'Zplus/Zmoins',8x,
     + 'Zplus2/Zmoins2',/)
  105 format( ' the output is determined by a string of letters:'//
     +  ' a     all available output.'/
     +  ' b     wave magnetic field components.'/
     +  ' c     parallel compressibility of each species, and wave'/
     +  '        magnetic field polarization in wave vector frame.'/
     +  ' d     dispersion function and derivatives.'/
     +  ' e     wave electric field components.'/
     +  ' f     frequency.'/
     +  ' g     group velocity components.'/
     +  ' m     dv (+-) dB/(mu ro)1/2 pour A. Mangeney.'/
     +  ' p     perp. component (or modulus) of wave vector.'/
     +  ' r     refractive index.'/
     +  ' s     spatial growth-rates.'/
     +  ' t     dielectric tensor and derivatives.'/
     +  ' v     velocity components of each species.'/
     +  ' z     z-component of wave vector (or polar angle).'//
     +  ' demander les variables zpf en premier et dans cet ordre.'/)
c
      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      block data
      complex xx(1:6),xp(1:6)
      common  /be/     ibern,ipol,iplas,pn,gyr,itrin
      common  /xpz/    xx,pp,zz,a,b,d,ass,vd,dn,ta,xp,
     +	              cv,pm,zm,xoi,xc,pzl,aixoi
      real   pp(1:6),zz(1:6),a(1:6),b(1:6)
      real   d(1:6)
      real   ass(1:6),vd(1:6),dn(1:6),ta(1:6)
      real   pm(1:3),zm(1:3)
      real   cv,xoi,xc,pzl,aixoi
c
      integer*4	ffdopen,id1,id2,lrecl
      common   /ffdata/ ffdopen,id1,id2,lrecl
C
C***********************************************************************
C essai avec ne=30cm-3, Bo=23 gammas : xc = fce in kHz
C plasma sans derive, 
C protons et electrons : ta = parallel temperature in keV
C     +ta/.1314,.1314,1.,1.,1.,1./, pour betap//=betae=3
C***********************************************************************
C     +vd/1.,-1.,0.,0.,0.,0./,
C     +a/0.25,1.,1.,1.,1.,1./,
C     +a/1.,1.,1.,1.,1.,1./,
      data dn/30.e6,30.e6,0.,0.,0.,0./,
     +ta/.1314,.1314,1.,1.,1.,1./,
     +xc/0.644/, 
     +a/1.,1.,1.,1.,1.,1./,
     +ass/1.,0.,0.,0.,0.,0./,
     +vd/0.,0.,0.,0.,0.,0./,
     +d/1.,1.,1.,1.,1.,1./,
     +b/1.,1.,1.,1.,1.,1./,
     +pm/.10,.10,0.02/,
     +zm/1.3963,1.3963,0./,
c ================================================
     +xoi/0.1/,
     +aixoi/0.0/,
     + pzl/0.0/,
     +ibern/0/,
C  polar coordinates for the wave vector : ipol=1
     +ipol/1/,
     +iplas/0/
c **** correspondence between array and names in whamp:  ****
c  array(iof+ 0 12 18 24 30 36 42  48 54 60 66 78 79 82 85  86  87 88
c            xx pp zz a  b  d  ass vd dn ta xp cv pm zm xoi xc  pzl aixo
c
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inimaxw
c
c      ce s/p initialise les parametres des distributions maxwelliennes
c
      complex xx(6),xp(6)
      common /xpz/ xx,pp(6),zz(6),a(6),b(6),d(6),ass(6),vd(6),
     +   dn(6),ta(6),xp,cv,pm(3),zm(3),xoi,xc,pzl,aixoi
      common /disfunc/ ren(6),isp(6),jma,red,den,st(6),px,rn,t(6)
      do 2 j=1,6
c modif du rapport des masses. Le 03 07 90 (Fabrice Mottez).
	 ren(j)=1836.1*ass(j)
c	 ren(j)=100.*ass(j)
         if(ren(j).eq.0.) ren(j)=1.
         t(j)=ta(j)/ta(1)
         isp(j)=sqrt(ass(j))
         if(isp(j).lt.4) isp(j)=isp(j)+1
         if(dn(j).le.0.) goto 2
         jma=j
         red=red+dn(j)/ren(j)
         if(isp(j).eq.1) den=den+dn(j)
    2 continue
c
      rn=ren(1)
c                  ****  normalized temperatures and velocities.  ****
      do 3 j=1,jma
         ren(j)=ren(j)/rn
         t(j)=t(j)*ren(j)
    3 st(j)=sqrt(t(j))
      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine normv1(listing,vref)
c
c    si les vitesses de derive vd(i) entrees sont normalisees a v(1)
c    elles sont ici renormalisees a v(i)  (sinon, on n'appelle pas
c     normv1) (FM) 29 06 90
      complex xx(6),xp(6)
      common /xpz/ xx,pp(6),zz(6),a(6),b(6),d(6),ass(6),vd(6),
     +   dn(6),ta(6),xp,cv,pm(3),zm(3),xoi,xc,pzl,aixoi
      common /disfunc/ ren(6),isp(6),jma,red,den,st(6),px,rn,t(6)
      dimension t2(6),vd2(6),vref(6)
      data t2/6*0./
      data vd2/6*0./
c
      icor=0
         if(ta(1).ne.t2(1))icor=1
      do 1005 i=1,jma
 1004 format(90x,'ta =',f8.5,'   vd = ',f9.3)
         if(ta(i).eq.t2(i).and.vd(i).eq.vd2(i))then
      if(icor.eq.0)go to 1002
      go to 1003
      endif
         if(vd(i).eq.vd2(i))go to 1003
         vref(i)=vd(i)
 1003    continue
         vd(i)=vref(i)*sqrt(t(1)/t(i))*ren(i)
      t2(i)=ta(i)
      vd2(i)=vd(i)
 1002    write(listing,1004)ta(i),vd(i)
 1005    continue
      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine norfpp1(pn,dek)
c
c        normalisation a la frequence plasma de la 1ere population
c
      complex xx(6),xp(6)
      common /xpz/ xx,pp(6),zz(6),a(6),b(6),d(6),ass(6),vd(6),
     +   dn(6),ta(6),xp,cv,pm(3),zm(3),xoi,xc,pzl,aixoi
      common /disfunc/ ren(6),isp(6),jma,red,den,st(6),px,rn,t(6)
c     test 29 06 90 (FM)
      plas1=sqrt(dn(1)/dek)
      pn=plas1/xc
      pn=px/xc
      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine ecrilis(listing,spe)
c                  ****  print plasma parameters.  *********************
c
      complex xx(6),xp(6)
      character spe(5)*3
      common /xpz/ xx,pp(6),zz(6),a(6),b(6),d(6),ass(6),vd(6),
     +   dn(6),ta(6),xp,cv,pm(3),zm(3),xoi,xc,pzl,aixoi
      common /disfunc/ ren(6),isp(6),jma,red,den,st(6),px,rn,t(6)
      write(listing,101)px,xc,den
  101 format(' electron plasma freq.:',  f7.3,
     +       'khz,  gyro freq.:',  f10.4,  'khz   ',
     +       'electron density:',  e9.3,  'm-3'  / )
      do 4 j=1,jma
  102 format(' ',  a3,  '  dn=',  e10.4,  '  t=',  f9.6,  '  d=',  f4.2,
     +'  a=',  f5.2,  '  b=',  f4.2,  ' vd=',  f9.3  /  )
  4   write(listing,102)spe(isp(j)),dn(j),ta(j),d(j),a(j),b(j),vd(j)
      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine denofpe
c
c          ce s/p normalise a fpe totale au lieu de fce
c
      complex xx(6),xp(6)
      common /xpz/ xx,pp(6),zz(6),a(6),b(6),d(6),ass(6),vd(6),
     +   dn(6),ta(6),xp,cv,pm(3),zm(3),xoi,xc,pzl,aixoi
      common/be/ibern,ipol,iplas,pn,gyr,itrin
c     test 27 06 90 (FM)
      type *,'DENOFPE'
           xoi=xoi/pn
           aixoi=aixoi/pn
           do 604 i=1,3
         pm(i)=pm(i)/pn
         if(ipol.ne.1) zm(i)=zm(i)/pn
  604      continue
      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inizp
c
c   ce s/p  initialise z(3) et p(3)
c
      complex xx(6),xp(6)
      common /xpz/ xx,pp(6),zz(6),a(6),b(6),d(6),ass(6),vd(6),
     +   dn(6),ta(6),xp,cv,pm(3),zm(3),xoi,xc,pzl,aixoi
      delp=1.e-14
      delz=1.e-14
      p21=abs(pm(2)-pm(1))
      z21=abs(zm(2)-zm(1))
      ap2=abs(pm(2))
      az2=abs(zm(2))
      if(p21.gt.1.e-14*ap2.and.p21.lt.ap2) delp=1.e-14*ap2/p21
      if(z21.gt.1.e-14*az2.and.z21.lt.az2) delz=1.e-14*az2/z21
      pm(3)=pm(3)*(1.-delp)
      zm(3)=zm(3)*(1.-delz)
      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine nwstrtf
c
c     ce  s/p reinitialise la frequence initiale
c
	double complex eps(6,4),dir,dix,diz,dip,efl(3),bfl(3)
      complex dop,dox,doz,dx,x,xo,xvo,xx(6),xp(6),ri
      common /reinit/ dop,dox,doz,kv,dx,xo,xvo,po,zo,zlg,plg,pvo,zvo
      common /xpz/ xx,pp(6),zz(6),a(6),b(6),d(6),ass(6),vd(6),
     +   dn(6),ta(6),xp,cv,pm(3),zm(3),xoi,xc,pzl,aixoi
      common /cout/ x,p,z,efl,bfl,dir,dix,diz,dip,eps,vg(2),sg(2),ri
      common/be/ibern,ipol,iplas,pn,gyr,itrin
c                    ****  new start frequency.  ***********************
      if(ipol.eq.0) goto 400
      ppol=p
      zpol=z
      popol=po
      zopol=zo
      pvopol=pvo
      zvopol=zvo
cpjk	cart changes p = mag of vector, z = angle in radians to
cpjk		p,z = cartesian components
      call cart(p,z)
      call cart(po,zo)
      call cart(pvo,zvo)
  400 if(kv.eq.0) goto 41
      dkp=p-pvo
      dkz=z-zvo
      dx=(dkp*dop+dkz*doz)/dox
      x=xvo-dx
      x1=real(x)
      if(ifix(x1).ne.gyr.and.ibern.eq.1) x=xvo
      goto 49
   41 dkp=p-po
      dkz=z-zo
      dx=(dkp*dip+dkz*diz)/dix
      x=xo-dx
      x1=real(x)
      if(ifix(x1).ne.gyr.and.ibern.eq.1) x=xo
   49 if(ipol.eq.0) goto 500
      p=ppol
      z=zpol
      po=popol
      zo=zopol
      pvo=pvopol
      zvo=zvopol
  500 continue
      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine frqplas
c
c     ce s/p normalise les frequences(reelles et imaginaire) a fpe
c
      common /xpz/ xx(12),pp(6),zz(6),a(6),b(6),d(6),ass(6),vd(6),
     +   dn(6),ta(6),xp(12),cv,pm(3),zm(3),xoi,xc,pzl,aixoi
      common/be/ibern,ipol,iplas,pn,gyr,itrin
c     test 27 06 90 (FM)
c     type *,'FRQPLAS'
      xoi=xoi*pn
      aixoi=aixoi*pn
      do 32 i=1,3
         pm(i)=pm(i)*pn
         if(ipol.ne.1) zm(i)=zm(i)*pn
  32  continue
      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine zpinit
c
c    ce s/p calcule les z et p courants
c
	double complex eps(6,4),dir,dix,diz,dip,efl(3),bfl(3)
      complex xx(6),xp(6),x,ri,dop,dox,doz,xo,xvo,dx
      common /xpz/ xx,pp(6),zz(6),a(6),b(6),d(6),ass(6),vd(6),
     +   dn(6),ta(6),xp,cv,pm(3),zm(3),xoi,xc,pzl,aixoi
      common /cout/ x,p,z,efl,bfl,dir,dix,diz,dip,eps,vg(2),sg(2),ri
      common /reinit/ dop,dox,doz,kv,dx,xo,xvo,po,zo,zlg,plg,pvo,zvo
      plg=pm(1)
      if(pm(3).lt.0.) plg=pm(2)
      p=plg
      zlg=zm(1)
      if(zm(3).lt.0.) zlg=zm(2)
      z=zlg
      if(pzl.ne.1) return
      p=10.**plg
      z=10.**zlg
      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inigene(listing,vref)
c
c      ce s/p initialise tout les parametres(standarts+input)
c
      complex xx(6),xp(6),ome,fpx
      character spe(5)*3
      common /xpz/ xx,pp(6),zz(6),a(6),b(6),d(6),ass(6),vd(6),
     +   dn(6),ta(6),xp,cv,pm(3),zm(3),xoi,xc,pzl,aixoi
      common/be/ibern,ipol,iplas,pn,gyr,itrin
      common /disfunc/ ren(6),isp(6),jma,red,den,st(6),px,rn,t(6)
      common /iniv/ ome,fpx,pfq,xa,dek
      dimension vref(6)
      data spe/'e- ','h+ ','he+','o+ ','   '/
    1 den=0.
      red=0.
c
      call inimaxw
c
c     supprime normalisation vitesse a V espece 1 (PC)? 29 06 90 (FM)
c     call normv1(listing,vref)
c
CLacombe      dek=12405.
	dek=12423.
      pfq=red/dek
      px=sqrt(pfq)
c
CL      call norfpp1(pn,dek)
c
      xa=xc/rn
      tr=ta(1)/rn
      cv=tr*(1022.+tr)/(511.+tr)**2
      cv=1./sqrt(cv)
      dek=dek*rn
c
      call ecrilis(listing,spe)
      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inixzp
c
c   ce s/p normalise x,z,p pour les calculs de difu
c
	double complex eps(6,4),dip,dix,dir,diz,efl(3),bfl(3)
      complex xx(6),xp(6),ome,x,fpx,ri
      common /xpz/ xx,pp(6),zz(6),a(6),b(6),d(6),ass(6),vd(6),
     +   dn(6),ta(6),xp,cv,pm(3),zm(3),xoi,xc,pzl,aixoi
      common /disfunc/ ren(6),isp(6),jma,red,den,st(6),px,rn,t(6)
      common /cout/ x,p,z,efl,bfl,dir,dix,diz,dip,eps,vg(2),sg(2),ri
      common /iniv/ ome,fpx,pfq,xa,dek
      common/be/ibern,ipol,iplas,pn,gyr,itrin
      ome=(x*xa)**2
      fpx=pfq/ome
      do 11 j=1,jma
         xx(j)=x*ren(j)
         pp(j)=p*st(j)
         zz(j)=z*st(j)
         if(ipol.eq.1) zz(j)=z
   11 xp(j)=dn(j)/dek/ren(j)/ome
      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine iterat(k,l,*,*,*,*)
c
c   ce s/p  appel le s/p difu avec 2 sorties :convergence ou non
c
	double complex eps(6,4),dir,dix,dip,diz,efl(3),bfl(3)
      complex xx(6),xp(6),x,cx,ri,ome,fpx
      common /xpz/ xx,pp(6),zz(6),a(6),b(6),d(6),ass(6),vd(6),
     +   dn(6),ta(6),xp,cv,pm(3),zm(3),xoi,xc,pzl,aixoi
      common /disfunc/ ren(6),isp(6),jma,red,den,st(6),px,rn,t(6)
      common /cout/ x,p,z,efl,bfl,dir,dix,diz,dip,eps,vg(2),sg(2),ri
      common /iniv/ ome,fpx,pfq,xa,dek
      common/be/ibern,ipol,iplas,pn,gyr,itrin
c
      call difu(2,jma,ierr)
c
      do 20 i=1,20
         adir=abs(dir)
         irk=0
         cx=dir/dix
   15    x=x-cx
         x1=real(x)
         if(ibern.eq.0.or.z.eq.0.) goto 155
         if(k.eq.0.and.(ifix(x1).ne.gyr.or.ierr.ne.0)) goto 21
         if(k.ne.0.and.(ifix(x1).ne.gyr.or.abs(real(cx)).gt..2)) goto
     +   166
  155    if(ibern.eq.1.and.ifix(x1).ne.gyr) goto 166
         ome=(x*xa)**2
         fpx=pfq/ome
         do 16 j=1,jma
            xp(j)=dn(j)/dek/ren(j)/ome
   16	 xx(j)=x*ren(j)
c modif valeur de controle de convergence, le 02 07 90 (FM)
C	 small=1.e-6*abs(x)
C        if(l.eq.1) small=1.e-4
	 small=1.e-5*abs(x)
         if(l.eq.1) small=1.e-3
         if(abs(cx).le.small.and.irk.eq.0) then
            if(l.eq.1) then
               go to 22
               else
               return 1
            endif
         endif
         call difu(2,jma,ierr)
         if(abs(dir).lt.adir) goto 20
  166    x=x+cx
         cx=cx/2.
         irk=irk+1
         if(irk.gt.10) goto 21
         goto 15
   20 continue
c
c         **** meilleure initialisation ********************************
c
   21 if(ibern.eq.0) return 2
      k=k+1
      if(k.gt.2.or.z.eq.0.) then
         if(k.ne.1) z=zoo
         k=0
         return 2
      endif
      if(k.eq.1) goto 211
      if(ipol.eq.1) z=p*sin(z)
      x=cmplx(xoo,-2.5*z)
      goto 22
  211 l=1
      zoo=z
      z=0.
      return 3
   22 l=0
      z=zoo
      xoo=real(x)
      return 4
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine updatpz(listing,kfs,ibel,ibelo,*,*,*)
c
c  ce s/p update p et z (fin de balayage=retour sur input)
c
	double complex eps(6,4),dir,dix,dip,diz,efl(3),bfl(3)
       complex xx(6),xp(6),x,ri,xo,xvo,dox,dop,doz,dx
      common /xpz/ xx,pp(6),zz(6),a(6),b(6),d(6),ass(6),vd(6),
     +   dn(6),ta(6),xp,cv,pm(3),zm(3),xoi,xc,pzl,aixoi
      common /cout/ x,p,z,efl,bfl,dir,dix,diz,dip,eps,vg(2),sg(2),ri
      common /reinit/ dop,dox,doz,kv,dx,xo,xvo,po,zo,zlg,plg,pvo,zvo
      common/be/ibern,ipol,iplas,pn,gyr,itrin
  111 format(' ')
      po=p
      zo=z
      xo=x
      if(kv.eq.0) goto 35
      xvo=x
      zvo=z
      zlo=zlg
      pvo=p
      plo=plg
      dox=dix
      doz=diz
      dop=dip
      kv=0
   35 continue
      goto(36,38) kfs
   36 plg=plg+pm(3)
c                   **** update p and z  *******************************
      if(plg.ge.pm(1).and.plg.le.pm(2)) goto 39
      write(listing,111)
      zlg=zlg+zm(3)
      if(zlg.lt.zm(1).or.zlg.gt.zm(2)) goto 54
      kv=1
      plg=plo
      p=pvo
   37 continue
      if(pzl.ne.0)then
         z=zlg+pzl*(10.**zlg-zlg)
         else
         z=zlg
      endif
      goto 40
c
   38 zlg=zlg+zm(3)
      if(zlg.ge.zm(1).and.zlg.le.zm(2)) goto 37
      plg=plg+pm(3)
      write(listing,111)
      if(plg.lt.pm(1).or.plg.gt.pm(2)) goto 54
      kv=1
      zlg=zlo
      z=zvo
   39 continue
      if(pzl.ne.0)then
      p=plg+pzl*(10.**plg-plg)
      else
      p=plg
      endif
   40 if(kv.eq.0) goto 401
      ibel=0
      if(ibelo.eq.0) return 3
      ibelo=0
      return 2
  401 if(ibel.eq.0) return 3
      ibel=0
      return 2
   54 if(iplas.eq.1) call denofpe
      return 1
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine difu(kol,jma,ierr)
c        arguments: kol     =2 d and its x-derivative computed.
c                           =4 all derivatives and wave fields.
c                   jma     =1 - 6, number of components.
c                   ierr    error flag
	double complex xsi(6,4),e(6,4),df,u1,u2,u3,u12,u13,u32,
     + a,b,c,da,db,dc,d,dx,dz,dp,efl(3),bfl(3)
	double precision q,g
      complex x,xx,xp,ri
      common /xpz/ xx(6),pp(6),zz(6),aa(6,2),dd(6),ass(6),vd(6),
     + dn(6),ta(6),xp(6),cv,pm(3),zm(3),xoi,xc,pzl,aixoi
      common /cout/x,p,z,efl,bfl,d,dx,dz,dp,e,vg(2),sg(2),ri
Cqi   common /entsor/ listing,wfreq,niou,stex(7),stext(7)
      common/be/ibern,ipol,iplas,pn,gyr,itrin
      dimension ppo(6),zzo(6)
c
c                     *********** form dielectric tensor ************
      ierr=0
      do 1 k=1,4
           do 1 i=1,6
    1 e(i,k)=(0.d0,0.d0)
      e(1,1)=1.d0
      e(4,1)=1.d0
      e(6,1)=1.d0
c
      do 5 j=1,jma
           if(ipol.ne.1) goto 11
           ppo(j)=pp(j)
           zzo(j)=zz(j)
           call cart(pp(j),zz(j))
  11   continue
           if(aa(j,1).ne.aa(j,2)) goto 2
           aa(j,2)=0.
           dd(j)=1.
   2       ib=1
           df=xp(j)/(aa(j,1)*(aa(j,1)-aa(j,2)))
           ut=aa(j,1)-dd(j)*aa(j,2)
    3      call ksi(xsi,j,ib,kol,ierr)
           do 4 k=1,kol
                do 4 i=1,6
    4      e(i,k)=e(i,k)+df*ut*xsi(i,k)
c
           if(ib.eq.2) go to 5
           ut=(dd(j)-1.)*aa(j,1)
           if(ut.eq.0.) go to 5
           ib=2
           goto 3
c
    5 continue
c                       *** dielectric tensor computed ***
c
c       ******* form refractive index, cv=speed of light/therm. speed **
      u1=pp(1)*cv/xx(1)
      u3=zz(1)*cv/xx(1)
      u12=u1**2
      u32=u3**2
      u2=u12+u32
      u13=2.d0*u1*u3
c
c                      ******** form dispersion function ********
      a=u12*e(1,1)+u13*e(3,1)+u32*e(6,1)
      b=u2*(e(1,1)*e(6,1)-e(3,1)**2)+(u3*e(5,1)-u1*e(2,1))**2
      c=(e(1,1)*e(6,1)-e(3,1)**2)*e(4,1)+e(6,1)*e(2,1)**2
      c=c+(e(1,1)*e(5,1)+2.d0*e(2,1)*e(3,1))*e(5,1)
      d=(u2-e(4,1))*a-b+c
      if(kol.le.1) goto 100
c        ****** complete x-derivative of dielectric tensor ******
      e(1,2)=e(1,2)-2.d0*(e(1,1)-1.d0)
      e(2,2)=e(2,2)-2.d0* e(2,1)
      e(3,2)=e(3,2)-2.d0* e(3,1)
      e(4,2)=e(4,2)-2.d0*(e(4,1)-1.d0)
      e(5,2)=e(5,2)-2.d0* e(5,1)
      e(6,2)=e(6,2)-2.d0*(e(6,1)-1.d0)
c          ****** x-derivative of dispersion function ******************
      da=(e(1,2)-2.d0*e(1,1))*u12+u13*(e(3,2)-2.d0*e(3,1))+
     +   (e(6,2)-2.d0*e(6,1))*u32
      db=2.d0*(u3*e(5,1)-u1*e(2,1))*(u3*(e(5,2)-e(5,1))-u1*(e(2,2)-
     +e(2,1)))+u2*((e(1,2)-e(1,1))*e(6,1)-2.d0*(e(3,2)-e(3,1))*e(3,1)+
     +(e(6,2)-e(6,1))*e(1,1))
      dc=(e(1,2)*e(6,1)+e(1,1)*e(6,2)-2.d0*e(3,1)*e(3,2))*e(4,1)
      dc=dc+(e(1,1)*e(6,1)-e(3,1)**2)*e(4,2)
      dc=dc+e(5,1)*(e(1,2)*e(5,1)+2.d0*e(1,1)*e(5,2)+
     +e(2,1)*e(3,2)+2.d0*e(2,2)*e(3,1))
      dc=dc+e(2,1)*(e(6,2)*e(2,1)+2.d0*e(6,1)*e(2,2)+
     +e(5,1)*e(3,2)+2.d0*e(5,2)*e(3,1))
      dx=((u2-e(4,1))*da-(2.d0*u2+e(4,2))*a-db+dc)/xx(1)
      if(kol.le.2) goto 100
      dz=(0.d0,0.d0)
      if(zz(1).eq.0.) goto 6
c         ****** z-derivative of dispersion function *******************
      da=u12*e(1,3)+u13*(e(3,3)+e(3,1))+u32*(e(6,3)+2.d0*e(6,1))
      db=2.d0*(u3*e(5,1)-u1*e(2,1))*(u3*(e(5,3)+e(5,1))-u1*e(2,3))+
     +2.d0*u32*(e(1,1)*e(6,1)-e(3,1)**2)+
     +u2*(e(1,3)*e(6,1)+e(1,1)*e(6,3)-2.d0*e(3,1)*e(3,3))
      dc=(e(1,3)*e(6,1)+e(1,1)*e(6,3)-2.d0*e(3,1)*e(3,3))*e(4,1)
      dc=dc+(e(1,1)*e(6,1)-e(3,1)**2)*e(4,3)
      dc=dc+e(5,1)*(e(1,3)*e(5,1)+2.d0*e(1,1)*e(5,3)+
     +e(2,1)*e(3,3)+2.d0*e(2,3)*e(3,1))
      dc=dc+e(2,1)*(e(6,3)*e(2,1)+2.d0*e(6,1)*e(2,3)+
     +e(5,1)*e(3,3)+2.d0*e(5,3)*e(3,1))
      dz=((u2-e(4,1))*da+(2.d0*u32-e(4,3))*a-db+dc)/zz(1)
   6  if(kol.le.3) goto 100
c        ****** p-derivative of dispersion function ********************
      dp=(0.d0,0.d0)
      if(pp(1).eq.0.) goto 7
c
      da=u12*(e(1,4)+2.d0*e(1,1))+u13*(e(3,4)+e(3,1))+u32*e(6,4)
      db=2.d0*(u3*e(5,1)-u1*e(2,1))*(u3*e(5,4)-u1*(e(2,4)+e(2,1)))+
     +2.d0*u12*(e(1,1)*e(6,1)-e(3,1)**2)+
     +u2*(e(1,4)*e(6,1)+e(1,1)*e(6,4)-2.d0*e(3,1)*e(3,4))
      dc=(e(1,4)*e(6,1)+e(1,1)*e(6,4)-2.d0*e(3,1)*e(3,4))*e(4,1)
      dc=dc+(e(1,1)*e(6,1)-e(3,1)**2)*e(4,4)
      dc=dc+e(5,1)*(e(1,4)*e(5,1)+2.d0*e(1,1)*e(5,4)+
     +e(2,1)*e(3,4)+2.d0*e(2,4)*e(3,1))
      dc=dc+e(2,1)*(e(6,4)*e(2,1)+2.d0*e(6,1)*e(2,4)+
     +e(5,1)*e(3,4)+2.d0*e(5,4)*e(3,1))
c
      dp=((u2-e(4,1))*da+(2.d0*u12-e(4,4))*a-db+dc)/pp(1)
c                     ******** compute electric field ******************
    7 u13=u13/2.d0
      if(u13.eq.0.d0) goto 8
      a=(u32-e(1,1))*e(5,1)-(u13+e(3,1))*e(2,1)
      b=a/((u13+e(3,1))*(u2-e(4,1))+e(5,1)*e(2,1))
      c=a/((u13+e(3,1))*e(5,1)-(u12-e(6,1))*e(2,1))
      a=(1.d0,0.d0)
      goto 10
    8 if(abs(u2-e(6,1)).lt.1.d-3) goto 9
      a=(1.d0,0.d0)
      b=(0.d0,1.d0)
      g=abs(e(1,1)-u32+b*e(2,1))
      if(abs(e(1,1)-u32-b*e(2,1)).lt.g) b=(0.d0,-1.d0)
      if(u3.eq.0.) b=-e(1,1)/e(2,1)
      c=(0.d0,0.d0)
      goto 10
    9 a=0.d0
      b=0.d0
      c=1.d0
   10 q=a*conjg(a)+b*conjg(b)+c*conjg(c)
      q=sqrt(q)
      efl(1)=a/q
      efl(2)=b/q
      efl(3)=c/q
      v=1./299.7925
c       the electric field is 1 mv/m
c       the magnetic field will be in gamma
      bfl(1)=-v*u3*efl(2)
      bfl(2)=v*(u3*efl(1)-u1*efl(3))
      bfl(3)=v*u1*efl(2)
  100 if(ipol.ne.1) return
      do 101 j=1,jma
           pp(j)=ppo(j)
           zz(j)=zzo(j)
  101 continue
      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine ksi(xsi,j,ib,kol,ierr)
c        arguments: xsi    contains the susceptibility tensor
c                          on return.
c                   j      component number.
c                   ib     index for aa (alpha)
c                   kol    determines whether derivatives
c                          should be evaluated.
c                   ierr   error flag, is set =1 if damping
c                          is too strong.
C         CORRECTION DU SIGNE DES xsi(5,i) (Gerard Belmont, 01/02/91).
C  Pour accroitre la precision du calcul des tenseurs dielectrique "e"
C    et de conductivite "sig", on retranche "a" sur la diagonale de 
C    "xsi"; DIFU et COMPOL sont donc modifies (Belmont, janvier 91).
C
	double precision al,ala,cay,cimy,caby
	double complex xsi(6,4),b(8),c(8)
      double complex xy,bl,cl,dl,bly,rc(2,2),ps,psp,psy,ppy,dp,dpp,zy,y
      complex x,xx,xp,ai
      common /xpz/ xx(6),pp(6),zz(6),aa(6,2),dd(6),ass(6),vd(6),
     + dn(6),ta(6),xp(6),cv,pm(3),zm(3),xoi,xc,pzl,aixoi
c          **** residues for pade approximant **************************
      data b/(-1.734012457471826d-2,-4.630639291680322d-2),
     +       (-1.734012457471826d-2, 4.630639291680322d-2),
     +       (-7.399169923225014d-1, 8.395179978099844d-1),
     +       (-7.399169923225014d-1,-8.395179978099844d-1),
     +       (5.840628642184073    , 9.536009057643667d-1),
     +       (5.840628642184073    ,-9.536009057643667d-1),
     +       (-5.583371525286853   ,-1.120854319126599 d1),
     +       (-5.583371525286853   , 1.120854319126599 d1)/,
     +    c/ ( 2.237687789201900,  -1.625940856173727),
     +       (-2.237687789201900,  -1.625940856173727),
     +       ( 1.465234126106004,  -1.789620129162444),
     +       (-1.465234126106004,  -1.789620129162444),
     +       ( .8392539817232638,  -1.891995045765206),
     +       (-.8392539817232638,  -1.891995045765206),
     +       ( .2739362226285564,  -1.941786875844713),
     +       (-.2739362226285564,  -1.941786875844713)/
c
      x=xx(j)
      z=zz(j)
      p=pp(j)
      a=aa(j,ib)
      vz=vd(j)*z
      ai=(0.,1.)*a
      if(ass(j).eq.0.) ai=-ai
      al=.5d0*p*p
      ala=a*al
c
	do 1 i=1,6
	do 1 k=1,4
 1      xsi(i,k)=0.d0
c
      xi=aimag(x)
      if(xi.ge.0.) goto 3
c          *******        test for strong damping   ********************
      nx=ifix(real(x)+.5)
      rx=x-float(nx)
      if(xi.ge.-.6*abs(rx).or.xi.ge.-z) goto 3
      ierr=1
    3 xsi(1,1)=0.d0
      xsi(6,1)=0.d0+2.d0*a*vd(j)**2
      do 4 l=1,8
           bl=b(l)
           cl=c(l)
           dl=cl+vd(j)
           y=x-dl*z
           bly=bl/y
c        ****** evaluate the r-function ********************************
      cay=dble(y)
      cimy=dimag(y)
      caby=abs(y)
           call ryla(y,ala,rc)
           xy=1.d0+a*z*cl/y
           ps=xy*rc(1,1)
           psp=xy*rc(1,2)
c        ****** form susceptibility tensor *****************************
           xsi(1,1)=xsi(1,1)+a*bl*y*ps
           xsi(2,1)=xsi(2,1)+ai*bl*psp
           xsi(3,1)=xsi(3,1)+a*p*bl*dl*ps
           xsi(4,1)=xsi(4,1)+bly*psp
           xsi(5,1)=xsi(5,1)-ai*p*bly*dl*psp
           xsi(6,1)=xsi(6,1)+2.d0*a*bly*dl**2*(x-vz+al*ps)
c
           if(kol.le.1) goto 4
c        ****** form x-derivatives of xsi ******************************
           psy=xy*rc(2,1)
           ppy=xy*rc(2,2)
           xy=x/y
           dp =xy*(psy-a*z*cl/y*rc(1,1))
           dpp=xy*(ppy-a*z*cl/y*rc(1,2))
           xsi(1,2)=xsi(1,2)+a*bl*(y*dp+x*ps)
           xsi(2,2)=xsi(2,2)+ai*bl*dpp
           xsi(3,2)=xsi(3,2)+a*p*bl*dl*dp
           xsi(4,2)=xsi(4,2)+bly*(dpp-xy*psp)
           xsi(5,2)=xsi(5,2)-ai*p*bly*dl*(dpp-xy*psp)
           xsi(6,2)=xsi(6,2)+2.d0*a*bly*dl**2*(x+al*dp-xy*(x-vz+al*ps))
c
           if(kol.le.2) goto 4
c                    ****** form z-derivatives of xsi ******************
           zy=z/y
           dp =zy*(a*cl*xy*rc(1,1)-dl*psy)
           dpp=zy*(a*cl*xy*rc(1,2)-dl*ppy)
           zy=dl*zy
           xsi(1,3)=xsi(1,3)+a*bl*y*(dp-zy*ps)
           xsi(2,3)=xsi(2,3)+ai*bl*dpp
           xsi(3,3)=xsi(3,3)+a*p*bl*dl*dp
           xsi(4,3)=xsi(4,3)+bly*(dpp+zy*psp)
           xsi(5,3)=xsi(5,3)-ai*p*bly*dl*(dpp+zy*psp)
       xsi(6,3)=xsi(6,3)+2.d0*a*bly*dl**2*(al*dp-vz+zy*(x-vz+al*ps))
c
           if(kol.le.3) goto 4
c                   ****** form p-derivatives of xsi *******************
           call ryla(y-1.d0,ala,rc)
           dp=2.d0*(psp-ps)
           dpp=2.d0*ala*((y/(y-1.d0))**2*rc(1,2)-psp)-y*dp
           xsi(1,4)=xsi(1,4)+a*bl*y*dp
           xsi(2,4)=xsi(2,4)+ai*bl*dpp
           xsi(3,4)=xsi(3,4)+a*p*bl*dl*(dp+ps)
           xsi(4,4)=xsi(4,4)+bly*(2.d0*psp+dpp)
           xsi(5,4)=xsi(5,4)-ai*p*bly*dl*(psp+dpp)
           xsi(6,4)=xsi(6,4)+4.d0*a*bly*dl**2*al*psp
    4 continue
c      **** complete xsi(4, ) ******************************************
      xsi(4,1)=xsi(1,1)-2.d0*a**2*al*xsi(4,1)
      xsi(4,2)=xsi(1,2)-2.d0*a**2*al*xsi(4,2)
      xsi(4,3)=xsi(1,3)-2.d0*a**2*al*xsi(4,3)
      xsi(4,4)=xsi(1,4)-2.d0*a**2*al*xsi(4,4)
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine compol(jma)
c Calcule : 
c   - les composantes de la vitesse pour chaque espece j,
c   - les compressibilites parallele et Alfven pour chaque espece j, 
c   - les compressibilites totales des electrons, parallele et Alfven ,
c   - la polarisation magnetique axee sur le vecteur d'onde.
c On appelle "compressibilite parallele" le rapport entre la fluctuation
c  de densite de j et la fluctuation de champ magnetique parallele a Bo, 
c  normalise a la densite de j et a Bo. 
c La "compressibilite Alfven" est calculee avec la fluctuation 
c  de champ magnetique perpendiculaire au plan k,Bo.
C	double precision q,pi,bflmo
      double complex xsi(6,4),sig(6,6),e(6,4),d,dx,dz,dp,efl(3),bfl(3),
     + df,cj(3),ZPL,ZMO
      complex ZPSM,vpp(3),comte1,comte2
      complex x,xx,xp,ri,compze,polbk,compz,compye,compy,vp,ome,fpx
      common /xpz/ xx(6),pp(6),zz(6),aa(6,2),dd(6),ass(6),vd(6),
     + dn(6),ta(6),xp(6),cv,pm(3),zm(3),xoi,xc,pzl,aixoi
      common /cout/x,p,z,efl,bfl,d,dx,dz,dp,e,vg(2),sg(2),ri
      common /polar/ compz(6),polbk,compze,compy(6),compye,vp(6,3),
     +  ZPL(3),ZMO(3),ZPSM(3),zp2sm2,finebp,fineby,
     +  finbt1,finbt2,comte1,comte2
      common /iniv/ ome,fpx,pfq,xa,dek
      common/be/ibern,ipol,iplas,pn,gyr,itrin
      dimension ppo(6),zzo(6)
c
      ierr=0
      pi=3.14159265358979
C  vitesse thermique // de la 1ere population, supposee protonique :
C  noter le facteur 2 dans cette formule, selon Ronnmark.
      vtppar=90.85*sqrt(2.*11605000.*ta(1))
	   do 1 j=1,6
           do 1 i=1,6
 1    sig(i,j)=(0.d0,0.d0)
      compze=(0.0,0.0)
      compye=(0.0,0.0)
      dtot=0.
		do 12 j=1,3
 12   		vpp(j)=(0.0,0.0)
c
		do j=1,jma
	if(ass(j).eq.1.) dtot=dtot+dn(j)
		end do
      VA=299792.5e3*xa/sqrt(dtot/dek)
		write(25,200)VA,vtppar
 200  format(60x,'va =',e12.4,4x,'Vtp// =',e12.4,' m/s')
C*****    on suppose que les densites totales     **********************
C*****  des electrons et des protons sont egales  **********************
      do 5 j=1,jma
           if(ipol.ne.1) goto 11
           ppo(j)=pp(j)
           zzo(j)=zz(j)
           call cart(pp(j),zz(j))
   11     continue
           if(aa(j,1).ne.aa(j,2)) goto 2
           aa(j,2)=0.0
           dd(j)=1.0
   2       ib=1
           df=xp(j)/(aa(j,1)*(aa(j,1)-aa(j,2)))
           q=aa(j,1)-dd(j)*aa(j,2)
    3      call ksi(xsi,j,ib,1,ierr)
                do 4 i=1,6
    4      sig(i,j)=sig(i,j)-df*q*xsi(i,1)
           if(ib.eq.2) go to 205
           q=(dd(j)-1.0)*aa(j,1)
           if(q.eq.0.) go to 205
           ib=2
           goto 3
 205    continue
c
c sig est la conductivite MKS divisee par (i*omega*epsilonzero).
c calcul du courant divise par (i*omega*epsilonzero) :
c
      cj(1)=+sig(1,j)*efl(1)+sig(2,j)*efl(2)+sig(3,j)*efl(3)
      cj(2)=-sig(2,j)*efl(1)+sig(4,j)*efl(2)+sig(5,j)*efl(3)
      cj(3)=+sig(3,j)*efl(1)-sig(5,j)*efl(2)+sig(6,j)*efl(3)
c
c calcul de la vitesse multipliee par Bo, pour E=1mV/m :
c
c Correction by Catherine Lacombe [CLACOMBE@megasx.obspm.fr]
c The z component of the velocity perturbation is changed
c by a drift velocity : along z
c
      		do 41 i=1,2
41      vp(j,i)=(0.0,1.0)*cj(i)/(xp(j)*xx(j))
	vp(j,3)=(0.0,1.0)*cj(3)*(xx(j)-zz(j)*vd(j))/(xp(j)*xx(j)*xx(j))
     +         -(vp(j,1)*pp(j)*vd(j)/xx(j))
c
        compz=(0.0,0.0)
	if(efl(2)*pp(1)*xp(j)*xx(j).ne.0.)
     1  compz(j)=(pp(1)*cj(1)+zz(1)*cj(3))/(pp(1)*efl(2)*xp(j)*xx(j))
c compressibilite parallele :
	compz(j)=(0.0,1.0)*compz(j)
c compressibilite alfvenique :
        compy(j)=compz(j)*bfl(3)/bfl(2)
      if(ass(j).eq.0.)then
	compz(j)=-compz(j)
	compze=compze+compz(j)*dn(j)
	compy(j)=-compy(j)
	compye=compye+compy(j)*dn(j)
		do 42 i=1,3
 42   		vp(j,i)=-vp(j,i)
      end if
      if(ass(j).eq.1.) then
	do 43 i=1,3
 43     vpp(i)=vpp(i)+vp(j,i)*dn(j)/dtot
      end if
    5 continue
      finebp=180.0*ATAN2(AIMAG(compze),REAL(compze))/pi
      fineby=180.0*ATAN2(AIMAG(compye),REAL(compye))/pi
      compze=compze/dtot
      compye=compye/dtot
      bflmo=bfl(1)*conjg(bfl(1))+bfl(2)*conjg(bfl(2))
     *     +bfl(3)*conjg(bfl(3))
      bflmo=sqrt(bflmo)
      comte1=compze*bfl(3)/bflmo
      comte2=compye*bfl(2)/bflmo
      finbt1=180.0*ATAN2(aimag(comte1),real(comte1))/pi
      finbt2=180.0*ATAN2(aimag(comte2),real(comte2))/pi
      vomo=sqrt(pp(1)**2+zz(1)**2)
      polbk=(0.,1.)*(bfl(1)*zz(1)-bfl(3)*pp(1))/(bfl(2)*vomo)
		do i=1,3
C  parametres pour Andre Mangeney :
	ZPL(i)=vpp(i)+1.d-6*bfl(i)*va
	ZMO(i)=vpp(i)-1.d-6*bfl(i)*va
	ZPSM(i)=ZPL(i)/ZMO(i)
		end do
	zp2sm2=(zpl(1)*conjg(zpl(1))+zpl(2)*conjg(zpl(2))
     1 +zpl(3)*conjg(zpl(3)))/(zmo(1)*conjg(zmo(1))
     2 +zmo(2)*conjg(zmo(2))+zmo(3)*conjg(zmo(3)))
  100 if(ipol.ne.1) return
      do 101 j=1,jma
           pp(j)=ppo(j)
           zz(j)=zzo(j)
  101 continue
      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine ryla(y,al,rc)
	double precision al
      double complex y,rc(2,2)
c
c           ****  choose method of evaluation ****
      if(al.lt.4.d0) goto 1
      ay=abs(y)
      if(ay**2.gt.75.d0*al) goto 1
      if(ay.gt.40.d0+al/3) goto 1
      if(3.d0*(al-10.d0).gt.ay.and.ay**2.lt.15.d0*al) goto 3
c          ******** numerical integration ********
      call rint(y,al,rc)
      return
c        ******** taylor series ********
    1 call rtay(y,al,rc)
      return
c         ******** asymptotic series ********
    3 call rasy(y,al,rc)
      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine rasy(y,al,rc)
	implicit double precision (a-h,o-z)
      double complex y,y2,cot,p,py,pp,ppy,pn,pyn,qn,qyn,rc(2,2)
c          ******** asymptotic series ********
      call epsil(y)
      pi=3.14159265358979
      y2=y*y
      ya=dimag(y)
      if(abs(ya).gt.110)then
      cot=(0.d0,-1.d0)
      go to 60
      endif
      cot=cos(pi*y)/sin(pi*y)
 60   continue
*                  1.e99 is too big for s/370 hardware. set to largest
*                  possible for ibm machines
*     c=1.e99
*     c=7.2e+32
CL sur le VAX :
      c=1.d+38
      pn=-y/al
      pyn=pn
      a=1.d0/(al*sqrt(2.d0*pi*al))
      qn=pi*y2*cot*a
      qyn=qn*(2.d0-y*pi*cot)-y*pi**2*y2*a
c
      p=pn+qn
      py=pyn+qyn
      pp=-pn-1.5d0*qn
      ppy=-pyn-1.5d0*qyn
      ay=abs(y)+2.d0
c
      do 4 n=1,100
           m=n-1
           pyn=(pyn*(m*m-y2)-2.d0*y2*pn)/((2*m+1)*al)
           pn =pn*(m*m-y2)/((2*m+1)*al)
           qyn=(qyn*((m+.5d0)**2-y2)-2.d0*y2*qn)/(2.d0*n*al)
           qn =qn*((m+.5d0)**2-y2)/(2.d0*n*al)
           if(m.lt.ay) goto 3
           c=n*(abs(pn)+abs(qn))
           if(c.le.1.d-7*abs(pp)) goto 5
           if(c.ge.t) goto 5
    3      p =p + pn + qn
           py =py + pyn + qyn
           pp =pp -(n + 1.d0)*pn -(n +1.5d0)*qn
           ppy=ppy-(n + 1.d0)*pyn-(n +1.5d0)*qyn
    4 t=c
c
    5 rc(1,1)=p + pn + qn
      rc(2,1)=py+ pyn+ qyn
      rc(1,2)=pp+p
      rc(2,2)=ppy+py
      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine rtay(y,al,rc)
	implicit double precision (a-h,o-z)
      double complex y,y2,rc(2,2),pn,pyn,cot
c                  ******** taylor series ********
      call epsil(y)
      y2=y*y
   10 pn=y/(y2-1.d0)
      pyn=-y*(y2+1.d0)/(y2-1.d0)**2
      rc(1,1)=pn
      rc(1,2)=pn
      rc(2,1)=pyn
      rc(2,2)=pyn
c
      do 1 i=2,100
           cot=(2*i-1)/(y2-i**2)*al
           pyn=cot*(pyn-2.d0*y2/(y2-i**2)*pn)
           pn=cot*pn
           rc(1,1)=rc(1,1)+pn
           rc(2,1)=rc(2,1)+pyn
           rc(1,2)=rc(1,2)+i*pn
           rc(2,2)=rc(2,2)+i*pyn
           t=abs(pn)*1.d8
           if(t.lt.abs(rc(1,1))) goto 2
    1 continue
    2 continue
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine rint(yy,al,rc)
c                  ******** numerical integration ********
	implicit double precision (a-h,o-z)
      double complex rc(2,2),y,yy,cot,d,exf,f,h,o,p,r,ry,rp,rpy,s
      common itest
      dimension a(16), w(16)
c       abscissas for gaussian integration
      data a/               -.98940 09349 91649,-.94457 50230 73232,
     + -.86563 12023 87831, -.75540 44083 55003,-.61787 62444 02643,
     + -.45801 67776 57227, -.28160 35507 79258,-.09501 25098 37637,
     +                       .98940 09349 91649, .94457 50230 73232,
     +  .86563 12023 87831,  .75540 44083 55003, .61787 62444 02643,
     +  .45801 67776 57227,  .28160 35507 79258, .09501 25098 37637/,
     +   w /                 .02715 24594 11754, .06225 35239 38647,
     +  .09515 85116 82492,  .12462 89712 55533, .14959 59888 16576,
     +  .16915 65193 95002,  .18260 34150 44923, .18945 06104 55068,
     +                       .02715 24594 11754, .06225 35239 38647,
     +  .09515 85116 82492,  .12462 89712 55533, .14959 59888 16576,
     +  .16915 65193 95002,  .18260 34150 44923, .18945 06104 55068/,
     + pi/3.14159265358979/
c
      itest=0
	do 1 i=1,2
	do 1 j=1,2
 1    rc(i,j)=0.d0
      call epsil(yy)
      y=yy
      if(dble(y).lt.0) y=-yy
      sig=1.d0
      if(dble(yy).lt.0) sig=-1.d0
      ya=dimag(y)
      yr=dble(y)
      ul=pi-2.8d0*y/(36.d0+y)
      if(ul.gt.pi) ul=pi
      if(abs(ya).gt.110.d0)then
      cot=(0.d0,-1.d0)
      go to 60
      endif
      cot=cos(pi*y)/sin(pi*y)
 60   continue
      d=pi*(1.d0+cot**2)
      c=yr/al
      xo=log(c+sqrt(1.d0+c**2))
c
      do 10 i=1,16
           x=ul/2.d0*(1.d0+a(i))
           z=sin(x)
           c=cos(x)
           g=yr/al*x/z
           t=sqrt(1.d0+g**2)
           b=log(g+t)
           g=(1.d0/x-c/z)*g/t
           t=al*(t*c-1.d0)
c
cc         overflow si x gt 700 dans exp(x)
c
      xya=x*ya
      if(xya.gt.700)then
      z=1.d32
      else
           z=exp(x*ya)
      endif
           c=.5d0*(z+1.d0/z)
           s=(0.d0,.5d0)*(z-1.d0/z)
           f=cot+g
           h=1.d0-g*cot
           test=dble(t-y*b)
           if(test.gt.-670.d0) goto 20
           exf=0.d0
           itest=1
           goto 30
   20      exf=exp(t-y*b)
   30      o=b*c+x*s
           p=x*c-b*s
           xy=x*yr
           r=(f*c+h*s)*exf
           ry=(f*o-h*p+d*(c-g*s))*exf
           rp=((f*t-h*xy)*c+(h*t+f*xy)*s)*exf
           rpy=(f*(t*o-xy*p)-h*(t*p+xy*o)+((t+xy*g)*c-(g*t-xy)*s)*d)*
     +     exf
c
           x=xo/2.d0*(1.d0+a(i))
           z=exp(x)
           c=(z+1.d0/z)/2.d0-1.d0
           p=exp(al*c-y*x)
           rc(1,1)=rc(1,1)+w(i)*(ul*r+xo*p)
           rc(2,1)=rc(2,1)-w(i)*(ul*ry+xo*x*p)
           rc(1,2)=rc(1,2)+w(i)*(ul*rp+xo*al*c*p)
           rc(2,2)=rc(2,2)-w(i)*(ul*rpy+xo*al*x*c*p)
   10 continue
c
      o=y/al
      p=y**2/2.d0
      rc(1,1)=o*(y*rc(1,1)/2.d0-1.d0)*sig
      rc(2,1)=2.d0*rc(1,1)+o*(p*rc(2,1)+1.d0)*sig
      rc(1,2)=y*o*rc(1,2)/2.d0*sig
      rc(2,2)=2.d0*rc(1,2)+o*p*rc(2,2)*sig
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine errmsg( errcod )
*     .... arguments
      integer errcod
      type '( //// 1x,  a,  i5 //// )',
     +      'error code from "open" = ',  errcod
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine cart(p,z)
      po=p
      p=p*cos(z)
      z=po*sin(z)
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine trinit(xoi,wfreq,*,*)
c
c         ce s/p aide a la recherche d'une frequence d'initialisation
c         il resoud l'equation de dispersion pour les parametres d'entre
c        pour des frequences comprises entre fimin et fimax
c
      common /infile/ ifile
      character*1 essai
      common/be/ibern,ipol,iplas,pn,gyr,itrin
   10 continue
      if(wfreq.eq.1)then
      itrin=1
      type *,'   entrer fmin,fmax,fpas:'
           read (ifile,*) fimin,fimax,fipa
           finit=fimin
           flim=fimin+200.*fipa
           if(flim.le.fimax)fimax=flim
           xoi=fre(finit)
           wfreq=2
           return 1
      endif
      finit=finit+fipa
      xoi=fre(finit)
      if(finit.gt.fimax)then
           type *,'frequences explorees entre ',fimin,' et ',fimax
           type *,'encore un essai (oui/non)? :'
           read(ifile,20) essai
   20 format(a1)
           if(essai.eq.'o')then
                wfreq=1
                go to 10
                else
                wfreq=0
                if(iplas.eq.1)call denofpe
                return 2
           endif
           else
           return 1
      endif
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine epsil(y)
c
c  ce s/p corrige la valeur ' y ' de epsilon pour eviter une singularite
c
      double complex y
      yr=real(y)
      yi=dimag(y)
      n=ifix(yr+.5)
      delta=yr-float(n)
      eps=1.e-14*n
      if(abs(delta).lt.eps.and.yi.eq.0) yr=float(n)+sign(eps,delta)
      y=dcmplx(yr,yi)
      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      function fre(f)
c
      common/be/ibern,ipol,iplas,pn,gyr,itrin
      xfr=f
      gyr=float(ifix(xfr))
      if(ibern.eq.1)xfr=gyr+0.5
      if(iplas.eq.1)xfr=xfr*pn
      fre=xfr
      return
      end
