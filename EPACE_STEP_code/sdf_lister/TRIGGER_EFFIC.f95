c
c
c
c
c*****************************************************
c
      subroutine Trigger_effic(az,am,ennuc,sampex_time,eff)
c
c*****************************************************
c	calculates STEP MCP triggering efficiency as a function
c	of time, mass, and en/nuc of incident particle
c
c	gm 15-Jun-94
c
c	modification history:
c		5-May-95	initial version adapted from leica routine
c		7-Aug-95	modify include derf.for stmt to include
c				device/directory name to allow compiling
c				this routine from other directoriesc
c    3/21/96 by J. Dwyer modified for sdf_lister
c    3/22/96 by J. Dwyer added j = 13 to amass loop exit
c    3/22/96 by J. Dwyer added az to call, included az.eq.2 case is edep calculation
c    4/26/96 by J. Dwyer changed directory of calibration files
c	4/26/96	add "width_coef" in input data file /gm
c	12-Mar-98	change calibration directory to
c			$step_data:[data.cal]
c
c	*** variables in call ***
c
c	    Supplied by calling program:
c		am = mass of ion, in amu
c		ennuc = energy of ion in MeV/nuc
c		sampex_time (seconds since 1/1/92)  ** integer*4 variable **
c	    Returned by subroutine:
c		eff = efficiency 
c
      character*80   caldir   ! directory to use
      dimension eincident(1000),edepsave(1000,13),amass(13),
     1     fegain(2,100),thresh(2),fegain_now(2),dist(2),fract(2)
      integer    stime(100),sampex_time
      REAL*8   DERF,derfarg 
      real   az    ! charge of nucleus
      data amass/1.,3.,4.,12.,14.,16.,20.,24.,28.,32.,36.,56.,60./
c
c	check for first call
c
       caldir = 
     1  '$step_data:[data.cal] '    !  3/12/98
      if(ifirst.eq.1) goto 200
c	read input files -- first threshold data
      open(unit=33,name=
     1  caldir//'TRIGGER_EFFIC.DAT',
     1  status='old',action='read')
c	read in start, stop thresholds  (start threshold not used,
c		but left in for future possible use)
      read(33,*) thresh, width_coef
      close (33)
c
c
c	read in energy deposit array
c
      open(unit=34,name=
     1  caldir//'stepMRDEDX_nickel.KAL;1',
     1  status='old',action='read',recl=32000)
c 	skip header lines
      nspec=13
      nskip=46
      do 20 j=1,nskip
20	   read(34,25)
25	   format(x)
      do 30 j=1,1000
30	   read(34,*,end=40) eincident(j),(edepsave(j,k),k=1,13)
40	   close (34)
c
c	read in WSZ gain file
c
      open(unit=35,name=
     1    caldir//'step_gain_HISTORY.DAT',
     1    status='old',action='read')

      read(35,50)
50   	format(x)
      do 60 j=1,100
      read(35,*,end=65) stime(j),(fegain(i,j), i=2,1,-1)
60   	continue
65	   close (35)
      itimes=j-1
200   	ifirst=1
c	zero out efficiencies from last call
      eff=0.
      fract(1)=0.
      fract(2)=0.
c
c	find the Fe gain index for the current time
c
      do 300 j=2,itimes
      if(sampex_time.le.stime(j)) goto 350
300	  continue
      fegain_now(1)=fegain(1,itimes)
      fegain_now(2)=fegain(2,itimes)
      goto 355
350	   igain=j
c
c
c	interpolate Fe gain for current sampex time
c
      do 353 i=1,2
353	  fegain_now(i) = (float(sampex_time-stime(igain-1))/
     1   float(stime(igain)-stime(igain-1)))*
     1   (fegain(i,igain)-fegain(i,igain-1))
     1     + fegain(i,igain-1)
c
c
c	find energy deposit index
c
355	  do 360 j=1,1000
             if(ennuc.lt.eincident(j)) goto 370
360	  continue
370	  ien=j
c
c	if we run off the end of the table, quit
      if(ien.gt.1000) stop
c
c	interpolate energy deposit for this mass
c
c
      do 400 j=2,13
	      if(am.le.amass(j)) goto 450
400	  continue
          j = 13
450	  iedep=j
c
c
      if (az.eq.2) then 
        edep= edepsave(ien,3)
      else
        edep=((am-amass(iedep-1))/
     1     (amass(iedep)-amass(iedep-1)))*
     1    (edepsave(ien,iedep)-edepsave(ien,iedep-1))
     1    + edepsave(ien,iedep-1)
      end if
c
c
c	calculate the width from fit to dE/dx vs. sigma curve 6/24/94
c	(leica) -- adjusted for STEP 5/10/95	
c       
      if(edep.gt.0.) goto 600
470	   eff=1.e-10
c	type *, am,amass(iedep-1),amass(iedep),edepsave(ien,iedep),
c     *   edepsave(ien,iedep-1),ien,iedep
      goto 620
c
c          
600	   width=width_coef*(edep)**0.25
c
c
c
c
c	calculate distance to threshold in standard deviations
c	  --- adjust threshold for gain change compared with launch
c
c
      fract(1)=1.
      do 610 i=1,2
	    dist(i) = ((thresh(i)*fegain(i,1)/
     1         fegain_now(i)) - edep)/width 
c	type *,dist(i),thresh(i),fegain(i,1)/fegain_now(i),edep,width
c
c	calculate fraction of events above threshold
c      
      derfarg=dist(i)/sqrt(2.)
610	   fract(i) = 1. - (derf(derfarg)+1.)/2.      
c
c	efficiency is the fraction of events above threshold
c
c
      estart=fract(1)
      estop=fract(2)
	    eff=estart*estop
c	type *, estart, estop, eff
c
c	
c
      if(eff.le.0.) eff=1e-10
c
620	   return
      end  ! end trigger_effic
c
c
