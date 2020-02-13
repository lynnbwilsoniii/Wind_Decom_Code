From:	SMTP%"kellogg@megasx.obspm.fr"  2-NOV-1998 17:31:23.73
To:	ME
CC:	KELLOGG
Subj:	

Return-Path: kellogg@megasx.obspm.fr
Received: by waves.space.umn.edu (UCX V4.1-12, OpenVMS V6.2-1H3 Alpha);
	Mon, 2 Nov 1998 17:31:22 -0600
Date: Tue, 3 Nov 1998 00:30:24 +0200
Message-Id: <98110300302408@megasx.obspm.fr>
From: kellogg@megasx.obspm.fr
To: kellogg@waves.space.umn.edu, KELLOGG@waves.space.umn.edu
X-VMS-To: ME
X-VMS-Cc: KELLOGG

C
	  bzmax = 0.
	  bzmin = 0.
	  do i = 1,1024
	    if(Rdata(i,3).gt.bzmax) then
		bzmax = Rdata(i,3)
		ibzmax = i
	    endif
	    if(Rdata(i,3).lt.bzmin) then
		bzmin = Rdata(i,3)
		ibzmin = i
	    endif
	  enddo
c	the following is based on solving:
c	  bzmxang = antang + dangd*(1024-ibzmax)
c	  bzmnang = antang + dangd*(1024-ibzmin)
c	for antang, given bzmxang = 34. deg.
c
	antangb = 34. - dangd*(1024-ibzmax)
	if(antangb.lt.-180.) antangb = antangb + 360.
c
	  write(65,*) 'bzmin,max',sceti4,ibzmin,ibzmax		
C	  write(65,*) 'bzmin,max angles',bzmnang,bzmxang	
c        endif
C
C	ELIMINATE BZ GLITCH
C
	DO N = 1,1024
		WT(N) = 1.
	ENDDO
C
	N1 = IBZMIN - 30
	IF(N1.LT.1) N1 = N1 + 1024
	N2 = IBZMAX + 30
	IF(N2.GT.1024) N2 = N2 - 1024
C	PRINT*,'ELIM GIVES N1,N2=',N1,N2
	N1T = MIN0(N1,N2)
	N2T = MAX0(N1,N2)
	N1 = N1T
	N2 = N2T
C
	IF(IABS(N2-N1).LT.80) THEN
	  DO N = N1,N2
	    VDATA(N,3) = 0.
	    RDATA(N,3) = 0.
	    WT(N) = 0.
	  ENDDO
	ELSE
	  DO N = 1,N1
	    VDATA(N,3) = 0.
	    RDATA(N,3) = 0.
	    WT(N) = 0.
	  ENDDO
	  DO N = N2,1024
	    VDATA(N,3) = 0.
	    RDATA(N,3) = 0.
	    WT(N) = 0.
	  ENDDO
	ENDIF
C

