c    Subroutines for the program sdf_lister
c    
c     Contents:
c            subroutine expand_data_packet
c
c            integer function decode
c
c            integer function reames
c
c
c****************************************************
c
      subroutine expand_data_packet
c
c****************************************************
c
c     decodes packet data in unformatted sdf files
c
c     12/4/95   modified by J. Dwyer
c         modifications:
c              2/22/96 by J. Dwyer: corrected telescope number corresponding to tel bit
c
      include 'sdf_include.inc'   ! include type declarations
      include 'hex_include.inc'   ! include type declarations
c
c
      integer*2    itemp(5)
      integer*2    packet_int(733)   ! integer version of packet
      real   reames, decode  ! function declarations
c
      do 50 i=1,733
        packet_int(i) = zext(packet(i))
50    continue
c
      if (NPHA.gt.0) then
         do 100 i=1,NPHA            
           call mvbits(int(packet_int(483+(i-1)*5+1)),1,3,epha(i),0)
           call mvbits(int(packet_int(483+(i-1)*5)),0,7,epha(i),3)
           call mvbits(int(packet_int(483+(i-1)*5+1)),7,1,tpha(i),0)
           call mvbits(int(packet_int(483+(i-1)*5+2)),0,8,tpha(i),1)
           call mvbits(int(packet_int(483+(i-1)*5)),7,1,ramp(i),0)
           call mvbits(int(packet_int(483+(i-1)*5+1)),5,1,tel(i),0)
           call mvbits(int(packet_int(483+(i-1)*5+1)),6,1,cn(i),0)
           call mvbits(int(packet_int(483+(i-1)*5+1)),4,1,slant(i),0)
           call mvbits(int(packet_int(483+(i-1)*5+1)),0,1,ssd2(i),0)
           call mvbits(int(packet_int(483+(i-1)*5+3)),7,1,ab(i),0)
           call mvbits(int(packet_int(483+(i-1)*5+3)),0,6,rom(i),0)
           call mvbits(int(packet_int(483+(i-1)*5+4)),4,4,spin(i),0)
           call mvbits(int(packet_int(483+(i-1)*5+4)),0,4,sect(i),0)
c
           ab(i) = jmod(spin(i),2)   
c
c     cuts on data:
          data_flag(i) = .true.        
          if ((0.eq.tel(i)).and.(tel_flag.eq.1))   
     1              data_flag(i) = .false.
          if ((1.eq.tel(i)).and.(tel_flag.eq.2))                       
     1             data_flag(i) = .false.
          if ((ramp_flag.ge.0).and.(ramp(i).ne.ramp_flag)) 
     1             data_flag(i) = .false.
          if ((cal_flag.ge.0).and.(cal_flag.ne.cn(i)))
     1             data_flag(i) = .false.
          if  ((slant_flag.ge.0).and.(slant_flag.ne.slant(i)))  
     1             data_flag(i) = .false.
          if  ((ssd2_flag.ge.0).and.(ssd2_flag.ne.ssd2(i)))  
     1             data_flag(i) = .false.
          if  ((stateab_flag.ge.0).and.(stateab_flag.ne.ab(i)))  
     1            data_flag(i) = .false.
          if ((rom_box_flag.ge.0).and.(rom_box_flag.ne.rom(i))) 
     1            data_flag(i) = .false.
          if ((sector_flag.ge.0).and.(sector_flag.ne.sect(i))) 
     1            data_flag(i) = .false.
          if (epha(i).lt.min_en_channel)
     1           data_flag(i) = .false.
          if (tpha(i).lt.min_tof_channel)  
     1           data_flag(i) = .false.
          if (epha(i).eq.1022)  
     1           data_flag(i) = .false.
          if (tpha(i).eq.511)  
     1           data_flag(i) = .false.
100     continue  ! end do loop   
      end if
c  
200     continue  ! exit from loop to here whwn reach last PHA record
c   
c  discriminator rates:   
      do 5 i=1,5
        call mvbits(packet_int(38+2*i-1),0,8,itemp(i),0)
        call mvbits(packet_int(38+2*i),0,8,itemp(i),8)
5     continue
c
c    nonsectored discriminator rates:
      vsebar_1mf=reames(itemp(1))
      start_1mf=reames(itemp(2))
      stop_1mf=reames(itemp(3))
      d2_1mf=reames(itemp(4))
      d1_1mf=reames(itemp(5))
c  
c  packet info:
      call  mvbits(int(packet_int(2)), 0, 8,phase,0)
      call mvbits(int(packet_int(5)), 0, 6, nspins,0)
      call mvbits(int(packet_int(5)), 6, 2,first_spin,0)
      call mvbits(int(packet_int(6)), 0, 8,step_status,0)
c
c  sectored discriminator rates:
      do 10 i=1,32               
        sdrate(i)=decode('C',packet_int(6+i))        
10    continue
c
c  matrix rates:
      do 20 i=1,302       
        m1rate(i)=decode('A',packet_int(48+i))      
20    continue
c
      do 30 i=1,84
        m2rate(i)=decode('A',packet_int(350+i))
30    continue
c
      do 40 i=1,48
        m4rate(i)=decode('A',packet_int(434+i))
40    continue
c
      return
      end  ! end expand_data_packet
c
c
c
c
c*****************************************************
c
      real function decode (AorC,comp)
c
c*****************************************************
c
c     decodes A and C compression
c
      integer*2 comp
      character*1 AorC
      integer m,e
c
      if (comp .eq. 'FF'x) then
       decode = -1
       return
      endif
      if (AorC.eq.'A') then
       m=ibits(comp,0,4)
       e=ibits(comp,4,4)
       if (e.eq.0) then
        decode = m
       else
        decode = (16+m)*2.0**(e-1)
       endif
      else if (AorC.eq.'C') then
       if (btest(comp,6).and.btest(comp,7)) then
        m=ibits(comp,0,3)
        e=ibits(comp,3,5)
        decode = (8+m)*2.0**(e-12)
       else
        m=ibits(comp,0,4)
        e=ibits(comp,4,4)
        if (e.eq.0) then
         decode = m
        else
         decode = (16+m)*2.0**(e-1)
        endif
       endif
      else
       print *,'Compression type not A or C; decode = 0'
       decode = 0
      endif
c
      return
      end  ! end decode
c
c
c
c
c****************************************************
c
      real function reames(coded)
c
c****************************************************
c
c     decodes reames 24 -> 16 compression
c
      integer*2 coded,power,itemp1,itemp2
      real   decoded
      parameter (itemp1=2047,itemp2=2048)
c
      if (coded .eq. 'FFFF'x) then
       reames=-1
       return
      endif
      call mvbits(coded,11,5,power,0)
      if (power.gt.1) then
       coded=iiand(coded,itemp1)
       coded=iior(coded,itemp2)
       decoded=coded*2.0**(power-1)
      else
       decoded=coded
      endif
      reames=decoded
      return
      end ! end reames
c
