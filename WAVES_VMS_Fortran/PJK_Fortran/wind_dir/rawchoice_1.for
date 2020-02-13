From:	WAVES::GOETZ         2-JUL-1997 17:18:33.11
To:	PJK
CC:	GOETZ
Subj:	packet_subtype

I use packet_subtype to decide whether the FFT event I just got is raw or 
otherwise.

As in:

        call w_item_i4(ch,'PACKET_SUBTYPE',my_subtype,1,n)
        if (my_subtype .eq. 1) then
            raw = .true.
        else
            raw = .false.
        endif

Dopey but it works.

K
