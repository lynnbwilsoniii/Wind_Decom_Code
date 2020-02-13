	integer*4	function	rassign(i)
	integer*4 i,ir,irtable(8)
	data irtable /1,3,1,4,2,3,2,4/

	  ir = i.and.7
	  rassign = irtable(i+1)

	return
	end

