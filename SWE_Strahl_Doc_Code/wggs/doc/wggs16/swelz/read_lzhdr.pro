FUNCTION read_lzhdr,ifnum

;Jim Byrnes Dec 1993

ON_ERROR,1

ef = {editfile, nm:'01234567890123456789012345678901234567890123', $
      ek:'EDITXXYYYYDDDHHMMSSVV   ', ern:'RERN', epvn:'EPVNEPVN', $
      erdt:'YYYYDDDHHMMSSUUU', dt:'DTDT', mk:'0123456789012345678901234567'}
dt = {time, yr:0l, dy:0l, ms:0l, us:0l}
fh = {filehd, spcid:0l, intsn:0l, instrnm:'NAME', phyrc:0l, phyrcmf:0l, $
      nphyrc:0l, mfcfst:0l, mfclst:0l, sccfst:bytarr(8), scclst:bytarr(8), $
      fst:replicate(dt,1), lst:replicate(dt,1), $
      nmfe:0l, nmf:0l, ngap:0l, dct:'TYPE', drn:0l, $
      dpvn:'DPVNDPVN', ddvn:'DDVNDDVN', drdt:'YYYYDDDHHMMSSUUU', $
      ifn:'01234567890123456789012345678901234567890123',rln:0l, $
      spares:lonarr(5), $
      mrn:0l, mpvn:'MPVNMPVN', mpdt:'YYYYDDDHHMMSSUUU',nef:0l, $
      efs:replicate(ef,20)}

point_lun,ifnum,0
readu,ifnum,fh
fh = conv_vax_unix(fh)
point_lun,ifnum,fh.rln
return,fh
end
