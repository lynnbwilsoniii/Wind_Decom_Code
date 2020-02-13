; decompress 8-bit values to 12-bit count values
; see wggs.pdf --> counts were measured in 12-bits
; and saved in a log-compressed 8-bit format
; table comes from file decompress.tbl, from wggs software

FUNCTION dcomp_tbl_kosta, counts

;counts is an array of 8-bit integers between 0 and 255

tbl = ['000', '001', '002', '003', '004', '005', '006', '007', '008', '009', '00a', '00b', '00c', '00d', '00e', '00f', $
       '010', '011', '012', '013', '014', '015', '016', '017', '018', '019', '01a', '01b', '01c', '01d', '01e', '01f', $
       '020', '021', '022', '023', '024', '025', '026', '027', '028', '029', '02a', '02b', '02c', '02d', '02e', '02f', $
       '030', '031', '032', '033', '034', '035', '036', '037', '038', '039', '03a', '03b', '03c', '03d', '03e', '03f', $
       '040', '042', '044', '046', '048', '04a', '04c', '04e', '050', '052', '054', '056', '058', '05a', '05c', '05e', $
       '060', '062', '064', '066', '068', '06a', '06c', '06e', '070', '072', '074', '076', '078', '07a', '07c', '07e', $
       '080', '084', '088', '08c', '090', '094', '098', '09c', '0a0', '0a4', '0a8', '0ac', '0b0', '0b4', '0b8', '0bc', $
       '0c0', '0c4', '0c8', '0cc', '0d0', '0d4', '0d8', '0dc', '0e0', '0e4', '0e8', '0ec', '0f0', '0f4', '0f8', '0fc', $
       '100', '108', '110', '118', '120', '128', '130', '138', '140', '148', '150', '158', '160', '168', '170', '178', $
       '180', '188', '190', '198', '1a0', '1a8', '1b0', '1b8', '1c0', '1c8', '1d0', '1d8', '1e0', '1e8', '1f0', '1f8', $
       '200', '210', '220', '230', '240', '250', '260', '270', '280', '290', '2a0', '2b0', '2c0', '2d0', '2e0', '2f0', $
       '300', '310', '320', '330', '340', '350', '360', '370', '380', '390', '3a0', '3b0', '3c0', '3d0', '3e0', '3f0', $
       '400', '420', '440', '460', '480', '4a0', '4c0', '4e0', '500', '520', '540', '560', '580', '5a0', '5c0', '5e0', $
       '600', '620', '640', '660', '680', '6a0', '6c0', '6e0', '700', '720', '740', '760', '780', '7a0', '7c0', '7e0', $
       '800', '840', '880', '8c0', '900', '940', '980', '9c0', 'a00', 'a40', 'a80', 'ac0', 'b00', 'b40', 'b80', 'bc0', $
       'c00', 'c40', 'c80', 'cc0', 'd00', 'd40', 'd80', 'dc0', 'e00', 'e40', 'e80', 'ec0', 'f00', 'f40', 'f80', 'fc0'] 

output = counts * 0L

thenumber = 0L
for i = 0L, n_elements(counts)-1L do begin
   thestring = tbl[counts[i]]
   ReadS, tbl[counts[i]], thenumber, Format='(Z)'
   output[i] = thenumber
endfor

return, output
END

