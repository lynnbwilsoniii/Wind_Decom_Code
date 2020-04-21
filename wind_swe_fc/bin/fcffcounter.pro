PRO fcffcounter
byte = 0
openr,lun1,'fc_data.prt',/get_lun
readf,lun1,string
