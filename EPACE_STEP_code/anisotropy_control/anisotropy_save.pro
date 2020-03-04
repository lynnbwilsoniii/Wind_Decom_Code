pro anisotropy_save

 fname       = strarr(1)  
 tag_name    = strarr(1)
 tag_titles  = intarr(27)
 tag_titles  = [94,95,96,97,98,99,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]  

 
   ;   fname="/Users/mdesai/Desktop/IDL/wind/fortran_vax/anisotropy_control/ANISCNO1094.all"
   ;   Nmax_row = 800000L
   ;   print,fname
   ;   stop 
   ;   readdata_struct_xdf,fname,Nmax_row,xdf
    ;  stop
   ; save,filename="/Users/mdesai/Desktop/IDL/wind/fortran_vax/anisotropy_control/aniscno1"+tag_name+".sav"
  

tag_los = 26L
while (tag_los lt 27) do begin  
 
  tag_name =  string(tag_titles(tag_los),format="(i3.3)")      
      
      fname="/Users/mdesai/Desktop/IDL/wind/fortran_vax/anisotropy_control/ANISCNO1"+tag_name+".all"
      Nmax_row = 800000L
      readdata_struct_xdf,fname,Nmax_row,xdf
      save,filename="/Users/mdesai/Desktop/IDL/wind/fortran_vax/anisotropy_control/aniscno1"+tag_name+".sav"
  
      fname="/Users/mdesai/Desktop/IDL/wind/fortran_vax/anisotropy_control/aniscno2"+tag_name+".all"
      Nmax_row = 800000L
      readdata_struct_xdf,fname,Nmax_row,xdf
      save,filename="/Users/mdesai/Desktop/IDL/wind/fortran_vax/anisotropy_control/aniscno2"+tag_name+".sav"
  
      fname="/Users/mdesai/Desktop/IDL/wind/fortran_vax/anisotropy_control/anisfet1"+tag_name+".all"
      Nmax_row = 800000L
      readdata_struct_xdf,fname,Nmax_row,xdf
      save,filename="/Users/mdesai/Desktop/IDL/wind/fortran_vax/anisotropy_control/anisfet1"+tag_name+".sav"
  
      fname="/Users/mdesai/Desktop/IDL/wind/fortran_vax/anisotropy_control/anisfet2"+tag_name+".all"
      Nmax_row = 800000L
      readdata_struct_xdf,fname,Nmax_row,xdf
      save,filename="/Users/mdesai/Desktop/IDL/wind/fortran_vax/anisotropy_control/anisfet2"+tag_name+".sav"
  
     
      fname="/Users/mdesai/Desktop/IDL/wind/fortran_vax/anisotropy_control/anishet1"+tag_name+".all"
      Nmax_row = 800000L
      readdata_struct_xdf,fname,Nmax_row,xdf
      save,filename="/Users/mdesai/Desktop/IDL/wind/fortran_vax/anisotropy_control/anishet1"+tag_name+".sav"
  
      fname="/Users/mdesai/Desktop/IDL/wind/fortran_vax/anisotropy_control/anishet2"+tag_name+".all"
      Nmax_row = 800000L
      readdata_struct_xdf,fname,Nmax_row,xdf
      save,filename="/Users/mdesai/Desktop/IDL/wind/fortran_vax/anisotropy_control/anishet2"+tag_name+".sav"
  
      fname="/Users/mdesai/Desktop/IDL/wind/fortran_vax/anisotropy_control/anishte1"+tag_name+".all"
      Nmax_row = 800000L
      readdata_struct_xdf,fname,Nmax_row,xdf
      save,filename="/Users/mdesai/Desktop/IDL/wind/fortran_vax/anisotropy_control/anishte1"+tag_name+".sav"
  
      fname="/Users/mdesai/Desktop/IDL/wind/fortran_vax/anisotropy_control/anishte2"+tag_name+".all"
      Nmax_row = 800000L
      readdata_struct_xdf,fname,Nmax_row,xdf
      save,filename="/Users/mdesai/Desktop/IDL/wind/fortran_vax/anisotropy_control/anishte2"+tag_name+".sav"
    
      fname="/Users/mdesai/Desktop/IDL/wind/fortran_vax/anisotropy_control/schktel1"+tag_name+".all"
      Nmax_row = 800000L
      readdata_struct_xdf,fname,Nmax_row,xdf
      save,filename="/Users/mdesai/Desktop/IDL/wind/fortran_vax/anisotropy_control/schktel1"+tag_name+".sav"
  
      fname="/Users/mdesai/Desktop/IDL/wind/fortran_vax/anisotropy_control/schktel2"+tag_name+".all"
      Nmax_row = 800000L
      readdata_struct_xdf,fname,Nmax_row,xdf
      save,filename="/Users/mdesai/Desktop/IDL/wind/fortran_vax/anisotropy_control/schktel2"+tag_name+".sav"
  
    

 tag_los = tag_los + 1
endwhile

 ;stop
end
