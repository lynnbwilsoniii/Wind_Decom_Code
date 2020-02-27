FUNCTION MFI_GLITCH_MASK, DATA_ARR=DATA, RATIO=RATIO
; returns mask with 1 corresponding to glitches and 0 to normal data.

;- arrays of data gradients etc. 
n_data   = n_elements(data)
d_data   = data[1L:n_data-1L] - data[0L:n_data-2L]
d_data0  = d_data[ 0L:n_data-23L]
d_data1  = d_data[ 1L:n_data-22L]
d_data2  = d_data[ 2L:n_data-21L]
d_data3  = d_data[ 3L:n_data-20L]
d_data4  = d_data[ 4L:n_data-19L]
d_data5  = d_data[ 5L:n_data-18L]
d_data6  = d_data[ 6L:n_data-17L]
d_data7  = d_data[ 7L:n_data-16L]
d_data8  = d_data[ 8L:n_data-15L]
d_data9  = d_data[ 9L:n_data-14L]
d_data10 = d_data[10L:n_data-13L]
d_data11 = d_data[11L:n_data-12L]
d_data12 = d_data[12L:n_data-11L]
d_data13 = d_data[13L:n_data-10L]
d_data14 = d_data[14L:n_data- 9L]
d_data15 = d_data[15L:n_data- 8L]
d_data16 = d_data[16L:n_data- 7L]
d_data17 = d_data[17L:n_data- 6L]
d_data18 = d_data[18L:n_data- 5L]
d_data19 = d_data[19L:n_data- 4L]
d_data20 = d_data[20L:n_data- 3L]
d_data21 = d_data[21L:n_data- 2L]
mask0    = replicate(0, n_data-22L)
mask1    = replicate(0, n_data-22L)
mask2    = replicate(0, n_data-22L)
mask3    = replicate(0, n_data-22L)
mask4    = replicate(0, n_data-22L)
mask5    = replicate(0, n_data-22L)
mask6    = replicate(0, n_data-22L)
mask7    = replicate(0, n_data-22L)
mask8    = replicate(0, n_data-22L)
mask9    = replicate(0, n_data-22L)
mask10   = replicate(0, n_data-22L)
mask11   = replicate(0, n_data-22L)
mask12   = replicate(0, n_data-22L)
mask13   = replicate(0, n_data-22L)
mask14   = replicate(0, n_data-22L)
sub_temp012567   = where((d_data0 ne 0) and (d_data1 ne 0) and (d_data2 ne 0) and $
                         (d_data5 ne 0) and (d_data6 ne 0) and (d_data7 ne 0), n_temp012567)
sub_temp012678   = where((d_data0 ne 0) and (d_data1 ne 0) and (d_data2 ne 0) and $
                         (d_data6 ne 0) and (d_data7 ne 0) and (d_data8 ne 0), n_temp012678)
sub_temp012789   = where((d_data0 ne 0) and (d_data1 ne 0) and (d_data2 ne 0) and $
                         (d_data7 ne 0) and (d_data8 ne 0) and (d_data9 ne 0), n_temp012789)
sub_temp0128910  = where((d_data0 ne 0) and (d_data1 ne 0) and (d_data2 ne 0) and $
                         (d_data8 ne 0) and (d_data9 ne 0) and (d_data10 ne 0), n_temp0128910)
sub_temp01291011 = where((d_data0 ne 0) and (d_data1 ne 0) and (d_data2 ne 0) and $
                         (d_data9 ne 0) and (d_data10 ne 0) and (d_data11 ne 0), n_temp01291011)
sub_temp012101112= where((d_data0 ne 0) and (d_data1 ne 0) and (d_data2 ne 0) and $
                         (d_data10 ne 0) and (d_data11 ne 0) and (d_data12 ne 0), n_temp012101112)
sub_temp012111213= where((d_data0 ne 0) and (d_data1 ne 0) and (d_data2 ne 0) and $
                         (d_data11 ne 0) and (d_data12 ne 0) and (d_data13 ne 0), n_temp012111213)
sub_temp012121314= where((d_data0 ne 0) and (d_data1 ne 0) and (d_data2 ne 0) and $
                         (d_data12 ne 0) and (d_data13 ne 0) and (d_data14 ne 0), n_temp012121314)
sub_temp012131415= where((d_data0 ne 0) and (d_data1 ne 0) and (d_data2 ne 0) and $
                         (d_data13 ne 0) and (d_data14 ne 0) and (d_data15 ne 0), n_temp012131415)
sub_temp012141516= where((d_data0 ne 0) and (d_data1 ne 0) and (d_data2 ne 0) and $
                         (d_data14 ne 0) and (d_data15 ne 0) and (d_data16 ne 0), n_temp012141516)
sub_temp012151617= where((d_data0 ne 0) and (d_data1 ne 0) and (d_data2 ne 0) and $
                         (d_data15 ne 0) and (d_data16 ne 0) and (d_data17 ne 0), n_temp012151617)
sub_temp012161718= where((d_data0 ne 0) and (d_data1 ne 0) and (d_data2 ne 0) and $
                         (d_data16 ne 0) and (d_data17 ne 0) and (d_data18 ne 0), n_temp012161718)
sub_temp012171819= where((d_data0 ne 0) and (d_data1 ne 0) and (d_data2 ne 0) and $
                         (d_data17 ne 0) and (d_data18 ne 0) and (d_data19 ne 0), n_temp012171819)
sub_temp012181920= where((d_data0 ne 0) and (d_data1 ne 0) and (d_data2 ne 0) and $
                         (d_data18 ne 0) and (d_data19 ne 0) and (d_data20 ne 0), n_temp012181920)
sub_temp012192021= where((d_data0 ne 0) and (d_data1 ne 0) and (d_data2 ne 0) and $
                         (d_data19 ne 0) and (d_data20 ne 0) and (d_data21 ne 0), n_temp012192021)

;- fill masks based on ratio
if (n_temp012567 gt 0) then begin
   mean_abs_change_l = (abs(d_data0) + abs(d_data1) + abs(d_data2))/3d
   mean_abs_change_r = (abs(d_data5) + abs(d_data6) + abs(d_data7))/3d
           sign_temp = d_data3*d_data4
           mask_temp = (sign_temp[sub_temp012567] lt 0) and $
                       (abs(d_data3[sub_temp012567])/mean_abs_change_l[sub_temp012567] gt 1.0d0*ratio) and $
                       (abs(d_data4[sub_temp012567])/mean_abs_change_r[sub_temp012567] gt 1.0d0*ratio)
   mask0[sub_temp012567] or= mask_temp
endif
if (n_temp012678 gt 0) then begin
   mean_abs_change_l = (abs(d_data0) + abs(d_data1) + abs(d_data2))/3d
   mean_abs_change_r = (abs(d_data6) + abs(d_data7) + abs(d_data8))/3d
           sign_temp = (d_data3+d_data4)*d_data5
           mask_temp = (sign_temp[sub_temp012678] lt 0) and $
                       (abs(d_data3[sub_temp012678])/mean_abs_change_l[sub_temp012678] gt 1.2d0*ratio) and $
                       (abs(d_data5[sub_temp012678])/mean_abs_change_r[sub_temp012678] gt 1.2d0*ratio)
   mask0[sub_temp012678] or= mask_temp
   mask1[sub_temp012678] or= mask_temp
endif
if (n_temp012789 gt 0) then begin
   mean_abs_change_l = (abs(d_data0) + abs(d_data1) + abs(d_data2))/3d
   mean_abs_change_r = (abs(d_data7) + abs(d_data8) + abs(d_data9))/3d
           sign_temp = (d_data3+d_data4+d_data5)*d_data6
           mask_temp = (sign_temp[sub_temp012789] lt 0) and $
                       (abs(d_data3[sub_temp012789])/mean_abs_change_l[sub_temp012789] gt 1.4d0*ratio) and $
                       (abs(d_data6[sub_temp012789])/mean_abs_change_r[sub_temp012789] gt 1.4d0*ratio)
   mask0[sub_temp012789] or= mask_temp
   mask1[sub_temp012789] or= mask_temp
   mask2[sub_temp012789] or= mask_temp
endif   
if (n_temp0128910 gt 0) then begin
   mean_abs_change_l = (abs(d_data0) + abs(d_data1) + abs(d_data2))/3d
   mean_abs_change_r = (abs(d_data8) + abs(d_data9) + abs(d_data10))/3d
           sign_temp = (d_data3+d_data4+d_data5+d_data6)*d_data7
           mask_temp = (sign_temp[sub_temp0128910] lt 0) and $
                       (abs(d_data3[sub_temp0128910])/mean_abs_change_l[sub_temp0128910] gt 1.5d0*ratio) and $
                       (abs(d_data7[sub_temp0128910])/mean_abs_change_r[sub_temp0128910] gt 1.5d0*ratio)
   mask0[sub_temp0128910] or= mask_temp
   mask1[sub_temp0128910] or= mask_temp
   mask2[sub_temp0128910] or= mask_temp
   mask3[sub_temp0128910] or= mask_temp
endif
if (n_temp01291011 gt 0) then begin
   mean_abs_change_l = (abs(d_data0) + abs(d_data1) + abs(d_data2))/3d
   mean_abs_change_r = (abs(d_data9) + abs(d_data10) + abs(d_data11))/3d
           sign_temp = (d_data3+d_data4+d_data5+d_data6+d_data7)*d_data8
           mask_temp = (sign_temp[sub_temp01291011] lt 0) and $
                       (abs(d_data3[sub_temp01291011])/mean_abs_change_l[sub_temp01291011] gt 1.6d0*ratio) and $
                       (abs(d_data8[sub_temp01291011])/mean_abs_change_r[sub_temp01291011] gt 1.6d0*ratio)
   mask0[sub_temp01291011] or= mask_temp
   mask1[sub_temp01291011] or= mask_temp
   mask2[sub_temp01291011] or= mask_temp
   mask3[sub_temp01291011] or= mask_temp
   mask4[sub_temp01291011] or= mask_temp
endif
if (n_temp012101112 gt 0) then begin
   mean_abs_change_l = (abs(d_data0) + abs(d_data1) + abs(d_data2))/3d
   mean_abs_change_r = (abs(d_data10) + abs(d_data11) + abs(d_data12))/3d
           sign_temp = (d_data3+d_data4+d_data5+d_data6+d_data7+d_data8)*d_data9
           mask_temp = (sign_temp[sub_temp012101112] lt 0) and $
                       (abs(d_data3[sub_temp012101112])/mean_abs_change_l[sub_temp012101112] gt 1.7d0*ratio) and $
                       (abs(d_data9[sub_temp012101112])/mean_abs_change_r[sub_temp012101112] gt 1.7d0*ratio)
   mask0[sub_temp012101112] or= mask_temp
   mask1[sub_temp012101112] or= mask_temp
   mask2[sub_temp012101112] or= mask_temp
   mask3[sub_temp012101112] or= mask_temp
   mask4[sub_temp012101112] or= mask_temp
   mask5[sub_temp012101112] or= mask_temp
endif
if (n_temp012111213 gt 0) then begin
   mean_abs_change_l = (abs(d_data0) + abs(d_data1) + abs(d_data2))/3d
   mean_abs_change_r = (abs(d_data11) + abs(d_data12) + abs(d_data13))/3d
           sign_temp = (d_data3+d_data4+d_data5+d_data6+d_data7+d_data8+d_data9)*d_data10
           mask_temp = (sign_temp[sub_temp012111213] lt 0) and $
                       (abs(d_data3[sub_temp012111213])/mean_abs_change_l[sub_temp012111213]  gt 1.8d0*ratio) and $
                       (abs(d_data10[sub_temp012111213])/mean_abs_change_r[sub_temp012111213] gt 1.8d0*ratio)
   mask0[sub_temp012111213] or= mask_temp
   mask1[sub_temp012111213] or= mask_temp
   mask2[sub_temp012111213] or= mask_temp
   mask3[sub_temp012111213] or= mask_temp
   mask4[sub_temp012111213] or= mask_temp
   mask5[sub_temp012111213] or= mask_temp
   mask6[sub_temp012111213] or= mask_temp
endif
if (n_temp012121314 gt 0) then begin
   mean_abs_change_l = (abs(d_data0) + abs(d_data1) + abs(d_data2))/3d
   mean_abs_change_r = (abs(d_data12) + abs(d_data13) + abs(d_data14))/3d
           sign_temp = (d_data3+d_data4+d_data5+d_data6+d_data7+d_data8+d_data9+d_data10)*d_data11
           mask_temp = (sign_temp[sub_temp012121314] lt 0) and $
                       (abs(d_data3[sub_temp012121314])/mean_abs_change_l[sub_temp012121314]  gt 1.9d0*ratio) and $
                       (abs(d_data11[sub_temp012121314])/mean_abs_change_r[sub_temp012121314] gt 1.9d0*ratio)
   mask0[sub_temp012121314] or= mask_temp
   mask1[sub_temp012121314] or= mask_temp
   mask2[sub_temp012121314] or= mask_temp
   mask3[sub_temp012121314] or= mask_temp
   mask4[sub_temp012121314] or= mask_temp
   mask5[sub_temp012121314] or= mask_temp
   mask6[sub_temp012121314] or= mask_temp
   mask7[sub_temp012121314] or= mask_temp
endif
if (n_temp012131415 gt 0) then begin
   mean_abs_change_l = (abs(d_data0) + abs(d_data1) + abs(d_data2))/3d
   mean_abs_change_r = (abs(d_data13) + abs(d_data14) + abs(d_data15))/3d
           sign_temp = (d_data3+d_data4+d_data5+d_data6+d_data7+d_data8+d_data9+d_data10+d_data11)*d_data12
           mask_temp = (sign_temp[sub_temp012131415] lt 0) and $
                       (abs(d_data3[sub_temp012131415])/mean_abs_change_l[sub_temp012131415]  gt 2.0d0*ratio) and $
                       (abs(d_data12[sub_temp012131415])/mean_abs_change_r[sub_temp012131415] gt 2.0d0*ratio)
   mask0[sub_temp012131415] or= mask_temp
   mask1[sub_temp012131415] or= mask_temp
   mask2[sub_temp012131415] or= mask_temp
   mask3[sub_temp012131415] or= mask_temp
   mask4[sub_temp012131415] or= mask_temp
   mask5[sub_temp012131415] or= mask_temp
   mask6[sub_temp012131415] or= mask_temp
   mask7[sub_temp012131415] or= mask_temp
   mask8[sub_temp012131415] or= mask_temp
endif
if (n_temp012141516 gt 0) then begin
   mean_abs_change_l = (abs(d_data0) + abs(d_data1) + abs(d_data2))/3d
   mean_abs_change_r = (abs(d_data14) + abs(d_data15) + abs(d_data16))/3d
           sign_temp = (d_data3+d_data4+d_data5+d_data6+d_data7+d_data8+d_data9+d_data10+d_data11+$
                        d_data12)*d_data13
           mask_temp = (sign_temp[sub_temp012141516] lt 0) and $
                       (abs(d_data3[sub_temp012141516])/mean_abs_change_l[sub_temp012141516]  gt 2.1d0*ratio) and $
                       (abs(d_data13[sub_temp012141516])/mean_abs_change_r[sub_temp012141516] gt 2.1d0*ratio)
   mask0[sub_temp012141516] or= mask_temp
   mask1[sub_temp012141516] or= mask_temp
   mask2[sub_temp012141516] or= mask_temp
   mask3[sub_temp012141516] or= mask_temp
   mask4[sub_temp012141516] or= mask_temp
   mask5[sub_temp012141516] or= mask_temp
   mask6[sub_temp012141516] or= mask_temp
   mask7[sub_temp012141516] or= mask_temp
   mask8[sub_temp012141516] or= mask_temp
   mask9[sub_temp012141516] or= mask_temp
endif
if (n_temp012151617 gt 0) then begin
   mean_abs_change_l = (abs(d_data0) + abs(d_data1) + abs(d_data2))/3d
   mean_abs_change_r = (abs(d_data15) + abs(d_data16) + abs(d_data17))/3d
           sign_temp = (d_data3+d_data4+d_data5+d_data6+d_data7+d_data8+d_data9+d_data10+d_data11+$
                        d_data12+d_data13)*d_data14
           mask_temp = (sign_temp[sub_temp012151617] lt 0) and $
                       (abs(d_data3[sub_temp012151617])/mean_abs_change_l[sub_temp012151617]  gt 2.2d0*ratio) and $
                       (abs(d_data14[sub_temp012151617])/mean_abs_change_r[sub_temp012151617] gt 2.2d0*ratio)
   mask0[sub_temp012151617] or= mask_temp
   mask1[sub_temp012151617] or= mask_temp
   mask2[sub_temp012151617] or= mask_temp
   mask3[sub_temp012151617] or= mask_temp
   mask4[sub_temp012151617] or= mask_temp
   mask5[sub_temp012151617] or= mask_temp
   mask6[sub_temp012151617] or= mask_temp
   mask7[sub_temp012151617] or= mask_temp
   mask8[sub_temp012151617] or= mask_temp
   mask9[sub_temp012151617] or= mask_temp
   mask10[sub_temp012151617] or= mask_temp
endif
if (n_temp012161718 gt 0) then begin
   mean_abs_change_l = (abs(d_data0) + abs(d_data1) + abs(d_data2))/3d
   mean_abs_change_r = (abs(d_data16) + abs(d_data17) + abs(d_data18))/3d
           sign_temp = (d_data3+d_data4+d_data5+d_data6+d_data7+d_data8+d_data9+d_data10+d_data11+$
                        d_data12+d_data13+d_data14)*d_data15
           mask_temp = (sign_temp[sub_temp012161718] lt 0) and $
                       (abs(d_data3[sub_temp012161718])/mean_abs_change_l[sub_temp012161718]  gt 2.3d0*ratio) and $
                       (abs(d_data15[sub_temp012161718])/mean_abs_change_r[sub_temp012161718] gt 2.3d0*ratio)
   mask0[sub_temp012161718] or= mask_temp
   mask1[sub_temp012161718] or= mask_temp
   mask2[sub_temp012161718] or= mask_temp
   mask3[sub_temp012161718] or= mask_temp
   mask4[sub_temp012161718] or= mask_temp
   mask5[sub_temp012161718] or= mask_temp
   mask6[sub_temp012161718] or= mask_temp
   mask7[sub_temp012161718] or= mask_temp
   mask8[sub_temp012161718] or= mask_temp
   mask9[sub_temp012161718] or= mask_temp
   mask10[sub_temp012161718] or= mask_temp
   mask11[sub_temp012161718] or= mask_temp
endif
if (n_temp012171819 gt 0) then begin
   mean_abs_change_l = (abs(d_data0) + abs(d_data1) + abs(d_data2))/3d
   mean_abs_change_r = (abs(d_data17) + abs(d_data18) + abs(d_data19))/3d
           sign_temp = (d_data3+d_data4+d_data5+d_data6+d_data7+d_data8+d_data9+d_data10+d_data11+$
                        d_data12+d_data13+d_data14+d_data15)*d_data16
           mask_temp = (sign_temp[sub_temp012171819] lt 0) and $
                       (abs(d_data3[sub_temp012171819])/mean_abs_change_l[sub_temp012171819]  gt 2.4d0*ratio) and $
                       (abs(d_data16[sub_temp012171819])/mean_abs_change_r[sub_temp012171819] gt 2.4d0*ratio)
   mask0[sub_temp012171819] or= mask_temp
   mask1[sub_temp012171819] or= mask_temp
   mask2[sub_temp012171819] or= mask_temp
   mask3[sub_temp012171819] or= mask_temp
   mask4[sub_temp012171819] or= mask_temp
   mask5[sub_temp012171819] or= mask_temp
   mask6[sub_temp012171819] or= mask_temp
   mask7[sub_temp012171819] or= mask_temp
   mask8[sub_temp012171819] or= mask_temp
   mask9[sub_temp012171819] or= mask_temp
   mask10[sub_temp012171819] or= mask_temp
   mask11[sub_temp012171819] or= mask_temp
   mask12[sub_temp012171819] or= mask_temp
endif
if (n_temp012181920 gt 0) then begin
   mean_abs_change_l = (abs(d_data0) + abs(d_data1) + abs(d_data2))/3d
   mean_abs_change_r = (abs(d_data18) + abs(d_data19) + abs(d_data20))/3d
           sign_temp = (d_data3+d_data4+d_data5+d_data6+d_data7+d_data8+d_data9+d_data10+d_data11+$
                        d_data12+d_data13+d_data14+d_data15+d_data16)*d_data17
           mask_temp = (sign_temp[sub_temp012181920] lt 0) and $
                       (abs(d_data3[sub_temp012181920])/mean_abs_change_l[sub_temp012181920]  gt 2.5d0*ratio) and $
                       (abs(d_data17[sub_temp012181920])/mean_abs_change_r[sub_temp012181920] gt 2.5d0*ratio)
   mask0[sub_temp012181920] or= mask_temp
   mask1[sub_temp012181920] or= mask_temp
   mask2[sub_temp012181920] or= mask_temp
   mask3[sub_temp012181920] or= mask_temp
   mask4[sub_temp012181920] or= mask_temp
   mask5[sub_temp012181920] or= mask_temp
   mask6[sub_temp012181920] or= mask_temp
   mask7[sub_temp012181920] or= mask_temp
   mask8[sub_temp012181920] or= mask_temp
   mask9[sub_temp012181920] or= mask_temp
   mask10[sub_temp012181920] or= mask_temp
   mask11[sub_temp012181920] or= mask_temp
   mask12[sub_temp012181920] or= mask_temp
   mask13[sub_temp012181920] or= mask_temp
endif
if (n_temp012192021 gt 0) then begin
   mean_abs_change_l = (abs(d_data0) + abs(d_data1) + abs(d_data2))/3d
   mean_abs_change_r = (abs(d_data19) + abs(d_data20) + abs(d_data21))/3d
           sign_temp = (d_data3+d_data4+d_data5+d_data6+d_data7+d_data8+d_data9+d_data10+d_data11+$
                        d_data12+d_data13+d_data14+d_data15+d_data16+d_data17)*d_data18
           mask_temp = (sign_temp[sub_temp012192021] lt 0) and $
                       (abs(d_data3[sub_temp012192021])/mean_abs_change_l[sub_temp012192021]  gt 2.6d0*ratio) and $
                       (abs(d_data18[sub_temp012192021])/mean_abs_change_r[sub_temp012192021] gt 2.6d0*ratio)
   mask0[sub_temp012192021] or= mask_temp
   mask1[sub_temp012192021] or= mask_temp
   mask2[sub_temp012192021] or= mask_temp
   mask3[sub_temp012192021] or= mask_temp
   mask4[sub_temp012192021] or= mask_temp
   mask5[sub_temp012192021] or= mask_temp
   mask6[sub_temp012192021] or= mask_temp
   mask7[sub_temp012192021] or= mask_temp
   mask8[sub_temp012192021] or= mask_temp
   mask9[sub_temp012192021] or= mask_temp
   mask10[sub_temp012192021] or= mask_temp
   mask11[sub_temp012192021] or= mask_temp
   mask12[sub_temp012192021] or= mask_temp
   mask13[sub_temp012192021] or= mask_temp
   mask14[sub_temp012192021] or= mask_temp
endif

;- mask                          
mask   = [0, 0, 0, 0, mask0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
mask or= [0, 0, 0, 0, 0, mask1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
mask or= [0, 0, 0, 0, 0, 0, mask2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
mask or= [0, 0, 0, 0, 0, 0, 0, mask3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
mask or= [0, 0, 0, 0, 0, 0, 0, 0, mask4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
mask or= [0, 0, 0, 0, 0, 0, 0, 0, 0, mask5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
mask or= [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, mask6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
mask or= [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, mask7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
mask or= [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, mask8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
mask or= [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, mask9, 0, 0, 0, 0, 0, 0, 0, 0, 0]
mask or= [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, mask10, 0, 0, 0, 0, 0, 0, 0, 0]
mask or= [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, mask11, 0, 0, 0, 0, 0, 0, 0]
mask or= [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, mask12, 0, 0, 0, 0, 0, 0]
mask or= [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, mask13, 0, 0, 0, 0, 0]
mask or= [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, mask14, 0, 0, 0, 0]

;- check first 4 and last 18 points
sub_data_start   = lindgen(4)
sub_data_end0    = lindgen(6) + (n_data-18L)
sub_data_end1    = lindgen(6) + (n_data-12L)
sub_data_end2    = lindgen(6) + (n_data- 6L)
moments_start    = moment(data[sub_data_start])
moments_end0     = moment(data[sub_data_end0])
moments_end1     = moment(data[sub_data_end1])
moments_end2     = moment(data[sub_data_end2])
sub_glitch_start = where(abs(data[sub_data_start] - moments_start[0]) gt 2d0*sqrt(moments_start[1]), n_glitch_start)
sub_glitch_end0  = where(abs(data[sub_data_end0]  - moments_end0[0])  gt 2d0*sqrt(moments_end0[1]),  n_glitch_end0)
sub_glitch_end1  = where(abs(data[sub_data_end1]  - moments_end1[0])  gt 2d0*sqrt(moments_end1[1]),  n_glitch_end1)
sub_glitch_end2  = where(abs(data[sub_data_end2]  - moments_end2[0])  gt 2d0*sqrt(moments_end2[1]),  n_glitch_end2)
if (n_glitch_start gt 0) then mask[sub_data_start[sub_glitch_start]] = 1
if (n_glitch_end0  gt 0) then mask[sub_data_end0[sub_glitch_end0]]   = 1
if (n_glitch_end1  gt 0) then mask[sub_data_end1[sub_glitch_end1]]   = 1
if (n_glitch_end2  gt 0) then mask[sub_data_end2[sub_glitch_end2]]   = 1

;- return mask
return, mask

END