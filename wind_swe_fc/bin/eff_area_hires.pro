; IDL Funtion: eff_area_hires.pro
; SCCS Info
; 
;           %Z%%M%   VERSION %I%    %G%  %U% 
;



FUNCTION eff_area_hires, vel_vec, vb_norm, v_grid, GRID_ONLY=GRID_ONLY, INTERPOL=INTERPOL 


;---------------------------------------------------------------------------
; Declare COMMON blocks
;----------------------------------------------------------------------------

   COMMON cup_effarea, co_wovrs, co_area_cm2 

   COMMON geometric, co_pi

;-----------------------------------------------------------------------------
; Create variable with angualar response 
;-----------------------------------------------------------------------------

; effective area array
eff_area = dindgen(910)

eff_area[  0]=3.3820000e+06 & eff_area[  1]=3.3821000e+06 & eff_area[  2]=3.3822000e+06
eff_area[  3]=3.3823000e+06 & eff_area[  4]=3.3824000e+06 & eff_area[  5]=3.3825000e+06
eff_area[  6]=3.3826000e+06 & eff_area[  7]=3.3827000e+06 & eff_area[  8]=3.3828000e+06
eff_area[  9]=3.3829000e+06 & eff_area[ 10]=3.3830000e+06 & eff_area[ 11]=3.3830000e+06
eff_area[ 12]=3.3830000e+06 & eff_area[ 13]=3.3830000e+06 & eff_area[ 14]=3.3830000e+06
eff_area[ 15]=3.3830000e+06 & eff_area[ 16]=3.3830000e+06 & eff_area[ 17]=3.3830000e+06
eff_area[ 18]=3.3830000e+06 & eff_area[ 19]=3.3830000e+06 & eff_area[ 20]=3.3830000e+06
eff_area[ 21]=3.3829000e+06 & eff_area[ 22]=3.3828000e+06 & eff_area[ 23]=3.3827000e+06
eff_area[ 24]=3.3826000e+06 & eff_area[ 25]=3.3825000e+06 & eff_area[ 26]=3.3824000e+06
eff_area[ 27]=3.3823000e+06 & eff_area[ 28]=3.3822000e+06 & eff_area[ 29]=3.3821000e+06
eff_area[ 30]=3.3820000e+06 & eff_area[ 31]=3.3819000e+06 & eff_area[ 32]=3.3818000e+06
eff_area[ 33]=3.3817000e+06 & eff_area[ 34]=3.3816000e+06 & eff_area[ 35]=3.3815000e+06
eff_area[ 36]=3.3814000e+06 & eff_area[ 37]=3.3813000e+06 & eff_area[ 38]=3.3812000e+06
eff_area[ 39]=3.3811000e+06 & eff_area[ 40]=3.3810000e+06 & eff_area[ 41]=3.3809000e+06
eff_area[ 42]=3.3808000e+06 & eff_area[ 43]=3.3807000e+06 & eff_area[ 44]=3.3806000e+06
eff_area[ 45]=3.3805000e+06 & eff_area[ 46]=3.3804000e+06 & eff_area[ 47]=3.3803000e+06
eff_area[ 48]=3.3802000e+06 & eff_area[ 49]=3.3801000e+06 & eff_area[ 50]=3.3800000e+06
eff_area[ 51]=3.3798000e+06 & eff_area[ 52]=3.3796000e+06 & eff_area[ 53]=3.3794000e+06
eff_area[ 54]=3.3792000e+06 & eff_area[ 55]=3.3790000e+06 & eff_area[ 56]=3.3788000e+06
eff_area[ 57]=3.3786000e+06 & eff_area[ 58]=3.3784000e+06 & eff_area[ 59]=3.3782000e+06
eff_area[ 60]=3.3780000e+06 & eff_area[ 61]=3.3779000e+06 & eff_area[ 62]=3.3778000e+06
eff_area[ 63]=3.3777000e+06 & eff_area[ 64]=3.3776000e+06 & eff_area[ 65]=3.3775000e+06
eff_area[ 66]=3.3774000e+06 & eff_area[ 67]=3.3773000e+06 & eff_area[ 68]=3.3772000e+06
eff_area[ 69]=3.3771000e+06 & eff_area[ 70]=3.3770000e+06 & eff_area[ 71]=3.3769000e+06
eff_area[ 72]=3.3768000e+06 & eff_area[ 73]=3.3767000e+06 & eff_area[ 74]=3.3766000e+06
eff_area[ 75]=3.3765000e+06 & eff_area[ 76]=3.3764000e+06 & eff_area[ 77]=3.3763000e+06
eff_area[ 78]=3.3762000e+06 & eff_area[ 79]=3.3761000e+06 & eff_area[ 80]=3.3760000e+06
eff_area[ 81]=3.3758000e+06 & eff_area[ 82]=3.3756000e+06 & eff_area[ 83]=3.3754000e+06
eff_area[ 84]=3.3752000e+06 & eff_area[ 85]=3.3750000e+06 & eff_area[ 86]=3.3748000e+06
eff_area[ 87]=3.3746000e+06 & eff_area[ 88]=3.3744000e+06 & eff_area[ 89]=3.3742000e+06
eff_area[ 90]=3.3740000e+06 & eff_area[ 91]=3.3738000e+06 & eff_area[ 92]=3.3736000e+06
eff_area[ 93]=3.3734000e+06 & eff_area[ 94]=3.3732000e+06 & eff_area[ 95]=3.3730000e+06
eff_area[ 96]=3.3728000e+06 & eff_area[ 97]=3.3726000e+06 & eff_area[ 98]=3.3724000e+06
eff_area[ 99]=3.3722000e+06 & eff_area[100]=3.3720000e+06 & eff_area[101]=3.3717000e+06
eff_area[102]=3.3714000e+06 & eff_area[103]=3.3711000e+06 & eff_area[104]=3.3708000e+06
eff_area[105]=3.3705000e+06 & eff_area[106]=3.3702000e+06 & eff_area[107]=3.3699000e+06
eff_area[108]=3.3696000e+06 & eff_area[109]=3.3693000e+06 & eff_area[110]=3.3690000e+06
eff_area[111]=3.3689000e+06 & eff_area[112]=3.3688000e+06 & eff_area[113]=3.3687000e+06
eff_area[114]=3.3686000e+06 & eff_area[115]=3.3685000e+06 & eff_area[116]=3.3684000e+06
eff_area[117]=3.3683000e+06 & eff_area[118]=3.3682000e+06 & eff_area[119]=3.3681000e+06
eff_area[120]=3.3680000e+06 & eff_area[121]=3.3676000e+06 & eff_area[122]=3.3672000e+06
eff_area[123]=3.3668000e+06 & eff_area[124]=3.3664000e+06 & eff_area[125]=3.3660000e+06
eff_area[126]=3.3656000e+06 & eff_area[127]=3.3652000e+06 & eff_area[128]=3.3648000e+06
eff_area[129]=3.3644000e+06 & eff_area[130]=3.3640000e+06 & eff_area[131]=3.3638000e+06
eff_area[132]=3.3636000e+06 & eff_area[133]=3.3634000e+06 & eff_area[134]=3.3632000e+06
eff_area[135]=3.3630000e+06 & eff_area[136]=3.3628000e+06 & eff_area[137]=3.3626000e+06
eff_area[138]=3.3624000e+06 & eff_area[139]=3.3622000e+06 & eff_area[140]=3.3620000e+06
eff_area[141]=3.3617000e+06 & eff_area[142]=3.3614000e+06 & eff_area[143]=3.3611000e+06
eff_area[144]=3.3608000e+06 & eff_area[145]=3.3605000e+06 & eff_area[146]=3.3602000e+06
eff_area[147]=3.3599000e+06 & eff_area[148]=3.3596000e+06 & eff_area[149]=3.3593000e+06
eff_area[150]=3.3590000e+06 & eff_area[151]=3.3586000e+06 & eff_area[152]=3.3582000e+06
eff_area[153]=3.3578000e+06 & eff_area[154]=3.3574000e+06 & eff_area[155]=3.3570000e+06
eff_area[156]=3.3566000e+06 & eff_area[157]=3.3562000e+06 & eff_area[158]=3.3558000e+06
eff_area[159]=3.3554000e+06 & eff_area[160]=3.3550000e+06 & eff_area[161]=3.3546000e+06
eff_area[162]=3.3542000e+06 & eff_area[163]=3.3538000e+06 & eff_area[164]=3.3534000e+06
eff_area[165]=3.3530000e+06 & eff_area[166]=3.3526000e+06 & eff_area[167]=3.3522000e+06
eff_area[168]=3.3518000e+06 & eff_area[169]=3.3514000e+06 & eff_area[170]=3.3510000e+06
eff_area[171]=3.3506000e+06 & eff_area[172]=3.3502000e+06 & eff_area[173]=3.3498000e+06
eff_area[174]=3.3494000e+06 & eff_area[175]=3.3490000e+06 & eff_area[176]=3.3486000e+06
eff_area[177]=3.3482000e+06 & eff_area[178]=3.3478000e+06 & eff_area[179]=3.3474000e+06
eff_area[180]=3.3470000e+06 & eff_area[181]=3.3466000e+06 & eff_area[182]=3.3462000e+06
eff_area[183]=3.3458000e+06 & eff_area[184]=3.3454000e+06 & eff_area[185]=3.3450000e+06
eff_area[186]=3.3446000e+06 & eff_area[187]=3.3442000e+06 & eff_area[188]=3.3438000e+06
eff_area[189]=3.3434000e+06 & eff_area[190]=3.3430000e+06 & eff_area[191]=3.3425700e+06
eff_area[192]=3.3421400e+06 & eff_area[193]=3.3417100e+06 & eff_area[194]=3.3412800e+06
eff_area[195]=3.3408500e+06 & eff_area[196]=3.3404200e+06 & eff_area[197]=3.3399900e+06
eff_area[198]=3.3395600e+06 & eff_area[199]=3.3391300e+06 & eff_area[200]=3.3387000e+06
eff_area[201]=3.3382400e+06 & eff_area[202]=3.3377800e+06 & eff_area[203]=3.3373200e+06
eff_area[204]=3.3368600e+06 & eff_area[205]=3.3364000e+06 & eff_area[206]=3.3359400e+06
eff_area[207]=3.3354800e+06 & eff_area[208]=3.3350200e+06 & eff_area[209]=3.3345600e+06
eff_area[210]=3.3341000e+06 & eff_area[211]=3.3336200e+06 & eff_area[212]=3.3331400e+06
eff_area[213]=3.3326600e+06 & eff_area[214]=3.3321800e+06 & eff_area[215]=3.3317000e+06
eff_area[216]=3.3312200e+06 & eff_area[217]=3.3307400e+06 & eff_area[218]=3.3302600e+06
eff_area[219]=3.3297800e+06 & eff_area[220]=3.3293000e+06 & eff_area[221]=3.3288000e+06
eff_area[222]=3.3283000e+06 & eff_area[223]=3.3278000e+06 & eff_area[224]=3.3273000e+06
eff_area[225]=3.3268000e+06 & eff_area[226]=3.3263000e+06 & eff_area[227]=3.3258000e+06
eff_area[228]=3.3253000e+06 & eff_area[229]=3.3248000e+06 & eff_area[230]=3.3243000e+06
eff_area[231]=3.3236900e+06 & eff_area[232]=3.3230800e+06 & eff_area[233]=3.3224700e+06
eff_area[234]=3.3218600e+06 & eff_area[235]=3.3212500e+06 & eff_area[236]=3.3206400e+06
eff_area[237]=3.3200300e+06 & eff_area[238]=3.3194200e+06 & eff_area[239]=3.3188100e+06
eff_area[240]=3.3182000e+06 & eff_area[241]=3.3176600e+06 & eff_area[242]=3.3171200e+06
eff_area[243]=3.3165800e+06 & eff_area[244]=3.3160400e+06 & eff_area[245]=3.3155000e+06
eff_area[246]=3.3149600e+06 & eff_area[247]=3.3144200e+06 & eff_area[248]=3.3138800e+06
eff_area[249]=3.3133400e+06 & eff_area[250]=3.3128000e+06 & eff_area[251]=3.3121500e+06
eff_area[252]=3.3115000e+06 & eff_area[253]=3.3108500e+06 & eff_area[254]=3.3102000e+06
eff_area[255]=3.3095500e+06 & eff_area[256]=3.3089000e+06 & eff_area[257]=3.3082500e+06
eff_area[258]=3.3076000e+06 & eff_area[259]=3.3069500e+06 & eff_area[260]=3.3063000e+06
eff_area[261]=3.3056300e+06 & eff_area[262]=3.3049600e+06 & eff_area[263]=3.3042900e+06
eff_area[264]=3.3036200e+06 & eff_area[265]=3.3029500e+06 & eff_area[266]=3.3022800e+06
eff_area[267]=3.3016100e+06 & eff_area[268]=3.3009400e+06 & eff_area[269]=3.3002700e+06
eff_area[270]=3.2996000e+06 & eff_area[271]=3.2989200e+06 & eff_area[272]=3.2982400e+06
eff_area[273]=3.2975600e+06 & eff_area[274]=3.2968800e+06 & eff_area[275]=3.2962000e+06
eff_area[276]=3.2955200e+06 & eff_area[277]=3.2948400e+06 & eff_area[278]=3.2941600e+06
eff_area[279]=3.2934800e+06 & eff_area[280]=3.2928000e+06 & eff_area[281]=3.2921100e+06
eff_area[282]=3.2914200e+06 & eff_area[283]=3.2907300e+06 & eff_area[284]=3.2900400e+06
eff_area[285]=3.2893500e+06 & eff_area[286]=3.2886600e+06 & eff_area[287]=3.2879700e+06
eff_area[288]=3.2872800e+06 & eff_area[289]=3.2865900e+06 & eff_area[290]=3.2859000e+06
eff_area[291]=3.2850900e+06 & eff_area[292]=3.2842800e+06 & eff_area[293]=3.2834700e+06
eff_area[294]=3.2826600e+06 & eff_area[295]=3.2818500e+06 & eff_area[296]=3.2810400e+06
eff_area[297]=3.2802300e+06 & eff_area[298]=3.2794200e+06 & eff_area[299]=3.2786100e+06
eff_area[300]=3.2778000e+06 & eff_area[301]=3.2770900e+06 & eff_area[302]=3.2763800e+06
eff_area[303]=3.2756700e+06 & eff_area[304]=3.2749600e+06 & eff_area[305]=3.2742500e+06
eff_area[306]=3.2735400e+06 & eff_area[307]=3.2728300e+06 & eff_area[308]=3.2721200e+06
eff_area[309]=3.2714100e+06 & eff_area[310]=3.2707000e+06 & eff_area[311]=3.2697900e+06
eff_area[312]=3.2688800e+06 & eff_area[313]=3.2679700e+06 & eff_area[314]=3.2670600e+06
eff_area[315]=3.2661500e+06 & eff_area[316]=3.2652400e+06 & eff_area[317]=3.2643300e+06
eff_area[318]=3.2634200e+06 & eff_area[319]=3.2625100e+06 & eff_area[320]=3.2616000e+06
eff_area[321]=3.2607900e+06 & eff_area[322]=3.2599800e+06 & eff_area[323]=3.2591700e+06
eff_area[324]=3.2583600e+06 & eff_area[325]=3.2575500e+06 & eff_area[326]=3.2567400e+06
eff_area[327]=3.2559300e+06 & eff_area[328]=3.2551200e+06 & eff_area[329]=3.2543100e+06
eff_area[330]=3.2535000e+06 & eff_area[331]=3.2526000e+06 & eff_area[332]=3.2517000e+06
eff_area[333]=3.2508000e+06 & eff_area[334]=3.2499000e+06 & eff_area[335]=3.2490000e+06
eff_area[336]=3.2481000e+06 & eff_area[337]=3.2472000e+06 & eff_area[338]=3.2463000e+06
eff_area[339]=3.2454000e+06 & eff_area[340]=3.2445000e+06 & eff_area[341]=3.2435100e+06
eff_area[342]=3.2425200e+06 & eff_area[343]=3.2415300e+06 & eff_area[344]=3.2405400e+06
eff_area[345]=3.2395500e+06 & eff_area[346]=3.2385600e+06 & eff_area[347]=3.2375700e+06
eff_area[348]=3.2365800e+06 & eff_area[349]=3.2355900e+06 & eff_area[350]=3.2346000e+06
eff_area[351]=3.2336300e+06 & eff_area[352]=3.2326600e+06 & eff_area[353]=3.2316900e+06
eff_area[354]=3.2307200e+06 & eff_area[355]=3.2297500e+06 & eff_area[356]=3.2287800e+06
eff_area[357]=3.2278100e+06 & eff_area[358]=3.2268400e+06 & eff_area[359]=3.2258700e+06
eff_area[360]=3.2249000e+06 & eff_area[361]=3.2224200e+06 & eff_area[362]=3.2199400e+06
eff_area[363]=3.2174600e+06 & eff_area[364]=3.2149800e+06 & eff_area[365]=3.2125000e+06
eff_area[366]=3.2100200e+06 & eff_area[367]=3.2075400e+06 & eff_area[368]=3.2050600e+06
eff_area[369]=3.2025800e+06 & eff_area[370]=3.2001000e+06 & eff_area[371]=3.1962400e+06
eff_area[372]=3.1923800e+06 & eff_area[373]=3.1885200e+06 & eff_area[374]=3.1846600e+06
eff_area[375]=3.1808000e+06 & eff_area[376]=3.1769400e+06 & eff_area[377]=3.1730800e+06
eff_area[378]=3.1692200e+06 & eff_area[379]=3.1653600e+06 & eff_area[380]=3.1615000e+06
eff_area[381]=3.1567500e+06 & eff_area[382]=3.1520000e+06 & eff_area[383]=3.1472500e+06
eff_area[384]=3.1425000e+06 & eff_area[385]=3.1377500e+06 & eff_area[386]=3.1330000e+06
eff_area[387]=3.1282500e+06 & eff_area[388]=3.1235000e+06 & eff_area[389]=3.1187500e+06
eff_area[390]=3.1140000e+06 & eff_area[391]=3.1084820e+06 & eff_area[392]=3.1029640e+06
eff_area[393]=3.0974460e+06 & eff_area[394]=3.0919280e+06 & eff_area[395]=3.0864100e+06
eff_area[396]=3.0808920e+06 & eff_area[397]=3.0753740e+06 & eff_area[398]=3.0698560e+06
eff_area[399]=3.0643380e+06 & eff_area[400]=3.0588200e+06 & eff_area[401]=3.0526550e+06
eff_area[402]=3.0464900e+06 & eff_area[403]=3.0403250e+06 & eff_area[404]=3.0341600e+06
eff_area[405]=3.0279950e+06 & eff_area[406]=3.0218300e+06 & eff_area[407]=3.0156650e+06
eff_area[408]=3.0095000e+06 & eff_area[409]=3.0033350e+06 & eff_area[410]=2.9971700e+06
eff_area[411]=2.9904530e+06 & eff_area[412]=2.9837360e+06 & eff_area[413]=2.9770190e+06
eff_area[414]=2.9703020e+06 & eff_area[415]=2.9635850e+06 & eff_area[416]=2.9568680e+06
eff_area[417]=2.9501510e+06 & eff_area[418]=2.9434340e+06 & eff_area[419]=2.9367170e+06
eff_area[420]=2.9300000e+06 & eff_area[421]=2.9227000e+06 & eff_area[422]=2.9154000e+06
eff_area[423]=2.9081000e+06 & eff_area[424]=2.9008000e+06 & eff_area[425]=2.8935000e+06
eff_area[426]=2.8862000e+06 & eff_area[427]=2.8789000e+06 & eff_area[428]=2.8716000e+06
eff_area[429]=2.8643000e+06 & eff_area[430]=2.8570000e+06 & eff_area[431]=2.8492000e+06
eff_area[432]=2.8414000e+06 & eff_area[433]=2.8336000e+06 & eff_area[434]=2.8258000e+06
eff_area[435]=2.8180000e+06 & eff_area[436]=2.8102000e+06 & eff_area[437]=2.8024000e+06
eff_area[438]=2.7946000e+06 & eff_area[439]=2.7868000e+06 & eff_area[440]=2.7790000e+06
eff_area[441]=2.7705000e+06 & eff_area[442]=2.7620000e+06 & eff_area[443]=2.7535000e+06
eff_area[444]=2.7450000e+06 & eff_area[445]=2.7365000e+06 & eff_area[446]=2.7280000e+06
eff_area[447]=2.7195000e+06 & eff_area[448]=2.7110000e+06 & eff_area[449]=2.7025000e+06
eff_area[450]=2.6940000e+06 & eff_area[451]=2.6833000e+06 & eff_area[452]=2.6725999e+06
eff_area[453]=2.6618999e+06 & eff_area[454]=2.6511999e+06 & eff_area[455]=2.6404999e+06
eff_area[456]=2.6297998e+06 & eff_area[457]=2.6190998e+06 & eff_area[458]=2.6083998e+06
eff_area[459]=2.5976998e+06 & eff_area[460]=2.5869998e+06 & eff_area[461]=2.5747998e+06
eff_area[462]=2.5625998e+06 & eff_area[463]=2.5503998e+06 & eff_area[464]=2.5381999e+06
eff_area[465]=2.5259999e+06 & eff_area[466]=2.5137999e+06 & eff_area[467]=2.5015999e+06
eff_area[468]=2.4894000e+06 & eff_area[469]=2.4772000e+06 & eff_area[470]=2.4650000e+06
eff_area[471]=2.4514999e+06 & eff_area[472]=2.4379999e+06 & eff_area[473]=2.4244999e+06
eff_area[474]=2.4109998e+06 & eff_area[475]=2.3974998e+06 & eff_area[476]=2.3839997e+06
eff_area[477]=2.3704996e+06 & eff_area[478]=2.3569996e+06 & eff_area[479]=2.3434996e+06
eff_area[480]=2.3299995e+06 & eff_area[481]=2.3152995e+06 & eff_area[482]=2.3005996e+06
eff_area[483]=2.2858997e+06 & eff_area[484]=2.2711997e+06 & eff_area[485]=2.2564998e+06
eff_area[486]=2.2417998e+06 & eff_area[487]=2.2270998e+06 & eff_area[488]=2.2123999e+06
eff_area[489]=2.1977000e+06 & eff_area[490]=2.1830000e+06 & eff_area[491]=2.1673000e+06
eff_area[492]=2.1515999e+06 & eff_area[493]=2.1358999e+06 & eff_area[494]=2.1201999e+06
eff_area[495]=2.1044998e+06 & eff_area[496]=2.0887998e+06 & eff_area[497]=2.0730997e+06
eff_area[498]=2.0573997e+06 & eff_area[499]=2.0416997e+06 & eff_area[500]=2.0259996e+06
eff_area[501]=2.0092997e+06 & eff_area[502]=1.9925997e+06 & eff_area[503]=1.9758998e+06
eff_area[504]=1.9591998e+06 & eff_area[505]=1.9424999e+06 & eff_area[506]=1.9257999e+06
eff_area[507]=1.9091000e+06 & eff_area[508]=1.8924000e+06 & eff_area[509]=1.8757001e+06
eff_area[510]=1.8590001e+06 & eff_area[511]=1.8414001e+06 & eff_area[512]=1.8238000e+06
eff_area[513]=1.8062000e+06 & eff_area[514]=1.7885999e+06 & eff_area[515]=1.7709999e+06
eff_area[516]=1.7533998e+06 & eff_area[517]=1.7357998e+06 & eff_area[518]=1.7181997e+06
eff_area[519]=1.7005997e+06 & eff_area[520]=1.6829996e+06 & eff_area[521]=1.6643997e+06
eff_area[522]=1.6457997e+06 & eff_area[523]=1.6271998e+06 & eff_area[524]=1.6085998e+06
eff_area[525]=1.5899999e+06 & eff_area[526]=1.5713999e+06 & eff_area[527]=1.5528000e+06
eff_area[528]=1.5342000e+06 & eff_area[529]=1.5156001e+06 & eff_area[530]=1.4970001e+06
eff_area[531]=1.4775001e+06 & eff_area[532]=1.4580000e+06 & eff_area[533]=1.4385000e+06
eff_area[534]=1.4189999e+06 & eff_area[535]=1.3994999e+06 & eff_area[536]=1.3799998e+06
eff_area[537]=1.3604998e+06 & eff_area[538]=1.3409997e+06 & eff_area[539]=1.3214997e+06
eff_area[540]=1.3019996e+06 & eff_area[541]=1.2816997e+06 & eff_area[542]=1.2613997e+06
eff_area[543]=1.2410998e+06 & eff_area[544]=1.2207998e+06 & eff_area[545]=1.2004999e+06
eff_area[546]=1.1801999e+06 & eff_area[547]=1.1599000e+06 & eff_area[548]=1.1396000e+06
eff_area[549]=1.1193001e+06 & eff_area[550]=1.0990001e+06 & eff_area[551]=1.0778801e+06
eff_area[552]=1.0567600e+06 & eff_area[553]=1.0356400e+06 & eff_area[554]=1.0145199e+06
eff_area[555]=9.9339984e+05 & eff_area[556]=9.7227979e+05 & eff_area[557]=9.5115973e+05
eff_area[558]=9.3003968e+05 & eff_area[559]=9.0891962e+05 & eff_area[560]=8.8779956e+05
eff_area[561]=8.6586962e+05 & eff_area[562]=8.4393969e+05 & eff_area[563]=8.2200975e+05
eff_area[564]=8.0007981e+05 & eff_area[565]=7.7814988e+05 & eff_area[566]=7.5621994e+05
eff_area[567]=7.3429000e+05 & eff_area[568]=7.1236006e+05 & eff_area[569]=6.9043013e+05
eff_area[570]=6.6850019e+05 & eff_area[571]=6.4686013e+05 & eff_area[572]=6.2522007e+05
eff_area[573]=6.0358002e+05 & eff_area[574]=5.8193996e+05 & eff_area[575]=5.6029991e+05
eff_area[576]=5.3865985e+05 & eff_area[577]=5.1701979e+05 & eff_area[578]=4.9537974e+05
eff_area[579]=4.7373968e+05 & eff_area[580]=4.5209962e+05 & eff_area[581]=4.3263968e+05
eff_area[582]=4.1317973e+05 & eff_area[583]=3.9371978e+05 & eff_area[584]=3.7425984e+05
eff_area[585]=3.5479989e+05 & eff_area[586]=3.3533994e+05 & eff_area[587]=3.1588000e+05
eff_area[588]=2.9642005e+05 & eff_area[589]=2.7696010e+05 & eff_area[590]=2.5750016e+05
eff_area[591]=2.4143012e+05 & eff_area[592]=2.2536008e+05 & eff_area[593]=2.0929004e+05
eff_area[594]=1.9322001e+05 & eff_area[595]=1.7714997e+05 & eff_area[596]=1.6107993e+05
eff_area[597]=1.4500989e+05 & eff_area[598]=1.2893986e+05 & eff_area[599]=1.1286982e+05
eff_area[600]=9.6799781e+04 & eff_area[601]=8.7173800e+04 & eff_area[602]=7.7547819e+04
eff_area[603]=6.7921837e+04 & eff_area[604]=5.8295856e+04 & eff_area[605]=4.8669875e+04
eff_area[606]=3.9043894e+04 & eff_area[607]=2.9417912e+04 & eff_area[608]=1.9791931e+04
eff_area[609]=1.0165950e+04 & eff_area[610]=5.3996863e+02 & eff_area[611]=4.8597177e+02
eff_area[612]=4.3197490e+02 & eff_area[613]=3.7797804e+02 & eff_area[614]=3.2398118e+02
eff_area[615]=2.6998431e+02 & eff_area[616]=2.1598745e+02 & eff_area[617]=1.6199059e+02
eff_area[618]=1.0799373e+02 & eff_area[619]=5.3996863e+01 & eff_area[620]=0.0000000e+00
eff_area[621:909] = 0.0

eff_area_windsim = eff_area

; array of corresponding angles
   angle_effarea_deg        = dindgen(910)/10. 

;-----------------------------------------------------------------------------
; Do calculations 
;-----------------------------------------------------------------------------

; The original calculation of the normal angle always used the bulk
;	speed of the maxwellian distribution.  This is now changed to use instead
;	the cutoff velocity of the grid and the portion of the bulk speed 
;	parallel to the cup
;	   costheta  = vb_norm / norm(vel_vec) 

;  component of bulk speed parallel to cup

   vb_para   = SQRT( NORM(vel_vec)^2. - vb_norm^2. )
   
; angle to normal
   costheta = v_grid / SQRT( vb_para^2. + v_grid^2.)

; make sure angle is not too large (this limits it to 88 degrees)
   costheta  = costheta > replicate(co_wovrs, n_elements(costheta)) 


   IF keyword_set(GRID_ONLY) THEN BEGIN 

      calculated_eff_area  = 1d5 * co_area_cm2 *    $ 
               ( (1.d - co_wovrs) * ( 1.d - co_wovrs/costheta) )^9.d

   ENDIF

   IF keyword_set(INTERPOL) THEN BEGIN 

      angle_interpol_deg = ACOS(costheta) * 180./ co_pi

      calculated_eff_area = CALL_FUNCTION('interpol',  eff_area_windsim, $ 
                                            angle_effarea_deg,   $ 
                                            angle_interpol_deg  ) 

   ENDIF ELSE BEGIN 

;	multiply angle by 10 to get index number

      angle_round_deg = round( 10. * ACOS(costheta) * 180./ co_pi ) 

      calculated_eff_area =  eff_area_windsim( angle_round_deg ) 

             ; As eff_aera_windsim is calculated with a resolution 
             ; of one tenth of a degree this way eff_aera is taken at the 
             ; closest degree value. 
           
   ENDELSE 

   return, calculated_eff_area

END 










