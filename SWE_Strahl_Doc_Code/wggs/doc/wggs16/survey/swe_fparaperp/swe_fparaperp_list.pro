;============================== SWE_fParaPerp_List ===========================
; Defines the list of variable names, 'list', as an output parameter.
; Note: See MPH notes for details on the pitch-angle binning/averaging process.
PRO swe_fparaperp_list,list ;                   (MPH, Last modified: 07/20/04).

list = $ ;              Output parameter, used to identify time-series by name.

                                ; Phase-density, averaged over ALL detectors...
   ['f_omni [38-1005 eV, x2]',$ ;   Energies: [38, 77, 116, 290, 580, 1005] eV.
    'f_omni [58-1237 eV, x2]',$ ;             [58, 96, 193, 425, 773, 1237] eV.
    'f_omni [38-1005 eV, x3]',$ ;             [38,   96,  290,  580,  1005] eV.
    'f_omni [58-1237 eV, x3]',$ ;             [58,  116,  425,  773,  1237] eV.
    'f_omni [77-1237 eV, x3]',$ ;             [77,    193,    580,    1237] eV.

                                ; Phase-dens., avg. over PARALLEL HEMISPHERE...
    'f_para [38-1005 eV, x2]',$ ;   Energies: [38, 77, 116, 290, 580, 1005] eV.
    'f_para [58-1237 eV, x2]',$ ;             [58, 96, 193, 425, 773, 1237] eV.
    'f_para [38-1005 eV, x3]',$ ;             [38,   96,  290,  580,  1005] eV.
    'f_para [58-1237 eV, x3]',$ ;             [58,  116,  425,  773,  1237] eV.
    'f_para [77-1237 eV, x3]',$ ;             [77,    193,    580,    1237] eV.

                                ; Phase-dens., avg. over QUASI-PERPEND. dets...
    'f_perp [38-1005 eV, x2]',$ ;   Energies: [38, 77, 116, 290, 580, 1005] eV.
    'f_perp [58-1237 eV, x2]',$ ;             [58, 96, 193, 425, 773, 1237] eV.
    'f_perp [38-1005 eV, x3]',$ ;             [38,   96,  290,  580,  1005] eV.
    'f_perp [58-1237 eV, x3]',$ ;             [58,  116,  425,  773,  1237] eV.
    'f_perp [77-1237 eV, x3]',$ ;             [77,    193,    580,    1237] eV.

                                ; Phase-dens., avg. for ANTI-PARA HEMISPHERE...
    'f_anti [38-1005 eV, x2]',$ ;   Energies: [38, 77, 116, 290, 580, 1005] eV.
    'f_anti [58-1237 eV, x2]',$ ;             [58, 96, 193, 425, 773, 1237] eV.
    'f_anti [38-1005 eV, x3]',$ ;             [38,   96,  290,  580,  1005] eV.
    'f_anti [58-1237 eV, x3]',$ ;             [58,  116,  425,  773,  1237] eV.
    'f_anti [77-1237 eV, x3]']  ;             [77,    193,    580,    1237] eV.

end
