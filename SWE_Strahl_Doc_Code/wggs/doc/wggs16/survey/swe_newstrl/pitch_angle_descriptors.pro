; ========================= Pitch_Angle_Descriptors ==========================
; This routine simply allows me to define a very long list to be passed to
;  'select_pltvar' when the new-mode (mode7) strahl survey-data is selected.
;                                             MPH, (Last modified: 02/24/2004).
function pitch_angle_descriptors ; ----------------This function has no inputs.

;        This list allows the user to select among strahl-data presentations... 
return,['C-Cnts. ( B):--raw',$ ; Max. counts at  B-beam "core":   (raw values).
        'C-Cnts. ( B): smth',$ ;                        (smoothed time-series).
        'C-PhDen ( B):--raw',$ ; Max.  B-"core" PHASE-DENSITY (f) (raw values).
        'C-PhDen ( B): smth',$ ;                        (smoothed time-series).
       ; ------------
        'C-Cnts. (aB):--raw',$ ; Max. counts at aB-beam "core":   (raw values).
        'C-Cnts. (aB): smth',$ ;                        (smoothed time-series).
        'C-PhDen (aB):--raw',$ ; Max. aB-"core" PHASE-DENSITY (f)  (raw values).
        'C-PhDen (aB): smth',$ ;                        (smoothed time-series).
       ; ------------
        'C-Cnts. ( S):--raw',$ ; Max. counts at  S-beam "core":   (raw values).
        'C-Cnts. ( S): smth',$ ;                        (smoothed time-series).
        'C-PhDen ( S):--raw',$ ; Max.  S-"core" PHASE-DENSITY (f) (raw values).
        'C-PhDen ( S): smth',$ ;                        (smoothed time-series).
       ; ------------
        'C-Cnts. (aS):--raw',$ ; Max. counts at aS-beam "core":   (raw values).
        'C-Cnts. (aS): smth',$ ;                        (smoothed time-series).
        'C-PhDen (aS):--raw',$ ; Max. aS-"core" PHASE-DENSITY (f) (raw values).
        'C-PhDen (aS): smth',$ ;                        (smoothed time-series).
       ; ------------
       ; ------------
        'C-PwrtB ( B):--raw',$ ; Pitch wrt B at  B-beam "core":   (raw values).
        'C-PwrtB ( B): smth',$ ;                        (smoothed time-series).
        'C-PwrtB ( B): wgtM',$ ;           (values weighted by max.).
        'C-PwrtB ( B): wMiC',$ ;           (values weighted by max.--in color).
       ; ------------
        'C-PwrtB (aB):--raw',$ ; Pitch wrt B at aB-beam "core":   (raw values).
        'C-PwrtB (aB): smth',$ ;                        (smoothed time-series).
        'C-PwrtB (aB): wgtM',$ ;           (values weighted by max.).
        'C-PwrtB (aB): wMiC',$ ;           (values weighted by max.--in color).
       ; ------------
        'C-PwrtB ( S):--raw',$ ; Pitch wrt B at  S-beam "core":   (raw values).
        'C-PwrtB ( S): smth',$ ;                        (smoothed time-series).
        'C-PwrtB ( S): wgtM',$ ;           (values weighted by max.).
        'C-PwrtB ( S): wMiC',$ ;           (values weighted by max.--in color).
       ; ------------
        'C-PwrtB (aS):--raw',$ ; Pitch wrt B at aS-beam "core":   (raw values).
        'C-PwrtB (aS): smth',$ ;                        (smoothed time-series).
        'C-PwrtB (aS): wgtM',$ ;           (values weighted by max.).
        'C-PwrtB (aS): wMiC',$ ;           (values weighted by max.--in color).
       ; ------------
       ; ------------
        'C-BaWhM ( B):--raw',$ ;  Ang. width of  B-beam "core":   (raw values).
        'C-BaWhM ( B): smth',$ ;                        (smoothed time-series).
        'C-BaWhM ( B): wgtM',$ ;           (values weighted by max.).
        'C-BaWhM ( B): wMiC',$ ;           (values weighted by max.--in color).
       ; ------------
        'C-BaWhM (aB):--raw',$ ;  Ang. width of aB-beam "core":   (raw values).
        'C-BaWhM (aB): smth',$ ;                        (smoothed time-series).
        'C-BaWhM (aB): wgtM',$ ;           (values weighted by max.).
        'C-BaWhM (aB): wMiC',$ ;           (values weighted by max.--in color).
       ; ------------
        'C-BaWhM ( S):--raw',$ ;  Ang. width of  S-beam "core":   (raw values).
        'C-BaWhM ( S): smth',$ ;                        (smoothed time-series).
        'C-BaWhM ( S): wgtM',$ ;           (values weighted by max.).
        'C-BaWhM ( S): wMiC',$ ;           (values weighted by max.--in color).
       ; ------------
        'C-BaWhM (aS):--raw',$ ;  Ang. width of aS-beam "core":   (raw values).
        'C-BaWhM (aS): smth',$ ;                        (smoothed time-series).
        'C-BaWhM (aS): wgtM',$ ;           (values weighted by max.).
        'C-BaWhM (aS): wMiC']  ;           (values weighted by max.--in color).

end
