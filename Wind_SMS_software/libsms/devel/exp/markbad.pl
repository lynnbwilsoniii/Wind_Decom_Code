#! /usr/bin/perl
# markbad.pl -- test EDB quality logic for libsms
# Jim Raines, 24Mar00


# simulate state of libsms variables
@qual = 0 x 61;
@msn = 0 x 61; 

for ($i = 0; $i < 62; $i++){
  $msn[$i] = $i;
}
$msn[60] = 0; # hey, that's how it is
$msn[61] = 1; # hey, that's how it is

# put in some problems
$msn[50] = -1;
$msn[35] = -1;
$msn[59] = -1;

$qual[55] = 2; 
$qual[2] = 1;

# check for sequence
for ($i = 0; $i < 60; $i++){


  if ($msn[$i] != -1){ # be sure it was read in

    # check current EDB + 1
    if ($i >= 59){
      $offset = -60;
    }
    else{
      $offset = 0;
    }

    if ($qual[$i + 1] == 128 || $qual[$i + 1] == 64){ 
      $qual[$i] = $qual[$i] | 1; # if its bad
    }
    elsif ($qual[$i + 1] == 1){
      $qual[$i] = $qual[$i] | 1; # if its bad
    }
    elsif (!($msn[$i + 1] == $msn[$i] + 1 + $offset)){
      $qual[$i] = $qual[$i] | 1; # if its out of sequence
    }

    # check current EDB + 2
    if ($i >= 58){
      $offset = -60;
    }
    else{
      $offset = 0;
    }
    
    if ($qual[$i + 2]){ 
      $qual[$i] = $qual[$i] | 2; # if its bad
    }
    elsif (!($msn[$i + 2] == $msn[$i] + 2 + $offset)){
      $qual[$i] = $qual[$i] | 2; # if its out of sequence
    }
  }
  else{ # if it was not read in it is surely bad
    $qual[$i] = 128;
  }
}

# print results

for ($i = 0; $i < 62; $i++){
  printf("msn[%2.2d]=%2.2d qual[%2.2d]=%2.2d\n",$i,$msn[$i],$i,$qual[$i]);
}
