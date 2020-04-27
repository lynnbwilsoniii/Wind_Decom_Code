#! /usr/bin/perl 

@dvs = (0,2,4,6,8,10,12,14,16,18,20,22,24,28,30,
	29,27,25,23,21,19,17,15,13,11,9,7,5,3,1);

#printf("# dvs   voltage\n");
$spin = 0;
for ($i = 0; $spin < 60; $i++){
  $v = 6.190722*((1.1225857)**$dvs[$i]);
  printf("spin %2.2d dvs %2.2d voltage %f\n",$spin, $dvs[$i], $v);
  $spin += 2;
}
