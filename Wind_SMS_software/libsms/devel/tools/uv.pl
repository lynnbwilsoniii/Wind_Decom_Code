#! /usr/bin/perl
# uv -- update (build) version, e.g. uv.pl 1.0.0 gives 1.0.1
# Jim Raines, 23Feb00
#
# One line version if desired
#if ($ARGV[0] =~ /(\d+)\.(\d+)\.(\d+)/) {printf("%d.%d.%d\n",$1,$2,$3 + 1)}

$version = $ARGV[0];

if ($version =~ /(\d+)\.(\d+)\.(\d+)/){
  $maj = $1;
  $min = $2;
  $bld = $3;
}

printf("%d.%d.%d\n",$maj,$min,++$bld);

