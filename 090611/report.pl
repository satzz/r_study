use strict;
use warnings;
use IO::File;
my $file = IO::File->new('dayssn.dat', 'r');
my $new_file = IO::File->new('dayssn_new.dat', 'w');
for my $line(<$file>) {
    my @ret = split /\s+/, $line;
    $new_file->print (sprintf "%s %s\n", $ret[-2], $ret[-1]);
}


