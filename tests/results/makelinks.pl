my $dir;

open L, ">.link";
open U, ">.unlink";

foreach $dir ('base', 'lac', 'fe', 'deal.II', 'bits')
{
    print L "cd $dir\n";
    print U "cd $dir\n";
    my @files = `ls -l $dir`;
    foreach (@files)
    {
	next unless (/^l/);
	m/(\S+)\s*->\s*(\S+)/;
	print "$1 to $2\n";
	print L "ln -s $2 $1\n";
	print U "rm -f $1\n";
    }
    print L "cd ..\n";
    print U "cd ..\n";
}

close L;
close U;
