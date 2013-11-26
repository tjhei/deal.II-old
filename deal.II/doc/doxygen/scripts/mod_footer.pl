
use Sys::Hostname;
my $host = hostname;

my $hosting = << 'EOT'
&nbsp;&nbsp; Hosting provided by
<a href="http://www.iwr.uni-heidelberg.de/"><img src="http://www.dealii.org/pictures/IWRlogo4.png" alt="IWR"></a>
<a href="http://www.uni-heidelberg.de/"><img src="http://www.dealii.org/pictures/UniLogo4.png" alt="Universität Heidelberg"></a>
EOT
    ;

if ($host eq "simweb")
{
    s/\$doxygenversion/\$doxygenversion $hosting/;
}
