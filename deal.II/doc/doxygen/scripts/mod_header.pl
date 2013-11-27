

# Modify these to enter the current data automatically
my ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime;
$year += 1900;

if (m'</head>')
{
    print '<link href="$relpath$deal.css" rel="stylesheet" type="text/css"></link>', "\n";
    print '<link rel="SHORTCUT ICON" href="http://www.dealii.org/deal.ico"></link>', "\n";
    print '<meta name="author" content="The deal.II Authors <authors@dealii.org>"></meta>', "\n";
    print '<meta name="copyright" content="Copyright (C) 1998 - ', $year, ' by the deal.II authors"></meta>', "\n";
    print '<meta name="deal.II-version" content="@DEAL_II_PACKAGE_VERSION@"></meta>', "\n";
}

s/\$projectname// unless (m/<title>/);
