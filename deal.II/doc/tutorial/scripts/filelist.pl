#!/usr/local/bin/perl

# Takes table title as argument, traverses the files in .
# and writes to STDOUT a list of all files 
# as a TeX-table, with modification date and revision number.
# Jan Schrage and others <deal@iwr.uni-heidelberg.de> 1999

use File::Find;
use File::stat;
use Time::localtime;

unless ($ARGV[0]) { $ttitle="{\tt deal.II} tutorial"; }
else { $ttitle=$ARGV[0];}

@ignore=("CVS","/#.*#",".*~","^[./]*\$","\.#");

tablehead($ttitle); 
find(\&process,".");
tablefoot();

sub process {
    local ($what="");
    local ($last_rev,$rev,$line,@parts,@cvsout);

    $what=$File::Find::dir."/".$_;

    foreach $i (@ignore) {
	if ($what =~ $i) {return;}
    }
      
    @cvsout=`cvs status $what`;
    
    $last_rev=ctime(stat($what)->mtime);

    
    while ($line=shift(@cvsout))
    {
	if ($line =~ "Working revision:" ) 
	{
	    @parts=split(/:\w*/,$line);
	    $rev=@parts[1];
	    
	    print $what." &".$last_rev." &".$rev;
	}
    }
    
}

sub tablehead {
    local ($what=$_);

    print
"%% Table compiled by filelist.pl 
%% Jan Schrage <deal\@iwr.uni-heidelberg.de> 1999
\\begin{table}
\\caption{Directory listing of $ttitle}
\\begin{tabular}[l r r]
File & Last change  & Revision number \\\\ 
\\hrule \n";
}


sub tablefoot {
    print "\\end{table} \n";
}

