package kdocCxxLaTeX;

use File::Path;
use File::Basename;

use Carp;
use Ast;
use kdocAstUtil;

=head1 kdocCxxLaTex

Capabilities required from Ast bit: 

1. Create an inheritance tree

2. Referencing ability: convert a fully qualified class or member name
	to a node reference.

=cut

BEGIN
{
	@clist = ();
	@docQueue = ();

        eval { 
          use Sys::Hostname;
          $host = hostname();
          chomp $host;
        };
        eval { 
          $who = `whoami`;	
	  if($who =~ /DCL-W-IVVERB/) {
	      $who = `show process`;
	      ($who) = ( $who =~ /User: ([^\s]+)/ );
	  }
          chomp $who; 
        };
        $now = localtime;	
        chomp $now; 
        $gentext = "$who\@$host, $now.";
}

sub writeDoc
{
	( $lib, $rootnode, $outputdir, $opt ) = @_;

	$debug = $main::debug;

	print "Generating LaTeX documentation. \n" unless $main::quiet;

	mkpath( $outputdir ) unless -f $outputdir;

#	makeReferences( $rootnode );
	makeClassList( $rootnode );

        startMainDocument();

# ??? Wykomentowane rzeczy jeszcze nie przerobione

	writeGlobalDoc( $rootnode );
	writeAnnotatedList( $rootnode );
	writeHier( $rootnode );
#	writeHeaderList();

	foreach $node ( @{$rootnode->{Kids}} ) {
		next if !defined $node->{Compound} 
			|| defined $node->{ExtSource}
			|| $node->{NodeType} eq "Forward";

		push @docQueue, $node;
	}

        # Próbujemy posortowaæ t± kolejkê
        @docQueue = sort { $b->{astNodeName} cmp $a->{astNodeName} } @docQueue;

	while( $#docQueue >= 0 ) {
		$node = pop @docQueue;
		writeClassDoc( $node );
	}

	print "Generating LaTeXized headers.\n" unless $main::quiet;
	foreach $header ( @main::ARGV ) {
		markupCxxHeader( $header, $rootnode );
	}

        finishMainDocument();
}

=head2 writeAnnotatedList

	Parameters: rootnode

	Writes out a list of classes with short descriptions to
	index-long.tex.

=cut

sub writeAnnotatedList
{
	my ( $root ) = @_;
	my $short;

	open(CLIST, ">$outputdir/index-long.tex") 
	|| die "Couldn't create $outputdir/index-long.tex\n";

        print MAIN "\\input{index-long.tex}\n\n";

	print CLIST sectionHeader( "Lista klas " . $lib );
        print CLIST <<EOF;
\\begin{longtable}{lp{8cm}}        
EOF
	foreach $node ( @clist ) {
		print  "undef in clist\n" if !defined $node;

		$docnode = $node->{DocNode};
		$short = "";

		if( defined $docnode && exists $docnode->{ClassShort} ) {
			$short = deref($docnode->{ClassShort}, $rootnode );
			if( !defined $short ) {
				print $root->{astNodeName}, "has undef short\n";
				next;
			}
				
		}

		print CLIST refName($node), ' & ', $short, "\\\\\n";                
	}

	print CLIST "\\end{longtable}\n";
	close CLIST;
}

=head2 writeClassList

	Parameters: rootnode

	Writes out a concise list of classes to index.tex

=cut

=head3

	Parameters: list, start index, end index

	Helper for writeClassList.  Prints a table containing a
	hyperlinked list of all nodes in the list from start index to
	end index. A table header is also printed.

=cut

sub writeListPart
{
	my( $list, $start, $stop ) = @_;

	print CLIST "<TABLE BORDER=\"0\">";

	print CLIST "<TR><TH>", 
		esc( $list->[ $start ]->{astNodeName} ),
		" - ", esc( $list->[ $stop ]->{astNodeName} ), 
		"</TH></TR>";

	for $ctr ( $start..$stop ) {
		print CLIST "<TR><TD>", refName( $list->[ $ctr ] ),
			"</TD></TR>\n";
	}

	print CLIST "</TABLE>";
}


=head2 writeAllMembers

	Parameters: node

	Writes a list of all methods to "full-list-<class file>"

=cut

sub writeAllMembers
{
	my( $node ) = @_;
	my $file = "$outputdir/full-list-".$node->{Ref};
	my %allmem = ();

	kdocAstUtil::allMembers( \%allmem, $node );

	open( ALLMEM, ">$file" ) || die "Couldn't create $file\n";

        my $thisClassName = $node->{astNodeName};
	print ALLMEM sectionHeader( "Wszystkie metody " . esc($thisClassName) ),
		"<UL>";

	my $mem;
	foreach $mem ( sort keys %allmem ) {
                my $parentName = $allmem{$mem}->{Parent}->{astNodeName};
		print ALLMEM "<LI>";
                print ALLMEM refName( $allmem{ $mem } );
                if($parentName ne $thisClassName) {
                  print ALLMEM " <small>[$parentName]</small>"
                }
                print ALLMEM "</LI>\n";
	}

	print ALLMEM "</UL>$docBotty";

	close ALLMEM;
}

=head2 writeHier

	Parameters: rootnode

	Writes out the class hierarchy index to hier.html.

=cut

sub writeHier
{
	my ( $root ) = @_;

	open( HIER, ">$outputdir/hier.tex") 
		|| die "Couldn't create $outputdir/hier.tex\n";

        print MAIN "\\input{hier.tex}\n\n";

	print HIER sectionHeader( $lib.": Class Hierachy" );

	printNodeHier( $root, 0 );

	close HIER;
}

=head3 printNodeHier

	Parameters: node

	Lists all classes that inherit from this node in an unordered list.

=cut

sub printNodeHier 
{
	my( $node, $level ) = @_;
	my $kid;
	my $src = "";

	# non-derived external classes are not printed.
	if ( defined $node->{ExtSource} ) {
		return if !defined $node->{InBy} 
			|| !kdocAstUtil::hasLocalInheritor( $node );

		$src = "{\\small ".$node->{ExtSource}.")}";
	}

#	print HIER "\\item ", 
        print HIER "\\verb!", '   ' x $level, "!",
                   refName( $node )," $src\\\\\n" 
	    unless $node == $rootnode;

	return if !defined $node->{InBy};

#	print HIER "\\begin{itemize}\n";

	foreach $kid ( sort { $a->{astNodeName} cmp $b->{astNodeName} } 
			@{ $node->{InBy} } ) {
		printNodeHier( $kid, $level + 1 );
	}
#	print HIER "\\end{itemize}\n";
}

=head2 writeHeaderList

	Generates the header-list.html file, which contains links
	to each processed header. The ARGV list is used.

=cut

sub writeHeaderList
{
	open(HDRIDX, ">$outputdir/header-list.html") 
	|| die "Couldn't create $outputdir/header-list.html\n";

	print HDRIDX sectionHeader( $lib . ": Header List" ), "<UL>\n";

	foreach $header ( sort @main::ARGV ) {
		$_ = $header;
		$header = basename ( $_ ) if $main::striphpath;
# convert dashes to double dash, convert path to dash
		s/-/--g/g;
		s/\/|\./-/g;

		print HDRIDX  "\t<LI>",hyper($_.".html",$header),"</LI>\n";
	}
	
	print HDRIDX "</UL>\n",$docBotty;

}

=head2 sectionHeader

	Parameters: libname, heading

	Returns a string containing an HTML section heading.

=cut

sub sectionHeader
{
	my( $heading, $desc ) = @_;

	$desc = "" if !defined $desc;

	my $libtext = "";
	if( $lib ne "" ) {
		$libtext = "<TR><TD><small>Dokumentacja $lib</small>".
			"</TD></TR>";
	}

	return <<EOF;
\\section{$heading}

EOF
	
}

=head2 writeClassDoc

	Write documentation for one compound node.

=cut

sub writeClassDoc
{
	my( $node ) = @_;
	if( exists $node->{ExtSource} ) {
		warn "Trying to write doc for ".$node->{AstNodeName}.
			" from ".$node->{ExtSource}."\n";
		return;
	}

	my $file = "$outputdir/".$node->{astNodeName}.".tex";
	my $docnode = $node->{DocNode};
	my $hasdoc = exists $node->{DocNode} ? 1 : 0;
	my @list = ();
	my $version = undef;
	my $author = undef;

	open( CLASS, ">$file" ) || die "Couldn't create $file\n";

        print MAIN "\\input{".$node->{astNodeName}.".tex}\n\n";

	# Header
	 
	my $source = kdocAstUtil::nodeSource( $node );
	my $short = "";

	if( $hasdoc ) {
		if ( exists $docnode->{ClassShort} ) {
#			$short .= deref($docnode->{ClassShort}, $rootnode );
			$short .= $docnode->{ClassShort};
		}

		if ( exists $docnode->{Deprecated} ) {
			$short .= "\n\n\\textbf{Przestarza³e: u¿ywaæ ostro¿nie.}";
		}

		if ( exists $docnode->{Internal} ) {
			$short .= "\n\n\\textbf{Tylko do wewnêtrznego u¿ytku.}";
		}
		$version = esc($docnode->{Version}) 
			if exists $docnode->{Version};
		$author = esc($docnode->{Author}) 
			if exists $docnode->{Author};
	}

	# pure virtual check
	if ( exists $node->{Pure} ) {
		$short .= 
			"\n\n\\textbf{Klasa abstrakcyjna}\n\n";
	}
	
	# full name, if not in global scope
	if ( $node->{Parent} != $rootnode ) {
		$short .= "\n\n\\texttt{" . $node->{astNodeName} ."}";
	}

	# include
        # MK --> hack (link z #include)
	$short .= "\n\n\\verb!#include <".$source.">!\n\n";

	# template form
	if ( exists $node->{Tmpl} ) {
		$short .= "\n\nKlasa parametryzowana (template): \\verb!<"
			.textRef($node->{Tmpl}, $rootnode)."> ".
			esc($node->{astNodeName}, 
			$rootnode )."!\n\n";
	}

	# inheritance
	if ( $node != $rootnode && exists $node->{InList} ) {
		my $comma = "Dziedziczy z: ";

		foreach $in ( @{ $node->{InList} } ) {
			next if $in == $rootnode;

			$short .= $comma.refName( $in );
			if ( exists $in->{ExtSource} ) {
				$short .= " {\\small(".$in->{ExtSource}
					.")}";
			}
			
			$comma = ", ";
		}

		$short .= "\n\n";
	}

	if ( $node != $rootnode && exists $node->{InBy} ) {
		my $comma .= "Klasy dziedzicz±ce: ";

		@list = ();
		kdocAstUtil::inheritedBy( \@list, $node );

		foreach $in ( @list ) {
			$short .= $comma.refName( $in );
			if ( exists $in->{ExtSource} ) {
				$short .= " {\\small(".
				$in->{ExtSource}.")}";
			}
	
			$comma = ", ";
		}
		$short .= "\n\n";
	}

	# print it

	print CLASS sectionHeader( 
		$node->{NodeType}." ".esc($node->{astNodeName}), 
		$short );

	if( $#{$node->{Kids}} < 0 ) {
		print CLASS <<EOF;
\\begin{quote}
Brak sk³adowych
\\end{quote}
EOF
	}
	else {
		listMethods( $node, "Sk³adowe publiczne", "public" );
		listMethods( $node, "Public Slots", "public_slots" );
		listMethods( $node, "Sk³adowe chronione", "protected" );
		listMethods( $node, "Protected Slots", "protected_slots" );
		listMethods( $node, "Signals", "signals" );

		if ( $main::doPrivate ) {
			listMethods( $node, "Sk³adowe prywatne", "private" );
			listMethods( $node, "Private Slots", "private_slots" );
		}
	}

	# long description
	if ( $hasdoc ) {
		print CLASS "\n\\subsection*{Opis klasy}\n\n";
		printDoc( $docnode, *CLASS, $rootnode, 1 );
	}

	# member doc
	my $kid;
	my ($numref, $ref);

	foreach $kid ( @{$node->{Kids}} ) {
			next if defined $kid->{ExtSource} 
			|| $node->{NodeType} eq "Forward"
			|| (!$main::doPrivate && 
				$kid->{Access} =~ /private/);

		if ( exists $kid->{Compound} ) {
			push @docQueue, $kid;
		}

		next if !defined $kid->{DocNode};

# ??? Przywróciæ kiedy¶
#		if( !exists $kid->{NumRef} ) {
#			warn $kid->{astNodeName}, " type ",
#			$kid->{NodeType}, " doesn't have a numref\n";
#		}

# ??? Przywróciæ kiedy¶ ???
#		( $numref = $kid->{NumRef} ) =~ s/^.*?#//g;
#		( $ref = $kid->{Ref} ) =~ s/^.*?#//g;

		printMemberName( $kid, $ref, $numref );
		printDoc( $kid->{DocNode}, *CLASS, $rootnode );

		if ( $kid->{NodeType} eq "method" ) {
			$ref = kdocAstUtil::findOverride( $rootnode, $node, 
				$kid->{astNodeName} );
			if ( defined $ref ) {
				print CLASS "\n\nPrzykrywa metodê z klasy ",
					refName( $ref ), "\n\n";
			}
		}
	}

	# done

#	if ( defined $version || defined $author ) {
#		print CLASS "\\begin{itemize}\n",
#			defined $version ? 
#                          "\\item Wersja: $version;\n" : "",
#			defined $author ? 
#                          "\\item Autor: $author;\n" : "",
#                          "\\item Generowane: $gentext;\n",
#                          "\\end{itemize}\n";
#	}
#	else {
#		print CLASS "Generowane: $gentext\n";
#	}

	close CLASS;

	# full member list

#      ??? Kiedy¶ jednak zrobiæ
#	writeAllMembers( $node );
}

sub writeGlobalDoc
{
	my( $node ) = @_;
	my $file = "$outputdir/all-globals.tex";
	my $docnode = $node->{DocNode};
	my $hasdoc = exists $node->{DocNode} ? 1 : 0;
	my @list = ();
	my $cumu = Ast::New( "nodelist" );
	my $kid;

	# make a list of nodes by file
	foreach $kid ( @{$node->{Kids}} ) {
		next if exists $kid->{ExtSource} 
			|| exists $kid->{Compound}
			|| (!$main::doPrivate && 
				$kid->{Access} =~ /private/);

		$cumu->AddPropList( kdocAstUtil::nodeSource( $kid ), $kid )
			unless !exists $kid->{Source};
	}

	open( CLASS, ">$file" ) || die "Couldn't create $file\n";

        print MAIN "\\input{all-globals.tex}\n\n";

	print CLASS sectionHeader( "Zmienne, typy i funkcje globalne " . $lib);
	@list = sort keys %$cumu;

	foreach $file ( @list ) {
		next if $file eq "astNodeName";

		listMethods( $node, esc($file), "", $cumu->{$file} );
	}

	# member doc
	my ($numref, $ref);

	foreach $file ( @list ) {
		next if $file eq "astNodeName";
		
		foreach $kid ( @{$cumu->{$file}} ) {
			next if exists $kid->{ExtSource} 
			|| exists $kid->{Compound}
			|| !exists $kid->{DocNode}
			|| (!$main::doPrivate && 
					$kid->{Access} =~ /private/);

# ??? Przywróciæ kiedy¶
#			if( !exists $kid->{NumRef} ) {
#				warn $kid->{astNodeName}, " type ",
#				$kid->{NodeType}, " doesn't have a numref\n";
#			}

# ??? Przywróciæ kiedy¶
#			( $numref = $kid->{NumRef} ) =~ s/^.*?#//g;
#				( $ref = $kid->{Ref} ) =~ s/^.*?#//g;

                        printMemberName( $kid, $ref, $numref );
                        
			print CLASS "\n\n{\\small\\verb!#include <",
				kdocAstUtil::nodeSource( $kid ), 
				">!}\n\n";

			printDoc( $kid->{DocNode}, *CLASS, $rootnode );
		}
	}

#	print CLASS $docBotty;
	close CLASS;
}

sub listMethods
{
	my( $class, $desc, $vis, $nodes ) = @_;
	my $name;
	my $type;
	my $flags;
	my @n=();
	
	if ( !defined $nodes ) { 
		kdocAstUtil::findNodes( \@n, $class->{Kids}, 
			"Access", $vis );
		$nodes = \@n;
	}

	return if ( $#{$nodes} < 0 );

print CLASS<<EOF;
\\subsection*{$desc}

\\begin{itemize}
EOF
	foreach $m ( @$nodes ) {
		next if exists $m->{ExtSource};
		if( exists $m->{Compound} ) {
			# compound docs not printed for rootnode
			next if $class eq $rootnode; 

			$name = refName( $m );
		}
		elsif( exists $m->{DocNode} ) {
			# compound nodes have their own section
			$name = refName( $m,  'NumRef' );
		} else {
			$name = esc( $m->{astNodeName} );
		}

		$type = $m->{NodeType};

		print CLASS "\\item ";

		if( $type eq "var" ) {
			print CLASS esc( $m->{Type}), 
				" \\textbf{", $name,"}", esc( $m->{Array}), "\n";
		}
		elsif( $type eq "method" ) {
			$flags = $m->{Flags};

			if ( !defined $flags ) {
				warn "Method ".$m->{astNodeName}.
					" has no flags\n";
			}

			$name = "\\emph{$name}" if $flags =~ /p/;
			my $extra = "";
			$extra .= "virtual " if $flags =~ "v";
			$extra .= "static " if $flags =~ "s";

			print CLASS $extra, esc($m->{ReturnType}),
				" \\textbf{", $name, "} (", 
				esc($m->{Params}), ") ", 
				$flags =~ /c/ ? " const\n": "\n";
		}
		elsif( $type eq "enum" ) {
			print CLASS "enum \\textbf{", $name, "} {",
				esc($m->{Params}),"}\n";
		}
		elsif( $type eq "typedef" ) {
			print CLASS "typedef ", 
				esc($m->{Type}), " \\textbf{",
				$name,"}", esc($m->{Array});
		}
		else { 
			# unknown type
			print CLASS esc($type), " \\textbf{",
				$name,"}\n";
		}

		print CLASS "\n";
	}

print CLASS<<EOF;
\\end{itemize}
EOF

}

=head2 printIndexEntry

	Parameters: member node

	Prints an index entry for a single node.

	TODO: stub

=cut

sub printIndexEntry
{
	my ( @node ) = @_;
}

=head2 printMemberName

	Parameters: member node, names...

	Prints the name of one member, customized to type. If names are
	specified, a name anchor is written for each one.

=cut

sub printMemberName
{
	my $m = shift;

	my $name = esc( $m->{astNodeName} );
	my $type = $m->{NodeType};
	my $ref;
	my $flags = undef;

	foreach $ref ( @_ ) {
# ??? Pomy¶leæ o tym
#		print CLASS "\\label{", $ref, "}";
	}

#	print CLASS "\n\n\\textbf{";
	print CLASS "\n\n\\subsubsection*{";

	if( $type eq "var" ) {
		print CLASS textRef($m->{Type}, $rootnode ), 
		" \\texttt{", $name,"} ", textRef($m->{Array}, $rootnode );
	}
	elsif( $type eq "method" ) {
		$flags = $m->{Flags};
		$name = "\\emph{$name}" if $flags =~ /p/;

		print CLASS textRef($m->{ReturnType}, $rootnode ),
		" \\texttt{", $name, "} (", 
		textRef($m->{Params}, $rootnode ), ") ";
	}
	elsif( $type eq "enum" ) {
		print CLASS "enum \\texttt{", $name, "} {",
		esc($m->{Params}),"} ";
	}
	elsif( $type eq "typedef" ) {
		print CLASS "typedef ", 
		textRef($m->{Type}, $rootnode ), " \\texttt{",
		$name,"} ", textRef($m->{Array}, $rootnode );
	}
	else {
		print CLASS $name, " {\\small (", 
			esc($type), ")} ";
	}

# extra attributes
	my @extra = ();

	if( !exists $m->{Access} ) {
		print "Member without access:\n";
		kdocAstUtil::dumpAst( $m );
	}

	($ref = $m->{Access}) =~ s/_slots//g;

	push @extra, $ref
		unless $ref =~ /public/
		|| $ref =~ /signal/;

	if ( defined $flags ) {
		my $f;
		my $n;
		foreach $f ( split( "", $flags ) ) {
			$n = $main::flagnames{ $f };
			warn "flag $f has no long name.\n" if !defined $n;
			push @extra, $n;
		}
	}

	if ( $#extra >= 0 ) {
		print CLASS " {\\small [", join( " ", @extra ), "]}";
	}

	print CLASS "}"; # Po subsubsection
	print CLASS "\n\n";
	
# finis
}



=head2 makeClassList

	Parameters: node

	fills global @clist with a list of all direct, non-external
	compound children of node.

=cut

sub makeClassList
{
	my ( $rootnode ) = @_;

	@clist = ();

	foreach $node ( @ {$rootnode->{Kids}} ) {
		if ( !defined $node ) {
			print "makeClassList: undefined child in rootnode!\n";
			next;
		}

		push( @clist, $node ) unless exists $node->{ExtSource}
				|| !exists $node->{Compound};
	}

	@clist = sort { $a->{astNodeName} cmp $b->{astNodeName}  }
			@clist;
}

# MK --> skopiowane z 1.0
sub markupCxxHeader
{
	my( $filename, $rootnode ) = @_;
	$className = "";
	my( $reference );
	my( @inheritance );
	my( $word );

	open( HFILE, $filename ) || die "Couldn't open $filename to read.\n";

	$_ = $filename;
	# convert dashes to double dash, convert path to dash
	s/-/--g/g;
	s/\/|\./-/g;
	$outputName = $_;
	$outputFilename = $outputdir."/".$outputName;

	open( HTMLFILE, ">$outputFilename.tex" ) 
		|| die "Couldn't open $outputFilename to write.\n";

        print MAIN "\\input{$outputName.tex}\n\n";

        print HTMLFILE sectionHeader( esc("$filename") );

        print HTMLFILE <<EOF;
\\selectlisting{cpp}
\\begin{listing}
EOF
	while( <HFILE> )
	{
          print HTMLFILE;

# ??? Co¶ zrobiæ
#		if( /^\s*(template.*\s+)?(class|struct)/ ) {
#                    if($rootnode) {
#			$_ = textRef($_, $rootnode);
#                    } else {
#                        print STDERR "$0: can not make hyperlinks in file $filename\n";
#                    }
#		}

	}

        print HTMLFILE <<EOF;
\\end{listing}
EOF
}

sub startMainDocument {

  open(MAIN, ">$outputdir/main.tex")
    || die "Couldn't create $outputdir/main.tex\n";
    
  print MAIN <<EOF;
\\documentclass[a4paper,10pt]{article}

% Geometria strony (twoside ?)
\\usepackage[a4paper,hmargin={2cm,2cm},vmargin={2cm,2cm}]{geometry}

\\usepackage[latin2]{inputenc}
\\usepackage{polski}

\\usepackage{longtable}

% Specyficzne listingi
\\usepackage{listings}
\\keywordstyle{\\bfseries\\sffamily}
%\\commentstyle{\\slshape}
\\commentstyle{\\itshape}
%\\stringstyle{\\ttfamily}
\\blankstringtrue
\\prelisting{\\small\\sffamily}

% Przynajmniej nie bêdzie krzycza³
% \\catcode`\\_=12

% Odstêpy miêdzy akapitami itp
\\setlength{\\parindent}{0cm}
\\addtolength{\\parskip}{1ex}

\\title{Dokumentacja referencyjna $lib}

\\begin{document}

\\maketitle
\\tableofcontents

\\begin{abstract}
Dokumentacja referencyjna $lib. 

Generowana: $gentext.
\\end{abstract}

EOF
}

sub finishMainDocument {
  print MAIN <<EOF;

\\end{document}
EOF
}

sub esc
{
	my $str = $_[ 0 ];

	return "" if !defined $str || $str eq "";

        # Trzeba zrobiæ sztuczkê by nie zamieniaæ w³asnych klamerek
        # lub backslashy
	$str =~ s/{/\\lbrace/g;
	$str =~ s/}/\\rbrace/g;
        $str =~ s/\\/\\ensuremath{\\backslash}/g;
#	$str =~ s/{/\\{/g;
#	$str =~ s/}/\\}/g;

        $str =~ s/</\\ensuremath{<}/g;
        $str =~ s/>/\\ensuremath{>}/g;
	$str =~ s/#/\\#/g;
	$str =~ s/%/\\%/g;
	$str =~ s/&/\\&/g;
	$str =~ s/\$/\\\$/g;
	$str =~ s/_/\\_/g;
        $str =~ s/~/\\ensuremath{\\sim}/g;
#        $str =~ s/\^/{\\ensuremath{^}}/g;

	return $str;
}

sub refName
{
	my ( $node ) = @_;
	confess "refName called with undef" if !defined $node->{astNodeName};

	my $ref = defined $_[1] ? $_[1] : 'Ref';

	$ref = $node->{ $ref };

	my $out;

#	if ( !defined $ref ) {
        $out =  esc($node->{astNodeName});
#	} else {
#		$out = '<A HREF="'.encodeURL($ref).'">'.
#			esc($node->{astNodeName}).'</A>';
#	}

	$out = "\\emph{".$out."}" if exists $node->{Pure};

#        print "DIAG: refName zwraca $out\n";
	return $out;

}

sub deref
{
	my ( $str, $rootnode ) = @_;
	confess "rootnode is null" if !defined $rootnode;

	my $out = "";
	my $text;

	foreach $text ( split (/(\@ref\s+[\w:#]+)/, $str ) ) {
		if ( $text =~ /\@ref\s+([\w:#]+)/ ) {
                        my $x = wordRef( $1, $rootnode );
#                        print "DIAG: w derefie $1 -> $x\n";
			$out .= $x;
		}
		else {
			$out .= esc($text);
		}
	}
	
#        print "DIAG: deref dla\n$str\nzwraca\n$out\n" if $str ne $out;
	return $out;
}

=head2 printDoc

	Parameters: docnode, *filehandle, rootnode, compound

	Print a doc node. If compound is specified and non-zero, various
	compound node properties are not printed.

=cut

sub printDoc
{
        local ($docNode, *CLASS, $rootnode, $comp ) = @_;
        my $node;
        my $type;
	my $text;
	my $lasttype = "none";

	$comp = defined $comp? $comp : 0;
	
	$text = $docNode->{Text};

        if ( defined $text ) {
                print CLASS "\n\n";

                foreach $node ( @$text ) {
                        $type = $node->{NodeType};
			$name = $node->{astNodeName};
                        warn "Node '", $name, "' has no type"
				if !defined $type;

			if( $lasttype eq "ListItem" && $type ne $lasttype ) {
				print CLASS "\\end{itemize}\n";
			}

                        if( $type eq "DocText" ) {
                                print CLASS "", deref( $name, $rootnode );
                        }
                        elsif ( $type eq "Pre" ) {
# ??? verbatim'ów nie escapeujê. Ale mo¿e w ogóle u¿yæ tu listings
                                print CLASS "\n\\begin{verbatim}\n",
                                      $name , "\n\\end{verbatim}\n";
                        }
			elsif( $type eq "Ref" ) {
				my $ref = $node->{Ref};
				if ( defined $ref ) {
					print "found reference for $name\n";
					print CLASS refName( $ref );
				}
				else {
					print CLASS $name;
				}
			}
                        elsif ( $type eq "ParaBreak" ) {
                                print CLASS "\n\n";
                        }
			elsif ( $type eq "ListItem" ) {
				if ( $lasttype ne "ListItem" ) {
					print CLASS "\n\\begin{itemize}\n";
				}
				print CLASS "\\item ", 
					deref( $name, $rootnode ), "\n";
			}

			$lasttype = $type;
                }

		if( $type eq "ListItem" ) {
			print CLASS "\n\\end{itemize}\n";
		}

                print CLASS "\n\n";

        }


	# Params
	my @paramlist = ();
	kdocAstUtil::findNodes( \@paramlist, $docNode->{Text},
			"NodeType", "Param" );

	if( $#paramlist >= 0 ) {
		my $pnode;
		print CLASS "\n\n\\textbf{Parametry}:\n\n",
			"\\begin{longtable}{lp{8cm}}\n";
		
		foreach $pnode ( @paramlist ) {
			print CLASS 
                          "\\emph{", esc($pnode->{Name}), '} & ',
                          deref($pnode->{astNodeName}, $rootnode ),
                          "\\\\\n";
		}
		print CLASS "\\end{longtable}\n";
	}

	# Return
	printTextItem( $docNode, CLASS, "Returns", "Wynik" );

        my @exception_list = ();
        kdocAstUtil::findNodes( \@exception_list, $docNode->{Text},
                                "NodeType", "Throws" );
        if( $#exception_list >= 0 ) {
		my $pnode;
		print CLASS "\n\n\\textbf{Wyj±tki}:\n",
			"\\begin{longtable}{lp{8cm}}\n";
		
		foreach $pnode ( @exception_list ) {
			print CLASS 
                          "\\emph{", esc($pnode->{Name}), '} & ',
                          deref($pnode->{astNodeName}, $rootnode ),
                          "\\\\\n";
		}
		print CLASS "\\end{longtable}\n";
        }

	# See
	$text = $docNode->{See};
	my $tref = $docNode->{SeeRef};
	
	if ( defined $text ) {
		my $comma = "\n\n\\textbf{Patrz te¿}: ";

		foreach $ctr ( 0..$#{$text} ) {
			if ( defined $tref->[ $ctr ] ) {
				print CLASS $comma, refName( $tref->[ $ctr ] );
			}
			else {
				print CLASS $comma, esc( $text->[ $ctr ] );
			}

			$comma = ", ";
		}
		print CLASS "\n\n";
	}

	return if $comp;

	printTextItem( $docNode, CLASS, "Since", "Od" );
	printTextItem( $docNode, CLASS, "Version", "Wersja" );
	printTextItem( $docNode, CLASS, "Id" );
	printTextItem( $docNode, CLASS, "Author", "Autor" );
}

=head3 printTextItem

	Parameters: node, *filehandle, prop, label

	If prop is set, it prints the label and the prop value deref()ed.

=cut

sub printTextItem
{
	local ( $node, *CLASS, $prop, $label ) = @_;
	my $text = $node->{ $prop };
	
	return unless defined $text;
	$label = $prop unless defined $label;
	
#	print CLASS "\n\n\\textbf{", $label, "}: ", deref( $text, $rootnode  ), "\n\n";
#	print CLASS "\n\n\\textbf{$label}:\n\n", deref( $text, $rootnode  ), "\n\n";
        my $txt = deref($text, $rootnode);
	print CLASS <<EOF

\\textbf{$label}:

\\begin{longtable}{lp{10cm}}
 & $txt \\\\
\\end{longtable}
EOF
}

sub textRef
{
	my ( $str, $rootnode ) = @_;
	my $word;
	my $out = "";

	foreach $word ( split( /([^\w:]+)/, $str ) ) {
		if ( $word =~ /^[^\w:]/ ) {
			$out .= esc($word);
		}
		else {
			$out .= wordRef( $word, $rootnode );
		}
	}

	return $out;
}

=head3 wordRef

	Parameters: word

	Prints a hyperlink to the word's' reference if found, otherwise
	just prints the word. Good for @refs etc.

=cut

sub wordRef
{
	my ( $str, $rootnode ) = @_;
	confess "rootnode is undef" if !defined $rootnode;

	return "" if $str eq "";

        # MK --> chcê linkowaæ Ref'y i ConstRef'y
#	my $ref = kdocAstUtil::findRef( $rootnode, $str );
        my $str2 = $str;
        $str2 =~ s/(Const)?Ref$//;
        my $ref = kdocAstUtil::findRef( $rootnode, $str2 );

# ??? Powy¿sze nigdy siê nie znajdzie bo jeszcze nie robiê referencji. A
# hashe zostaj±...
        $str =~ s/^\#//;

	return esc($str) if !defined $ref;

	return hyper( $ref->{Ref}, esc($str) );
}

sub hyper
{
	confess "hyper: undefed parameter $_[0], $_[1]"
#		unless defined $_[0] && defined $_[1];
		unless defined $_[1];
#	return "<A HREF=\"$_[0]\">".esc($_[1])."</A>";
        my $result = esc($_[1]);
#        print "DIAG: hyper zwraca $result\n";
	return $result;
}


1;
