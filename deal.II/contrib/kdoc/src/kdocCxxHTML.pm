package kdocCxxHTML;

use File::Path;
use File::Basename;

use Carp;
use Ast;
use kdocAstUtil;
use kdocHTMLutil;
use kdocUtil; 
use Iter;

use strict;
no strict "subs";

use vars qw/ @clist $host $who $now $gentext %toclinks $docBotty
	$lib $rootnode $outputdir $opt $debug *CLASS/;

=head1 kdocCxxHTML

Capabilities required from Ast bit: 

1. Create an inheritance tree

2. Referencing ability: convert a fully qualified class or member name
to a node reference.

=cut

BEGIN
{
	@clist = ();

	# Contents entries in HTML page header

	%toclinks = (
		'Index'		 => 'index.html',
		'Annotated List' => 'index-long.html',
		'Hierarchy'	 => 'hier.html',
		'Globals'	 => 'all-globals.html',
		'Files'		 => 'header-list.html'
	);

	# Page footer

	$who = kdocUtil::userName();
	$host = kdocUtil::hostName();
	$now = localtime;
	$gentext = "$who\@$host on $now, using kdoc $main::Version.";

	$docBotty =<<EOF
<HR>
	<table>
	<tr><td><small>Generated by: $gentext</small></td></tr>
	</table>
</BODY>
</HTML>
EOF

}

sub writeDoc
{
	( $lib, $rootnode, $outputdir, $opt ) = @_;

	$debug = $main::debug;

	mkpath( $outputdir ) unless -f $outputdir;

	makeSourceReferences( $rootnode );
	makeReferences( $rootnode );
	kdocAstUtil::makeClassList( $rootnode, \@clist );

	writeGlobalDoc( $rootnode, "$outputdir/".$toclinks{Globals} );
	writeClassList( $rootnode , "$outputdir/".$toclinks{Index} );
	writeAnnotatedList( $rootnode, 
		"$outputdir/".$toclinks{ "Annotated List" } );
	writeHier( $rootnode, "$outputdir/".$toclinks{Hierarchy} );
	writeHeaderList( "$outputdir/".$toclinks{Files} );


	# Document all compound nodes
	Iter::LocalCompounds( $rootnode, sub { writeClassDoc( shift ); } );
}

=head2 writeClassList

	Parameters: rootnode

	Writes out a concise list of classes to index.html

=cut

sub writeClassList
{
	my ( $root, $file ) = @_;

	open(CLIST, ">$file") 
		|| die "Couldn't create $file\n";

	newPgHeader( *CLIST{IO}, "$lib Class Index", "", "", \%toclinks );

	if ( defined $root->{DocNode} ) {
		printDoc( $root->{DocNode}, *CLIST, $root, 1 );
	}

	if ( $#clist < 0 ) {
		print CLIST "<h2>No classes</h2>";
		# TODO: Perhaps display C-specific index.
	}
	else {
		writeTable( *CLIST{IO}, \@clist, 
			exists $opt->{"html-cols"} ? $opt->{"html-cols"} : 3 );
	}

	print CLIST $docBotty;
	close CLIST;
}



=head2 writeAnnotatedList

	Parameters: rootnode

	Writes out a list of classes with short descriptions to
	index-long.html.

=cut

sub writeAnnotatedList
{
	my ( $root, $file ) = @_;
	my $short;

	open(CLIST, ">$file") 
	|| die "Couldn't create $file\n";

	newPgHeader( *CLIST{IO}, "$lib Annotated List", "", "", \%toclinks );

	print CLIST '<TABLE WIDTH="100%" BORDER=\"0\">';
	
	my $colnum = 0;
	my $colour;
	my $col = 0;

	foreach my $node ( @clist ) {
		print  "undef in clist\n" if !defined $node;

		my $docnode = $node->{DocNode};
		$short = "";

		if( defined $docnode ) {
			if ( exists $docnode->{ClassShort} ) {
				$short = deref($docnode->{ClassShort}, 
					$rootnode );
			}
			if ( defined $docnode->{Internal} ) {
				$short .= " <small>(internal)</small>";
			}
			if ( defined $docnode->{Deprecated} ) {
				$short .= " <small>(deprecated)</small>";
			}
		}
		
		$col = $col ? 0 : 1;
		$colour = $col ? "" : 'bgcolor="#eeeeee"';

		print CLIST "<TR $colour><TD>", refNameFull( $node ),
			"</TD><TD>", $short, "</TD></TR>";
	}

	print CLIST "</TABLE>", $docBotty;
	close CLIST;
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

#	print ALLMEM pageHeader( \%toclinks, esc($node->{astNodeName})
#		." - All Methods" ), "<UL>";

	newPgHeader( *ALLMEM, 
			$node->{NodeType}." ".esc($node->{astNodeName}).
			": All methods", "", "", \%toclinks );

	my $mem;
	my $col = 0;
	my $colour = "";

	my @memlist = sort { $a->{astNodeName} cmp $b->{astNodeName} } 
			values %allmem;
	writeTable( *ALLMEM{IO}, \@memlist, 3 );

	print ALLMEM "$docBotty";

	close ALLMEM;
}

=head2 writeHier

	Parameters: rootnode

	Writes out the class hierarchy index to hier.html.

=cut

sub writeHier
{
	my ( $root, $file ) = @_;

	open( HIER, ">$file") 
		|| die "Couldn't create $file\n";

	newPgHeader( *HIER{IO}, "$lib Class Hierarchy", "", "", \%toclinks );

	Iter::Hierarchy( $root,
		sub {	# down
			print HIER "<UL>";
		},
		sub {	# print
			my ( $node ) = @_;
			return if $node == $rootnode;

			my $src = defined $node->{ExtSource} ?
				" ($node->{ExtSource})" : "";

			print HIER "<LI>", refNameFull( $node )," $src\n";
		},
		sub {	# up
			if ( $_[0] == $root ) {
				print HIER "</UL>\n";
			}
			else {
				print HIER "</UL></LI>\n";
			}
		},
		sub { print HIER "</LI>\n"; }
	);

	print HIER $docBotty;
	close HIER;
}

=head2 writeHeaderList

	Generates the header-list.html file, which contains links
	to each processed header. The $rootnode->{Sources} List is used.

=cut

sub writeHeaderList
{
	my ( $file ) = @_;

	# Convert all to HTML

	my @clist = sort { $a->{astNodeName} cmp $b->{astNodeName} }
			@{$rootnode->{Sources}};

	foreach my $hdr ( @clist ) {
		writeSrcHTML( $outputdir."/".$hdr->{Ref}, $hdr->{Path} );
	}


	# Write the list

	open(HDRIDX, ">$file") || die "Couldn't create $file\n";


	newPgHeader( *HDRIDX{IO}, "$lib File Index", "", "", \%toclinks );

	writeTable( *HDRIDX{IO}, \@clist, 
			exists $opt->{"html-cols"} ? $opt->{"html-cols"} : 3 );

	print HDRIDX "</UL>\n",$docBotty;
	close HDRIDX;
}




=head2 writeClassDoc

	Write documentation for one compound node.

=cut

sub writeClassDoc
{
	my( $node ) = @_;

	print "Enter: $node->{astNodeName}\n" if $debug;
	if( exists $node->{ExtSource} ) {
		warn "Trying to write doc for ".$node->{AstNodeName}.
			" from ".$node->{ExtSource}."\n";
		return;
	}

	my $file = "$outputdir/".$node->{Ref};
	my $docnode = $node->{DocNode};
	my @list = ();
	my $version = undef;
	my $author = undef;

	open( CLASS, ">$file" ) || die "Couldn't create $file\n";

	# Header
	 
	my $short = "";
	my $extra = "";

	if( kdocAstUtil::hasDoc( $node ) ) {
		if ( exists $docnode->{ClassShort} ) {
			$short .= deref($docnode->{ClassShort}, 
				$rootnode).
				" <small>".
				hyper( "#longdesc", "More..." )."</small>";
		}

		if ( exists $docnode->{Deprecated} ) {
			$extra .= '<TR><TH colspan="2">'.
			'Deprecated! use with care</TH></TR>';
		}

		if ( exists $docnode->{Internal} ) {
			$extra .= '<TR><TH colspan="2">'.
			'Internal Use Only</TH></TR>';

		}
		$version = esc($docnode->{Version}) 
			if exists $docnode->{Version};
		$author = esc($docnode->{Author}) 
			if exists $docnode->{Author};
	}

	# pure virtual check
	if ( exists $node->{Pure} ) {
		$extra .= '<TR><TH colspan="2">'
			.'Contains pure virtuals</TH></TR>';
	}
	
	# full name, if not in global scope
	if ( $node->{Parent} != $rootnode ) {
		$extra .= tabRow( "Full name", 
			"<code>".refNameEvery( $node, $rootnode )."</code>" );
	}

	# include (not for namespaces)
	if ( $node->{NodeType} ne "namespace" 
		&& $node->{NodeType} ne "Forward" ) {
		$extra .= tabRow( 'Definition', '<code>#include &lt;'. 
			refName( $node->{Source} ).'&gt;</code>' );
	}

	# template form
	if ( exists $node->{Tmpl} ) {
		$extra .= tabRow( "Template form",
			"template "
			."&lt;".textRef($node->{Tmpl}, $rootnode )."&gt; " .
			esc($node->{astNodeName})
			."</code>" );
	}

	
	my $comma = "";
	my $out = "";

	# ancestors
	Iter::Ancestors( $node, $rootnode, undef, undef,
		sub { # print
			my ( $ances, $name, $type, $template ) = @_;

			if( !defined $ances ) {
				$out .= $comma.esc($name);
			}
			else {
				$out .= $comma.refNameFull( $ances, $rootnode, 1 );
			}
			
			$out .= " &lt;".wordRef($template, $rootnode )."&gt;"
				unless !defined $template;

			if ( exists $ances->{ExtSource} ) {
				$out .=" <small>(".$ances->{ExtSource}
				.")</small>";
			}

			$out .= " <small>[$type]</small>" 
				unless $type eq "public";

			$comma = ", ";

		},
		sub { # end
			$extra .= tabRow( "Inherits", $out );
		}
	);

	# descendants
	Iter::Descendants( $node, undef,
		sub { $comma = $out = ""; },		# start
		sub {					# print
			my ( $in ) = @_;
			$out .= $comma.refName( $in, 1 );

			if ( exists $in->{ExtSource} ) {
				$short .= " <small>(".
				$in->{ExtSource}.")</small>";
			}
	
			$comma = ", ";

		},
		sub {					# end
			$extra .= tabRow( "Inherited by", $out );
		}
	);

	$extra .= '<TR><TH>'.
		hyper( "full-list-".$node->{Ref},
			"List of all Methods" )."</TH></TR>";


	#### print it

	newPgHeader( *CLASS{IO}, 
		$node->{NodeType}." ".esc($node->{astNodeName}), 
		$short, $extra, \%toclinks );


	if( $#{$node->{Kids}} < 0 ) {
		print CLASS "<center><H4>No members</H4></center>\n";
	}
	else {
		Iter::MembersByType ( $node, 
			sub { print CLASS "<h4>", $_[0], "</h4><ul>"; },
			sub {	my ($node, $kid ) = @_;
                                listMember( $node, $kid ); 
                            },
			sub { print CLASS "</ul>"; }
		);
	}

	# long description
	if ( kdocAstUtil::hasDoc( $node ) ) {
		print CLASS "<HR><A NAME=\"longdesc\">",
			"<H2>Detailed Description</H2>";
		printDoc( $docnode, *CLASS, $node, 1 );
	}

	# member doc
	my $kid;
	my ($numref, $ref);

			
	Iter::DocTree( $node, 0, 0,
		sub {					# common
			my ( $node, $kid ) = @_;

			if( !exists $kid->{NumRef} ) {
				warn $kid->{astNodeName}, " type ",
				$kid->{NodeType}, " doesn't have a numref\n";
			}

			( $numref = $kid->{NumRef} ) =~ s/^.*?#//g;
			( $ref = $kid->{Ref} ) =~ s/^.*?#//g;

			printMemberName( $kid, $ref, $numref );
			printDoc( $kid->{DocNode}, *CLASS, $node );

			return;
		},
		undef,					# compound
		sub {					# other
			my ( $node, $kid ) = @_;

			if ( $kid->{NodeType} eq "method" ) {
				$ref = kdocAstUtil::findOverride( $rootnode, 
						$node, $kid->{astNodeName} );
				if ( defined $ref ) {
					print CLASS "<p>Overloaded function from base class ", 
					            refName( $ref );
                                        if ( exists $kid->{DocNode}->{Reimplemented} ) {
                                                print CLASS " for internal reasons; the API is not affected";
                                        }
                                        print CLASS ".</p>\n";
				}
			}

			return;
		}
	);

	# done

	if ( defined $version || defined $author ) {
		print CLASS "<HR><UL>",
			defined $version ? 
				"<LI><i>Version</i>: $version</LI>" : "",
			defined $author ? 
				"<LI><i>Author</i>: $author</LI>" : "",
			"<LI><i>Generated</i>: $gentext</LI></UL>",
			"</BODY></HTML>\n";
	}
	else {
		print CLASS $docBotty;
	}

	close CLASS;

	# full member list

	writeAllMembers( $node );
}

sub writeGlobalDoc
{
	my( $node, $file ) = @_;
	my $docnode = $node->{DocNode};
	my $hasdoc = exists $node->{DocNode} ? 1 : 0;
	my $cumu = Ast::New( "nodelist" );

	# make a list of nodes by file
	foreach my $kid ( @{$node->{Kids}} ) {
		next if exists $kid->{ExtSource} 
			|| exists $kid->{Compound}
			|| (!$main::doPrivate && 
				$kid->{Access} =~ /private/);

		if ( exists $kid->{Source} ) {
			$kid->{Source}->AddPropList( "Glob", $kid )
		}
	}

	open( CLASS, ">$file" ) || die "Couldn't create $file\n";

	newPgHeader( *CLASS{IO}, $lib." Globals", "", "", \%toclinks );

	my @list = sort { $a->{astNodeName} cmp $b->{astNodeName} } 
		@{$node->{Sources}};

	foreach my $source ( @list ) {
		next unless defined $source->{Glob};

		listMethods( $node, refName( $source ), "", $source->{Glob} );
	}

	# member doc
	my ($numref, $ref);

	foreach my $source ( @list ) {
		next unless defined $source->{Glob};

		foreach my $kid ( @{$source->{Glob}} ) {
			next if exists $kid->{ExtSource} 
			|| exists $kid->{Compound}
			|| !exists $kid->{DocNode}
			|| (!$main::doPrivate && 
					$kid->{Access} =~ /private/);

			if( !exists $kid->{NumRef} ) {
				warn $kid->{astNodeName}, " type ",
				$kid->{NodeType}, " doesn't have a numref\n";
			}

			( $numref = $kid->{NumRef} ) =~ s/^.*?#//g;
				( $ref = $kid->{Ref} ) =~ s/^.*?#//g;

			printMemberName( $kid, $ref, $numref );

			print CLASS "<p><small><code>#include &lt;",
				refName( $source ),
				"&gt;</code></small></p>";

			printDoc( $kid->{DocNode}, *CLASS, $node );
		}
	}

	print CLASS $docBotty;
	close CLASS;
}


sub listMember
{
	my( $class, $m ) = @_;
	my $name;

	if( exists $m->{Compound} ) {
		# compound docs not printed for rootnode
		next if $class eq $rootnode; 

		$name = refName( $m );
	}
	elsif( exists $m->{DocNode} ) {
		# compound nodes have their own page
		$name = refName( $m,  'NumRef' );
	} else {
		$name = esc( $m->{astNodeName} );
	}

	my $type = $m->{NodeType};

	print CLASS "<LI>";

	if( $type eq "var" ) {
		print CLASS esc( $m->{Type}), B( "b", $name ), esc( $m->{Array});
	}
	elsif( $type eq "method" ) {
		my $flags = $m->{Flags};

		if ( !defined $flags ) {
			warn "Method ".$m->{astNodeName}.  " has no flags\n";
		}

		$name = B("i", $name ) if $flags =~ /p/;
		$name = B("b", $name );

		my $extra = "";
		$extra .= "virtual " if $flags =~ "v";
		$extra .= "static " if $flags =~ "s";

		print CLASS $extra, textRef($m->{ReturnType}, $rootnode ),
			"\&nbsp;$name (", textRef( $m->{Params}, $rootnode ), ") ", 
			$flags =~ /c/ ? " const\n": "\n";
	}
	elsif( $type eq "enum" ) {
		print CLASS "enum $name {", esc($m->{Params}),"}\n";
	}
	elsif( $type eq "typedef" ) {
		print CLASS "typedef ", esc($m->{Type}), " $name", esc($m->{Array}), "\n";
	}
	else { 
		# unknown type
		print CLASS esc($type), " $name\n";
	}

	print CLASS "</LI>\n";

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
<H2>$desc</H2>
<UL>
EOF
	foreach my $m ( @$nodes ) {
		next if exists $m->{ExtSource};
		if( exists $m->{Compound} ) {
			# compound docs not printed for rootnode
			next if $class eq $rootnode; 

			$name = refName( $m );
		}
		elsif( exists $m->{DocNode} ) {
			# compound nodes have their own page
			$name = refName( $m,  'NumRef' );
		} else {
			$name = esc( $m->{astNodeName} );
		}

		$type = $m->{NodeType};
		$name = B( "b", $name );

		print CLASS "<LI>\n";

		if( $type eq "var" ) {
			print CLASS esc( $m->{Type}), " $name", esc( $m->{Array}), "\n";
		}
		elsif( $type eq "method" ) {
			$flags = $m->{Flags};

			if ( !defined $flags ) {
				warn "Method ".$m->{astNodeName}.
					" has no flags\n";
			}

			$name = "<i>$name</i>" if $flags =~ /p/;
			my $extra = "";
			$extra .= "virtual " if $flags =~ "v";
			$extra .= "static " if $flags =~ "s";

			print CLASS $extra, textRef($m->{ReturnType}, $rootnode), 
				"\&nbsp;$name (", textRef( $m->{Params}, $rootnode), ") ", 
				$flags =~ /c/ ? " const\n": "\n";
		}
		elsif( $type eq "enum" ) {
			print CLASS "enum $name {", esc($m->{Params}),"}\n";
		}
		elsif( $type eq "typedef" ) {
			print CLASS "typedef ", esc($m->{Type}), " $name", esc($m->{Array});
		}
		else { 
			# unknown type
			print CLASS esc($type), " $name\n";
		}

		print CLASS "</LI>\n\n";
	}

print CLASS<<EOF;
</UL>
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

	my $name = B( "underline", esc( $m->{astNodeName} ) );
	my $type = $m->{NodeType};
	my $ref;
	my $flags = undef;

	foreach $ref ( @_ ) {
		print CLASS "<A NAME=\"", $ref, "\"></A>";
	}

	print CLASS '<table width="100%"><tr bgcolor="#eeeeee"><td><strong>';

	if( $type eq "var" ) {
		print CLASS textRef($m->{Type}, $rootnode ), " $name", textRef( $m->{Array}, $rootnode), "\n";
	}
	elsif( $type eq "method" ) {
		$flags = $m->{Flags};
		$name = "<i>$name</i>" if $flags =~ /p/;

		print CLASS textRef($m->{ReturnType}, $rootnode ),
			"\&nbsp;$name (", textRef($m->{Params}, $rootnode ), ")\n";
	}
	elsif( $type eq "enum" ) {
		print CLASS "enum $name {", esc($m->{Params}),"}\n";
	}
	elsif( $type eq "typedef" ) {
		print CLASS "typedef ", 
		textRef($m->{Type}, $rootnode ), " $name", textRef( $m->{Array}, $rootnode);
	}
	else {
		print CLASS $name, " ", B( "small", "(", esc($type), ")" );
	}

	print CLASS "</strong></td></tr></table><p>";

	# extra attributes
	my @extra = ();

	if( !exists $m->{Access} ) {
		warn "Member without access:\n";
		kdocAstUtil::dumpAst( $m );
	}

	($ref = $m->{Access}) =~ s/_slots//g;

	push @extra, $ref
		unless $ref =~ /public/
		|| $ref =~ /signal/;

	if ( defined $flags ) {
		foreach my $f ( split( "", $flags ) ) {
			my $n = $main::flagnames{ $f };

			if ( defined $n ) {
				push @extra, $n;
			}
			else {
				warn "flag $f has no long name.";
			}

		}
	}

	if ( $#extra >= 0 ) {
		print CLASS " <small>[", join( " ", @extra ), "]</small>";
	}

	print CLASS "</p>";

	return;
}



sub writeSrcHTML
{
	my ( $outfile, $infile ) = @_;

	open ( OUT, ">$outfile" ) || die "Couldn't open $outfile for".
			"writing.\n";

	newPgHeader( *OUT{IO}, "Source: $infile", "", "", \%toclinks );
	makeHeader( *OUT{IO}, $infile );

	print OUT $docBotty;
	close OUT;
}

1;
