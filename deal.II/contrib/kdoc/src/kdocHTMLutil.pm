
=head1 kdocHTMLutil - Common HTML routines.

=cut

package kdocHTMLutil;

use kdocAstUtil;
use Carp;
use Iter;
use strict;
no strict qw/ subs/;
 
use vars qw( $VERSION @ISA @EXPORT $rcount $docNode $rootnode $comp *CLASS );

BEGIN {
	$VERSION = '$Revision$';
	@ISA = qw( Exporter );
	@EXPORT = qw( makeReferences refName refNameFull refNameEvery hyper 
			esc printDoc printTextItem wordRef textRef deref 
			encodeURL newPgHeader tabRow makeHeader 
			HeaderPathToHTML writeTable makeSourceReferences B);

	$rcount = 0;
}

## generic HTML generator routines

sub newPgHeader
{
	my ( $html, $heading, $desc, $rest, $toclist ) = @_; 
	my $bw=0;
	my $cspan = defined $main::options{"html-logo"} ? 2 : 1;

	print $html <<EOF;
<HTML>
<HEAD>
<TITLE>$heading</TITLE>
<META NAME="Generator" CONTENT="KDOC $main::version">
</HEAD>
<BODY bgcolor="#ffffff" text="#000000" link="#0000ff" vlink="#000099" alink= "#ffffff">
<TABLE WIDTH="100%" BORDER="$bw">
<TR>
<TD>
	<TABLE BORDER="$bw">
		<TR><TD valign="top" align="left" cellspacing="10">
		<h1>$heading</h1>
		</TD>
		<TD valign="top" align="right" colspan="1">$desc</TD></TR>
	</TABLE>
	<HR>
	<TABLE BORDER="$bw">
		$rest
	</TABLE>
	</TD>
EOF

#	print $html '<TABLE BORDER="',$bw,'"><TR><TD>';
	my @klist = keys %$toclist;

	print $html '<TD align="right"><TABLE BORDER="',$bw,'">';

	# image
	print $html '<TD rowspan="', ($#klist)+2,'"><IMG SRC="',
			$main::options{"html-logo"},'"></TD>'
		if defined $main::options{"html-logo"};

	# TOC

	foreach my $item ( sort @klist ) {
		print $html '<TR><TD>',
		'<small><A HREF="',$toclist->{$item},'">',
			$item, "</small></TD></TR>\n";
	}

	print $html "</TABLE></TD></TR></TABLE>\n";
	
}

sub writeTable
{
	my ( $file, $list, $columns ) = @_;

	my ( $ctr, $size ) = ( 0, int(($#$list+1)/$columns) );
	$size = 1 if $size < 1;

# spread out unallocated items across columns.
# The old behaviour was to dump them in the last column.
	my $s = $size * $columns;
	$size++ if $s < ($#$list+1);

	print $file '<TABLE WIDTH="100%" BORDER="0"><TR>';

	while ( $ctr <= $#$list ) {
		print $file '<TD VALIGN="top">';
		$s = $ctr+$size-1;

		if ( $s > $#$list ) {
			$s = $#$list;
		}
		elsif ( ($#$list - $s) < $columns) {
			$s = $#$list;
		}

		writeListPart( $file, $list, $ctr, $s );
		print $file "</TD>";
		$ctr = $s+1;
	}

	print $file '</TR></TABLE>';
}

=head3

	Parameters: fd, list, start index, end index

	Helper for writeClassList.  Prints a table containing a
	hyperlinked list of all nodes in the list from start index to
	end index. A table header is also printed.

=cut

sub writeListPart
{
	my( $file, $list, $start, $stop ) = @_;

	print $file "<TABLE BORDER=\"0\">";

	print $file '<TR bgcolor="b0b0b0"><TH>', 
		esc( $list->[ $start ]->{astNodeName} ),
		" - ", esc( $list->[ $stop ]->{astNodeName} ), 
		"</TH></TR>";

	my $col = 0;
	my $colour = "";
	
	for my $ctr ( $start..$stop ) {
		$col = $col ? 0 : 1;
		$colour = $col ? "" : 'bgcolor="#eeeeee"';

		print $file "<TR $colour><TD>", refNameFull( $list->[ $ctr ] ),
			"</TD></TR>\n";
	}

	print $file "</TABLE>";
}


=head2 makeReferences

	Parameters: rootnode

	Recursively traverses the Kids of the root node, setting
	the "Ref" property for each. This is the HTML reference for
	the node. 

	A "NumRef" property is also set for non-compound members,
	which is used for on-page links.

=cut

sub makeReferences
{
	my ( $rootnode ) = @_;

	$rootnode->AddProp( "rcount", 0 );

	return Iter::Tree ( $rootnode, 1, 
		sub { 					# common
			my ( $root, $node ) = @_;

			$root->{rcount}++; 
			$node->AddProp( 'NumRef', "#ref".$root->{rcount} ); 

			return;
		},
		sub { 					# compound
			my ( $root, $node ) = @_;
			return if defined $node->{ExtSource};

			my @heritage = kdocAstUtil::heritage( $node );
			
			foreach my $n ( @heritage ) { $n = encodeURL( $n );	}
			$node->AddProp( "Ref", join( "__", @heritage ). ".html" );

			$node->AddProp( "rcount", 0 );

			return;
		},
		sub {					# member
			my ( $root, $node ) = @_;
			$node->AddProp( 'Ref', $root->{Ref}.
					"#".encodeURL($node->{astNodeName}) )
				unless defined $node->{ExtSource};

			return;
		}
	);
}

sub makeSourceReferences
{
	my( $rootnode ) = shift;

	return if !exists $rootnode->{Sources};

	# Set up references

	foreach my $header ( @{$rootnode->{Sources}} ) {
		my $htmlname = HeaderPathToHTML( $header->{astNodeName} );
		$header->AddProp( "Ref", $htmlname );
	}


}


=head2 refName

	Parameters: node, refprop?

	Returns a hyperlinked name of the node if a reference exists,
	or just returns the name otherwise. Useful for printing node names.

	If refprop is specified, it is used as the reference property
	instead of 'Ref'.

=cut

sub refName
{
	my ( $node ) = @_;
	confess "refName called with undef" unless defined $node->{astNodeName};

	my $ref = defined $_[1] ? $_[1] : 'Ref';

	$ref = $node->{ $ref };

	my $out;

	if ( !defined $ref ) {
		$out =  $node->{astNodeName};
	} else {
		$out = hyper( encodeURL($ref), $node->{astNodeName} );
	}

	$out = "<i>".$out."</i>" if exists $node->{Pure};

	return $out;

}

=head2 refNameFull

	Parameters: node, rootnode, refprop?

	Returns a hyperlinked, fully qualified (ie including parents)
	name of the node if a reference exists, or just returns the name
	otherwise. Useful for printing node names.

	If refprop is specified, it is used as the reference property
	instead of 'Ref'.

=cut

sub refNameFull
{
	my ( $node, $rootnode, $refprop ) = @_;

	my $ref = defined $refprop ? $refprop : 'Ref';
	$ref = $node->{ $ref };
	my $name = join( "::", kdocAstUtil::heritage( $node ) );

	my $out;

	if ( !defined $ref ) {
		$out =  esc($name);
	} else {
		$out = hyper( encodeURL( $ref ), $name );
	}

	$out = "<i>".$out."</i>" if exists $node->{Pure};

	return $out;
}


=head2 refNameEvery

	Parameters: node

	Like refNameFull, but every separate link in the chain is
	referenced.

=cut

sub refNameEvery
{
	my ( $node, $rootnode ) = @_;



	# make full name
	my $name = $node->{astNodeName};

	my $parent = $node->{Parent};

	while ( $parent != $rootnode ) {
		$name = refName($parent)."::".$name;
		$parent = $parent->{Parent};
	}

	return $name;
}

=head2 hyper

	Parameters: hyperlink, text

	Returns an HTML hyperlink. The text is escaped.

=cut

sub hyper
{
	confess "hyper: undefed parameter $_[0], $_[1]"
		unless defined $_[0] && defined $_[1];
	return "<A HREF=\"$_[0]\">".esc($_[1])."</A>";
}


sub B
{
		my $tag = shift;

		return "<$tag>". join( "", @_). "</$tag>";
}

=head2 esc

	Escape special HTML characters.

=cut

sub esc
{
	my $str = $_[ 0 ];

	return "" if !defined $str || $str eq "";

	$str =~ s/&/&amp;/g;
	$str =~ s/</&lt;/g;
	$str =~ s/>/&gt;/g;

	return $str;
}


=head2 printDoc

	Parameters: docnode, *filehandle, rootnode, compound

	Print a doc node. If compound is specified and non-zero, various
	compound node properties are not printed.

=cut

sub printDoc
{
	my $docNode = shift;
	local ( *CLASS, $rootnode ) = @_;
        my ( $comp ) = @_;

        my $type;
	my $lasttype = "none";

	$comp = defined $comp? $comp : 0;

	if ( defined $docNode->{Main} ) {
		print CLASS "<H2>", 
			deref( $docNode->{Main}, $rootnode ), "</H2>\n";
	}
	
	my $text = $docNode->{Text};

        if ( defined $text ) {
                print CLASS "<p>";

                foreach my $node ( @$text ) {
                        $type = $node->{NodeType};
			my $name = $node->{astNodeName};
                        warn "Node '", $name, "' has no type"
				if !defined $type;

			if( $lasttype eq "ListItem" && $type ne $lasttype ) {
				print CLASS "</ul>\n\n<p>\n";
			}

                        if( $type eq "DocText" ) {
                                print CLASS "", deref( $name, $rootnode );
                        }
                        elsif ( $type eq "Pre" ) {
                                print CLASS "</p>\n\n<pre>\n",
					esc( $name ), "\n</pre>\n\n<p>\n";
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
			elsif ( $type eq "DocSection" ) {
				print CLASS "</p>\n\n<H3>",
					deref( $name, $rootnode),"</H3>\n<p>\n";
			}
			elsif ( $type eq "Image" ) {
				print CLASS "</p>\n<img url=\"",
					$node->{Path}, "\">\n<p>\n";
			}
                        elsif ( $type eq "ParaBreak" ) {
                                print CLASS "</p>\n\n<p>";
                        }
			elsif ( $type eq "ListItem" ) {
				if ( $lasttype ne "ListItem" ) {
					print CLASS "</p>\n\n<ul>\n";
				}
				print CLASS "<li>", 
					deref( $name, $rootnode ), "</li>\n";
			}

			$lasttype = $type;
                }

		if( $type eq "ListItem" ) {
			print CLASS "</ul>\n\n<p>\n";
		}

                print CLASS "</p>\n";

        }


	# Params
	my @paramlist = ();
	kdocAstUtil::findNodes( \@paramlist, $docNode->{Text},
			"NodeType", "Param" );

	if( $#paramlist >= 0 ) {
		my $pnode;
		print CLASS "<p><b>Parameters</b>:",
			"<TABLE BORDER=\"0\" CELLPADDING=\"5\">\n";
		
		foreach $pnode ( @paramlist ) {
			print CLASS "<TR><TD align=\"left\" valign=\"top\"><i>",
				esc($pnode->{Name}),
				"</i></TD><TD align=\"left\" valign=\"top\">",
				deref($pnode->{astNodeName}, $rootnode ),
				"</TD></TR>\n";
		}
		print CLASS "</TABLE></P>\n";
	}

	# Return
	printTextItem( $docNode, *CLASS, "Returns" );

	# Exceptions
	$text = $docNode->{Throws};

	if ( defined $text ) {
		my $comma = "<p><b>Throws</b>: ";

		foreach my $tosee ( @$text ) {
			print CLASS $comma, esc( $tosee );
			$comma = ", ";
		}
		print CLASS "</p>\n";
	}

	# See
	my $comma = "";
	
	Iter::SeeAlso ( $docNode, undef,
		sub { # start
			print CLASS "<p><b>See also</b>: ";
		},
		sub { # print
			my ( $label, $ref ) = @_;
			$label = defined $ref ? refName( $ref ): esc( $label );

			print CLASS $comma, $label;
			$comma = ", ";
		},
		sub { # end
			print CLASS "</p>\n";
		}
	);
	
	return if $comp;

	printTextItem( $docNode, *CLASS, "Since" );
	printTextItem( $docNode, *CLASS, "Version" );
	printTextItem( $docNode, *CLASS, "Id" );
	printTextItem( $docNode, *CLASS, "Author" );
}

=head3 printTextItem

	Parameters: node, *filehandle, prop, label

	If prop is set, it prints the label and the prop value deref()ed.

=cut

sub printTextItem
{
	my $node = shift;
	local *CLASS = shift;
	my ( $prop, $label ) = @_;

	my $text = $node->{ $prop };
	
	return unless defined $text;
	$label = $prop unless defined $label;
	
	print CLASS "<p><b>", $label, "</b>: ", 
			deref( $text, $rootnode  ), "</p>\n";
}


=head3 wordRef

	Parameters: word

	Prints a hyperlink to the word's reference if found, otherwise
	just prints the word. Good for @refs etc.

=cut

sub wordRef
{
	my ( $str, $rootnode ) = @_;
	confess "rootnode is undef" if !defined $rootnode;

	return "" if $str eq "";

	my $ref = kdocAstUtil::findRef( $rootnode, $str );

	return esc($str) if !defined $ref;

	warn fullName( $ref ). " hasn't a reference." unless defined $ref->{Ref};

	# return a hyperlinked reference to $str if that has a node with some 
        # source. if it has no source, then it is most probably a forward
	# declaration, and in any case we don't have a file to which we can link
	if (defined $ref->{Ref} && defined $ref->{Source} ) {
	    return hyper( $ref->{Ref}, $str );
	}
	else {
	    return esc($str);
	}
}

=head2 textRef

	Parameters: string
	Returns: hyperlinked, escaped text.

	Tries to find a reference for EVERY WORD in the string, replacing it
	with a hyperlink where possible. All non-hyper text is escaped.

	Needless to say, this is quite SLOW.

=cut

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

=head2 deref

	Parameters: text
	returns text

	dereferences all @refs in the text and returns it.

=cut

sub deref
{
	my ( $str, $rootnode ) = @_;
	confess "rootnode is null" if !defined $rootnode;
	my $out = "";
	my $text;

	# escape @x commands. by using the `split' command, we get a
	# list of strings that contain either the matched command or the
	# string in between. if an element of this list matches a
	# command, then we process it further, otherwise we simply
	# copy it over to the output
	#
	# note the special treatment of the @item command as that does not
	# take a parameter
        foreach $text ( split (/(\@item|\@\w+(?:\s+.+?(?=\s)|\{.*?\}))/, $str ) ) {
	        # check whether $text is an @command or the text between
	        # @commands
		if (  $text =~ /\@(item)|\@(\w+)(?:\s+(.+?)(?:\s|$)|\{(.*?)\})/ )   {
                        my $command = $1 . $2;
			my $content = $3 . $4;

			# @ref -- cross reference
			if ( $command eq "ref" ) {
			    $content =~ s/^\s*#//g;
			    $out .= wordRef( $content, $rootnode );
			}

			# @p  -- typewriter
			elsif ( $command eq "p" ) {
			    $out .= "<code>".esc($content)."</code>";
			}

			# @em -- emphasized
			elsif ( $command eq "em" ) {
			    $out .= "<em>".esc($content)."</em>";
			}

			# @sect1-4 -- section header
			elsif ( $command =~ /^sect([1-4])$/ ) {
			    $out .= "<h$1>".esc($content)."</h$1>";
			}

			# @begin{...} -- start environment
			elsif ( $command =~ /^begin$/ ) {
			    if ( $content eq "itemize" ) {
				$out .= "<ul>";
			    }
			    elsif ( $content eq "enumerate" ) {
				$out .= "<ol>";
			    }
			    elsif ( $content eq "verbatim" ) {
				$out .= "<pre><code>";
			    }
			    else {
				print "Unknown command @", "$command($content)\n";
				$out .= esc($text);
			    }
			}

			# @end{...} -- close environment
			elsif ( $command =~ /^end$/ ) {
			    if ( $content eq "itemize") {
				$out .= "</ul>";
			    }
			    elsif ( $content eq "enumerate") {
				$out .= "</ol>";
			    }
			    elsif ( $content eq "verbatim") {
				$out .= "</code></pre>";
			    }
			    else {
				print "Unknown command @", "$command($content)\n";
				$out .= esc($text);
			    }
			}

			# @item -- start an item in an itemized or
			# enumerated list. note that this is special
			# as @item does not take an argument, which is
			# the reason why we have treated it specially
			# above 
			elsif ( $command =~ /^item$/ ) { 
			    $out .= "<li>"; 
			}

			# unknown command. warn and copy command
			else {
			    print "Unknown command @", $command, "\n";
			    $out .= esc($text);
			}
		}
		# no @x command, thus regular text. simply forward it
		else {
			$out .= esc($text);
		}
	}

	return $out;
}

=head2 encodeURL

	Parameters: url

	Returns: encoded URL

=cut

sub encodeURL
{
	my $url = shift;
	$url =~ s/:/%3A/g;
	$url =~ s/</%3C/g;
	$url =~ s/>/%3E/g;
	$url =~ s/ /%20/g;
	$url =~ s/%/%25/g;

	return $url;
}

=head2 tabRow

	Returns a table row with each element in the arg list as
	one cell.
	
=cut

sub tabRow
{
	return "<TR><TH>$_[0]</TH><TD>$_[1]</TD></TR>\n";
}

=head2 makeHeader

	Writes an HTML version of a file.

=cut

sub makeHeader
{
	my ( $out, $filename ) = @_;

	open ( SOURCE, "$filename" ) || die "Couldn't read $filename\n";

	print $out "<pre>\n";

	while ( <SOURCE> ) {
		print $out esc( $_ );
	}

	print $out "</pre>\n";
}

=head2 HeaderPathToHTML

	Takes the path to a header file and returns an html file name.

=cut

sub HeaderPathToHTML
{
	my ( $path ) = @_;

	$path =~ s/_/__/g;
	$path =~ s/\//___/g;
	$path =~ s/\./_/g;
	$path =~ s/:/____/g;

	return $path.".html";
}

# for printing debug node.

sub fullName
{
		return join( "::", kdocAstUtil::heritage( shift ) );
}

1;
