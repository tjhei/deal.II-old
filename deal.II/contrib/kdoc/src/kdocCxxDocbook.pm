
package kdocCxxDocbook;
use Carp;
use File::Path;
use Iter;

use strict;
no strict "subs";

use vars qw/ $lib $rootnode $outputdir $opt $paraOpen/;

=head2 kdocCxxDocBook

	TODO:

	Templates
	Fix tables, add index.
	Global docs
	Groups

=cut

sub writeDoc
{
	( $lib, $rootnode, $outputdir, $opt ) = @_;

	makeReferences( $rootnode );

	$lib = "kdoc-out" if $lib eq "";	# if no library name set

	mkpath( $outputdir) unless -f $outputdir;

	open ( DOC, ">$outputdir/$lib.sgml" ) || die "Couldn't write output.";

	my $time = localtime;

print DOC<<EOF;
<!doctype book PUBLIC "-//Davenport//DTD DocBook V3.0//EN" [
]>
<book id="$lib-lib">
	<bookinfo>
		<date>$time</date>
		<title>$lib API Documentation</title>
	</bookinfo>
EOF

	printLibDoc();
	printHierarchy();
	printClassIndex();
	printClassDoc();

print DOC<<EOF;
</book>
EOF

}

sub printLibDoc
{
	return unless kdocAstUtil::hasDoc( $rootnode );
	
	print DOC chapter( "$lib-intro", "Introduction" );
	printDoc( $rootnode->{DocNode}, *DOC, $rootnode );
	print DOC C( "chapter" );
}


sub printHierarchy
{

	print DOC chapter( "class-hierarchy", "$lib Class Hierarchy" );

	Iter::Hierarchy( $rootnode,
		sub {	# down
			print DOC "<ItemizedList>\n";
		},
		sub {	# print
			my $node = shift;
			return if $node == $rootnode;

			my $src = defined $node->{ExtSource} ? " ($node->{ExtSource})":"";
			print DOC "<ListItem><para>", refName($node), "$src</para>\n"
		},
		sub {	# up
			if ( $_[0] == $rootnode ) {
				print DOC "</ItemizedList>\n";
			}
			else {
				print DOC "</ItemizedList></ListItem>\n";
			}
		},
		sub { print DOC "</ListItem>"; }
	);
		
	print DOC C( "chapter" ); 
}

sub printClassIndex
{
	my @clist = ();
	kdocAstUtil::makeClassList( $rootnode, \@clist );

	print DOC chapter( "class-index", "$lib Class Index" ),
		O( "Table", "TocEntry", 0, "PgWide", 1, "ColSep", 0 ),
		tblk( "title", $lib,' classes' ),
		O( "tgroup", "Cols", 2  ), O( "TBody" );

	foreach my $kid ( @clist ) {

		# Internal, deprecated, abstract
		my $name = refName( $kid );

		if ( $kid->{Abstract} ) {
			$name = tblk( "Emphasis", $name );
		}

		my $extra = "";

		if ( $kid->{Internal} ) {
			$extra .= " internal";
		}

		if ( $kid->{Deprecated} ) {
			$extra .= " ".tblk( "Emphasis", "deprecated" );
		}

		$extra = " [$extra]" unless $extra eq "";
		
		# print class entry
		print DOC tblk( 'Row', tblk( 'ENTRY', $name )
			. tblk( 'ENTRY', deref( $kid->{DocNode}->{ClassShort},
				$rootnode).$extra ));
	}

	print DOC C( "TBody", "TGroup", "Table", "chapter" );
}

sub printClassDoc
{
	print DOC chapter( "class-doc", "$lib Class Documentation" );

	Iter::LocalCompounds( $rootnode, sub { docChildren( shift ); } );

	print DOC C("Chapter"), "\n";
}

sub docChildren
{
	my ( $node ) = @_;

	return if $node == $rootnode;
	
	## document self
	printClassInfo( $node );
	
	if ( kdocAstUtil::hasDoc( $node ) ) {
		printDoc( $node->{DocNode}, *DOC{IO}, $rootnode, 1 );
	}

	# First a member index by type
	print DOC O( "BridgeHead", "Renderas", "Sect2" ),
		"Interface Synopsis", C( "BridgeHead" );

	Iter::MembersByType( $node,
		sub {	# start
		print DOC O( 'Table', 'PgWide', 1, "colsep", 0,
				"rowsep", 0, "tocentry", 0 ),
			tblk( 'Title', fullName($node)," ",$_[0] ),
			O( "TGroup", "cols", 1 ), O( "TBody" );
		},
		\&sumListMember, # print
		sub {	# end
			print DOC C( "TBody", "TGroup", "Table" ), "\n";
		},
		sub {	# no kids
			print DOC tblk( "para", "(Empty interface)" );
		}
	);

	print DOC O( "BridgeHead", "Renderas", "Sect2" );
	print DOC "Member Documentation", C( "BridgeHead" );

	# Then long docs for each member
	Iter::MembersByType ( $node, undef, \&printMemberDoc, undef,
		sub { print DOC tblk( "para", "(Empty interface)" ); }
	);
	
	print DOC C( "Sect1" );

	return;
}

sub printClassInfo
{
	my $node = shift;

	print DOC O( "Sect1", "id", $node->{DbRef},
				"XRefLabel", fullName( $node, "::" ) ), "\n",
		tblk( "Title", esc($node->{NodeType})," ",fullName( $node ) );


	if ( defined $node->{DocNode} &&  $node->{DocNode}->{ClassShort} ) {
		printClassInfoField( "Description", 
			deref( $node->{DocNode}->{ClassShort}, $rootnode ) );
	}

	printClassInfoField( "Header", tblk( "Literal", 
			$node->{Source}->{astNodeName} ));

	my ($text, $comma ) = ("", "");

	Iter::Ancestors( $node, $rootnode, undef, undef,
		sub { # print
			my ( $node, $name, $type ) = @_;
			$name = refName( $node ) if defined $node;
			$name .= " (".$node->{ExtSource}.")" if defined $node->{ExtSource};
			$text .= $comma.$name;
			$text .= " [$type]" unless $type eq "public";

			$comma = ", ";
		},
		sub { # end
			printClassInfoField( "Inherits", $text );
		}
	);
	
	$text = $comma = "";

	Iter::Descendants( $node, undef, undef,
		sub { # print
			my $desc = shift;
			$text .= $comma.refName( $desc );
			$text .= " (".$desc->{ExtSource}.")" if defined $desc->{ExtSource};

			$comma = ", ";
		},
		sub { # end
			printClassInfoField( "Inherited By", $text );
		}
	);

	if ( $node->{Internal} ) {
		printClassInfoField( "Note", "Internal use only." );
	}

	if ( $node->{Deprecated} ) {
		printClassInfoField( "Note", "Deprecated, to be removed." );
	}

	return;
}

sub printClassInfoField
{
	my ( $label, $text ) = @_;

	print DOC tblk( "formalpara", tblk( "title", $label ),
			tblk( "para", " ".$text ) );
}

sub sumListMember
{
	my( $class, $m ) = @_;

	print DOC O( "Row"), O( "Entry" );


	my $type = $m->{NodeType};
	my $name = esc( $m->{astNodeName} );
	my $pname = tblk( "Emphasis", $name );

	if( $type eq "var" ) {
		print DOC tblk( "Literal", esc($m->{Type}) ), " $pname\n";
	}
	elsif( $type eq "method" ) {
		my $flags = $m->{Flags};

		if ( !defined $flags ) {
			warn "Method ".$m->{astNodeName}.
				" has no flags";
		}

		my $extra = "";
		$extra .= "virtual " if $flags =~ "v";
		$extra .= "static " if $flags =~ "s";

		my $params = esc( $m->{Params} );
		$params =~ s/^\s+//g;
		$params =~ s/\s+$//g;
		$params = " $params " unless $params eq "";
		my $c = $flags =~ /c/ ? " const" : "";
		my $p = $flags =~ /p/ ? " [pure]" : "";


		print DOC esc($m->{ReturnType}), "  $pname\($params\)$c$p\n";
	}
	elsif( $type eq "enum" ) {
		my $n = $name eq "" ? "" : $pname." ";

		print DOC tblk("Literal","enum"),
			" $n\{ ",tblk("Literal",esc($m->{Params}))," }";
	}
	elsif( $type eq "typedef" ) { 
		print DOC tblk( "Literal", "typedef " ), esc($m->{Type}), 
				tblk( "Emphasis", $name );
	}
	else { 
		# unknown type
		print DOC tblk( "Literal", esc( $type ) )," $pname\n";
	}

	print DOC C( "Entry", "Row" ),"\n";

	return;
}

=head2 printMemberDoc

	params: classnode, membernode

	Prints title and long documentation for one class member.

=cut

sub printMemberDoc
{
	my ( $class, $mem ) = @_;

	return unless kdocAstUtil::hasDoc( $mem );

	print DOC O( "BridgeHead",
			"id",			$mem->{DbRef},
			"XRefLabel",	fullName( $mem, "::" ),
			"Renderas",		"Sect3" );

	# title
	my $type = $mem->{NodeType};
	my $name = esc( $mem->{astNodeName} );
	my $pname = tblk( "Emphasis", $name );

	if( $type eq "var" ) {
		print DOC tblk( "Literal", esc($mem->{Type}) ), " $pname\n";
	}
	elsif( $type eq "method" ) {
		my $flags = $mem->{Flags};

		if ( !defined $flags ) {
			warn "Method ".$mem->{astNodeName}.
				" has no flags";
		}

		my $extra = "";
		$extra .= "virtual " if $flags =~ "v";
		$extra .= "static " if $flags =~ "s";

		my $params = $mem->{Params};
		$params =~ s/^\s+//g;
		$params =~ s/\s+$//g;
		$params = " $params " unless $params eq "";
		my $c = $flags =~ /c/ ? " const" : "";
		my $p = $flags =~ /p/ ? " [pure]" : "";

		print DOC deref($mem->{ReturnType}, $rootnode), "  $pname\(".
			deref( $params, $rootnode )."\)$c$p\n";
	}
	elsif( $type eq "enum" ) {
		my $n = $name eq "" ? "" : $pname." ";

		print DOC tblk("Literal","enum"),
			" $n\{ ",tblk("Literal",esc($mem->{Params}))," }";
	}
	elsif( $type eq "typedef" ) { 
		print DOC tblk( "Literal", "typedef" ), " ", esc($mem->{Type}), $pname;
	}
		# TODO nested compounds
	else { 
		# unknown type
		print DOC tblk( "Literal", esc( $type ) )," $pname\n";
	}

	print DOC C( "BridgeHead" );

	# documentation
	printDoc( $mem->{DocNode}, *DOC, $rootnode );

	if ( $type eq "method" ) {
		my $ref = kdocAstUtil::findOverride( $rootnode,
				$class, $mem->{astNodeName} );
		if ( defined $ref ) {
			print DOC tblk( "formalpara",
				tblk( "title", "Reimplemented from" ),
				tblk( "para", fullName( $ref, "::" )) ), "\n";
		}
	}
}

=head2 printDoc

Parameters: docnode, *filehandle, rootnode, compound

Print a doc node. If compound is specified and non-zero, various
compound node properties are not printed.

=cut

sub printDoc
{
	my $docNode = shift;
	local *CLASS = shift;

        my ( $rootnode, $comp ) = @_;
        my $node;
        my $type;
	my $text;
	my $lasttype = "none";

	$comp = 0 if !defined $comp;
	
	$text = $docNode->{Text};

        if ( defined $text ) {
		$paraOpen = 0;

                foreach $node ( @$text ) {
                        $type = $node->{NodeType};
			my $name = $node->{astNodeName};
                        warn "Node '", $name, "' has no type"
				if !defined $type;

			if( $lasttype eq "ListItem" && $type ne $lasttype ) {
				print CLASS "</ItemizedList>\n";
			}

                        if( $type eq "DocText" ) {
                                print CLASS "", pc(), po(), 
					deref( $name, $rootnode );
                        }
                        elsif ( $type eq "Pre" ) {
				print CLASS "", pc(), 
					tblk( "ProgramListing", esc( $name ) );
                        }
			elsif( $type eq "DocSection" ) {
				print CLASS "", pc(), O( "BridgeHead",
							"Renderas", "Sect4" ),
					deref( $name, $rootnode ),
					C( "BridgeHead" ),"\n";
			}
			elsif( $type eq "Ref" ) {
				my $ref = $node->{Ref};
				if ( defined $ref ) {
					print CLASS refName( $ref );
				}
				else {
					print CLASS $name;
				}
			}
			elsif ( $type eq "Image" ) {
				print CLASS pc(),"<Graphic FileRef=\"",
					$node->{Path}, "\"></Graphic>";
			}
			elsif ( $type eq "ListItem" ) {
				if ( $lasttype ne "ListItem" ) {
					print CLASS "", pc(),"<ItemizedList>\n";
				}
				print CLASS "", tblk( "ListItem",
					tblk("para", deref($name,$rootnode )) );
			}
                        elsif ( $type eq "ParaBreak" || $type eq "Param" ) {
				# ignore parabreak, they're taken
				# care of already.

				# ignore parameters, handled later
			}
			else {
				warn "Unhandled doc type $type\n";
			}

			$lasttype = $type;
                }

		if( $lasttype eq "ListItem" ) {
			print CLASS "", pc(), "</ItemizedList>";
		}

		print CLASS "", pc();
        }

	# Params
	my @paramlist = ();
	kdocAstUtil::findNodes( \@paramlist, $docNode->{Text},
			"NodeType", "Param" );

	if( $#paramlist >= 0 ) {
		my $pnode;
		print CLASS "", O( 'Table', 'PgWide', 0, "colsep", 0,
			"frame", "none", "tocentry", 0 ),
			tblk( 'Title', 'Parameters' ),
			O( "TGroup", "cols", "2" ), O( "TBody" );

		foreach $pnode ( @paramlist ) {
			print CLASS "<ROW><ENTRY>", esc($pnode->{Name}),
				"</ENTRY><ENTRY>",
				deref($pnode->{astNodeName}, $rootnode ),
				"</ROW>\n";
		}
		print CLASS "", C( "TBody", "TGroup", "Table" );
	}

	# Return
	printTextItem( $docNode, *CLASS, "Returns" );

	# Exceptions
	$text = $docNode->{Throws};

	if ( defined $text ) {
		my $comma = "<formalpara><title>Exceptions</title><para>";

		foreach my $tosee ( @$text ) {
			print CLASS $comma, esc( $tosee );
			$comma = ", ";
		}
		print CLASS C( "para", "formalpara" );
	}

	# See
	my $comma = "";
	
	Iter::SeeAlso ( $docNode, undef,
		sub { # start
			print CLASS "", O( "Tip" ),tblk( "Title", "See Also"),O( "Para" );
		},
		sub { # print
			my ( $label, $ref ) = @_;
			$label = defined $ref ? refName( $ref ):esc( $label );

			print CLASS $comma, $label;
			$comma = ", ";
		},
		sub { # end
			print CLASS "", C( "Para", "Tip" );
		}
	);
	
	printTextItem( $docNode, *CLASS, "Since" );
	printTextItem( $docNode, *CLASS, "Version" );
	printTextItem( $docNode, *CLASS, "Id" );
	printTextItem( $docNode, *CLASS, "Author" );
}

sub makeReferences
{
	my $root = shift;
	my $idcount = 0;

	return Iter::Tree( $root, 1,
		sub {
				my ( $parent, $node ) = @_;
				return if $node->{ExtSource};

				$idcount++;

				$node->AddProp( "DbRef", "docid-$idcount" );

				print fullName( $node ), " = ", $node->{DbRef},"\n";

				return;
		}
	);
}

sub printTextItem
{
	my $node = shift;
	local *CLASS = shift;
	my ( $prop, $label ) = @_;

	my $text = $node->{ $prop };
	
	return unless defined $text;
	$label = $prop unless defined $label;
	
	print CLASS "<formalpara><title>", $label, "</title><para> ", 
		deref( $text, $rootnode  ), "</para></formalpara>\n";
}


# utilities


sub refName
{
	my( $node ) = @_;

	return fullName( $node ) if defined $node->{ExtSource}
		|| !kdocAstUtil::hasDoc( $node );

	return '<xref LinkEnd="'.$node->{DbRef}.'">';
}

sub fullName
{
	my( $node, $sep ) = @_;

	$sep = "::" unless defined $sep;

	my @heritage = kdocAstUtil::heritage( $node );
	foreach my $n ( @heritage ) { $n = esc( $n ); }

	return join( $sep, @heritage );
}

=head2 deref

	Parameters: text, rootnode
	returns text

	dereferences all @refs in the text and returns it.

=cut

sub deref
{
	my ( $str, $rootnode ) = @_;
	confess "rootnode is null" if !defined $rootnode;
	my $out = "";
	my $text;

	foreach $text ( split (/(\@\w+\s+[\w:#]+)/, $str ) ) {
		if ( $text =~ /\@ref\s+([\w:#]+)/ ) {
			my $name = $1;
			$name =~ s/^\s*#//g;
			$out .= wordRef( $name, $rootnode );
		}
		elsif ( $text =~ /\@p\s+([\w:]+)/ ) {
			$out .= tblk( "Literal", esc($1) );
		}
		else {
			$out .= esc($text);
		}
	}

	return $out;
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

	return refName( $ref );
}


=head2 esc

	Escape special SGML characters for normal text.

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


sub chapter
{
	my ( $id, $title ) = @_;

	return "<chapter id=\"$id\"><title>$title</title>";
}


=head2 tblk

	Params: tagname, text..

	Inserts text in a block tag of type tagname (ie both
	opening and closing tags)
=cut

sub tblk
{
	my $tag = shift;
	return "<$tag>".join( "", @_ )."</$tag>";
}


=head2 O

	Params: tagname, list of id and idtext interspersed.

=cut

sub O
{
	my $tag = shift;

	carp "mismatched ids and tags" if ($#_+1) % 2 != 0;

	my $out = "<$tag";

	while ( $#_ >= 0 ) {
		$out .= " ".shift( @_ ).'="'.shift( @_ ).'"';
	}

	$out .= ">";

	return $out;
}

=head2 C

	Params: list of tagnames

	returns a list of close tags in the order specified in the
	params.

=cut

sub C
{
	my $out = "";

	foreach my $tag ( @_ ) {
		$out .= "</$tag>";
	}

	return $out;
}

sub po
{
	my $text = $paraOpen ? "" : "<para>";
	$paraOpen = 1;

	return $text;
}

sub pc
{
	my $text = $paraOpen ? "</para>" : "";
	$paraOpen = 0;

	return $text;
}
1;
