package Iter;

=head1 Iterator Module

A set of iterator functions for traversing the various trees and indexes.
Each iterator expects closures that operate on the elements in the iterated
data structure.


=head2 Generic

	Params: $node, &$loopsub, &$skipsub, &$applysub, &$recursesub

Iterate over $node's children. For each iteration:
	
If loopsub( $node, $kid ) returns false, the loop is terminated. 
If skipsub( $node, $kid )  returns true, the element is skipped.

Applysub( $node, $kid ) is called
If recursesub( $node, $kid ) returns true, the function recurses into 
the current node.

=cut

sub Generic
{
	my ( $root, $loopcond, $skipcond, $applysub, $recursecond ) = @_;

	return sub {
		foreach my $node ( @{$root->{Kids}} ) {

			if ( defined  $loopcond ) {
				return 0 unless $loopcond->( $root, $node );
			}

			if ( defined $skipcond ) {
				next if $skipcond->( $root, $node );
			}

			my $ret = $applysub->( $root, $node );
			return $ret if defined $ret && $ret;

			if ( defined $recursecond 
					&& $recursecond->( $root, $node ) ) {
				$ret = Generic( $node, $loopcond, $skipcond,
						$applysub, $recursecond)->();
				if ( $ret ) {
					return $ret;
				}
			}
		}

		return 0;
	};
}

sub Class
{
	my ( $root, $applysub, $recurse ) = @_;

	return Generic( $root, undef,
		sub {
			return !( $node->{NodeType} eq "class" 
				|| $node->{NodeType} eq "struct" );
		}, 
		$applysub, $recurse );
}

=head2 Tree

	Params: $root, $recurse?, $commonsub, $compoundsub, $membersub,
		$skipsub

Traverse the ast tree starting at $root, skipping if skipsub returns true.

Applying $commonsub( $node, $kid),
then $compoundsub( $node, $kid ) or $membersub( $node, $kid ) depending on
the Compound flag of the node.

=cut

sub Tree
{
	my ( $rootnode, $recurse, $commonsub, $compoundsub, $membersub, 
		 $skipsub ) = @_;

	my $recsub = $recurse ? sub { return 1 if $_[1]->{Compound}; } 
				: undef;

	Generic( $rootnode, undef, $skipsub,
		sub { 					# apply
			my ( $root, $node ) = @_;
			my $ret;

			if ( defined $commonsub ) {
				$ret = $commonsub->( $root, $node );
				return $ret if defined $ret;
			}

			if ( $node->{Compound} && defined $compoundsub ) {
				$ret = $compoundsub->( $root, $node );
				return $ret if defined $ret;
			}
			
			if( !$node->{Compound} && defined $membersub ) {
				$ret = $membersub->( $root, $node );
				return $ret if defined $ret;
			}
			return;
		},
		$recsub 				# skip
	)->();
}

=head2 LocalCompounds

Apply $compoundsub( $node ) to all locally defined compound nodes
(ie nodes that are not external to the library being processed).

=cut

sub LocalCompounds
{
		my ( $rootnode, $compoundsub ) = @_;

		return unless defined $rootnode && defined $rootnode->{Kids};

		foreach my $kid ( sort { $a->{astNodeName} cmp $b->{astNodeName} }
								 @{$rootnode->{Kids}} ) {
				next if !defined $kid->{Compound};

				$compoundsub->( $kid ) unless defined $kid->{ExtSource};
				LocalCompounds( $kid, $compoundsub );
		}
}

=head2 Hierarchy

	Params: $node, $levelDownSub, $printSub, $levelUpSub

This allows easy hierarchy traversal and printing.

Traverses the inheritance hierarchy starting at $node, calling printsub
for each node. When recursing downward into the tree, $levelDownSub($node) is
called, the recursion takes place, and $levelUpSub is called when the
recursion call is completed. 

=cut

sub Hierarchy
{
	my ( $node, $ldownsub, $printsub, $lupsub, $nokidssub ) = @_;

	return if defined $node->{ExtSource}
		&& (!defined $node->{InBy} 
			|| !kdocAstUtil::hasLocalInheritor( $node ));

	$printsub->( $node );

	if ( defined $node->{InBy} ) {
		$ldownsub->( $node );

		foreach my $kid ( 
				sort {$a->{astNodeName} cmp $b->{astNodeName}}
				@{ $node->{InBy} } ) {
			Hierarchy( $kid, $ldownsub, $printsub, $lupsub );
		}

		$lupsub->( $node );
	}
	elsif ( defined $nokidssub ) {
		$nokidssub->( $node );
	}

	return;
}


sub Ancestors
{
	my ( $node, $rootnode, $noancessub, $startsub, $printsub,
		$endsub ) = @_;
	my @anlist = ();

	return if $node eq $rootnode;

	if ( !exists $node->{InList} ) {
		$noancessub->( $node ) unless !defined $noancessub;
		return;
	}
	
	foreach my $innode ( @{ $node->{InList} } ) {
		my $nref = $innode->{Node};	# real ancestor
		next if defined $nref && $nref == $rootnode;

		push @anlist, $innode;
	}

	if ( $#anlist < 0 ) {
		$noancessub->( $node ) unless !defined $noancessub;
		return;
	}

	$startsub->( $node ) unless !defined $startsub;

	foreach my $innode ( sort { $a->{astNodeName} cmp $b->{astNodeName} }
				@anlist ) {

		# print
		$printsub->( $innode->{Node}, $innode->{astNodeName},
			$innode->{Type}, $innode->{TmplType} ) 
			unless !defined $printsub;
	}

	$endsub->( $node ) unless !defined $endsub;

	return;

}

sub Descendants
{
	my ( $node, $nodescsub, $startsub, $printsub, $endsub ) = @_;

	if ( !exists $node->{InBy} ) {
		$nodescsub->( $node ) unless !defined $nodescsub;
		return;
	}

	
	my @desclist = ();
	DescendantList( \@desclist, $node );
	
	if ( $#desclist < 0 ) {
		$nodescsub->( $node ) unless !defined $nodescsub;
		return;
	}

	$startsub->( $node ) unless !defined $startsub;

	foreach my $innode ( sort { $a->{astNodeName} cmp $b->{astNodeName} }
				@desclist ) {

		$printsub->( $innode) 
			unless !defined $printsub;
	}

	$endsub->( $node ) unless !defined $endsub;

	return;

}

sub DescendantList
{
	my ( $list, $node ) = @_;

	return unless exists $node->{InBy};

	foreach my $kid ( @{ $node->{InBy} } ) {
		push @$list, $kid;
		DescendantList( $list, $kid );
	}
}

=head2 DocTree

=cut

sub DocTree
{
	my ( $rootnode, $allowforward, $recurse, 
		$commonsub, $compoundsub, $membersub ) = @_;
		
	Generic( $rootnode, undef,
		sub {				# skip
			my( $node, $kid ) = @_;

			unless (!(defined $kid->{ExtSource}) 
					&& ($allowforward || $kid->{NodeType} ne "Forward")
					&& ($main::doPrivate || !($kid->{Access} =~ /private/))
					&& exists $kid->{DocNode} ) {

				return 1;
			}

			return;
		},
		sub { 				# apply
			my ( $root, $node ) = @_;

			my $ret;

			if ( defined $commonsub ) {
				$ret = $commonsub->( $root, $node );
				return $ret if defined $ret;
			}

			if ( $node->{Compound} && defined $compoundsub ) {
				$ret = $compoundsub->( $root, $node );
				return $ret if defined $ret;
			}
			elsif( defined $membersub ) {
				$ret = $membersub->( $root, $node );
				return $ret if defined $ret;
			}

			return;
		},
		sub { return 1 if $recurse; return; }	# recurse
		)->();

}

sub MembersByType
{
	my ( $node, $startgrpsub, $methodsub, $endgrpsub, $nokidssub ) = @_;

# public
	# types
	# data
	# methods
	# signals
	# slots
	# static
# protected
# private (if enabled)

	if ( !defined $node->{Kids} ) {
			$nokidssub->( $node ) if defined $nokidssub;
			return;
	}

	foreach my $access ( qw/public protected private/ ) {
		next if $access eq "private" && !$main::doPrivate;

		my @types = ();
		my @data = ();
		my @signals = ();
		my @slots =();
		my @methods = ();
		my @static = ();
		my @modules = ();
		my @interfaces = ();

		# Build lists
		foreach my $kid ( @{$node->{Kids}} ) {
			next if !(($kid->{Access} =~ /$access/
					&& !$kid->{ExtSource})
				|| ( $access eq "public" 
					&& $kid->{Access} eq "signals" ));

			my $type = $kid->{NodeType};

			if ( $type eq "method" ) {
				if ( $kid->{Flags} =~ "s" ) {
					push @static, $kid;
				}
				elsif ( $kid->{Flags} =~ "l" ) {
					push @slots, $kid;
				}
				elsif ( $kid->{Flags} =~ "n" ) {
					push @signals, $kid;
				}
				else {
					push @methods, $kid;
				}
			}
			elsif ( $kid->{Compound} ) {
				if ( $type eq "module" ) {
					push @modules, $kid;
				}
				elsif ( $type eq "interface" ) {
					push @interfaces, $kid;
				}
				else {
					push @types, $kid;
				}
			}
			elsif ( $type eq "typedef" || $type eq "enum" ) {
				push @types, $kid;
			}
			else {
				push @data, $kid;
			}
		}

		# apply
		$access = ucfirst( $access );
		
		doGroup( "$access Types", $node, \@types, $startgrpsub,
			$methodsub, $endgrpsub);
		doGroup( "Modules", $node, \@modules, $startgrpsub,
			$methodsub, $endgrpsub);
		doGroup( "Interfaces", $node, \@interfaces, $startgrpsub,
			$methodsub, $endgrpsub);
		doGroup( "$access Methods", $node, \@methods, $startgrpsub,
			$methodsub, $endgrpsub);
		doGroup( "$access Slots", $node, \@slots, $startgrpsub,
			$methodsub, $endgrpsub);
		doGroup( "Signals", $node, \@signals, $startgrpsub,
			$methodsub, $endgrpsub);
		doGroup( "$access Static Methods", $node, \@static, 
			$startgrpsub, $methodsub, $endgrpsub);
		doGroup( "$access Members", $node, \@data, $startgrpsub,
			$methodsub, $endgrpsub);
	}
}

sub doGroup
{
	my ( $name, $node, $list, $startgrpsub, $methodsub, $endgrpsub ) = @_;

	return if $#$list < 0;

	$startgrpsub->( $name ) if defined $startgrpsub;
	
	if ( defined $methodsub ) {
		foreach my $kid ( @$list ) {
			$methodsub->( $node, $kid );
		}
	}

	$endgrpsub->( $name ) if defined $endgrpsub;
}

sub ByGroupLogical
{
	my ( $root, $startgrpsub, $itemsub, $endgrpsub ) = @_;

	return 0 unless defined $root->{Groups};

	foreach my $groupname ( sort keys %{$root->{Groups}} ) {
		next if $groupname eq "astNodeName"||$groupname eq "NodeType";

		my $group = $root->{Groups}->{ $group };
		next unless $group->{Kids};
		
		$startgrpsub->( $group->{astNodeName}, $group->{Desc} );

		foreach my $kid (sort {$a->{astNodeName} cmp $b->{astNodeName}}
					@group->{Kids} ) {
			$itemsub->( $root, $kid );
		}
		$endgrpsub->( $group->{Desc} );	
	}

	return 1;
}

sub SeeAlso
{
	my ( $node, $nonesub, $startsub, $printsub, $endsub ) = @_;

	if( !defined $node ) {
		$nonesub->();
		return;
	}

	my $doc = $node;

	if ( $node->{NodeType} ne "DocNode" ) {
		$doc = $node->{DocNode};
		if ( !defined $doc ) {
			$nonesub->() if defined $nonesub;
			return;
		}
	}

	if ( !defined $doc->{See} ) {
		$nonesub->() if defined $nonesub;
		return;
	}

	my $see = $doc->{See};
	my $ref = $doc->{SeeRef};

	if ( $#$see < 1 ) {
		$nonesub->() if defined $nonesub;
		return;
	}

	$startsub->( $node ) if defined $startsub;

	for my $i ( 0..$#$see ) {
		my $seelabel = $see->[ $i ];
		my $seenode = undef;
		if ( defined $ref ) {
			$seenode = $ref->[ $i ]; 
		}

		$printsub->( $seelabel, $seenode ) if defined $printsub;
	}

	$endsub->( $node ) if defined $endsub;

	return;
}

1;
