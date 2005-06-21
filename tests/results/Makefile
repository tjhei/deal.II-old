# Makefile,v 1.10 2003/05/02 15:30:57 wolf Exp

# Generate links to identical output files

all: .links


# if one calls linkfiles-TARGET-to-SOURCE, then it looks into the target
# directory, and for each output file it does not find there but in
# the source directory, it creates a link from the source to the
# target directory. This way, one has only to store the files that
# differ and cross-link all the other files.
#
# in order to find out whether something is a link, we use "test -L ...".
# unfortunately, -L is not available in test when plain sh is used, so work
# around this with a piece of autoconf magic
linkfiles-%:
	@TARGET=`echo $@ | $(PERL) -pi -e 's/linkfiles-//; s/-to-.*//;'` ; \
	SOURCE=`echo $@ | $(PERL) -pi -e 's/.*-to-//;'` ; \
	echo "Creating links from $$SOURCE into $$TARGET" ; \
	cd $$SOURCE ; \
	for i in */*output ; do \
		if test ! -f ../$$TARGET/$$i ; then \
			if $(TEST_MINUS_L) ../$$TARGET/$$i ; then \
				rm ../$$TARGET/$$i ; \
			fi ; \
			ln -s `pwd`/$$i ../$$TARGET/$$i ; \
		fi ; \
	done


# if called with linksdirs-TARGET-to-SOURCE, creates a link from the source
# directory to the target directory
#
# use same hack for test -L as above
linkdirs-%:
	@TARGET=`echo $@ | $(PERL) -pi -e 's/linkdirs-//; s/-to-.*//;'` ; \
	SOURCE=`echo $@ | $(PERL) -pi -e 's/.*-to-//;'` ; \
	echo "Aliasing $$TARGET to $$SOURCE" ; \
	if $(TEST_MINUS_L) $$TARGET ; then rm $$TARGET ; fi ; \
	ln -s $$SOURCE $$TARGET

# This is a list of directories linked to others. The master directories right now are
#    sparc-sun-solaris2.7+gcc2.95
#    i686-pc-linux-gnu+gcc3.2
# They contain no symlinks, but all files are stored.
#
# The second class relies on a master direcory, but has some differing files.
# Therefore, most of their entries are symlinks, the others are files. They are
#    linkfiles-i686-pc-linux-gnu+gcc2.95
#    linkfiles-i686-pc-linux-gnu+gcc3.4
#    i686-pc-linux-gnu+icc7.1
#
# All other configurations produce the same output as another, so there we 
# link whole directories.

.links:   linkfiles-i686-pc-linux-gnu+icc7.1-to-i686-pc-linux-gnu+gcc3.2 \
	  linkfiles-i686-pc-linux-gnu+gcc3.4-to-i686-pc-linux-gnu+gcc3.2 \
	  linkfiles-i686-pc-linux-gnu+gcc4.0-to-i686-pc-linux-gnu+gcc3.4 \
	  linkfiles-i686-pc-linux-gnu+gcc4.1-to-i686-pc-linux-gnu+gcc3.4 \
	  linkfiles-i686-pc-linux-gnu+gcc2.95-to-sparc-sun-solaris2.7+gcc2.95 \
	  linkfiles-mips-sgi-irix6.5+MIPSpro7.4-to-i686-pc-linux-gnu+gcc3.2 \
	  linkdirs-sparc-sun-solaris2.8+gcc2.95-to-sparc-sun-solaris2.7+gcc2.95 \
	  linkdirs-sparc-sun-solaris2.9+gcc2.95-to-sparc-sun-solaris2.7+gcc2.95 \
	  linkdirs-i686-pc-linux-gnu+icc7-to-i686-pc-linux-gnu+icc7.1 \
	  linkdirs-i686-pc-linux-gnu+gcc3.3-to-i686-pc-linux-gnu+gcc3.2
	@touch .links

clean:
	rm .links


include ../../common/Make.global_options
include ../Makefile.paths
