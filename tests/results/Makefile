# $Id$

# Generate links to identical output files

all: .links


# if one calls link-TARGET-to-SOURCE, then it looks into the target
# directory, and for each output file it does not find there but in
# the source directory, it creates a link from the source to the
# target directory. This way, one has only to store the files that
# differ and cross-link all the other files.
link-%:
	@TARGET=`echo $@ | $(PERL) -pi -e 's/link-//; s/-to-.*//;'` ; \
	SOURCE=`echo $@ | $(PERL) -pi -e 's/.*-to-//;'` ; \
	cd $$SOURCE ; \
	for i in */*output ; do \
		if test ! -f ../$$TARGET/$$i ; then \
			ln -s $$i ../$$TARGET/$$i ; \
		fi ; \
	done


.links: link-i686-pc-linux-gnu+icc7.1-to-i686-pc-linux-gnu+gcc3.2
	ln -s sparc-sun-solaris2.7+gcc2.95 sparc-sun-solaris2.8+gcc2.95
	ln -s sparc-sun-solaris2.7+gcc2.95 sparc-sun-solaris2.9+gcc2.95
	(cd i686-pc-linux-gnu+gcc2.95; sh .link)
	touch .links

clean:
	(cd i686-pc-linux-gnu+gcc2.95; sh .unlink)
	rm sparc-sun-solaris2.8+gcc2.95
	rm sparc-sun-solaris2.9+gcc2.95
	rm .links


include ../../common/Make.global_options