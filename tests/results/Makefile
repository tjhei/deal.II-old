# $Id$

# Generate links to identical output files

all: .links


# if one calls linkfiles-TARGET-to-SOURCE, then it looks into the target
# directory, and for each output file it does not find there but in
# the source directory, it creates a link from the source to the
# target directory. This way, one has only to store the files that
# differ and cross-link all the other files.
linkfiles-%:
	@TARGET=`echo $@ | $(PERL) -pi -e 's/linkfiles-//; s/-to-.*//;'` ; \
	SOURCE=`echo $@ | $(PERL) -pi -e 's/.*-to-//;'` ; \
	cd $$SOURCE ; \
	for i in */*output ; do \
		if test ! -f ../$$TARGET/$$i ; then \
			if test -L ../$$TARGET/$$i ; then \
				rm ../$$TARGET/$$i ; \
			fi ; \
			ln -s ../$$SOURCE/$$i ../$$TARGET/$$i ; \
		fi ; \
	done


# if called with linksdirs-TARGET-to-SOURCE, creates a link from the source
# directory to the target directory
linkdirs-%:
	@TARGET=`echo $@ | $(PERL) -pi -e 's/linkfiles-//; s/-to-.*//;'` ; \
	SOURCE=`echo $@ | $(PERL) -pi -e 's/.*-to-//;'` ; \
	if test -L $$TARGET ; then rm $$TARGET ; fi ; \
	ln -s $$SOURCE $$TARGET

.links:   linkfiles-i686-pc-linux-gnu+icc7.1-to-i686-pc-linux-gnu+gcc3.2 \
	  linkfiles-i686-pc-linux-gnu+gcc3.4-to-i686-pc-linux-gnu+gcc3.2 \
	  linkdirs-sparc-sun-solaris2.8+gcc2.95-to-sparc-sun-solaris2.7+gcc2.95 \
	  linkdirs-sparc-sun-solaris2.9+gcc2.95-to-sparc-sun-solaris2.7+gcc2.95 \
	  linkdirs-i686-pc-linux-gnu+icc7-to-i686-pc-linux-gnu+icc7.1
	(cd i686-pc-linux-gnu+gcc2.95; sh .link)
	touch .links

clean:
	(cd i686-pc-linux-gnu+gcc2.95; sh .unlink)
	rm .links


include ../../common/Make.global_options