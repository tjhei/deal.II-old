# $Id$

# include paths and global variables
include ../../common/Make.global_options

# check xrefs in all subdirs
validate-xrefs:
	for dir in * ; do \
	  if test -d $$dir -a ! $$dir = CVS ; then \
	    cd $$dir ; \
	    echo "Checking `pwd`" ; \
	    $(PERL) $D/doc/auto/scripts/validate-xrefs.pl *html ; \
	    cd .. ; \
	  fi ; \
	done
