# $Id$

# include paths and global variables
include ../../common/Make.global_options

# check xrefs in all subdirs
validate-xrefs:
	@for dir in * ; do \
	  if test -d $$dir -a ! $$dir = CVS ; then \
	    cd $$dir ; \
	    if (ls -l | grep html > /dev/null) ; then \
	      echo "Checking `pwd`" ; \
	      $(PERL) $D/common/scripts/validate-xrefs.pl *html ; \
            fi ; \
	    cd .. ; \
	  fi ; \
	done
