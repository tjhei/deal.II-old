# $Id$
# Makefile for the /contrib subdirectory


# by default: check whether the updated kdoc is up-to-date with the
# sources
default: bin/kdoc


# let's see whether the installed kdoc is as recent as the sources. we
# check so by comparing the installation date of the executable with
# the modification dates of the source `kdoc' and the subpackages used
# by it. if it is not up-to-date, then install it afresh
#
# note that we only check whether bin/kdoc is up-to-date. this
# suffices, as all the other files are installed at the same time and
# all-or-none, so they should have the same time stamp
bin/kdoc: src/kdoc $(shell echo src/kdoc*.pm)
	@echo ================== Remaking kdoc ====
	$(MAKE) install

# specific targets
install:
	cd src ; $(MAKE) ; $(MAKE) install

clean:
	cd src ; $(MAKE) clean
	-rm -r bin doc man share


