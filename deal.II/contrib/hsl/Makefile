# $Id$
# Copyright (C) 2001, 2002, 2003 by Wolfgang Bangerth


D = ../..

include $D/common/Make.global_options

ifneq ($(with-multithreading),no)
  MT = MT
else
  MT = ==
endif



# get lists of files we need
f-files     = $(shell a="`echo source/*.f`"; if test "$$a" != "source/*.f" ; then echo $$a ; fi)
o-files     = $(f-files:source/%.f=$(LIBDIR)/contrib/hsl/%.$(OBJEXT))



$(LIBDIR)/contrib/hsl/%.$(OBJEXT) :
	@echo =====hsl==============optimized==$(MT)== $(<F)
	@$(F77) $(F77FLAGS.o) -c $< -o $@


# rules
ifeq ($(enable-shared),yes)
  lib: $(LIBDIR)/libhsl.so $(LIBDIR)/bin/detached_ma27$(EXEEXT)
else
  lib: $(LIBDIR)/libhsl.a $(LIBDIR)/bin/detached_ma27$(EXEEXT)
endif


$(LIBDIR)/libhsl.a: $(forward-declarations) $(o-files)
	@echo =====hsl==============optimized==$(MT)== Linking library:   $(@F)
	@ar ru $@ $(o-files)

# link the object files to a shared library. since this is not C++,
# simply use the C compiler, rather than $(SHLIBLD)
$(LIBDIR)/libhsl.so: $(forward-declarations) $(o-files)
	@echo =====hsl==============optimized==$(MT)== Linking library:   $(@F)
	@$(SHLIBLD) $(LDFLAGS) -shared -o $@ $(o-files) $(F77LIBS)



# rule to make the program that runs an MA27 solver detached from the
# main program, and communicates through a pipe. since this program
# has actually nothing much to do, compile it in debug mode
$(LIBDIR)/bin/detached_ma27$(EXEEXT): source/detached_ma27.cc include/hsl/hsl.h \
				$(lib-contrib-hsl)
	@echo =====hsl=========================$(MT)== Making   $(@F)
	@$(CXX) $(CXXFLAGS.g) $< -o $@ $(lib-contrib-hsl) $(lib-base.o) $(F77LIBS) -lpthread



clean:
	-rm -f *~ lib/lib* lib/o/*.$(OBJEXT) lib/Makefile.dep



# Rule to generate the dependency file. This file is
# automagically remade whenever needed, i.e. whenever
# one of the f-files changed. Make detects whether
# to remake this file upon inclusion at the bottom
# of this file.
#
# note that we take care if there are no f-files
Makefile.dep: $(f-files) Makefile
	@echo ============================ Remaking Makefile
	@(if test "x$(f-files)" != "x" ; then \
           for i in source/*.f ; do \
	     echo $$i | $(PERL) -pi -e 's#source/(.*)\.f#\$$(LIBDIR)/contrib/hsl/\1.$(OBJEXT):source/\1.f#;' ;\
	    done ; \
          fi) \
		> Makefile.dep


# include all the dependencies
include Makefile.dep


.PHONY: clean
.PHONY: lib lib.a lib.g.a
