# $Id$
# Copyright (C) 2001, 2002 by Wolfgang Bangerth


D = ../..

include $D/common/Make.global_options

ifneq ($(with-multithreading),no)
  MT = MT
else
  MT = ==
endif



# get lists of files we need
f-files     = $(shell a="`echo source/*.f`"; if test "$$a" != "source/*.f" ; then echo $$a ; fi)
o-files     = $(f-files:source/%.f=$(LIBDIR)/contrib/hsl/%.o)



$(LIBDIR)/contrib/hsl/%.o :
	@echo =====hsl==============optimized==$(MT)== $(<F)
	@$(F77) $(F77FLAGS.o) -c $< -o $@


# rules
ifeq ($(enable-shared),yes)
  lib: $(LIBDIR)/libhsl.so
else
  lib: $(LIBDIR)/libhsl.a
endif


$(LIBDIR)/libhsl.a: $(forward-declarations) $(o-files)
	@echo =====base=============optimized==$(MT)== Linking library:   $@
	@ar ruv $@ $(o-files)

$(LIBDIR)/libhsl.so: $(forward-declarations) $(o-files)
	@echo =====base=============optimized==$(MT)== Linking library:   $@
	@$(CC) $(LDFLAGS) -shared -o $@ $(o-files)




clean:
	-rm -f *~ lib/lib* lib/o/*.o lib/Makefile.dep



#Rule to generate the dependency file. This file is
#automagically remade whenever needed, i.e. whenever
#one of the f-files changed. Make detects whether
#to remake this file upon inclusion at the bottom
#of this file.
#
#note that we take care if there are no f-files
Makefile.dep: $(f-files) Makefile
	@echo ============================ Remaking Makefile
	(if test "x$(f-files)" != "x" ; then \
           for i in source/*.f ; do \
	     echo $$i | perl -pi -e 's#source/(.*)\.f#\$$(LIBDIR)/contrib/hsl/\1.o:source/\1.f#;' ;\
	    done ; \
          fi) \
		> Makefile.dep


# include all the dependencies
include Makefile.dep


.PHONY: clean
.PHONY: lib lib.a lib.g.a
