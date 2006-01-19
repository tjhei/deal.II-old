include ../../common/Make.global_options

deplibs = 

# for cygwin, llapack and other libraries are necessary to link with
ifeq ($(findstring cygwin,$(TARGET)),cygwin)
  deplibs = $(LIBS)
endif


# rules
all: AMD UMFPACK $(LIBDIR)/liblac_umfpack$(lib-suffix)

ifneq ($(with-multithreading),no)
  MT = MT
else
  MT = ==
endif



AMD:
	cd AMD/Source ; $(MAKE) all

UMFPACK:
	cd UMFPACK/Source ; $(MAKE) all


$(LIBDIR)/liblac_umfpack$(shared-lib-suffix): $D/lib/contrib/umfpack/*.$(OBJEXT)

	@echo "=====umfpack==========optimized==$(MT)== Linking library:   $(@F)"
	@$(SHLIBLD) $(LDFLAGS) $(SHLIBFLAGS) -o $@ $D/lib/contrib/umfpack/*.$(OBJEXT)
 $(deplibs)


$(LIBDIR)/liblac_umfpack$(static-lib-suffix): $D/lib/contrib/umfpack/*.$(OBJEXT)

	@echo "=====umfpack==========optimized==$(MT)== Linking library:   $(@F)"
	@$(AR) ru $@ $D/lib/contrib/umfpack/*.$(OBJEXT)

	@$(RANLIB) $@


clean:


.PHONY: AMD UMFPACK clean lib
