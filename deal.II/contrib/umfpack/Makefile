include ../../common/Make.global_options

# rules
lib: $(LIBDIR)/liblac_umfpack$(lib-suffix)

ifneq ($(with-multithreading),no)
  MT = MT
else
  MT = ==
endif



AMD:
	cd AMD/Source ; $(MAKE) all

UMFPACK:
	cd UMFPACK/Source ; $(MAKE) all


$(LIBDIR)/liblac_umfpack$(shared-lib-suffix): AMD UMFPACK
	@echo "=====umfpack==========optimized==$(MT)== Linking library:   $(@F)"
	@$(SHLIBLD) $(LDFLAGS) -shared -o $@ $D/lib/contrib/umfpack/*.o


$(LIBDIR)/liblac_umfpack$(static-lib-suffix): AMD UMFPACK
	@echo "=====umfpack==========optimized==$(MT)== Linking library:   $(@F)"
	@$(AR) ru $@ $D/lib/contrib/umfpack/*.o
	@$(RANLIB) $@


clean:


.PHONY: AMD UMFPACK clean lib
