include ../../common/Make.global_options

# rules
ifeq ($(enable-shared),yes)
  lib: $(LIBDIR)/liblac_umfpack.so
else
  lib: $(LIBDIR)/liblac_umfpack.a
endif


AMD:
	cd AMD/Source ; $(MAKE) all

UMFPACK:
	cd UMFPACK/Source ; $(MAKE) all


$(LIBDIR)/liblac_umfpack.so: AMD UMFPACK
	@echo "=====umfpack==========optimized==$(MT)== Linking library:   $(@F)"
	@$(SHLIBLD) $(LDFLAGS) -shared -o $@ $D/lib/contrib/umfpack/*.o


$(LIBDIR)/liblac_umfpack.a: AMD UMFPACK
	@echo "=====umfpack==========optimized==$(MT)== Linking library:   $(@F)"
	@$(AR) ru $@ $D/lib/contrib/umfpack/*.o
	@$(RANLIB) $@


clean:


.PHONY: AMD UMFPACK clean lib
