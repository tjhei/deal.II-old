#-------------------------------------------------------------------------------
# AMD Makefile for compiling on Unix systems (for GNU make only)
#-------------------------------------------------------------------------------

include ../../../../common/Make.global_options

#-------------------------------------------------------------------------------
# source files
#-------------------------------------------------------------------------------

AMD = amd_aat amd_1 amd_2 amd_dump amd_postorder amd_post_tree amd_defaults \
	amd_order amd_control amd_info amd_valid amd_preprocess

INC = ../Include/amd.h amd_internal.h

#-------------------------------------------------------------------------------
# object files for each version
#-------------------------------------------------------------------------------

AMDI = $(addprefix $D/lib/umfpack/, $(addsuffix .$(OBJEXT), $(subst amd_,amd_i_,$(AMD))))
AMDL = $(addprefix $D/lib/umfpack/, $(addsuffix .$(OBJEXT), $(subst amd_,amd_l_,$(AMD))))

#-------------------------------------------------------------------------------
# compile each int and long routine (with no real/complex version)
#-------------------------------------------------------------------------------

$D/lib/umfpack/amd_i_%.$(OBJEXT): amd_%.c $(INC)
	@echo =====umfpack==========optimized==$(MT)== $(<F)
	$(CC) $(CFLAGS) -I../Include -DDINT -c $< -o $@

$D/lib/umfpack/amd_l_%.$(OBJEXT): amd_%.c $(INC)
	@echo =====umfpack==========optimized==$(MT)== $(<F)
	$(CC) $(CFLAGS) -I../Include -DDLONG -c $< -o $@

all: $(AMDI) $(AMDL)
