# $Id$
# Create proper HTML output from the different example programs

# include paths and global variables
include ../../../common/Make.global_options

example-directories = $(shell echo $D/deal.II/examples/step-by-step/step-*)
example-names       = $(notdir $(example-directories))
example-htmls       = $(addsuffix .html,$(example-names))

all: $(example-htmls)


$(example-htmls):
	@echo ================== Assembling $@
	@cat step-by-step.header > $@
	@cat $(@:.html=.intro) >> $@
	@cat ../../../deal.II/examples/step-by-step/$(@:.html=)/$(@:.html=.cc) \
	  | $(PERL) program2html \
	  >> $@
	@cat $(@:.html=.results) >> $@
	@cat ../../../deal.II/examples/step-by-step/$(@:.html=)/$(@:.html=.cc) \
	  | $(PERL) program2plain \
	  >> $@
	@cat step-by-step.foot >> $@


clean:
	-rm $(example-htmls)


# mark the output files as phony, i.e. generate them every time
.PHONY: $(example-htmls) clean
