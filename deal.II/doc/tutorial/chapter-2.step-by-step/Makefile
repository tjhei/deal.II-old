# $Id$
# Create proper HTML output from the different example programs

example-directories = $(shell echo ../../../deal.II/examples/step-by-step/step-*)
example-names       = $(notdir $(example-directories))
example-htmls       = $(addsuffix .html,$(example-names))

all: $(example-htmls)


$(example-htmls):
	@echo ================== Assembling $@
	@cat step-by-step.header > $@
	@cat $(@:.html=.intro) >> $@
	@cat ../../../deal.II/examples/step-by-step/$(@:.html=)/$(@:.html=.cc) \
	  | perl program2html \
	  >> $@
	@cat $(@:.html=.results) >> $@
	@cat ../../../deal.II/examples/step-by-step/$(@:.html=)/$(@:.html=.cc) \
	  | perl program2plain \
	  >> $@
	@cat step-by-step.foot >> $@


clean:
	-rm $(example-htmls)


# mark the output files as phony, i.e. generate them every time
.PHONY: $(example-htmls) clean
