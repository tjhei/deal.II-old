step-1.html \
step-2.html:
	cat step-by-step.header > $@
	cat ../../../deal.II/examples/step-by-step/$(@:.html=)/$(@:.html=.cc) \
	  | perl process-files \
	  >> $@
	cat step-by-step.foot >> $@

.PHONY: step-1.html step-2.html
