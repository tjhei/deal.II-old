# $Id$

step-by-step:
	cd chapter-2.step-by-step ; $(MAKE)

validate-xrefs:
	cd chapter-1.elements ; $(MAKE) validate-xrefs
	cd chapter-2.step-by-step ; $(MAKE) validate-xrefs
	cd chapter-3.laplace ; $(MAKE) validate-xrefs

clean:
	cd chapter-2.step-by-step ; $(MAKE) clean


.PHONY: step-by-step clean validate-xrefs
