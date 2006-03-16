# $Id$
# Create proper HTML output from the different example programs

# include paths and global variables
include ../../../common/Make.global_options


# plain names of those files that we shall treat
example-names = $(notdir $(shell echo $D/examples/step-? \
                                      $D/examples/step-??))

# prefix names for generated intermediate files
gen-example-names = $(addprefix generated/, $(example-names))

# names of commented program files, plain files, and tocs for the commented
# and commented and plain versions of the program as well as a combined one
example-prog       = $(addsuffix _prog.html,$(gen-example-names))
example-plain      = $(addsuffix _plain.html,$(gen-example-names))
example-toc-prog   = $(addsuffix _toc.prog,$(gen-example-names))
example-toc-plain  = $(addsuffix _toc.plain,$(gen-example-names))
example-toc        = $(addsuffix _toc.combined,$(gen-example-names))

# same for intermediate files for doxygen output
example-dox-prog   = $(addsuffix _prog.dox,$(gen-example-names))
example-dox-plain  = $(addsuffix _plain.dox,$(gen-example-names))


# finally a target for combined HTML files
example-htmls       = $(addsuffix .html,$(example-names))
example-doxygen     = $(addprefix doxygen/,$(addsuffix .h,$(example-names)))





# a makefile command that extracts the base name of a program
get_basename = $(shell echo $(1) | $(PERL) -pi -e 's/(generated|doxygen).//g; s/\..*//g; s/_(prog|plain|toc.*)//g;')



all: $(example-htmls) $(example-doxygen)


$(example-prog):
	@echo ================== Making $@
	@cat $D/examples/$(call get_basename, $@)/*.cc \
	  | $(PERL) program2html \
	  > $@


$(example-plain):
	@echo ================== Making $@
	@echo "<a name=\"PlainProg\"></a>" > $@
	@echo "<h1> The plain program</h1>" >> $@
	@echo "<p>" >> $@
	@echo "(If you are looking at a locally installed deal.II version, then the" >> $@
	@echo "program can be found at <i>" >> $@
	@echo $D/examples/$(call get_basename,$@)/*.cc | $(PERL) -pi -e 's!/! /!g;' >> $@
	@echo "</i>. Otherwise, this is only" >> $@
	@echo "the path on some remote server.) </p>" >> $@
	@cat $D/examples/$(call get_basename, $@)/*.cc | $(PERL) program2plain >> $@


$(example-toc-prog):
	@echo ================== Making $@
	@cat $D/examples/$(call get_basename, $@)/*.cc \
	  | $(PERL) program2toc \
	  > $@


$(example-toc-plain):
	@echo ================== Making $@
	@cat generated/$(call get_basename, $@)_toc.prog \
	  | $(PERL) -pi -e 's/href=\"\#/href=\"\#plain-/g;' \
	  > $@


$(example-toc):
	@echo ================== Making $@
	@echo "<table class=\"tutorial\">  <tr>    <td>" > $@
	@echo "<ol>" >> $@
	@echo "  <li> <a href=\"#Intro\" class=bold>Introduction</a>" >> $@
	@echo "  <li> <a href=\"#CommProg\" class=bold>The commented program</a>" >> $@
	@cat generated/$(call get_basename, $@)_toc.prog >> $@
	@echo "  <li> <a href=\"#Results\" class=bold>Results</a>" >> $@
	@echo "  <li> <a href=\"#PlainProg\" class=bold>The plain program</a>" >> $@
	@cat generated/$(call get_basename, $@)_toc.plain >> $@
	@echo "</ol> </td> </tr> </table>" >> $@




$(example-htmls):
	@echo ================== Assembling $@
	@cat head.html > $@
	@echo "<h1>Overview of $(call get_basename,$@)</h1>" >> $@
	@cat generated/$(call get_basename, $@)_toc.combined  \
	     $(call get_basename, $@).data/intro.html  \
	     generated/$(call get_basename, $@)_prog.html   \
	     $(call get_basename, $@).data/results.html\
	     generated/$(call get_basename, $@)_plain.html  \
	     foot.html                                 >> $@



$(example-dox-prog):
	@echo ================== Making $@
	@cat $D/examples/$(call get_basename, $@)/*.cc \
	  | $(PERL) program2doxygen \
	  > $@


$(example-dox-plain):
	@echo ================== Making $@
	@echo " * <a name=\"PlainProg\"></a>" > $@
	@echo " * <h1> The plain program</h1>" >> $@
	@echo " * " >> $@
	@echo " * (If you are looking at a locally installed deal.II version, then the" >> $@
	@echo " * program can be found at <i>" >> $@
	@echo " * " $D/examples/$(call get_basename,$@)/*.cc | $(PERL) -pi -e 's!/! /!g;' >> $@
	@echo " * </i>. Otherwise, this is only" >> $@
	@echo " * the path on some remote server.)" >> $@
	@echo " * @code" >> $@
	@cat $D/examples/$(call get_basename, $@)/*.cc | $(PERL) program2doxyplain >> $@
	@echo " * @endcode" >> $@


$(example-doxygen):
	@echo ================== Assembling $@
	@echo "/**" > $@
	@echo " * @page $(shell echo $(call get_basename,$@) | $(PERL) -pi -e 's/-/_/g;') \
	                The $(call get_basename,$@) tutorial program" >> $@
	@echo "@htmlonly" >> $@
	@cat generated/$(call get_basename, $@)_toc.combined >> $@
	@cat $(call get_basename, $@).data/intro.html | \
	  $(PERL) -pi -e 's#(src|SRC|href|HREF)=\"([-a-zA-Z0-9]+.data)#\1=\"../../tutorial/chapter-2.step-by-step/\2#g; s/(href|HREF)=.step-..(\d+)/href=\"step_\1/g;' >> $@
	@echo "@endhtmlonly" >> $@
	@cat generated/$(call get_basename, $@)_prog.dox >> $@
	@echo "@htmlonly" >> $@
	@cat $(call get_basename, $@).data/results.html | \
	  $(PERL) -pi -e 's#(src|SRC|href|HREF)=\"([-a-zA-Z0-9]+.data)#\1=\"../../tutorial/chapter-2.step-by-step/\2#g; s/(href|HREF)=.step-..(\d+)/href=\"step_\1/g;' >> $@
	@echo "@endhtmlonly" >> $@
	@cat generated/$(call get_basename, $@)_plain.dox >> $@
	@echo " */" >> $@


validate-xrefs:
	@$(PERL) $D/common/scripts/validate-xrefs.pl \
                $(filter-out head.html foot.html, \
                             $(shell echo *.html))


clean:
	-rm -f $(example-htmls)
	-rm -f $(example-prog) $(example-plain) $(example-toc) \
		$(example-toc-prog)  $(example-toc-plain) \
		$(example-dox-prog) $(example-dox-plain) $(example-doxygen)


.PHONY: validate-xrefs clean


Makefile.dep: $(shell echo $D/examples/*/*.cc) Makefile
	@echo ================== Generating $@
	@for i in $(example-names) ; do \
		echo generated/$${i}_prog.html generated/$${i}_plain.html \
		     generated/$${i}_toc.prog \
		     generated/$${i}_prog.dox generated/$${i}_plain.dox : \
			`echo $D/examples/*/$${i}.cc` ; \
		echo generated/$${i}_toc.plain : generated/$${i}_toc.prog ; \
		echo generated/$${i}_toc.combined : \
			generated/$${i}_toc.prog generated/$${i}_toc.plain ; \
		echo $$i.html : generated/$${i}_prog.html generated/$${i}_plain.html \
				generated/$${i}_toc.combined \
				$$i.data/intro.html $$i.data/results.html ; \
		echo doxygen/$$i.h : generated/$${i}_prog.dox generated/$${i}_plain.dox \
				generated/$${i}_toc.combined \
				$$i.data/intro.html $$i.data/results.html ; \
	 done \
	> $@

include Makefile.dep
