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
example-commprogs  = $(addsuffix _prog.html,$(gen-example-names))
example-plainprogs = $(addsuffix _plain.html,$(gen-example-names))
example-toc-prog   = $(addsuffix _toc.prog,$(gen-example-names))
example-toc-plain  = $(addsuffix _toc.plain,$(gen-example-names))
example-toc        = $(addsuffix _toc.combined,$(gen-example-names))

example-htmls       = $(addsuffix .html,$(example-names))


# a makefile command that extracts the base name of a program
get_basename = $(shell echo $(1) | $(PERL) -pi -e 's/generated.//g; s/\..*//g; s/_(prog|plain|toc.*)//g;')



all: $(example-htmls)


$(example-commprogs):
	@echo ================== Making $@
	@cat $D/examples/$(call get_basename, $@)/*.cc \
	  | $(PERL) program2html \
	  > $@


$(example-plainprogs):
	@echo ================== Making $@
	@cat $D/examples/$(call get_basename, $@)/*.cc \
	  | $(PERL) program2plain \
	  | $(PERL) -e '$$path="$D/examples/$(call get_basename,$@)/*.cc)"; \
                        $$path =~ s!/! /!g; \
		        while (<>) { s!PROGRAMPATH!$$path!g; print; };' \
	  > $@


$(example-toc-prog):
	@echo ================== Making $@
	@cat $D/examples/$(call get_basename, $@)/*.cc \
	  | $(PERL) program2toc \
	  > $@


$(example-toc-plain): $(example-toc-prog)
	@echo ================== Making $@
	@cat generated/$(call get_basename, $@)_toc.prog \
	  | $(PERL) -pi -e 's/href=\"\#/href=\"\#plain-/g;' \
	  > $@


$(example-toc): $(example-toc-prog) $(example-toc-plain)
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




$(example-htmls): $(example-commprogs) $(example-plainprogs) \
		  $(example-toc)
	@echo ================== Assembling $@
	@cat head.html > $@
	@echo "<h2>Overview of $(call get_basename,$@)</h2>" >> $@
	@cat generated/$(call get_basename, $@)_toc.combined  \
	     $(call get_basename, $@).data/intro.html  \
	     generated/$(call get_basename, $@)_prog.html   \
	     $(call get_basename, $@).data/results.html\
	     generated/$(call get_basename, $@)_plain.html  \
	     foot.html                                 >> $@




validate-xrefs:
	@$(PERL) $D/common/scripts/validate-xrefs.pl \
                $(filter-out head.html foot.html, \
                             $(shell echo *.html))


clean:
	-rm -f $(example-htmls)
	-rm -f $(example-commprogs) $(example-plainprogs) $(example-toc) \
		$(example-toc-prog)  $(example-toc-plain)


# mark the output files as phony, i.e. generate them every time
.PHONY: $(example-htmls) $(example-commprogs) \
        $(example-plainprogs) $(example-toc) \
	$(example-toc-prog)  $(example-toc-plain) \
	validate-xrefs clean

