# $Id$
# Create proper HTML output from the different example programs

# include paths and global variables
include ../../../common/Make.global_options

example-directories = $(shell echo $D/deal.II/examples/step-by-step/step-*)
example-names       = $(notdir $(example-directories))
example-htmls       = $(addsuffix .html,$(example-names))
example-sgmls       = $(addsuffix .sgml,$(example-names))

all: $(example-htmls)


$(example-htmls):
	@echo ================== Assembling $@
	@cat head.html > $@
	@cat $(@:.html=.data/intro.html) >> $@
	@cat $D/deal.II/examples/step-by-step/$(@:.html=)/$(@:.html=.cc) \
	  | $(PERL) program2html \
	  >> $@
	@cat $(@:.html=.data/results.html) >> $@
	@cat $D/deal.II/examples/step-by-step/$(@:.html=)/$(@:.html=.cc) \
	  | $(PERL) program2plain \
	  | $(PERL) -e '$$path="$D/deal.II/examples/step-by-step/$(@:.html=)/$(@:.html=.cc)"; \
                        $$path =~ s!/! /!g; \
		        while (<>) { s!PROGRAMPATH!$$path!g; print; };' \
	  >> $@
	@cat foot.html >> $@


# Note: SGML support is very preliminary and not very functional at present!
$(example-sgmls):
	@echo ================== Assembling $@
	@cat $(@:.sgml=.data/intro.html) >> $@
	@echo "</sect2>" >> $@
	@cat $D/deal.II/examples/step-by-step/$(@:.sgml=)/$(@:.sgml=.cc) \
	  | $(PERL) program2html \
	  >> $@
	@echo "</sect2>" >> $@
	@cat $(@:.sgml=.data/results.html) >> $@
	@echo "</sect2>" >> $@
	@cat $D/deal.II/examples/step-by-step/$(@:.sgml=)/$(@:.sgml=.cc) \
	  | $(PERL) program2plain \
	  | $(PERL) -e '$$path="$D/deal.II/examples/step-by-step/$(@:.sgml=)/$(@:.sgml=.cc)"; \
                        $$path =~ s!/! /!g; \
		        while (<>) { s!PROGRAMPATH!$$path!g; print; };' \
	  >> $@
	@echo "</sect2>" >> $@
	@$(PERL) -pi -e 's!<a .*</a>!!gi;       \
                         s!<h1>(.*)</h1>!<sect2><title>$$1</title>!gi; \
                         s!<p>!\n<para>!gi;                            \
                         s!</p>!</para>!gi;                            \
                         s!<pre><code>!<programlisting>!gi;            \
                         s!</code></pre>!</programlisting>!gi; ' $@

step-by-step.sgml: $(example-sgmls)
	@echo ================== Concating $@
	@cat step-by-step.header.sgml          \
             | perl -pi -e 's/TODAY/$(shell date)/;'   \
             > $@
	@for step in $(example-names) ; do      \
	     echo "<sect1><title>$$step</title>" >> $@ ; \
	     cat $$step.sgml     >> $@ ; \
	     echo "</sect1>"   >> $@ ; \
	 done
	@echo "</chapter>" >> $@
	@echo "</book>" >> $@
	@rm $(example-sgmls)


clean:
	-rm $(example-htmls)


# mark the output files as phony, i.e. generate them every time
.PHONY: $(example-htmls) clean
