DOC=repositionSpk
MAIN=$(DOC).xml
#SECTS=intro.xml tools.xml structure.xml xsl.xml tips.xml \
	source.xml fdl.xml
#XSLFOMODS=ulink.xsl keycombo.xsl
#XSLHTMLMODS=fileext.xsl# admon.xsl keycombo.xsl css.xsl
#XSLS=$(XSLFOMODS) $(XSLHTMLMODS)
#FIGS=simple.fig
#PNGS=$(FIGS:.fig=.png)
#PDFS=$(FIGS:.fig=.pdf)
#SRC=$(MAIN) $(SECTS) $(MAIN).sed $(addsuffix .sed,$(SECTS)) \
	$(addsuffix .sed,$(XSLS)) $(DOC).css.sed builddate Makefile.sed
SRC=$(MAIN)

#STYLESHEET_IMAGES=/usr/share/xml/docbook/xsl-stylesheets/images

all: $(DOC).pdf $(DOC).ps $(DOC)

$(DOC).pdf: $(SRC) $(PDFS) $(XSLFOMODS)
	for mod in $(XSLFOMODS); do \
		MODARGS="$$MODARGS -m $$mod"; \
	done; \
	xmlto pdf -v --extensions $$MODARGS $(MAIN)

$(DOC).ps: $(DOC).pdf
	pdftops $< $@

$(DOC): $(SRC) $(PNGS) $(XSLHTMLMODS)
	-rm -rf $@
	for mod in $(XSLHTMLMODS); do \
		MODARGS="$$MODARGS -m $$mod"; \
	done; \
	xmlto html -o $@ $$MODARGS $(MAIN)
	#cp -a $(STYLESHEET_IMAGES) $@/stylesheet-images
	#cp $(PNGS) $(DOC).css $@

# Since this file would be parsed for SGML markup, we have to
# make a safe version (with &, < and > converted to SGML entities)
# In addition, tabs get ignored for some reason, so for the
# Makefile we have to convert them to spaces.
%.sed: %
	fold -w 72 $< | \
		sed -e "s/$$(echo x | tr x \\t)/        /g" \
		    -e "s/&/\\&amp;/g" \
		    -e "s/</\\&lt;/g" \
		    -e "s/>/\\&gt;/g" > $@

builddate:
	echo -n $$(date "+%e %B %Y") > $@

clean:
	-$(RM) *.log *.dvi *.aux *.tex *.sed *.out *-t
	-$(RM) $(PNGS) $(PDFS) builddate *.html
	-$(RM) -r $(DOC) $(DOC).ps $(DOC).pdf $(DOC).fo

distclean: clean
	-$(RM) *~ $(DOC).tar.gz docbook.tar.gz
	-$(RM) -r docbook

$(DOC).tar.gz: distclean
	(cd ..; tar zcf /tmp/$(DOC).tar.gz $(DOC) )
	mv /tmp/$(DOC).tar.gz .

docbook: $(DOC).tar.gz all
	-$(RM) -r $@
	mkdir $@
	cp $(DOC).tar.gz $(DOC).ps $(DOC).pdf $@
	tar cf - $(DOC) | (cd $@; tar xf -)

docbook.tar.gz: docbook
	tar zcf docbook.tar.gz docbook

# Make png from xfig
%.png: %.fig
	fig2dev -Lpng $< $@

# Make pdf from xfig
%.pdf: %.fig
	fig2dev -Lpdf $< $@

.PHONY: distclean clean all builddate
