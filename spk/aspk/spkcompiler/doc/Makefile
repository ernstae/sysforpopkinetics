SUFFIXES = .dox
.html.dox : 
	doxygen $<

API_SRC = ../libspkcompiler/client.h \
          ../libspkcompiler/nonmem/nonmem.h \
          ../libspkcompiler/Symbol.h \
          ../libspkcompiler/SymbolTable.h \
	  ../libspkcompiler/ClientTranslator.h \
	  ../libspkcompiler/upper.cpp \
	  ../libspkcompiler/upper.h \
	  ../libspkcompiler/lower.cpp \
	  ../libspkcompiler/lower.h

NMTRAN_SRC = ../libspkcompiler/nonmem/explang.cpp \
	     ../libspkcompiler/nonmem/lex_explang.c \
	     ../libspkcompiler/nonmem/explang.ypp \
	     ../libspkcompiler/nonmem/lex_explang.l

html : nmtran_to_cpp_html compiler_html spksourceml_html spkreportml_html

nmtran_to_cpp_html: nmtran.cfg $(NMTRAN_SRC)
	doxygen nmtran.cfg

compiler_html : nmtran_to_cpp_html api.cfg $(API_SRC) 
	doxygen api.cfg

spksourceml_html : SpkSourceML.dtd
	javac -classpath ./tools/dtddoc/java/dtdparser119.jar:./tools/dtddoc/java/jakarta-oro-2.0.6.jar ./tools/dtddoc/java/DTDDoc.java
	java -classpath ./tools/dtddoc/java/dtdparser119.jar:./tools/dtddoc/java/jakarta-oro-2.0.6.jar:.:./tools/dtddoc/java DTDDoc SpkSourceML.dtd SpkSourceML_html

spkreportml_html : SpkReportML.dtd
	javac -classpath ./tools/dtddoc/java/dtdparser119.jar:./tools/dtddoc/java/jakarta-oro-2.0.6.jar ./tools/dtddoc/java/DTDDoc.java
	java -classpath ./tools/dtddoc/java/dtdparser119.jar:./tools/dtddoc/java/jakarta-oro-2.0.6.jar:.:./tools/dtddoc/java DTDDoc SpkReportML.dtd SpkReportML_html

clean : 
	rm -rf html
	rm -rf latex
	rm -rf man
	rm -rf api
	rm -rf SpkSourceML_html
	rm -rf SpkReportML_html
	rm -rf nonmem_to_cpp 

all  : api.cfg nmtran.cfg
	rm -rf html
	rm -rf latex
	rm -rf man
	rm -rf api
	rm -rf SpkSourceML_html
	rm -rf SpkReportML_html
	rm -rf nonmem_to_cpp
	doxygen nmtran.cfg
	doxygen api.cfg
	javac -classpath ./tools/dtddoc/java/dtdparser119.jar:./tools/dtddoc/java/jakarta-oro-2.0.6.jar ./tools/dtddoc/java/DTDDoc.java
	java -classpath ./tools/dtddoc/java/dtdparser119.jar:./tools/dtddoc/java/jakarta-oro-2.0.6.jar:.:./tools/dtddoc/java DTDDoc SpkSourceML.dtd SpkSourceML_html
	java -classpath ./tools/dtddoc/java/dtdparser119.jar:./tools/dtddoc/java/jakarta-oro-2.0.6.jar:.:./tools/dtddoc/java DTDDoc SpkReportML.dtd SpkReportML_html
