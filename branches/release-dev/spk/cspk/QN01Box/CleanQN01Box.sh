# ! /bin/bash
for file in \
	aclocal.m4 \
	configure \
	QN01Box/config.h \
	QN01Box/config.h.in \
	QN01Box/stamp-h1 \
	Test/RunTest \
	junk \
	junk.* \
	*/junk \
	*/junk.* \
	Makefile \
	*/Makefile \
	Makefile.in \
	*/Makefile.in \
	*.gz \
	*.log \
	*.status \
	*/*.o \
	*/*.a 
do
	if [ -e $file ] 
	then
		rm $file
	fi
done
if [ -e qn01box-* ]
then
	rm -r -f qn01box-* 
fi
for dir in \
	Doc \
	autom4te.cache \
	Test/.deps \
	lib/.deps 
do
	if [ -e $dir ]
	then
		rm -r -f $dir
	fi
done

