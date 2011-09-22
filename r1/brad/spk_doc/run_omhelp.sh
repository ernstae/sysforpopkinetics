#! /bin/bash
if [ -e htm ]
then
	rm -r htm
fi
if [ -e spk_doc.tgz ]
then
	rm spk_doc.tgz
fi
mkdir htm
cd htm
omhelp ../spk_doc.omh -debug -noframe
omhelp ../spk_doc.omh -debug -xml -noframe
omhelp ../spk_doc.omh -printable
omhelp ../spk_doc.omh -xml -printable
cd ..
version=`date +%g-%m-%d`
mv htm spk_doc-$version
tar -cvzf spk_doc.tgz spk_doc-$version
mv spk_doc-$version htm
