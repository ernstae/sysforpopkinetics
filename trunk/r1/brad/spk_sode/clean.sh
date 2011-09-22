#! /bin/bash
list="
	junk
	spk_sode.dvi
	spk_sode.aux
	spk_sode.log
	spk_sode.pdf
	htm
"
for file in $list 
do
	if [ -e $file ]
	then
		echo "rm -r $file"
		rm -r $file
	fi
done
