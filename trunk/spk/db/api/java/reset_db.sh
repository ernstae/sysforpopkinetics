D=../../admin

TMP_NAME="/tmp/junk$$";
echo 'use spktest;' > $TMP_NAME
cat $TMP_NAME $D/drop.sql   | mysql --force -ptester -utester > /dev/null 2>&1
cat $TMP_NAME $D/schema.sql | mysql --force -ptester -utester
