D=../../admin

HOST=$1

TMP_NAME="/tmp/junk$$";
echo host: $HOST
echo 'use spktest;' > $TMP_NAME
cat $TMP_NAME $D/drop.sql   | mysql --force -h $HOST -ptester -utester > /dev/null 2>&1
cat $TMP_NAME $D/schema.sql | mysql --force -h $HOST -ptester -utester
