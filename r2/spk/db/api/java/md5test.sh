#!/bin/sh

# Script to compute the MD5 sum of a password, using the MySQL
# database engine and Spkdb.md5sum, for comparison purposes

PASSWORD=$1


echo "select md5('$PASSWORD');" | mysql -utester -ptester

java TestMd5 $PASSWORD
