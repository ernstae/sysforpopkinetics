echo "/**********************************************************************" > notice.cpp
cat < notice.txt >> notice.cpp
echo "**********************************************************************/" >> notice.cpp
cp notice.cpp notice.c
cp notice.cpp notice.java
echo "########################################################################" > notice.sh
sed 's/^/# /' < notice.txt >> notice.sh
echo "########################################################################" >> notice.sh
cp notice.sh notice.pl
