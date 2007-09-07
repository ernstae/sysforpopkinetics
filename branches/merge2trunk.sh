#! /bin/bash
if [ "$2" == "" ]
then
	echo "merge2trunk.sh branch revision [--dry-run]"
	echo "where revision corresponds to the start of the changes"
	exit 1
fi
revision="$2"
dry_run=""
if [ "$3" != "" ]
then
	if [ "$3" != "--dry-run" ]
	then
		echo "merge2trunk.sh branch revision [--dry-run]"
		echo "where revision corresponds to the start of the changes"
		exit 1
	else
		dry_run="--dry-run"
	fi
fi
branch="brad"
repository="svn+ssh://toronto/u01/local/rfpk_r2" 
#
check=`pwd | sed -e 's|.*r2/branches||'`
if [ "$check" != "" ]
then
	echo "Must execute merge2trunk.sh from r2/branches"
	exit 1
fi
if [ ! -d "$branch" ]
then
	echo "Cannot find local copy of r2/branch/$branch"
	exit 1
fi
if ! cd ../trunk
then
	echo "cannot change into the r2/trunk directory"
	exit 1
fi
echo "pwd"
pwd
echo "svn merge $dry_run -r$revision:HEAD $repository/branches/$branch"
svn merge $dry_run -r$revision:HEAD "$repository/branches/$branch"
