#! /bin/bash
if [ "$1" == "" ]
then
	echo "merge2trunk.sh branch [--dry-run]"
	exit 1
fi
dry_run=""
if [ "$2" != "" ]
then
	if [ "$2" != "--dry-run" ]
	then
		echo "merge2trunk.sh branch [--dry-run]"
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
echo "svn merge $dry_run -r2419:HEAD $repository/branches/brad"
svn merge $dry_run -r2419:HEAD "$repository/branches/brad"
