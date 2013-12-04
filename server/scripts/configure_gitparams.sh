#!/bin/bash 


GIT_BRANCH=$(git rev-parse --abbrev-ref HEAD | sed 's/\//\\\//')
GIT_COMMIT=$(git rev-parse HEAD)

UNAME=$(uname)

if [ "$UNAME" == "Linux" ]; then
	sed -e "s/^\(.*{git_branch,.*\"\)[^\"]*\(\"}.*\)$/\1${GIT_BRANCH}\2/g" -i rel/files/sys.config ;
	sed -e "s/^\(.*{git_commit,.*\"\)[^\"]*\(\"}.*\)$/\1${GIT_COMMIT}\2/g" -i rel/files/sys.config ;
else
	sed -e "s/^\(.*{git_branch,.*\"\)[^\"]*\(\"}.*\)$/\1${GIT_BRANCH}\2/g" -i bck rel/files/sys.config ;
	sed -e "s/^\(.*{git_commit,.*\"\)[^\"]*\(\"}.*\)$/\1${GIT_COMMIT}\2/g" -i bck rel/files/sys.config ;
fi;
