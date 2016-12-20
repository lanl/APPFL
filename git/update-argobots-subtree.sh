#!/bin/bash 
#
# update appfl argobots subtree
#
# This script should be run from the root directory of the appfl
# source.
#
. git/update-subtree-funcs.sh 

# Check for 'dirty' status... 
dirty=$(is_git_dirty)
if [ "$dirty" == "yes" ]; then 
    echo "Your repository has uncommitted changes.  Aborting..."
    exit 1;
fi 


if [ ! -d runtime/argobots ]; then 
    echo "Unable to find runtime/argobots in current working directory."
    exit 1;
fi 

git subtree pull --squash -d -P runtime/argobots https://github.com/pmodels/argobots.git master 

