# Some helpful utilitiy functions. 


# ----- active_git_brach
#  Return the name of the active branch. 
function active_git_branch {
    echo $(git branch | sed -n -e 's/^\* \(.*\)/\1/p');
}
    

# ----- is_git_dirty 
#  Does the git repository have any uncommitted state?
function is_git_dirty {
    [[ $(git diff --shortstat 2> /dev/null | tail -n1) != "" ]] && echo "yes";
}

