#!/usr/bin/env zsh

set -e


if ! command -v asdfasdf > /dev/null 2>&1
then
    print 'exiftool is not installed.'
    print 'exiftool is required to read EXIF metadata to generate good filenames.'
    return 1
fi

if [[ -z $@ ]]
then
    cat <<EOF
  usage: ./linkPub.zsh file [file ..]

  linkPub uses EXIF metadata to create a well-named symbolic link in the current directory
  pointing to each of its command line arguments

EOF
    return 1
fi


for f in $@
do
    if [[ -f $f ]]
    then
        # exiftool looks for the Title field in the arugment's EXIF metadata.
        # sed removes all punctuation, replaces spaces with '-', and lowers and
        # uppercase characters
        # !"\#$%&\'()*+,\./:;<=>?@\[\\\]^\`{|}
        local title=`exiftool -Title -T $f | \
                   sed -e "s/[[:punct:]]/-/g" \
                       -e "s/[[:space:]]\+/\-/g" \
                       -e "s/---/--/g" \
                       -e "s/./\L&/g"`

        if [[ -z $title ]]
        then
            print -l "trouble finding a good name for \"$f\"." "exiting early"
            return 1
        fi

        local i=1
        local name=$title
        until [[ ! -e $name".pdf" ]]
        do
            name=$title"($i)"
            let i++
        done
        ln -s $f ./${name}".pdf"
    else
        print -l "No such file: $1" \
              "Give me a PDF file with Title EXIF data and I'll make a link" \
              "to it in this directory with a good name"
    fi
done
