#! /usr/bin/env bash

# Do getoptions
GETOPT=$(getopt --options="hvt:" --longoptions="help,verbose,target::" -n "publish.sh" -- "$@")
if [[ $? != 0 ]]; then
    exit 1;
fi

eval set -- "$GETOPT"

VERBOSE=0
TARGET=0
while true; do
    case "$1" in
	-h|--help)
	    echo "./publish.sh [-v|--verbose] (-t|--target) <directory to publish to>"
	    exit 0
	    ;;
	-v|--verbose)
	    VERBOSE=1
	    shift
	    ;;
	-t|--target)
	    TARGET=$2
	    shift
	    shift
	    ;;
	--)
	    shift
	    break
	    ;;
	*)
	    echo $1
	    shift
	    echo "Internal Error!"
	    exit 1;
	    ;;
    esac
done

if [[ $TARGET == 0 ]] ; then
    echo "No target specified!" 1>&2
    exit 1
fi

COMMAND="rsync --verbose --archive --delete --exclude='.htaccess' ./_site/ '$TARGET'"

if [[ $VERBOSE != 0 ]]; then
    echo $COMMAND
fi

eval exec "$COMMAND"
