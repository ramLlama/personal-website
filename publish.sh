#! /usr/bin/env bash

# Do getoptions
GETOPT=$(getopt --options="hv" --longoptions="help,verbose" -n "publish.sh" -- "$@")
if [[ $? != 0 ]]; then
    exit 1;
fi

eval set -- "$GETOPT"

VERBOSE=0;
while true; do
    case "$1" in
	-h|--help)
	    echo "./publish.sh <directory to publish to>"
	    exit 0
	    ;;
	-v|--verbose)
	    VERBOSE=1
	    shift
	    ;;
	--)
	    shift
	    break
	    ;;
	*)
	    shift
	    echo "Internal Error!"
	    exit 1;
	    ;;
    esac
done

COMMAND="rsync --verbose --archive --delete --exclude='.htaccess' ./_site/ '$1'"

if [[ $VERBOSE != 0 ]]; then
    echo $COMMAND
fi

eval exec "$COMMAND"
