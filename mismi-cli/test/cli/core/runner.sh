MISMI="${1:-./dist/build/s3/s3}"

type "$MISMI" > /dev/null 2>&1 || {
    echo "No mismi executable specified on command line or on path."
    exit 1
}

ROOT=$(dirname "$0")/../../..
ROOT=$(cd "$ROOT" > /dev/null 2>&1 && pwd)
TMP=${ROOT}/tmp
TEST=${TMP}/test/$$
mkdir -p ${TEST}

cleanup () {
    echo "Cleaning up (${TEST})"
    echo "Cleaning up (s3://ambiata-dev-view/test/mismi/cli/$HOSTNAME/$$/)"
    rm -rf "${TEST}"
    aws s3 rm --recursive "s3://ambiata-dev-view/test/mismi/cli/$HOSTNAME/$$/"
}

trap cleanup SIGHUP SIGINT SIGQUIT SIGTERM

banner () {
    echo
    echo == "$*" ==
    echo == "Running in ${TEST}" ==
    echo
}

unique_s3_url () {
    echo "s3://ambiata-dev-view/test/mismi/cli/$HOSTNAME/$$/${1:-notset}"
}

exit_cleanup() {
    cleanup
    exit ${1:-1}
}
