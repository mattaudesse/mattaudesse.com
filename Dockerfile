from centos:7.6.1810

run yum install -y zlib-devel

env STACKDIR /tmp/build
env APPDIR   $STACKDIR/app
env STACK    $STACKDIR/stack
env PATH     $STACKDIR:$PATH

run mkdir -p  $STACKDIR
run chmod 777 $STACKDIR
run curl -sSL https://get.haskellstack.org/ | sh -s - -d $STACKDIR

run     useradd matt
user    matt
run     mkdir -p $APPDIR
workdir $APPDIR

# Pre-cache project dependencies -----------------------------------------------
copy --chown=matt:matt \
    LICENSE \
    Setup.hs \
    package.yaml \
    stack.yaml \
    $APPDIR/

run $STACK build --only-dependencies
run $STACK build --only-dependencies mattaudesse-com-lib:exe:mattaudesse-com
run $STACK build --only-dependencies mattaudesse-com-lib:test:spec
# ------------------------------------------------------------------------------

copy --chown=matt:matt Makefile README.md psc-package.json $APPDIR/
copy --chown=matt:matt static                              $APPDIR/static
copy --chown=matt:matt hs                                  $APPDIR/hs
copy --chown=matt:matt ps                                  $APPDIR/ps

run make build-hs && make test-hs
