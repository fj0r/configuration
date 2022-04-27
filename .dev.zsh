export STACK_ROOT=/opt/stack
export GHC_ROOT=/opt/ghc
export PATH=${GHC_ROOT}/bin:$PATH
export LD_LIBRARY_PATH=$(fd . $(ghc --print-libdir) -t d -d 1 -X echo {} | sed "s/ /:/g"):/opt/language-server/haskell/lib/$(ghc --numeric-version):$LD_LIBRARY_PATH

export CARGO_HOME=/opt/cargo
export RUSTUP_HOME=/opt/rustup
export PATH=${CARGO_HOME}/bin:$PATH

export GOROOT=/opt/go GOPATH=${HOME:-/root}/go
export PATH=${GOPATH}/bin:${GOROOT}/bin:$PATH
export GO111MODULE=on
