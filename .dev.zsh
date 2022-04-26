export STACK_ROOT=/opt/stack
export GHC_ROOT=/opt/ghc
export PATH=${GHC_ROOT}/bin:$PATH

export CARGO_HOME=/opt/cargo
export RUSTUP_HOME=/opt/rustup
export PATH=${CARGO_HOME}/bin:$PATH

export GOROOT=/opt/go GOPATH=${HOME:-/root}/go
export PATH=${GOPATH}/bin:${GOROOT}/bin:$PATH
export GO111MODULE=on
