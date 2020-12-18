### GO
export GOROOT=/opt/go \
       GOPATH=${HOME}/go
export PATH=$GOPATH/bin:$GOROOT/bin:$PATH \
       GO_VERSION=1.12.1

# set -ex \
#   ; cd /opt \
#   ; cat ~/pub/Platform/go${GO_VERSION}.linux-amd64.tar.gz \
#       | tar xzf -