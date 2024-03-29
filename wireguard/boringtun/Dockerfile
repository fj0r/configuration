FROM fj0rd/rs AS builder

WORKDIR /src
RUN git clone --depth=1 https://github.com/cloudflare/boringtun.git \
 && cd boringtun \
 && cargo build --release \
 && strip ./target/release/boringtun

FROM debian:testing-slim

WORKDIR /app
COPY --from=builder /src/boringtun/target/release/boringtun /usr/local/bin

ENV WG_LOG_LEVEL=info \
    WG_THREADS=4 \
    WG_SUDO=1 \
    WG_QUICK_USERSPACE_IMPLEMENTATION=/usr/local/bin

RUN apt-get update \
 && echo "resolvconf resolvconf/linkify-resolvconf boolean false" \
      | debconf-set-selections \
 && DEBIAN_FRONTEND=noninteractive \
    apt-get install -y --no-install-recommends \
      wireguard-tools \
      iproute2 iptables \
      tcpdump \
      resolvconf

CMD ["wg-quick", "up", "$1"]
