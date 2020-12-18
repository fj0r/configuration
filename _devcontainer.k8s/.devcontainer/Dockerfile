ARG IMAGE=nnurphy/js
FROM ${IMAGE}

ARG USERNAME=dev
ARG USER_UID=1000
ARG USER_GID=$USER_UID
ENV DEBIAN_FRONTEND=noninteractive

# Install kubectl
ENV KUBECONFIG=/kube/config
RUN curl -LO https://storage.googleapis.com/kubernetes-release/release/$(curl -s https://storage.googleapis.com/kubernetes-release/release/stable.txt)/bin/linux/amd64/kubectl
RUN chmod +x ./kubectl
RUN mv ./kubectl /usr/bin/kubectl

# Install Telepresence
RUN set -eux \
  ; apt update \
  ; apt install -y --no-install-recommends \
    zsh git gnupg sudo iptables \
  ; curl -s https://packagecloud.io/install/repositories/datawireio/telepresence/script.deb.sh | bash \
  ; apt install -y --no-install-recommends telepresence \
  ; apt-get autoremove -y \
  ; apt-get clean -y \
  ; rm -rf /var/lib/apt/lists/* \
  \
  ; groupadd --gid ${USER_GID} ${USERNAME} \
  ; useradd -s /bin/zsh --uid $USER_UID --gid ${USER_GID} -m ${USERNAME} \
  ; echo ${USERNAME} ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/${USERNAME}\
  ; chmod 0440 /etc/sudoers.d/${USERNAME}

ENV DEBIAN_FRONTEND=
USER ${USERNAME}
WORKDIR /home/${USERNAME}


