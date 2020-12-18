FROM {{image}}

ENV UID=1000
ENV USER={{user}}
ENV HOME=/home/${USER}

RUN set -ex \
#    ; echo '%wheel ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers \
    ; useradd -ms /bin/zsh -G sudo -u ${UID} ${USER}
RUN mkdir /{{workdir}} && chown -R ${USER}:${USER} /{{workdir}}
WORKDIR /{{workdir}}
USER ${USER}
