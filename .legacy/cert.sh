#!/bin/bash
cn=172.178.5.21
subj="/C=CN/ST=Shandong/L=Linyi/O=xmh/OU=eng/CN=$cn"
wd=$(pwd)/cert

openssl genrsa -out $wd/ca.key 4096
openssl req -x509 -new -nodes -sha512 -days 3650 \
    -subj $subj \
    -key $wd/ca.key \
    -out $wd/ca.crt

openssl genrsa -out $wd/$cn.key 4096
openssl req -sha512 -new \
    -subj $subj \
    -key $wd/$cn.key \
    -out $wd/$cn.csr

openssl x509 -req -sha512 -days 3650 \
    -extfile v3.ext \
    -CA $wd/ca.crt -CAkey $wd/ca.key -CAcreateserial \
    -in $wd/$cn.csr \
    -out $wd/$cn.crt

openssl x509 -inform PEM -in $wd/$cn.crt -out $wd/$cn.cert

sudo mkdir -p /etc/docker/certs.d/$cn/
sudo cp $wd/$cn.cert /etc/docker/certs.d/$cn/
sudo cp $wd/$cn.key /etc/docker/certs.d/$cn/
sudo cp $wd/ca.crt /etc/docker/certs.d/$cn/
