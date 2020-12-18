```
sudo systemctl stop systemd-resolved
```

修改 `/etc/systemd/resolved.conf`

```
[Resolve]
DNS=127.0.0.1
DNSStubListener=no
```

```
sudo ln -sf /run/systemd/resolve/resolv.conf /etc/resolv.conf
sudo systemctl start systemd-resolved
```

### k8s
kubectl edit configmaps coredns --namespace kube-system

```
#forward . /etc/resolv.conf
forward . 172.178.5.16
```