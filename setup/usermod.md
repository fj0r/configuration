# /etc/skel/
```
sudo useradd -ms /bin/zsh user
```

# group
```
sudo usermod -aG sudo user
```

# rename user
```
sudo usermod -l new_user -d /home/new_user -m old_user
sudo groupmod -n new_user old_user
```