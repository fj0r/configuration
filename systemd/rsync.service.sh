#!/usr/bin/env bash
while getopts 'f:t:n:' c; do
    case "$c" in
        f) from=${OPTARG} ;;
        t) to=${OPTARG} ;;
        n) svc=${OPTARG} ;;
    esac
done

if [ -z "$svc" ]; then
    svc=${svc:-${from//\//-}}
fi

echo $from $to $svc

cat << EOF | sudo tee rsync-$svc.service #/etc/systemd/system/rsync-$svc.service
[Unit]
Description=rsync
After=network.target

[Service]
User=root
Group=root
Type=forking
WorkingDirectory=/root
ExecStart=/usr/local/bin/watchexec -r -w $from -- rsync -avP --delete $from $to
Restart=always

[Install]
WantedBy=multi-user.target
EOF

sudo systemctl daemon-reload
sudo systemctl enable rsync-$svc.service
sudo systemctl start rsync-$svc.service

#bash rsync.service.sh -f /root/task/scripts/ -t 123 -n abc