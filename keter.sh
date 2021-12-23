sudo mkdir -p /opt/keter /opt/keter/bin /opt/keter/etc /opt/keter/incoming
sudo cp /home/wale/keter /opt/keter/bin
sudo touch /opt/keter/etc/keter-config.yaml
sudo chown -R wale:wale /opt/keter
sudo touch /etc/systemd/system/keter.service
sudo echo "\
[Unit] 
Description=Keter 
After=network.service 

[Service]
Type=simple
ExecStart=/opt/keter/bin/keter /opt/keter/etc/keter-config.yaml

[Install]
WantedBy=multi-user.target" | sudo tee -a /etc/systemd/system/keter.service

sudo systemctl enable keter
sudo systemctl start keter
sudo mv keter-config.yaml /opt/keter/keter-config.yaml

