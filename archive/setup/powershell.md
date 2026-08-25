### readline
```
Install-Module PSReadLine
Install-Module posh-git
Install-Module oh-my-posh
Install-Module powershell-yaml

```

### Configuration
```
{
    "guid": "{574e775e-4f2a-5b96-ac1e-a2962a402336}",
    "hidden": false,
    "name": "pwsh",
    "commandline": "C:/Program Files/PowerShell/7/pwsh.exe -nologo",
    "source": "Windows.Terminal.PowershellCore",
    "startingDirectory": ".",
    "fontSize": 11,
    "historySize": 9001,
    "padding": "5, 5, 20, 25",
    "snapOnInput": true,
    "useAcrylic": false,
},
```

### install docker module
```
Register-PSRepository -Name DockerPS-Dev -SourceLocation https://ci.appveyor.com/nuget/docker-powershell-dev
Install-Module Docker -Repository DockerPS-Dev -Force
```

### Dns
```
Get-NetAdapter | Where-Object InterfaceAlias -eq WLAN | Select-Object InterfaceIndex | Get-DnsClientServerAddress
Get-NetAdapter | Where-Object InterfaceAlias -eq WLAN | Set-DnsClientServerAddress -ServerAddresses ("172.178.5.16","8.8.8.8")
```

### process
```
Get-Process | Where-Object CPU -gt 10 | Sort-Object CPU -Desending
```
