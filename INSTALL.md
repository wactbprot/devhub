# Installation

```
sudo apt install openjdk-17-jdk-headless
```

## Standalone version
 
### tools.deps and tools.build

```shell
clj -T:build clean
clj -T:build prep
clj -T:build uber
```

```shell
java -jar devhub.jar
```

## systemd

```shell
cd /path/to/devhub
sudo mkdir /usr/local/share/devhub
sudo cp devhub.jar /usr/local/share/devhub
```

**Note**: 

Problem: *fatal: unsafe repository ('/path/to/repo' is owned by someone else)*

Services (e.g. scripts to be executed with `EXECUTE` like `git log`)
that need to run under a specific user account (refered to as `<user>`
below):

add the line `User=<user>` below the `[Service]` section of
`devhub.service`.



```shell
sudo cp devhub.service  /etc/systemd/system/

sudo systemctl enable devhub.service
sudo systemctl start devhub.service
```

Check status of `devhub` service by:

```shell
sudo systemctl status devhub.service


## ● devhub.service - Device Hub Server
##      Loaded: loaded (/etc/systemd/system/devhub.service; enabled; vendor preset: enabled)
##      Active: active (running) since Sun 2021-06-13 14:59:59 CEST; 8s ago
##    Main PID: 579285 (java)
##       Tasks: 34 (limit: 8942)
##      Memory: 383.6M
##      CGroup: /system.slice/devhub.service
##              └─579285 /usr/bin/java -Xmx1024M -Xms1024M -jar devhub.jar nogui
##
## Jun 13 15:00:03 aleph java[579285]:                     \ \
## Jun 13 15:00:03 aleph java[579285]:                      > \
## Jun 13 15:00:03 aleph java[579285]:                     / ^ \
## Jun 13 15:00:03 aleph java[579285]:                    /_/ \_\
## Jun 13 15:00:03 aleph java[579285]:      _                  _               _
## Jun 13 15:00:03 aleph java[579285]:   __| |   ___  __   __ | |__    _   _  | |__
## Jun 13 15:00:03 aleph java[579285]:  / _` |  / _ \ \ \ / / | '_ \  | | | | | '_ \
## Jun 13 15:00:03 aleph java[579285]: | (_| | |  __/  \ V /  | | | | | |_| | | |_) |
## Jun 13 15:00:03 aleph java[579285]:  \__,_|  \___|   \_/   |_| |_|  \__,_| |_.__/
## Jun 13 15:00:03 aleph java[579285]:
```

If `javascript` or `Python` pre- or postprocessing is needed copy the
`js` and `py` folder under `./resources` to `/usr/local/share/devhub`.


```shell
sudo cp -r resources /usr/local/share/devhub
```

## devel install notes

### tcp

The `TCP` action works out of the box.

### vxi11

Use `ant` (in case ant is missing: `sudo apt install ant`)  to build the `jvxi11` jar:

```
cd resources
git clone https://github.com/wactbprot/jvxi11.git
cd jvxi11
ant compile
ant jar
```

[org/epics/pvioc/pdrv/vxi11/package-tree](http://epics-pvdata.sourceforge.net/docbuild/pvIOCJava/2.0-BETA/documentation/html/org/epics/pvioc/pdrv/vxi11/package-tree.html)

### modbus

`modbus` works out of the box. The used library is:

[jlibmodbus](https://mvnrepository.com/artifact/com.github.kochedykov/jlibmodbus/1.2.9.0)
