# Installation

## Standalone version

### tools.deps

`tools.deps` version with [depstar](https://github.com/seancorfield/depstar):

```shell
clojure -X:uberjar :aot true :jar devhub.jar :main-class devhub.server :aliases '[:dev]'
```

```shell
java -jar devhub.jar
## =>
##                    __                           
##                    \ \                          
##                     \ \                         
##                      > \                        
##                     / ^ \                       
##                    /_/ \_\                      
##      _                  _               _       
##   __| |   ___  __   __ | |__    _   _  | |__    
##  / _` |  / _ \ \ \ / / | '_ \  | | | | | '_ \   
## | (_| | |  __/  \ V /  | | | | | |_| | | |_) |  
##  \__,_|  \___|   \_/   |_| |_|  \__,_| |_.__/   
```


### leiningen (old version)

A Standalone version of **devhub** is generated with:

```shell
git clone git@github.com:wactbprot/devhub.git
cd devhub
lein uberjar
```

Distribute `devhub-x.y.z-standalone.jar` and run with:

```shell
java -jar devhub-<latest-version>.jar
```


## Development version

```shell
git clone git@github.com:wactbprot/devhub.git
cd devhub
lein deps
```

## tcp

The `TCP` action works out of the box.

## vxi11

Use ant to build the jvxi11 jar:

```
cd resources
git clone https://github.com/wactbprot/jvxi11.git
cd jvxi11
ant compile
ant jar
```

[org/epics/pvioc/pdrv/vxi11/package-tree](http://epics-pvdata.sourceforge.net/docbuild/pvIOCJava/2.0-BETA/documentation/html/org/epics/pvioc/pdrv/vxi11/package-tree.html)

## modbus

`modbus` works out of the box. The used library is:

[jlibmodbus](https://mvnrepository.com/artifact/com.github.kochedykov/jlibmodbus/1.2.9.0)

