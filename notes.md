#### Centos
* Use the minimal iso image
* `yum update`, `adduser joy`, `passwd joy`, `visudo`
* To `ssh` into guest VM from host: use NAT + port forwarding
* Install java: 
    
    `yum search java | grep 'java-'`
    
    `yum install java-1.7.0-openjdk-devel`

* Start `ssh` server: 

	`sudo /sbin/service sshd start`
	
* shutdown:

    `sudo shutdown -h now`

	
#### Erlang

* [efficiency guide](http://erlang.org/doc/efficiency_guide/)


#### SBE
* build & run example

    `git clone git@github.com:real-logic/simple-binary-encoding.git`

    `yum install ant ant-junit ant-trax`
    
    `ant`

    `java -jar sbe.jar example-schema.xml`
    
    `java -jar sbe.jar example-extension-schema.xml`
    
    copy `baseline/` and `extensions/` folder to the java example folder
    
    `ant examples:java`

* run benchmark:

    `sudo yum install gcc gcc-cpp gcc-c++`
    
    `ant -f perf-build.xml protobuf:build`
    
    add `<property name="protobuf.home location="deps/protobuf-2.5.0/"/>` to `perf-build.xml`
    
    copy the previously generated `baseline` to `perf/java/uk/co/real_logic/sbe/examples`
    
    `ant -f perf-build.xml clean`
    
    `ant -f perf-build.xml init`
    
    `java -Dsbe.output.dir=target/perf/java -jar target/dist/sbe.jar perf/resources/sbe/fix-message-samples.xml`
    
    `java -Dsbe.output.dir=target/perf/java -jar target/dist/sbe.jar perf/resources/sbe/car.xml`
    
    [change centos setup](http://serverfault.com/questions/389696/centos-etc-hosts-doesnt-resolve-my-hostname)
    
    	edit `/etc/sysconfig/network`, change `HOSTNAME` to `centos`
    
    	edit `/etc/hosts`, make sure having a line `127.0.0.1 centos.dd centos`
    
    `ant -f perf-build.xml java:perf:test`
   
#### Notes

* The SBE reference implementation consists of a compiler that takes a message schema as input and then generates language specific stubs. The stubs are used to directly encode and decode messages from buffers. The SBE tool can also generate a binary representation of the schema that can be used for the on-the-fly decoding of messages in a dynamic environment, such as for a log viewer or network sniffer.
    
* every `composite`, `enum`, `set` generate a new `java` file
* speed: tens of nanoseconds.
* `types`: `type`, `enum`, `set`, `composite`
* `message`: `field` -> `group` -> `data`

#### performance

* 1G RAM, 20G Disk, centos VM in dropbox

* using original example

[exec] Benchmark                                         Mode Thr    Cnt  Sec         Mean   Mean error    Units

[exec] u.c.r.protobuf.CarBenchmark.testDecode           thrpt   1     30    1      263.377       37.179   ops/ms

[exec] u.c.r.protobuf.CarBenchmark.testEncode           thrpt   1     30    1      175.462       15.670   ops/ms

[exec] u.c.r.protobuf.MarketDataBenchmark.testDecode    thrpt   1     30    1      787.063       96.413   ops/ms

[exec] u.c.r.protobuf.MarketDataBenchmark.testEncode    thrpt   1     30    1      692.691       79.078   ops/ms

[exec] u.c.r.sbe.CarBenchmark.testDecode                thrpt   1     30    1     2779.810       16.916   ops/ms

[exec] u.c.r.sbe.CarBenchmark.testEncode                thrpt   1     30    1     2779.987      106.962   ops/ms

[exec] u.c.r.sbe.MarketDataBenchmark.testDecode         thrpt   1     30    1    11274.246      126.895   ops/ms

[exec] u.c.r.sbe.MarketDataBenchmark.testEncode         thrpt   1     30    1    12806.140      181.391   ops/ms