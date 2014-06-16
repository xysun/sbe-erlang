#### Centos
* `yum update`, `adduser joy`, `passwd joy`, `visudo`
* To `ssh` into guest VM from host: use NAT + port forwarding
* [Install java](http://www.if-not-true-then-false.com/2010/install-sun-oracle-java-jdk-jre-7-on-fedora-centos-red-hat-rhel/)
	
#### Erlang

* [efficiency guide](http://erlang.org/doc/efficiency_guide/)
* install `erlsom`: 
    * `git clone "git@github.com:danw/erlsom.git"
    * move the erlsom/ directory to /usr/local/lib/erlang/lib/
    * cd /usr/local/lib/erlang/lib/erlsom, makedir ebin
    * `cd src/`, open erlang shell, `make:all([{outdir, "../ebin"}]).`

#### SBE
* build & run example

    `git clone git@github.com:real-logic/simple-binary-encoding.git`

    `yum install ant ant-junit ant-trax`
    
    `ant`
    
    change the `jaxp_parser_impl` alternative from `xerces-j2` to `/usr/share/java/libgcj-4.4.7.jar`: `sudo alternatives --config jaxp_parser_impl`

    `ant examples:java`

* run benchmark:

    `sudo yum install gcc gcc-cpp gcc-c++`
    
    `ant -f perf-build.xml protobuf:build` (if `ant` emits error, may need to download using `curl` manually and put into `deps/pb`, then run `protobuf:build` without dependency on `protobuf:download`.)
    
    add `<property name="protobuf.home location="deps/protobuf-2.5.0/"/>` to `perf-build.xml`
    
    [change centos setup](http://serverfault.com/questions/389696/centos-etc-hosts-doesnt-resolve-my-hostname)
    
    	edit `/etc/sysconfig/network`, change `HOSTNAME` to `centos`
    
    	edit `/etc/hosts`, make sure having a line `127.0.0.1 centos.dd centos`
    
    `ant -f perf-build.xml`
   
#### Notes

* The SBE reference implementation consists of a compiler that takes a message schema as input and then generates language specific stubs. The stubs are used to directly encode and decode messages from buffers. The SBE tool can also generate a binary representation of the schema that can be used for the on-the-fly decoding of messages in a dynamic environment, such as for a log viewer or network sniffer.
    
* every `composite`, `enum`, `set` generate a new `java` file
* speed: tens of nanoseconds.
* `types`: `type`, `enum`, `set`, `composite`
* `message`: `field` -> `group` -> `data`

#### performance

* 1G RAM, 30G SSD on [DigitalOcean](https://www.digitalocean.com/?refcode=52476c7ad3e1)

* using original example

[exec] Benchmark                                         Mode Thr    Cnt  Sec         Mean   Mean error    Units

[exec] u.c.r.protobuf.CarBenchmark.testDecode           thrpt   1     30    1      354.925       13.302   ops/ms

[exec] u.c.r.protobuf.CarBenchmark.testEncode           thrpt   1     30    1      277.557        6.342   ops/ms

[exec] u.c.r.protobuf.MarketDataBenchmark.testDecode    thrpt   1     30    1     1180.244       21.789   ops/ms

[exec] u.c.r.protobuf.MarketDataBenchmark.testEncode    thrpt   1     30    1     1056.558       22.026   ops/ms

[exec] u.c.r.sbe.CarBenchmark.testDecode                thrpt   1     30    1     4513.438       81.523   ops/ms

[exec] u.c.r.sbe.CarBenchmark.testEncode                thrpt   1     30    1     4249.915      179.351   ops/ms

[exec] u.c.r.sbe.MarketDataBenchmark.testDecode         thrpt   1     30    1    17647.609      591.072   ops/ms

[exec] u.c.r.sbe.MarketDataBenchmark.testEncode         thrpt   1     30    1    20334.654      275.050   ops/ms
