### SBE-Erlang

This is the Erlang implementation of the [simple binary encoding](https://github.com/real-logic/simple-binary-encoding)

### License (See LICENSE file for full license)

Copyright (c) 2014, Xiayun Sun

Licensed under The BSD 3-Clause License.

### Features

* Support all primitive types, fixed-length arrays and variable length strings. (Section 2.4.1 and 2.7 of [SBE specification](http://www.fixtradingcommunity.org/pg/file/fplpo/read/1196757/simple-binary-encoding-release-candidate-2))
* Compatible with Java SBE implementation

### Directory layout

Main source code

```
src/
```

Unit tests

```
test/
```

Examples of usage

```
examples/
```

Original [Java SBE implementation](https://github.com/real-logic/simple-binary-encoding) (to demonstrate compatibility)

```
java/
```


### Build

Please first refer to [this wiki](https://github.com/xysun/sbe-erlang/wiki/System-setup) to make sure all system requirements are met, and also for troubleshooting. 

* Clone the repo: 

    ```
    git clone git@github.com:xysun/sbe-erlang.git
    ```

* Full clean build of both Erlang and Java

    ```
    make build
    ```

* Run unit tests

    ```
    make test
    ```

* Run examples: encode and decode a message in Erlang

    ```
    make example
    ```

* Demonstrate compatibility with Java implementation:

    This will encode a message with Erlang, pass the generated binary to Java to decode, and then reverse the process. 

    ```
    make javacompatible
    ```

* Run benchmarking

    This will benchmark both Erlang and Java implementation

    ```
    make perf
    ```

* Use the `sbetool`: 

    ```
    make sbetool schema=$schema_location outputdir=$outputdir
    // $schema_location is the path to the xml schema file
    // $outputdir is where to put the generated stubs
    // eg: make sbetool schema=examples/resources/example-schema-simple.xml outputdir=examples/
    ```

### Performance

The below benchmark uses the car example (schema can be found [here](https://github.com/xysun/sbe-erlang/blob/master/examples/resources/example-schema-simple.xml)) and is generated from a fresh 512MB RAM, 20GB SSD CentOS 6.5 x64 DigitalOcean server. 

Units are in nanoseconds per operation. 

|Implementation | Encoding | Decoding|
|---------------|:--------:|:-------:|
|SBE-Java       |   187.4ns       | 216.4ns        |
|Protobuf-Java |    5699ns      |  3797ns       |
|SBE-Erlang     |  7733ns        |6108ns         |

As can be seen from the table speed of SBE-Erlang is comparable to Protobuff Java implementation, but about 30-40x slower than SBE-Java. 

The major bottleneck for SBE-Erlang's performance is due to the fact that Erlang is a functional language and directly manipulating buffers is not allowed. As a result everytime a `setSomeProperty` method is called a new binary buffer is allocated with the newly added data. This clearly violates the Copy-Free and Allocation-Free [design principles](https://github.com/real-logic/simple-binary-encoding/wiki/Design-Principles) of SBE. 

It is however possible to improve the performance by providing a `setAll` method that will set all fixed-length blocks in one method call, since all data types and lengths are known from the schema file, thus only allocating once. A preliminary testing showed that encoding speed could be brought down by another 1000ns. Adding this feature is a work-in-progress. 

### API Documentation

Please check [this wiki](https://github.com/xysun/sbe-erlang/wiki/Erlang-Users-Guide) for the API documentation. 

### Todo

* a `setAll` method to improve encoding efficiency. 
* Support decimal/float. 
* More complete parsing of other type attributes, eg. `minValue`, `maxValue`, etc.
