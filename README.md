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

For build troubleshooting please refer to [this wiki]

```
git clone https://github.com/xysun/sbe-erlang.git
```

Full clean build of both Erlang and Java

```
make build
```

Run unit tests

```
make test
```

Run examples: encode and decode a message in Erlang

```
make example
```

Demonstrate compatibility with Java implementation:

This will encode a message with Erlang, pass the generated binary to Java to decode, and then reverse the process. 

```
make javacompatible
```

Run benchmarking

This will benchmark both Erlang and Java implementation

```
make perf
```

Use the `sbetool`: 

```
make sbetool schema=$schema_location outputdir=$outputdir
// $schema_location is the path to the xml schema file
// $outputdir is where to put the generated stubs
// eg: make sbetool schema=examples/resources/example-schema-simple.xml outputdir=examples/
```

### Performance

The below benchmark uses the car example (schema can be found in `examples/resources/example-schema-simple.xml`) and is generated from a 1GB RAM, 30GB SSD CentOS 6.5 x64 DigitalOcean server. 

Units are in nanoseconds per operation. 

|Implementation | Encoding | Decoding|
|---------------|:--------:|:-------:|
|SBE-Java       |   187.4ns       | 216.4ns        |
|Protobuf-Java |    5699ns      |  3797ns       |
|SBE-Erlang     |  8858ns        |7226ns         |

As can be seen from the table speed of SBE-Erlang is comparable to Protobuff Java implementation, but about 30-40x slower than SBE-Java. 

The major bottleneck for SBE-Erlang's performance is due to the fact that Erlang is a functional language and directly manipulating buffers is not allowed. As a result everytime a `setSomeProperty` method is called a new binary buffer is allocated with the newly added data. This clearly violates the Copy-Free and Allocation-Free [design principles](https://github.com/real-logic/simple-binary-encoding/wiki/Design-Principles) of SBE. 

It is however possible to improve the performance by providing a `setAll` method that will set all fixed-length blocks in one method call, since all data types and lengths are known from the schema file, thus only allocating once. A preliminary testing showed that encoding speed could be brought down to sub-7000ns. Adding this feature is a work-in-process. 

### Todo

* a `setAll` method to improve encoding efficiency. 
* Support decimal/float. 
* More complete parsing of other type attributes, eg. `minValue`, `maxValue`, etc.
