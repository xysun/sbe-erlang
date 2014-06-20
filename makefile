all: compile

compile:
	erl -make

compile-java:
	cd java/; ant; ant examples:java; cd ../

sbetool:
	erl -pa ebin/ -run sbetool main $(schema) $(outputdir) -run init stop -noshell

javacompatible: example javaread erlangread cleanup

example: compile
	$(info [**** Compiling Erlang examples ****])
	erl -pa ebin/ -run sbetool main schemas/example-schema-simple.xml src/example/ -run init stop -noshell
	erlc -o src/example/ebin src/example/baselinesimple/*.erl; erl -pa src/example/ebin/ -run example main -run init stop -noshell

javaread:
	$(info [***** Testing Java read erlang generated file ****])
	cp car_erlang java/; cd java/; ant examples:execjava; cd ../

erlangread:
	$(info [***** Testing Erlang read Java generated file ****])
	cp java/car_java .; erl -pa src/example/ebin/ -run example readJava -run init stop -noshell

perf-erlang:
	erlc -o src/example/ebin src/example/baselinesimple/carbenchmark.erl
	erl -pa src/example/ebin -run carbenchmark main -run init stop -noshell

perf-java:
	cd java/; ant -f perf-build.xml java:compile; ant -f perf-build.xml java:perf; cd ../

test: compile
	erlc -o ebin/ test/*.erl
	erl -pa ebin/ -eval "eunit:test(test_buffer, [verbose])"  -s init stop -noshell

cleanup:
	$(info [***** Cleaning up... *****])
	rm car_java; rm car_erlang
