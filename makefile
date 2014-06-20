all: compile

compile:
	erl -make

sbetool:
	erl -pa ebin/ -run sbetool main $(schema) $(outputdir) -run init stop -noshell

javacompatible: example javaread erlangread cleanup

example:
	$(info [**** Compiling Erlang examples ****])
	erlc -o src/example/ebin src/example/baselinesimple/*.erl; erl -pa src/example/ebin/ -run example main -run init stop -noshell

javaread:
	$(info [***** Testing Java read erlang generated file ****])
	cp car_erlang java/; cd java/; ant examples:execjava; cd ../

erlangread:
	$(info [***** Testing Erlang read Java generated file ****])
	cp java/car_java .; erl -pa src/example/ebin/ -run example readJava -run init stop -noshell

perf:
	erlc -o src/example/ebin src/example/baselinesimple/carbenchmark.erl
	erl -pa src/example/ebin -run carbenchmark main -run init stop -noshell

cleanup:
	$(info [***** Cleaning up... *****])
	rm car_java; rm car_erlang
