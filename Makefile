ERLC_FLAGS=
SOURCES=$(wildcard src/*.erl)
TESTS=$(wildcard tests/*.erl)
all:
	erlc -o ebin/ $(SOURCES) $(TESTS)
clean:
	rm ebin/*.beam 
test:
	make all
	erl -noshell -pa ebin \
	-eval 'eunit:test("ebin",[verbose])' \
	-s init stop
