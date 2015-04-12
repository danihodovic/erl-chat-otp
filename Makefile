ERLC_FLAGS=
SOURCES=$(wildcard src/*.erl)
all:
	erlc -o ebin/ $(SOURCES) 
clean:
	rm ebin/*.beam 
test:
	erl -noshell -pa ebin \
	-eval 'eunit:test("ebin",[verbose])' \
	-s init stop
