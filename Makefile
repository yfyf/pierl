all: compile

compile:
	mkdir -p ebin/
	erlc \
		-o ebin/ \
		src/*.erl

run:
	erl \
		-pa ebin/ \
		-s pierl

