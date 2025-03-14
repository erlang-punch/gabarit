.PHONY: compile test ct proper clean

all: compile

compile:
	rebar3 compile

test: ct proper

ct:
	rebar3 ct

proper:
	rebar3 proper

proptest:
	rebar3 proptest

clean:
	rebar3 clean
	rm -rf _build
