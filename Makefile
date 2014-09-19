APP=local

DIALYZER_OPTS=-Werror_handling -Wrace_conditions -Wunmatched_returns

all: compile xref eunit dialyze

init:
	@eval "if ! [ -f 'src/${APP}.app.src' ]; then ./rebar create-app appid=${APP}; fi"
	@./rebar prepare-deps

compile:
	@./rebar -r compile skip_deps=true

refresh:
	@./rebar refresh-deps
	@rm -f .dialyzer.plt

xref:
	@./rebar -r xref skip_deps=true

clean:
	@./rebar -r clean skip_deps=true
	@rm -f .dialyzer.plt

distclean:
	@git clean -d -f -x

eunit:
	@./rebar -r eunit skip_deps=true

edoc:
	@./rebar -r doc skip_deps=true

start: compile
	@erl -pz ebin apps/*/ebin deps/*/ebin -eval 'erlang:display({start_app, $(APP), application:ensure_all_started($(APP))}).'

.dialyzer.plt:
	touch .dialyzer.plt
	dialyzer --build_plt --plt .dialyzer.plt --apps erts kernel stdlib crypto compiler

dialyze: compile .dialyzer.plt
	dialyzer --plt .dialyzer.plt -r ebin $(DIALYZER_OPTS)
