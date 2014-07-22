REBAR = `which rebar`

compile:
	@$(REBAR) get-deps
	@$(REBAR) compile

clean:
	@$(REBAR) clean
	@rm -f erl_crash.dump
