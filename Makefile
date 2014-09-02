REBAR = `which rebar`
RELX = `which relx`

compile:
	@$(REBAR) get-deps
	@$(REBAR) compile
	@(if test ! -d "priv"; then mkdir priv ; mkdir priv/logs; elif test ! -d "priv/logs" ;  then   mkdir priv/logs;  fi)

clean:
	@$(REBAR) clean
	@rm -f erl_crash.dump

release:
	@$(REBAR) compile
	@$(RELX) release
