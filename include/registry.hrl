-define(DEFCONF, [
			{xml, "priv/request.xml"},
			{sign, "priv/request.bin"},
			{doc_root, file:get_cwd()},
			{hostname, "localhost"},
			{port, 8888}
		]).

-define(REG_SRV_URL,"http://vigruzki.rkn.gov.ru/services/OperatorRequest/?wsdl").

get_option(Key) ->
    case application:get_env(Key) of
      undefined -> proplists:get_value(Key, ?DEFCONF);
      {ok, Val} -> Val
    end.
