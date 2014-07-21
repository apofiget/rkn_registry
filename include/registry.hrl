-define(DEFCONF, [
			{xml, "priv/request.xml"},
			{sign, "priv/request.bin"},
			{trace, "priv/logs/updates.log"},
			{doc_root, "./"},
			{hostname, "localhost"},
			{listen_on, "127.0.0.1"},
			{get_last_update_period, 600},
			{port, 8888},
			{dump_csv, true},
			{csv_file, "priv/dump.csv"},
    	{csv_separator, ";"},
    	{csv_fields, [decision,url,domain,ip]}
		]).

-define(REG_SRV_URL,"http://vigruzki.rkn.gov.ru/services/OperatorRequest/?wsdl").

-define(REG_TYPE, [{1,<<"Реестр ЕАИС"/utf8>>}, {2, <<"Реестр НАП"/utf8>>}, 
	{3, <<"Реестр 398-ФЗ"/utf8>>}, {4, <<"Реестр 97-ФЗ, организаторы распространения информации"/utf8>>}]).

get_reg_type(Id) -> proplists:get_value(Id, ?REG_TYPE, <<"Неизвестный тип реестра"/utf8>>).

get_option(Key) ->
    case application:get_env(Key) of
      undefined -> proplists:get_value(Key, ?DEFCONF);
      {ok, Val} -> Val
    end.
