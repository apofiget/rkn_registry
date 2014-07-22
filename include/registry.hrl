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

-define(MONTH,[{1,"January"}, {2,"February"}, {3,"March"}, {4,"April"}, 
			   {5,"May"}, {6,"June"}, {7,"Jule"}, {8,"August"}, {9,"September"}, 
			   {10,"October"}, {11,"November"}, {12,"December"}]).

-define(REG_SRV_URL,"http://vigruzki.rkn.gov.ru/services/OperatorRequest/?wsdl").

-define(REG_TYPE, [{1,<<"Реестр ЕАИС"/utf8>>}, {2, <<"Реестр НАП"/utf8>>}, 
	{3, <<"Реестр 398-ФЗ"/utf8>>}, {4, <<"Реестр 97-ФЗ, организаторы распространения информации"/utf8>>}]).
