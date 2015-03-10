%% -*- coding: utf-8 -*-
-define(DEFCONF, [
                  {registry_url, "http://vigruzki.rkn.gov.ru/services/OperatorRequest/?wsdl"},
                  {xml, "/etc/rkn_registry/request.xml"},
                  {sign, "/etc/rkn_registry/request.bin"},
                  {trace, "/var/log/rkn_registry/updates.log"},
                  {www_log_path, "/var/log/rkn_registry/www/" },
                  {data_store_path, "/var/cache/rkn_registry/"},
                  {www_root, "/var/rkn_registry/www/"},
                  {hostname, "localhost"},
                  {listen_on, "127.0.0.1"},
                  {get_last_update_period, 600},
                  {get_registry_try_interval, 10},
                  {port, 8888},
                  {dump_csv, true},
                  {csv_separator, ";"},
                  {csv_fields, [decision,url,domain,ip]},
                  {dump_format_ver, "2.1"},
                  {email_on_update, false}
                 ]).

-define(MONTH,[{1,"January"}, {2,"February"}, {3,"March"}, {4,"April"},
               {5,"May"}, {6,"June"}, {7,"Jule"}, {8,"August"}, {9,"September"},
               {10,"October"}, {11,"November"}, {12,"December"}]).

-define(REG_TYPE, [{1,<<"Реестр ЕАИС"/utf8>>}, {2, <<"Реестр НАП"/utf8>>},
                   {3, <<"Реестр 398-ФЗ"/utf8>>}, {4, <<"Реестр 97-ФЗ, организаторы распространения информации"/utf8>>}]).

-define(RESULT_COMMENT, [{0, <<"запрос обрабатывается"/utf8>>}, {-1, <<"неверный алгоритм ЭП"/utf8>>}, {-2, <<"неверный формат ЭП"/utf8>>},
                         {-3, <<"недействительный сертификат ЭП"/utf8>>}, {-4, <<"некорректное значение ЭП"/utf8>>}, {-5, <<"ошибка проверки сертификата ЭП"/utf8>>},
                         {-6, <<"у заявителя отсутствует лицензия, дающая право оказывать услуги по предоставлению
доступа к информационно-телекоммуникационной сети Интернет"/utf8>>}, {-7, <<"отсутствует идентификатор запроса"/utf8>>},
  {-8, <<"неверный формат идентификатора запроса"/utf8>>}, {-9, <<"не найден запрос по указанному идентификатору"/utf8>>}, {-10, <<"повторите запрос позднее"/utf8>>}]).
