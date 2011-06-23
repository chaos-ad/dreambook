-ifndef(_logger_included).
-define(_logger_included, yeah).

-define(DEFAULT_LOGGER_CONFIG, "logger.conf").

-define(LOG_TRACE(Pattern, Args), dreambook_logger:format(?MODULE, trace, self(), Pattern, Args)).
-define(LOG_DEBUG(Pattern, Args), dreambook_logger:format(?MODULE, debug, self(), Pattern, Args)).
-define(LOG_INFO (Pattern, Args), dreambook_logger:format(?MODULE, info,  self(), Pattern, Args)).
-define(LOG_WARN (Pattern, Args), dreambook_logger:format(?MODULE, warn,  self(), Pattern, Args)).
-define(LOG_ERROR(Pattern, Args), dreambook_logger:format(?MODULE, error, self(), Pattern, Args)).
-define(LOG_FATAL(Pattern, Args), dreambook_logger:format(?MODULE, fatal, self(), Pattern, Args)).

-endif.
