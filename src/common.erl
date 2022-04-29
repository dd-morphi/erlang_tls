-module(common).

-export([configure_logger/1]).

configure_logger(File) ->
    logger:set_handler_config(default, level, debug),
    logger:add_handler(all,
                       logger_std_h,
                       #{filters => [{debug, {fun logger_filters:level/2, {stop, neq, debug}}}],
                         config => #{file => "log/" ++ File}}),
    logger:set_primary_config(level, all).
