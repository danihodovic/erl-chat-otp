-module(app_test).
-include_lib("eunit/include/eunit.hrl").


message_server_test_() ->
    {setup,
     fun () -> application:start(chatserver) end,
     fun (_) -> application:stop(chatserver) end,
     [
        fun(Zoom) -> ?_assertEqual(4, 4) end
     ]}.


