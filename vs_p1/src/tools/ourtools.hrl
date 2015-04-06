%%%-------------------------------------------------------------------
%%% @author Allquantor
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Apr 2015 13:06
%%%-------------------------------------------------------------------
-author("Allquantor").
-export([timestamp_to_millis/1]).

-define(MAXIMAL_RESPONSE_TIME_BEFORE_ERROR, 5000).


timestamp_to_millis({MegaSecs, Secs, MicroSecs}) ->
  (MegaSecs * 1000000 + Secs) * 1000 + round(MicroSecs / 1000).