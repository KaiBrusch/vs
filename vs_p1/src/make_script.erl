%%%-------------------------------------------------------------------
%%% @author kbrusch
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Apr 2015 7:06 PM
%%%-------------------------------------------------------------------
-module(make_script).
-author("kbrusch").
-expiort([findConistentWError/3]).



findConistentWError([Head | Tail], [], false) ->
  findConistentWError(Tail, []++[Head], false);

findConistentWError([], Akku,_) -> Akku.

findConistentWError([Head | Tail], Akku, false) ->
  case lists:last(Akku) == Head of
    true -> findConistentWError(Tail, []++[Head], false);
    false -> findConistentWError(Tail, []++[Head]++['ERROR'], true)
  end;

findConistentWError([Head | Tail], Akku, true) ->
  case lists:last(Akku) == Head of
    true -> findConistentWError(Tail, []++[Head], true);
    false -> Akku
  end.




%make() ->

%    c('tools/werkzeug.erl'), c('server/server.erl'), c('server/cmem.erl'), c('queue/hbq.erl'), c('queue/dlq.erl'), c('client/client.erl').


%net_adm:ping('dieHBQ@Allquantor.fritz.box').

%{'dieHBQ', 'dieHBQ@kai-b.fritz.box'} ! {self(), {request,initHBQ}}.
%{'dieHBQ', 'dieHBQ@kws-70-162.HAW.1X' } ! {self(), {request,initHBQ}}.
% 'dieHBQ@kws-70-162.HAW.1X'
% net_adm:ping('dieHBQ@ws-70-162.HAW.1X')