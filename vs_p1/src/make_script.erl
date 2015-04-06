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
-export([make/0]).


make() ->

    c('tools/werkzeug.erl'), c('server/server.erl'), c('server/cmem.erl'), c('queue/hbq.erl'), c('queue/dlq.erl'), c('client/client.erl').


net_adm:ping('dieHBQ@Allquantor.fritz.box').

{'dieHBQ', 'dieHBQ@Allquantor.fritz.box'} ! {self(), {request,initHBQ}}.
