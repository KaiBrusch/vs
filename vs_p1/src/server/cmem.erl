%%%-------------------------------------------------------------------
%%% @author kbrusch
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Apr 2015 10:06 AM
%%%-------------------------------------------------------------------


%%% CMEM
%% {Clientlifetime,CMEM} {Int,[{<PID>,Int,{Int,Int,Int}}]}
%% Clientlifetime,CMEM} {Size,[{ClientPID,LastMessageID,LastMessageTimestamp}]}

-module(cmem).
-author("kbrusch").
-export([initCMEM/2, getClientNNr/2, updateClient/5, delExpiredCl/1]).
-include("../tools/ourtools.hrl").







% initCMEM(RemTime, Datei)

%% Definition: Initialisiert die CMEM für den Server.

% pre: keine
% post: neues 2-Tupel erstellt
% return: {RemTime, []} - 2-Tupel mit RemTime als erstes Element und eine leere CMEM-Liste als 2. Element

initCMEM(Clientlifetime, Datei) ->
  {Clientlifetime, []}.


% updateClient(CMEM, ClientID, NNr, Datei)

%% Definition: Speichert/Aktualisiert im CMEM die ClientID mit der NNr.

%pre: nötige Übergabeparameter sind korrekt
%post: neuer Client gespeichert, oder einen bereits vorhandenen Client aktualisiert
%% return: aktualisiertes CMEM


%% {Clientlifetime,CMEM} {Int,[{<PID>,Int,{Int,Int,Int}}]}
%% Clientlifetime,CMEM} {Size,[{ClientPID,LastMessageID,LastMessageTimestamp}]}




updateClient({Clientlifetime,CMEM}, ClientID, NNr, Datei,sendmsg) ->
  F = fun({_ClientID,_LastMessageNumer, _Time}) -> _ClientID =/= ClientID end,
  _NewCMEM = lists:filter(F,CMEM),
  {Clientlifetime,_NewCMEM ++ [{ClientID,NNr,erlang:now()}]};

updateClient({Clientlifetime,CMEM}, ClientID, NNr, Datei,time) ->
  %Find = fun({_ClientID,_LastMessageNumer, _Time}) -> _ClientID == ClientID end,

  Filter = fun({_ClientID,_LastMessageNumer, _Time}) -> _ClientID =/= ClientID end,
  {CClientID,LLastMessageNumber,TTime} =
    case lists:keyfind(ClientID,1,CMEM) of
      {ClientID,LastMessageNumber,Time} ->
        {ClientID,LastMessageNumber,Time};
      false ->
        {ClientID,NNr,erlang:now()}
    end,


  _NewCMEM = lists:filter(Filter,CMEM),

  {Clientlifetime,_NewCMEM ++ [{ClientID,LLastMessageNumber,erlang:now()}]}.


% getClientNNr(CMEM, ClientID)

%% Definition: Liefert dem Server die nächste Nachrichtennummer die an die ClientID geschickt werden soll.

% pre: keine
% post: nicht veränderte CMEM, da nur lesend
% return: ClientID als Integer-Wert, wenn nicht vorhanden wird 1 zurückgegeben

getClientNNr({RemTime, CMEMLIST}, ClientID) ->
  get_last_message_id(ClientID, CMEMLIST).


get_last_message_id(_, []) ->
  1;

get_last_message_id(ClientID, CMEM) ->
  {ClientID, Last_message_id, _Time} = lists:keyfind(ClientID, 1, CMEM),
  Last_message_id.


% delExpiredCl(CMEM, Clientlifetime)

%%Definition: In dieser Methode werden die Clients gelöscht, welche die Clientlifetime überschritten haben.

%pre: keine
%post: veränderte CMEM
%return: Das Atom ok als Rückgabewert // falsch veränderte CMEM

delExpiredCl({Clientlifetime, Queue}) ->
  Now = timestamp_to_millis(erlang:now()),
  F = fun({_ClientID,_LastMessageNumer, _Time}) -> (Now - timestamp_to_millis(_Time)) < Clientlifetime * 1000   end,
  NewQueue = lists:filter(F,Queue),
  {Clientlifetime,NewQueue}.



