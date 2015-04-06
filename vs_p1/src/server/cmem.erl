%%%-------------------------------------------------------------------
%%% @author kbrusch
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Apr 2015 10:06 AM
%%%-------------------------------------------------------------------

-module(cmem).
-author("kbrusch").
-export([initCMEM/2, getClientNNr/2, updateClient/4, delExpiredCl/1, delExpiredCl/1]).
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

updateClient(CMEM, ClientID, NNr, Datei) ->

  case exists(ClientID, CMEM) of

    true -> update_last_message(ClientID, NNr, CMEM);
    false -> create(ClientID, CMEM)

  end.

% Listhelper
not_exists(ClientID, CMEM) ->
  not exists(ClientID, CMEM).

exists(_, []) ->
  false;

exists(ClientID, [{ClientID, _LastMessageNumber, _Time} | _Queue]) ->
  true;

exists(ClientID, [_ | _Queue]) ->
  exists(ClientID, _Queue).

create(ClientID, CMEM) ->
  [{ClientID, 1, erlang:now()}] ++ CMEM.


update_last_message(_, _, []) ->
  [];

update_last_message(ClientID, NNr, [{ClientID, _LastMessageNumber, Time} | _Queue]) ->
  [{ClientID, NNr, Time} | _Queue];

update_last_message(ClientID, NNr, [Head | Tail]) ->
  [Head | set_last_message(ClientID, NNr, Tail)].



update_time_for_client(_, _, []) ->
  [];

update_time_for_client(ClientID, CurrentTime, [{ClientID, LastMessageNumer, _Time} | Queue]) ->
  [{ClientID, LastMessageNumer, CurrentTime} | Queue];

update_time_for_client(ClientID, CurrentTime, [Head | Tail]) ->
  [Head | update_time_for_client(ClientID, CurrentTime, Tail)].


% getClientNNr(CMEM, ClientID)

%% Definition: Liefert dem Server die nächste Nachrichtennummer die an die ClientID geschickt werden soll.

% pre: keine
% post: nicht veränderte CMEM, da nur lesend
% return: ClientID als Integer-Wert, wenn nicht vorhanden wird 1 zurückgegeben

getClientNNr({RemTime, CMEMLIST}, ClientID) ->
  get_last_message_id(ClientID, CMEMLIST).


getClientNNr({Clientlifetime, CMEM}, ClientID) ->
  get_last_message_id(ClientID, CMEM).

get_last_message_id(_, []) ->
  1;

get_last_message_id(ClientID, CMEM) ->
  {ClientID, Last_message_id, _Time} = lists:keyfind(ClientID, 1, CMEM),
  Last_message_id.


% delExpiredCl(CMEM, Clientlifetime)

%%Definition: In dieser Methode werden die Clients gelöscht, welche die Clientlifetime überschritten haben.

%pre: keine
%post: veränderte CMEM
%return: Das Atom ok als Rückgabewert

delExpiredCl({Clientlifetime, Queue}) ->
  delExpiredHelper({Clientlifetime, Queue}, []).


delExpiredHelper({Clientlifetime, []}, Akku) ->
  {Clientlifetime, Akku};

delExpiredHelper({RemTime, [{_Id, _LastMessage, Time} | TailQueue]}, Akku) ->
  case expired(Time, RemTime) of

    true -> delExpiredHelper({RemTime, TailQueue}, Akku ++ [{_Id, _LastMessage, Time}]);
    false -> delExpiredHelper({RemTime, TailQueue}, Akku)

delExpiredHelper({Clientlifetime, [{_Id, _LastMessage, Time} | Queue]}, Akku) ->
  case expired(Time, Clientlifetime) of
    false -> delExpiredHelper({Clientlifetime, Queue}, Akku ++ [{_Id, _LastMessage, Time}]);
    true -> delExpiredHelper({Clientlifetime, Queue}, Akku)
  end.


expired(Time, RemTime) ->
  timestamp_to_millis(erlang:now()) - timestamp_to_millis(Time) >= timestamp_to_millis(RemTime).




