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
-export([initCMEM/2, getClientNNr/2, updateClient/4, delExpiredCl/1]).

% initCMEM(RemTime, Datei)

%% Definition: Initialisiert die CMEM für den Server.

% pre: keine
% post: neues 2-Tupel erstellt
% return: {RemTime, []} - 2-Tupel mit RemTime als erstes Element und eine leere CMEM-Liste als 2. Element

initCMEM(RemTime, Datei) ->
  {RemTime, []}.


% updateClient(CMEM, ClientID, NNr, Datei)

%% Definition: Speichert/Aktualisiert im CMEM die ClientID mit der NNr.

%pre: nötige Übergabeparameter sind korrekt
%post: neuer Client gespeichert, oder einen bereits vorhandenen Client aktualisiert
%% return: aktualisiertes CMEM

updateClient(CMEM, ClientID, NNr, Datei) ->

  if

    exists(ClientID, CMEM) -> update_last_message(ClientID,NNr, CMEM);

    not_exists(ClientID, CMEM) -> create(ClientID,CMEM)

  end.

% Listhelper
not_exists(ClientID, CMEM) ->
  not exists(ClientID, CMEM).

exists(_, []) ->
  false;

exists(ClientID, [{ClientID, _LastMessageNumber, _Time}| _Queue] ) ->
  true;

exists(ClientID, [_| _Queue]) ->
  exists(ClientID, _Queue).

create(ClientID, CMEM) ->
  [{ClientID, 1, erlang:now()}]++CMEM.


update_last_message(_, _, []) ->
  [];

update_last_message(ClientID, NNr, [{ClientID, _LastMessageNumber, Time}| _Queue]) ->
  [{ClientID, NNr, Time} | _Queue];

update_last_message(ClientID, NNr, [Head | Tail]) ->
  [Head | set_last_message(ClientID, NNr, Tail)].


get_last_message_id(_, []) ->
  1;

get_last_message_id(ClientID,[{ClientID, Last_message_id, _Time} | Queue]) ->
  Last_message_id;

get_last_message_id(ClientID,[_| Queue]) ->
  get_last_message_id(ClientID, Queue).

update_time_for_client(_,_, []) ->
  [];

update_time_for_client(ClientID, CurrentTime ,[{ClientID, LastMessageNumer,_Time} | Queue]) ->
  [{ClientID, LastMessageNumer, CurrentTime}| Queue];

update_time_for_client(ClientID, CurrentTime ,[Head| Tail]) ->
  [Head| update_time_for_client(ClientID, CurrentTime, Tail)].





% getClientNNr(CMEM, ClientID)

%% Definition: Liefert dem Server die nächste Nachrichtennummer die an die ClientID geschickt werden soll.

% pre: keine
% post: nicht veränderte CMEM, da nur lesend
% return: ClientID als Integer-Wert, wenn nicht vorhanden wird 1 zurückgegeben

getClientNNr(CMEM, ClientID) ->
  get_last_message_id(ClientID, CMEM).

% delExpiredCl(CMEM, Clientlifetime)

%%Definition: In dieser Methode werden die Clients gelöscht, welche die Clientlifetime überschritten haben.

%pre: keine
%post: veränderte CMEM
%return: Das Atom ok als Rückgabewert

delExpiredCl(CMEM) ->

  [CMEM || ]
  filter(Expired, CMEM, )




