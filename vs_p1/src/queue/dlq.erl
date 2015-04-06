%%%-------------------------------------------------------------------
%%% @author kbrusch
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Apr 2015 10:06 AM
%%%-------------------------------------------------------------------
-module(dlq).
-author("kbrusch").
-export([initDLQ/2,deliverMSG/4]).

% initDLQ(Size, Datei)

%% Definition: Vom HBQ-Prozess aufgerufene Methode die eine neue leere DLQ-ADT zurückliefert.

% pre: keine
% post: 2-Tupel mit Größenangabe, sowie einer leeren Liste
% return: {Size, []} - 2-Tupel mit Size als 1. Element und einer leeren Liste als 2. Element

initDLQ(Size, Datei) ->
  werkzeug:logging('DLQ init', Datei),
  {Size, []}.

% expectedNr(Queue)

%% Definition: Als nächstes zu speichernde Nachrichtennummer wird an HBQ-Prozess zurückgegeben.

% pre: eine Queue in Form der DLQ-ADT (siehe initDLQ)
% post: Queue ist unverändert und eine korrekte Nachrichtennummer wurde zurückgegeben
% return: nächste Nachrichtennummer die verwendet werden kann, sonst 1 bei leerer Liste

expectedNr({Size, []}) ->
  1.

expectedNr({Size, Queue}) ->
  get_next_message_numer(last(Queue)).

get_next_message_numer([NNr, Msg, TSclientout, TShbqin]) -> NNr+1.

% push2DLQ([NNr, Msg, TSclientout, TShbqin], Queue, Datei)

%% Definition: Die von dem HBQ-Prozess kommende Nachricht im Format: [NNr, Msg, TSclientout, TShbqin] wird in die DLQ eingefügt.
%% Ebenso wird an die Msg ein aktueller Zeitstempel angehängt, sowie nochmals am Ende des Nachrichtenformates (TSdlqin).

% pre: Übergabeparameter Queue ist korrekte DLQ-ADT (siehe initDLQ)
% post: die DLQ ist nun um eine Nachricht gewachsen, sofern die Größe es zugelassen hat
% return: die neue DLQ: NewDLQ ; wurde die maximale Größe der DLQ erreicht wird eine erkennbare Error-Meldung zurückgegeben


push2DLQ([{NNr, Msg, TSclientout, TShbqin}], {Size, Queue}, Datei) ->
  if
    len(Queue) == Size ->
      werkzeug:logging('dlq full', Datei),
      {dlq_full};

    len(Queue) < Size ->
      werkzeug:logging('added to dlq', Datei),
      {Size, Queue++[{NNr, Msg, TSclientout, erlang:now()}]}
  end.


% deliverMSG(MSGNr, ClientPID, Queue, Datei)

%% Definition: Der HBQ-Prozess ruft diese Methode der DLQ auf,
%% um sie aufzufordern die Nachricht mit angegebener MSGNr an die ClientPID zu senden.
%% An die Nachricht wird zusätzlich ein Ausgangszeitstempel (TSdlqout) angehängt.
%% Ist die MSGNr allerdings nicht vorhanden, wird die nächst größere Nummer verwendet.
%% Das Format der Nachricht die dem Client gesendet wird hat folgende Gestalt: {reply,[NNr, Msg, TSclientout, TShbqin, TSdlqin, TSdlqout], Terminated}. Terminated signalisiert, ob es noch weitere Nachrichten zu senden gibt (false). Sonst true.

% pre: die Queue ist in Form der DLQ-ADT (siehe initDLQ), sowie die ClientPID ist korrekt
% post: eine MSGNr wurde zurückgegeben und die Queue um diese Nachricht verkleinert
% return: die tatsächlich verschickte MSGNr als Integer-Wert an den HBQ-Prozess


deliverMSG(MSGNr, ClientPID, {Size, Queue}, Datei) ->
  find_message_numer(MSGNr, Queue).
% antwort nachricht bauen etc, und gucken ob es die letzte Nachricht ist

find_message_number(MSGNr, []) ->
  [];

find_message_number(MSGNr, [{MSGNr, Msg, TSclientout, TShbqin, TSdlqin, TSdlqout}|Queue]) ->
  {MSGNr, Msg, TSclientout, TShbqin, TSdlqin, TSdlqout};

find_message_number(MSGNr, [Head|Tail]) ->
  [Head | find_message_number(MSGNr, Tail)].


