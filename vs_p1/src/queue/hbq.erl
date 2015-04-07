%%%-------------------------------------------------------------------
%%% @author kbrusch
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Apr 2015 10:06 AM
%%%-------------------------------------------------------------------
-module(hbq).
-author("kbrusch").
-export([initHBQandDLQ/3, start/0]).
-define(QUEUE_LOGGING_FILE, fun() -> werkzeug:message_to_string(erlang:date()) ++ "-HBQ.txt" end).


%start()

%% Definition: Startet einen neuen HBQ-Prozess, der die HBQ verwaltet und alleinigen Zugriff auf die DLQ besitzt.

% pre: Server-Prozess ist gestartet
% post: Es wurde ein neuer HBQ-Prozess gestartet der nun vom Server verwendet werden kann
% return: hbq-process started als Atom sonst eine sinnvolle Error-Meldung


start() ->
  % receive values from config
  {ok, ConfigListe} = file:consult('../server.cfg'),
  {ok, HBQname} = werkzeug:get_config_value(hbqname, ConfigListe),
  {ok, DlqLimit} = werkzeug:get_config_value(dlqlimit, ConfigListe),

  % register this process
  erlang:register(HBQname, self()),

  % init logger file
  HBQLoggerFile = 'hbq.log',

  % lifetime loop
  loop(DlqLimit, HBQname, HBQLoggerFile, [], []).


% loop()

%% Definition: Die Hauptschleife der HBQ wartet auf Anfragen bzgl. des Servers.
%% Direkt nach der Erstellung des HBQ-Prozesses, bekommt dieser die Anfrage: {ServerPID, {request,initHBQ}} vom Server.
%% Es wird also die Methode „initHBQandDLQ(ServerPID)“ aufgerufen.
%% Erhält der HBQ-Prozess die Nachricht {ServerPID, {request,pushHBQ,[NNr,Msg,TSclientout]}} wird die Methode „pushHBQ(ServerPID, OldHBQ, [NNr,Msg,TSclientout])“ aufgerufen.
%% Eine weitee Nachricht die dieser Prozess erhalten kann ist: {ServerPID, {request,deliverMSG,NNr,ToClient}}.
%% Nun wird die Methode „deliverMSG(ServerPID, DLQ, NNr, ToClient)“ aufgerufen.
%% Wird nun der Server terminiert, so muss auch der HBQ-Prozess geschlossen werden.
%% Dieser erhält folgende Nachricht: {ServerPID, {request,dellHBQ}}. Es wird die Methode „dellHBQ(ServerPID)“ für diese Aufgabe aufgerufen.

% pre: keine
% post: der Prozess wurde erfolgreich terminiert
% return: hbq-process terminated als Atom

loop(DlqLimit, HBQname, HBQLoggerFile, HBQ, DLQ) ->
  receive

    {ServerPID, {request, initHBQ}} ->
      {_HBQ, _DLQ} = initHBQandDLQ(DlqLimit, ServerPID, HBQLoggerFile)
      , loop(DlqLimit, HBQname, HBQLoggerFile, _HBQ, _DLQ)
  ;
    {ServerPID, {request, pushHBQ, [NNr, Msg, TSclientout]}} ->
       _NewHBQ = pushHBQ(ServerPID, HBQ, [NNr, Msg, TSclientout])
      , {NewHBQ, NewDLQ} = pushSeries(_NewHBQ, DLQ)
      , werkzeug:logging(HBQLoggerFile, 'gepusht /n')
      , loop(DlqLimit, HBQname, HBQLoggerFile, NewHBQ, NewDLQ)

  ;
    {ServerPID, {request, deliverMSG, NNr, ToClient}} ->
      deliverMSG(ServerPID, DLQ, NNr, ToClient, HBQLoggerFile)
      , loop(DlqLimit, HBQname, HBQLoggerFile, HBQ, DLQ)
  ;
    {ServerPID, {request, dellHBQ}} ->
      dellHBQ(ServerPID, HBQname)


  end.


% initHBQandDLQ(ServerPID)

%% Definition: Initialisiert die HBQ und DLQ. Ruft die Methode „initDLQ(Size, Datei)“ in dem Modul „dlq.erl“ auf.
%% Diese erhält nun die DLQ-ADT in der Form: {Size, []} (siehe DLQ-Definition).
%% Dem Server wird die Meldung {reply,ok} geschickt, als Zeichen der korrekten Initialisierung.
%% Der loop- Methode wird das 2-Tupel {HBQ, DLQ} zurückgegeben (siehe return).

% pre: korrekte ServerPID unter der der Server erreichbar ist
% post: ein 2-Tupel wurde erstellt. Das 1. Element ist die HBQ und das 2. Element die DLQ.
% return: 2-Tupel: {[], DLQ}

initHBQandDLQ(Size, ServerPID, HBQLoggerFile) ->
  DLQ = dlq:initDLQ(Size, HBQLoggerFile),
  ServerPID ! {reply, ok},
  {[], DLQ}.


% pushHBQ(ServerPID, OldHBQ, [NNr, Msg, TSclientout])

%% Definition: Fügt die Msg (Textzeile) mit Nummer (NNr) und dem Sende-Zeitstempel (TSclientout) in die alte HBQ ein.
%% Dem Server wird über die ServerPID ebenfalls ein {reply, ok} zugeschickt.

% pre: ServerPID mit der der Server erreicht werden kann, sowie eine korrekte OldHBQ.
% post: Der alten HBQ wurde ein neues Element beigefügt und der Server hat eine Nachricht erhalten.
% return: NewHBQ

pushHBQ(ServerPID, OldHBQ, [NNr, Msg, TSclientout]) ->
  Tshbqin = erlang:now(),
  %erlang:display("das ist die queue"++werkzeug:to_String(OldHBQ)),
  %erlang:display("das ist die neue queue"++werkzeug:to_String(OldHBQ ++ [{NNr, Msg, TSclientout, Tshbqin}])),
  SortedHBQ = sortHBQ(OldHBQ ++ [{NNr, Msg, TSclientout, Tshbqin}]),
  ServerPID ! {reply, ok},
  SortedHBQ.

% deliverMSG(ServerPID, DLQ, NNr, ToClient), erweitert fuer logging

%% Definition: Beauftragt die DLQ die Nachricht mit geforderter NNr an den Client (ToClient) zu senden.
%% Sie ruft intern die Methode „deliverMSG(MSGNr, ClientPID, Queue, Datei)“ aus dem Modul „dlq.erl“ auf.
%% Dem Server ist im Anschluss an dieser Methode die Nachricht {reply, SendNNr} zurück zu senden.
%% SendNNr ist die vom DLQ-Modul zurückgegebene tatsächlich verschickte Nachrichtennummer.

%pre: korrekte Server-und ClientPID unter die beide Prozesse zu erreichen sind
%post: Der Client hat eine neue Nachricht erhalten, die DLQ ist um eine Nachricht kleiner geworden und der Server hat ein die tatsächlich gesendete Nachrichtennummer erhalten.
%return: Atom ok wird zurückgegeben

deliverMSG(ServerPID, DLQ, NNr, ToClient, HBQLoggerfile) ->
  {reply, {MSGNr, Msg, TSclientout, TShbqin, TSdlqin, TSdlqout}, Terminated} = dlq:deliverMSG(NNr, ToClient, DLQ, HBQLoggerfile),
  ToClient ! {reply, {MSGNr, Msg, TSclientout, TShbqin, TSdlqin, TSdlqout}, Terminated},
  ServerPID ! {reply, MSGNr}.


% dellHBQ(ServerPID)
%% Definition: Terminiert den HBQ-Prozess und schickt dem Server die Nachricht {reply, ok}

% pre: korrekte ServerPID an die ein {reply, ok} gesendet werden kann
% post: der Prozess wurde erfolgreich beendet
% return: Atom ok wird zurückgegeben

dellHBQ(ServerPID, HBQname) ->
  erlang:unregister(HBQname),
  ServerPID ! {reply, ok}.


%%Definition: Prüft auf Nachrichten / Nachrichtenfolgen, die ohne eine Lücke zu bilden in die DLQ eingefügt werden können.
%%Prüft außerdem, ob die Anzahl der Nachrichten, die in der HBQ sind, 2/3 der Anzahl beträgt die in die DLQ passen.
%%Ist dies der Fall, wird einmalig die Lücke der DLQ mit einer Fehlernachricht geschlossen (siehe Anforderung 6).

%pre: korrekt initialisierte HBQ- und DLQ-Datenstruktur
%post: veränderte HBQ- und DLQ-Datenstruktur
%return: {HBQ, DLQ} als 2-Tupel






pushSeries(HBQ, {Size, Queue}) ->

  %TODO TEST THAT SHEET WITH UNSORTED DLQ CAUSE DLQ SHOULD BE ALREADY SORTED
  ExpectedMessageNumber = dlq:expectedNrDLQ(dlq:sortDLQ({Size, Queue})),

  {CurrentLastMessageNumber, Msg, TSclientout, TShbqin} = head(HBQ),

  {NHBQ, NDLQ} = case {ExpectedMessageNumber == CurrentLastMessageNumber, two_thirds_reached(HBQ, Size)} of
                   {true, _} ->
                     NewDLQ = dlq:push2DLQ({CurrentLastMessageNumber, Msg, TSclientout, TShbqin}, {Size, Queue}, ?QUEUE_LOGGING_FILE),
                     NewHBQ = lists:filter(fun({Nr, _, _, _}) -> Nr =/= CurrentLastMessageNumber end,HBQ),
                     {NewHBQ, NewDLQ};
                   {false, false} ->
                     {HBQ, {Size, Queue}};
                   {false, true} ->
                     {ConsistentBlock, NewHBQ} = create_consistent_block(HBQ),
                     {NewHBQ, push_consisten_block_to_dlq(ConsistentBlock, {Size, Queue})}
                 end,
  {NHBQ, NDLQ}.



push_consisten_block_to_dlq(ConsistentBlock, DLQ) ->
  push_consisten_block_to_dlq_(ConsistentBlock, DLQ).

push_consisten_block_to_dlq_([H | T], DLQ) ->
  NewDLQ = dlq:push2DLQ(H, DLQ, ?QUEUE_LOGGING_FILE),
  push_consisten_block_to_dlq_(T, NewDLQ);

push_consisten_block_to_dlq_([], DLQ) ->
  DLQ.



create_consistent_block([H|T]) ->
  TAIL = erlang:tl(H ++ T),
  create_consistent_block_(H ++ T, TAIL, [], 0);
create_consistent_block([]) ->
  werkzeug:logging("create_consistent_block wurde mit einer Leeren HBQ aufgerufen, WTF"),
  {[],[]}.

produce_failure_message(NNr, _NNr) ->
  {_NNr, "Fehlernachricht von:" ++ NNr ++ " bis" ++ "_NNr", "Error", "Error"}.

create_consistent_block_([H | T], [_H | _T], Accu, Counter) ->
  {NNr, _, _, _} = H,
  {_NNr, _, _, _} = _H,

  case {erlang:abs(NNr - _NNr) > 1, Counter == 1} of
    {true, true} ->
      {Accu ++ H, _H ++ _T};
    {true, false} ->
      NewAccu = Accu ++ produce_failure_message(NNr, _NNr),
      create_consistent_block_(T, _T, NewAccu, Counter + 1);
    {false, true} ->
      create_consistent_block_(T, _T, Accu ++ H, Counter);
    {false, false} ->
      create_consistent_block_(T, _T, Accu ++ H, Counter)
  end;

create_consistent_block_([H | _], [], _, _) ->
  H.

head([]) ->
  1;
head(List) ->
  erlang:hd(List).
% pushSeries helper functions
two_thirds_reached(HBQ, Size) ->
  erlang:length(HBQ) >= 2 / 3 * Size.



sortHBQ(Queue) ->
  erlang:display("das ist die queue"++werkzeug:to_String(Queue)),
  ORDER = fun({NNr, _, _, _}, {_NNr, _, _, _}) ->
    NNr < _NNr end,
  lists:usort(ORDER, Queue).



