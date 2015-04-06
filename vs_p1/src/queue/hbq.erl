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
-export([initHBQandDLQ/1,start/0]).


%start()

%% Definition: Startet einen neuen HBQ-Prozess, der die HBQ verwaltet und alleinigen Zugriff auf die DLQ besitzt.

% pre: Server-Prozess ist gestartet
% post: Es wurde ein neuer HBQ-Prozess gestartet der nun vom Server verwendet werden kann
% return: hbq-process started als Atom sonst eine sinnvolle Error-Meldung


start() ->
  % receive values from config
  {ok, ConfigListe}       = file:consult("../server.cfg"),
  {ok, HBQname}           = werkzeug:get_config_value(hbqname, ConfigListe),
  {ok, DlqLimit}           = werkzeug:get_config_value(dlqlimit, ConfigListe),

  % register this process
  erlang:register(HBQname ,self()),

  % init logger file
  HBQLoggerFile= 'hbq.log',

  % lifetime loop
  loop(HBQLoggerFile,DlqLimit, _, _).
.




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

loop(DlqLimit,HBQname,HBQLoggerFile,HBQ,DLQ) ->
  receive

    {ServerPID, {request, initHBQ}} ->
      {HBQ, DLQ}=initHBQandDLQ(DlqLimit, ServerPID, HBQLoggerFile)
      , loop(DlqLimit,HBQname,HBQLoggerFile, HBQ, DLQ)
  ;
    {ServerPID, {request,pushHBQ,[NNr,Msg,TSclientout]}} ->
      werkzeug:logging(HBQLoggerFile, 'gepusht')
      , pushHBQ(ServerPID, HBQ,[NNr,Msg,TSclientout])
      , loop(DlqLimit,HBQname,HBQLoggerFile, HBQ)
  ;
    {ServerPID, {request,deliverMSG,NNr,ToClient}} ->
      deliverMSG(ServerPID, DLQ, NNr, ToClient)
      , loop(DlqLimit,HBQname,HBQLoggerFile, HBQ)
  ;
    {ServerPID, {request,dellHBQ}} ->
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
  ServerPID ! {reply, ok},
  DLQ = dlq:initDLQ(Size,HBQLoggerFile),
  {[], DLQ}.




% pushHBQ(ServerPID, OldHBQ, [NNr, Msg, TSclientout])

%% Definition: Fügt die Msg (Textzeile) mit Nummer (NNr) und dem Sende-Zeitstempel (TSclientout) in die alte HBQ ein.
%% Dem Server wird über die ServerPID ebenfalls ein {reply, ok} zugeschickt.

% pre: ServerPID mit der der Server erreicht werden kann, sowie eine korrekte OldHBQ.
% post: Der alten HBQ wurde ein neues Element beigefügt und der Server hat eine Nachricht erhalten.
% return: NewHBQ

pushHBQ(ServerPID, OldHBQ, [NNr, Msg, TSclientout]) ->
  ServerPID ! {reply, ok},
  OldHBQ++[{NNr, Msg, TSclientout}].



% deliverMSG(ServerPID, DLQ, NNr, ToClient)

%% Definition: Beauftragt die DLQ die Nachricht mit geforderter NNr an den Client (ToClient) zu senden.
%% Sie ruft intern die Methode „deliverMSG(MSGNr, ClientPID, Queue, Datei)“ aus dem Modul „dlq.erl“ auf.
%% Dem Server ist im Anschluss an dieser Methode die Nachricht {reply, SendNNr} zurück zu senden.
%% SendNNr ist die vom DLQ-Modul zurückgegebene tatsächlich verschickte Nachrichtennummer.

%pre: korrekte Server-und ClientPID unter die beide Prozesse zu erreichen sind
%post: Der Client hat eine neue Nachricht erhalten, die DLQ ist um eine Nachricht kleiner geworden und der Server hat ein die tatsächlich gesendete Nachrichtennummer erhalten.
%return: Atom ok wird zurückgegeben

deliverMSG(ServerPID, DLQ, NNr, ToClient) ->

  ToClient ! {}
  {reply,ok}.


% dellHBQ(ServerPID)
%% Definition: Terminiert den HBQ-Prozess und schickt dem Server die Nachricht {reply, ok}

% pre: korrekte ServerPID an die ein {reply, ok} gesendet werden kann
% post: der Prozess wurde erfolgreich beendet
% return: Atom ok wird zurückgegeben

dellHBQ(ServerPID,HBQname) ->
  erlang:unregister(HBQname),
  ServerPID ! {reply, ok}.



% pushSeries(HBQ, DLQ)

%%Definition: Prüft auf Nachrichten / Nachrichtenfolgen, die ohne eine Lücke zu bilden in die DLQ eingefügt werden können.
%%Prüft außerdem, ob die Anzahl der Nachrichten, die in der HBQ sind, 2/3 der Anzahl beträgt die in die DLQ passen.
%%Ist dies der Fall, wird einmalig die Lücke der DLQ mit einer Fehlernachricht geschlossen (siehe Anforderung 6).

%pre: korrekt initialisierte HBQ- und DLQ-Datenstruktur
%post: veränderte HBQ- und DLQ-Datenstruktur
%return: {HBQ, DLQ} als 2-Tupel

pushSeries(HBQ, {Size, Queue}) ->
  if
    len(HBQ) == 2/3 * Size ->
      ok;
    len(HBQ) < 2/3 * Size ->
      push_consisten(HBQ, Queue)
  end.



