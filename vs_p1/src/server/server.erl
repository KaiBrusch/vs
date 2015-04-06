%%%-------------------------------------------------------------------
%%% @author kbrusch
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Apr 2015 10:06 AM
%%%-------------------------------------------------------------------
-module(server).
-author("kbrusch").
-export([start/0]).


%start()

%% Definition: Startet einen neuen Server-Prozess,
%% damit dieser im Verlauf Nachrichten erhalten kann und den
%% HBQ-Prozess auffordern kann Nachrichten an einen Leser-Clients zu senden.
%% Der Server initialisiert eine HBQ, indem er dem HBQ-Prozess folgende Nachricht schickt:
%% {self(), {request,initHBQ}}. Während der HBQ-Prozess die Datenstrukturen HBQ und DLQ erzeugt,
%% erzeugt der Server die CMEM-Datenstrutktur, indem er „initCMEM(RemTime, Datei)“ aufruft.
%% Im Anschluss wartet er auf eine Antwort, seitens des HBQ-Prozesses: {reply, ok}.
%% Des Weiteren muss der Server im lokalen Namensdienst registriert werden (siehe Anforderung 8).

% pre: keine
% post: Server ist gestartet und im lokalen Namensdienst von Erlang registriert
% return: server started als Atom sonst eine sinnvolle Error-Meldung


-define(SERVER_LOGGING_FILE, fun() -> werkzeug:message_to_string(erlang:date()) ++ "-ClientLog.txt" end).
-include("../tools/ourtools.hrl").

initHBQ(HBQname, HBQnode) ->
  {HBQname, HBQnode} ! {self(), {request, initHBQ}},
  receive
    {reply, ok} ->
      werkzeug:logging(?SERVER_LOGGING_FILE, "HBQ and DLQ was intialized")
  after ?MAXIMAL_RESPONSE_TIME_BEFORE_ERROR ->
    werkzeug:logging(?SERVER_LOGGING_FILE, "HBQ and DLQ was not initialzed ERROR!")
  end.

start() ->

  % lade die Parameter aus der Config Datei
  {Latency, Clientlifetime, Servername, HBQname, HBQnode, DLQlimit} = readConfig(),

  % Logifle fuer den Server log
  % wo ist das file
  ServerLogFile = 'server.log',

  % registriere den Prozess mit dem Erlang Prozess
  erlang:register(Servername, self()),

  % Nachrichtennummer zum start
  INNR = 1,

  % Node uebergreifend registrieren
  net_adm:ping(HBQnode),

  % HBQ und DLQ initialisieren
  initHBQ(HBQname, HBQnode),

  % CMEM initialisieren
  CMEM = cmem:initCMEM(Clientlifetime, ServerLogFile),

  TimeOfLastConnection = erlang:now(),

  loop(Latency, Clientlifetime, Servername, HBQname, HBQnode, DLQlimit, CMEM, INNR, ServerLogFile, TimeOfLastConnection).


%readConfig()

%% Definition: Vor dem Start des Server-Prozesses muss die Konfigurationsdatei (siehe Vorlage) des Servers ausgelesen werden.
%% Das Modul „werkzeug.erl“ vom Professor wird hierfür verwendet.


% pre: Die Datei „server.cfg“ ist vorhanden
% post: Die Datei wurde erfolgreich ausgelesen und die erforderlichen Werte zurückgeliefert
% return: {Latency, Clientlifetime, Servername, HBQname, HBQnode, DLQlimit}

readConfig() ->

  {ok, ConfigListe} = file:consult("../server.cfg"),
  {ok, Latency} = werkzeug:get_config_value(latency, ConfigListe),
  {ok, Clientlifetime} = werkzeug:get_config_value(clientlifetime, ConfigListe),
  {ok, Servername} = werkzeug:get_config_value(servername, ConfigListe),
  {ok, DLQlimit} = werkzeug:get_config_value(dlqlimit, ConfigListe),
  {ok, HBQname} = werkzeug:get_config_value(hbqname, ConfigListe),
  {ok, HBQnode} = werkzeug:get_config_value(hbqnode, ConfigListe),
  erlang:display(HBQname),
  erlang:display(HBQnode),


  {Latency, Clientlifetime, Servername, HBQname, HBQnode, DLQlimit}.


%loop(Latency, Clientlifetime, Servername, HBQname, HBQnode, DLQlimit)

%% Anemerkung: Wir haben die loop um die CMEM erweitert

%% Definition: Zu Beginn dieser Methode prüft ruft der Server die Methode „delExpiredCl(CMEM, Clientlifetime)“ aus dem Modul „cmem.erl“ auf, um zu prüfen, ob Clients gelöscht werden können.
%% In dieser Hauptschleife wartet der Server auf Anfragen seitens der Clients. Ebenso müssen Nachrichten seitens des HBQ-Prozesses abgearbeitet werden.
%% Erhält der Server die Nachricht eines Clients mit folgendem Format: {ClientPID, getmessages}, wird die Methode „sendMessages(ClientPID)“ aufgerufen.
%% Wird die Nachricht {dropmessage,[INNr, Msg, TSclientout]} von einem Client empfangen, ruft der Server die Methode „dropmessage(HBQname, HBQnode)“ auf.
%% Eine weitere Nachricht die seitens eines Clients beim Server eintreffen kann ist folgende: {ClientPID, getmsgid}.
%% Es muss nun die Methode sendMSGID(ClientPID) aufgerufen werden.
%% Wird der Server terminiert, da sich nach einer gewissen Zeit kein Client mehr bei ihm gemeldet hat,
%% muss auch der HBQ-Prozess terminiert werden. Dies erfolgt mit der Nachricht: {self(), {request,dellHBQ}} an den HBQ-Prozess.
%% Der Server wartet auf folgende Antwort: {reply, ok}.

%pre: die nötigen Übergabeparameter sind korrekt eingelesen
%post: der Prozess wurde erfolgreich, nach angegebenen Kriterien, terminiert
%return: server terminated als Atom


loop(Latency, Clientlifetime, Servername, HBQname, HBQnode, DLQlimit, CMEM, INNR, ServerLogFile, TimeOfLastConnection) ->

  case timestamp_to_millis(erlang:now()) - timestamp_to_millis(TimeOfLastConnection) < Latency * 1000 of
    true ->
      CMEM = cmem:delExpiredCl(CMEM),


      %if
      %  erlang:now() - timeOfLastConnection > erlang:now() ->
      %    HBQ ! pushHBQ

      receive

        {dropmessage, [INNr, Msg, TSclientout]} ->
          dropmessage(HBQname, HBQnode, [INNr, Msg, TSclientout]),
          loop(Latency, Clientlifetime, Servername, HBQname, HBQnode, DLQlimit, CMEM, INNR, ServerLogFile, erlang:now())
      ;

        {ClientPID, getmessages} ->
          NewCMEM = sendMessages(ClientPID, CMEM, HBQname, HBQnode),
          loop(Latency, Clientlifetime, Servername, HBQname, HBQnode, DLQlimit, NewCMEM, INNR, ServerLogFile, erlang:now())
      ;

        {ClientPID, getmsgid} ->
          sendMSGID(ClientPID, INNR),
          NewCMEM = cmem:updateClient(CMEM, ClientPID, INNR, ServerLogFile),
          loop(Latency, Clientlifetime, Servername, HBQname, HBQnode, DLQlimit, NewCMEM, INNR + 1, ServerLogFile, erlang:now())

      end;
    false ->
      shutdownRoutine(HBQname,HBQnode)
  end.


shutdownRoutine(HBQName,HBQNode) ->
  {HBQName,HBQNode} ! self(),
  receive
    {reply,ok} ->
      werkzeug:logging(?SERVER_LOGGING_FILE,"HBQ Terminated Correctly"),
      werkzeug:logging(?SERVER_LOGGING_FILE,"Server Terminated Correctly"),
      ok
  after ?MAXIMAL_RESPONSE_TIME_BEFORE_ERROR ->
    werkzeug:logging(?SERVER_LOGGING_FILE,"cant correct shutdown HBQ no answer try again"),
    shutdownRoutine(HBQName,HBQNode)
  end.



% sendMSGID(ClientPID, CMEM)

%% Definition: Der Server muss nun im Modul „cmem.erl“ die Methode „getClientNNr(CMEM,ClientID)“ aufrufen,
%% damit dieser die aktuelle Nachrichtennummer erhalten kann.
%% Der Server schickt dann die Nachricht {nid, Number} an die angegeben ClientPID.
%% Number ist die empfangene Nachrichtennummer.

%pre: korrekt übergebene Client PID
%post: erfolgreiche Ermittlung einer neuen MSGID
%return: Atom ok wird zurückgegeben

sendMSGID(ClientPID, INNR) ->
  %LastMessageId = cmem:getClientNNr(CMEM,ClientPID),
  ClientPID ! {nid, INNR},
  ok.


%sendMessages(ToClient, CMEM)

%% Definition: In dieser Methode muss der Server zuerst die korrekte Nachrichtennummer aus dem CMEM holen.
%% Er ruft die Methode „getClientNNr(CMEM, ClientID)“ aus dem Modul „cmem.erl“ auf um diese zu bekommen.
%% Die Parameter „ToClient“ und „ClientID“ sind identisch.
%% Im Anschluss sendet der Server eine Nachricht an den HBQ-Prozess: {self(), {request,deliverMSG,NNr,ToClient}} Hiermit wird der HBQ-Prozess aufgefordert Nachrichten an den Client („ToClient“) zu senden.
%% Der Server wartet auf die Antwort der HBQ: {reply, SendNNr}.
%% Sobald er diese SendNNr hat, muss der Server ebenfalls den CMEM-Eintrag des jeweiligen Clients anpassen und ruft die Methode „updateClient(CMEM, ClientID, NNR, Datei)“ aus dem Modul „cmem.erl“ auf.


% pre: korrekt übergebene Client PID
% post: die Nachrichten wurden erfolgreich aus der DLQ geholt und an Client gesendet
% return: Atom ok wird zurückgegeben

% Wir haben es Erweitert, es wird die veränderte CMEM wiedergegeben
% außerdem kommen für den Prozess nötige Argumente hinzu

sendMessages(ToClient, CMEM, HBQname, HBQnode) ->
  NNr = cmem:getClientNNr(CMEM, ToClient),
  {HBQname, HBQnode} ! {self(), {request, deliverMSG, NNr, ToClient}},
  receive
    {reply, SendNNr} ->
      cmem:updateClient(CMEM, ToClient, SendNNr, ?SERVER_LOGGING_FILE)
  after ?MAXIMAL_RESPONSE_TIME_BEFORE_ERROR ->
    werkzeug:logging(?SERVER_LOGGING_FILE, "sendMessages Failed")
  end.


%dropmessage(HBQname, HBQnode)

%% Definition: Der Server sendet folgende Nachricht an den HBQ-Prozess: {self(), {request,pushHBQ, [NNr,Msg,TSclientout]}}.
%% Somit wird der HBQ-Prozess aufgefordert eine Nachricht zu sichern.
%% Der Server wartet auf Antwort der HBQ und erwartet folgende Nachricht: {reply, ok}.

%pre: HBQ-Prozess vorhanden und mit übergebenen Parametern ansprechbar
%post: erreichte Nachricht vom Client wurde erfolgreich in die HBQ eingetragen return: Atom ok wird zurückgegeben

dropmessage(HBQname, HBQnode, [NNr, Msg, Tsclientout]) ->
  {HBQname, HBQnode} ! {self(), {request, pushHBQ, [NNr, Msg, Tsclientout]}},
  receive
    {reply, ok} -> ok
  after ?MAXIMAL_RESPONSE_TIME_BEFORE_ERROR ->
    werkzeug:logging(?SERVER_LOGGING_FILE, "dropmessage for msg:" ++ Msg ++ " Failed")
  end.








