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
% post: Server ist gestartet und im lokalen Namensdienst von Erlang registriert return: server started als Atom sonst eine sinnvolle Error-Meldung

start() ->

  % lade die Parameter aus der Condig Datei
  config = readConfig(),


  HBQNode = HBQ ! {self(), {request, initHBQ},

  % registriere den Prozess
  erlang:register(config.Servername,self())

  loop(Latency, Clientlifetime, Servername, HBQname, HBQnode, DLQlimit)
.


%readConfig()

%% Definition: Vor dem Start des Server-Prozesses muss die Konfigurationsdatei (siehe Vorlage) des Servers ausgelesen werden.
%% Das Modul „werkzeug.erl“ vom Professor wird hierfür verwendet. pre: Die Datei „server.cfg“ ist vorhanden

% post: Die Datei wurde erfolgreich ausgelesen und die erforderlichen Werte zurückgeliefert
% return: {Latency, Clientlifetime, Servername, HBQname, HBQnode, DLQlimit}

readConfig() ->

  {ok, ConfigListe}       = file:consult("server.cfg"),
  {ok, Latency}           = get_config_value(lifetime, ConfigListe),
  {ok, Clientlifetime}    = get_config_value(clientlifetime, ConfigListe),
  {ok, Servername}        = get_config_value(servername, ConfigListe),
  {ok, DLQlimit}          = get_config_value(dlqlimit, ConfigListe),


  {Latency, Clientlifetime, Servername, HBQname, HBQnode, DLQlimit}.


%loop(Latency, Clientlifetime, Servername, HBQname, HBQnode, DLQlimit)

%% Definition: Zu Beginn dieser Methode prüft ruft der Server die Methode „delExpiredCl(CMEM, Clientlifetime)“ aus dem Modul „cmem.erl“ auf, um zu prüfen, ob Clients gelöscht werden können.
%% In dieser Hauptschleife wartet der Server auf Anfragen seitens der Clients. Ebenso müssen Nachrichten seitens des HBQ-Prozesses abgearbeitet werden. Erhält der Server die Nachricht eines Clients mit folgendem Format: {ClientPID, getmessages}, wird die Methode „sendMessages(ClientPID)“ aufgerufen.
%% Wird die Nachricht {dropmessage,[INNr, Msg, TSclientout]} von einem Client empfangen, ruft der Server die Methode „dropmessage(HBQname, HBQnode)“ auf.
%% Eine weitere Nachricht die seitens eines Clients beim Server eintreffen kann ist folgende: {ClientPID, getmsgid}. Es muss nun die Methode sendMSGID(ClientPID) aufgerufen werden.
%% Wird der Server terminiert, da sich nach einer gewissen Zeit kein Client mehr bei ihm gemeldet hat, muss auch der HBQ-Prozess terminiert werden. Dies erfolgt mit der Nachricht: {self(), {request,dellHBQ}} an den HBQ-Prozess. Der Server wartet auf folgende Antwort: {reply, ok}.

%pre: die nötigen Übergabeparameter sind korrekt eingelesen
%post: der Prozess wurde erfolgreich, nach angegebenen Kriterien, terminiert
%return: server terminated als Atom


loop(Latency, Clientlifetime, Servername, HBQname, HBQnode, DLQlimit) ->



  receive

    {dropmessage,[INNr, Msg, TSclientout]} -> dropmessage(HQname, HBQnode)
    ;

    {ClientPID, getmessages} -> sendMessages(ClientPID)
    ;

    {ClientPID, getmsgid} -> sendMSGID(ClientPID)

  after




  end.



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

sendMessages(ToClient, CMEM) -> ok.


%dropmessage(HBQname, HBQnode)

%% Definition: Der Server sendet folgende Nachricht an den HBQ-Prozess: {self(), {request,pushHBQ, [NNr,Msg,TSclientout]}}.
%% Somit wird der HBQ-Prozess aufgefordert eine Nachricht zu sichern.
%% Der Server wartet auf Antwort der HBQ und erwartet folgende Nachricht: {reply, ok}.

%pre: HBQ-Prozess vorhanden und mit übergebenen Parametern ansprechbar
%post: erreichte Nachricht vom Client wurde erfolgreich in die HBQ eingetragen return: Atom ok wird zurückgegeben

dropmessage(HBQname, HBQnode) -> ok.



% sendMSGID(ClientPID, CMEM)

%% Definition: Der Server muss nun im Modul „cmem.erl“ die Methode „getClientNNr(CMEM,ClientID)“ aufrufen,
%% damit dieser die aktuelle Nachrichtennummer erhalten kann.
%% Der Server schickt dann die Nachricht {nid, Number} an die angegeben ClientPID.
%% Number ist die empfangene Nachrichtennummer.

%pre: korrekt übergebene Client PID
%post: erfolgreiche Ermittlung einer neuen MSGID
%return: Atom ok wird zurückgegeben

sendMSGID(ClientPID, CMEM) -> ok.

%% API
-export([]).
