%%%-------------------------------------------------------------------
%%% @author kbrusch
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Apr 2015 10:06 AM
%%%-------------------------------------------------------------------
-module(client).
-author("kbrusch").

%% API
-export([]).


-define(GRUPPE, 3).
-define(TEAM, 06).
-define(MAXIMAL_RESPONSE_TIME_BEFORE_ERROR, 5000).
-define(CLIENT_LOGGING_FILE, fun() -> werkzeug:message_to_string(erlang:date()) ++ "-ClientLog.txt" end).




to_millis({MegaSecs,Secs,MicroSecs}) ->
  (MegaSecs*1000000 + Secs)*1000 + round(MicroSecs/1000).

is_time_over(Start,Lifetime) ->
  (to_millis(erlang:now()) - to_millis(Start))  >= Lifetime.



%start()

%% Definition: Startet einen neuen Client-Prozess der Nachrichten an den (laufenden) Server senden kann und Nachrichten abrufen kann.

%pre: keine
%post: Ein neuer Prozess wurde gestartet, der mit dem Server kommunizieren kann
%return: client started als Atom sonst eine sinnvolle Error-Meldung

start() ->

  {Clients, Lifetime, Servername, Servernode, Sendinterval} = readConfig(),
  loop(Lifetime,Servername,Servernode,Sendinterval).



% readConfig()

%% Definition: Vor dem Start des Client-Prozesses muss die Konfigurationsdatei (siehe Vorlage) des Clients ausgelesen werden.
%% Das Modul „werkzeug.erl“ vom Professor wird hierfür verwendet.

% pre: Die Datei „client.cfg“ ist vorhanden%
% post: Die Datei wurde erfolgreich ausgelesen und die erforderlichen Werte zurückgeliefert
% return: {Clients, Lifetime, Servername, Servernode, Sendinterval}

readConfig() ->
  {ok,Configfile} = file:consult("../server.cfg"),

  Clients = werkzeug:get_config_value(clients,Configfile),
  Lifetime = werkzeug:get_config_value(lifetime,Configfile),
  Servername = werkzeug:get_config_value(servername,Configfile),
  Servernode = werkzeug:get_config_value(servernode,Configfile),
  Sendinterval = werkzeug:get_config_value(sendinterval,Configfile),
  {Clients, Lifetime, Servername, Servernode, Sendinterval}.



%loop(Lifetime, Servername, Servernode, Sendinterval)

%% Definition: In der Hauptschleife werden die Nachrichten in bestimmten Zeitabständen an den Server versendet.
%% Dabei wird die Methode „sendMSG(Servername, Servernode)“ verwendet.
%% Weiterhin wird nach dem Senden von 5 Nachrichten die Methode „changeSendintervall(Sendinterval)“ aufgerufen, um das Sendeintervall neu zu berechnen.
%% Ebenfalls nach 5 Nachrichten wird die Methode „askForMSGID(Servername, Servernode)“ aufgerufen.
%% Hiermit wird der Server aufgefordert, ihm (dem Client) eine eindeutige Nachrichtennummer zu übermitteln.
%% Sobald die Rolle des Leser-Clients übernommen wird (nach 5 gesendeten Nachrichten, siehe Aufgabenstellung), wird die Methode „getMSG(Servername, Servernode)“ aufgerufen.
%% Weiterhin ist es notwendig in dieser Methode zu prüfen, wann die Lebenszeit des Clients vorüber ist.

% pre: die nötigen Übergabeparameter sind korrekt eingelesen und der Server ist noch nicht terminiert
% post: der Prozess wurde erfolgreich, nach angegebenen Kriterien, terminiert
% return: client terminated als Atom

loop(Lifetime, Servername, Servernode, Sendinterval) ->
  StartTime = erlang:now(),
  % setze Sie hier weil anders nicht möglich
  Flag = false,
  TransmittedMsg = 0,
  loop(Lifetime, Servername, Servernode, Sendinterval,StartTime,TransmittedMsg,redakteur).


loop(Lifetime,Servername, Servernode,Sendinterval,StartTime,TransmittedMsg,Role) when not is_time_over(StartTime,Lifetime) ->
  case {TransmittedMsg,Role} of
    {SomeNumber,redakteur} when SomeNumber < 5 ->
      sendMSG(Servername,Servernode);
    {SomeNumber,leser} when SomeNumber < 5 ->
      getMSG
  end,
  loop(Lifetime,Servername,Servernode,Sendinterval,StartTime,TransmittedMsg + 1);
loop(Lifetime,Servername, Servernode,Sendinterval,StartTime,TransmittedMsg) ->
  werkzeug:logging(?CLIENT_LOGGING_FILE,"ClientID-X Lifetime is over - terminating at" ++ werkzeug:to_String(now())),
  erlang:exit("Lifetime is over").




changeSendintervall(Sendinterval) -> a.


%  Definition: Dem Server wird folgende Nachricht übermittelt:
% {self(), getmessages}. Er wartet auf die Antwort des Servers (DLQ) mit
% folgendem Format: {reply,[NNr, Msg, TSclientout, TShbqin, TSdlqin, TSdlqout], Terminated}.
% Der Server wird somit aufgefordert alle Nachrichten einzeln an den Leser- Client zu übermitteln.
% Diesen Vorgang wiederholt der Client solange, bis er alle Nachrichten bekommen hat und somit auf dem neuesten Stand ist.
% Dies erkennt der Client an dem Flag Terminated(true).
% Mithilfe des Moduls „werkzeug.erl“ können die Nachrichten in der GUI (Logfile) ausgegeben werden.
% Nach jedem Erhalt einer Nachricht, fügt der Leser-Client eine eigene Nachricht,
% der jeweiligen erhaltenen Nachricht, hinzu, sowie ein selbst erstellten Timestamp (siehe Anforderung 12).
% pre: Server ist unter angegebenen Servername und Servernode weiterhin erreichbar post: Nachricht erfolgreich erhalten, damit sie geloggt werden kann
% return: Atom ok wird zurückgegeben

getMSG(Servername, Servernode) ->
  {Servername,Servernode} ! {self(),getmessages},
  receive
    {reply,[NNr, Msg, TSclientout, TShbqin, TSdlqin, TSdlqout], Terminated} ->
      ok
  after ?MAXIMAL_RESPONSE_TIME_BEFORE_ERROR ->
    werkzeug:logging(?CLIENT_LOGGING_FILE,"Leser did not response" ++ werkzeug:to_String(now()))
  end.



% todo: implement receive method as controllstructure and for logging
% todo: receive {nid, Number}

askForMSGID(Servername, Servernode) ->
  {Servername,Servernode} ! {self(),getmsgid},
  receive
    {nid,Number} -> Number
  after ?MAXIMAL_RESPONSE_TIME_BEFORE_ERROR ->
    werkzeug:logging(?CLIENT_LOGGING_FILE,"getMSG did not received response frome Server at" ++ werkzeug:to_String(now()))
  end.




%Definition: Hier wird dem Server die Nachricht {dropmessage, [INNr, Msg, TSclientout]} gesendet. Auf eine Antwort des Server wird nicht gewartet.
%pre: Server ist unter angegebenen Servername und Servernode weiterhin erreichbar
%post: Server hat erfolgreich eine Nachricht erhalten
%return: Atom ok wird zurückgegeben ! wo die receive dafür einbauen, im main loop?

sendMSG(Servername,Servernode) ->
  Msg = "clientname::" ++  "::" ++ "irgendEinText" ,
  INNr =  askForMSGID(Servername,Servernode),
  {Servername,Servernode} ! {dropmessage, [INNr, Msg, erlang:now()]}.


