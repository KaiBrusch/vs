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




%start()

%% Definition: Startet einen neuen Client-Prozess der Nachrichten an den (laufenden) Server senden kann und Nachrichten abrufen kann.

%pre: keine
%post: Ein neuer Prozess wurde gestartet, der mit dem Server kommunizieren kann
%return: client started als Atom sonst eine sinnvolle Error-Meldung

start() -> client.


% readConfig()

%% Definition: Vor dem Start des Client-Prozesses muss die Konfigurationsdatei (siehe Vorlage) des Clients ausgelesen werden.
%% Das Modul „werkzeug.erl“ vom Professor wird hierfür verwendet.

% pre: Die Datei „client.cfg“ ist vorhanden%
% post: Die Datei wurde erfolgreich ausgelesen und die erforderlichen Werte zurückgeliefert
% return: {Clients, Lifetime, Servername, Servernode, Sendinterval}

readConfig() -> {Clients, Lifetime, Servername, Servernode, Sendinterval}.



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

loop(Lifetime, Servername, Servernode, Sendinterval) -> terminated.



%% API
-export([]).
