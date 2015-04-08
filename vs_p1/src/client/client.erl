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
-export([start/0]).


-define(GRUPPE, '3').
-define(TEAM, '06').
-define(MAXIMAL_RESPONSE_TIME_BEFORE_ERROR, 5000).
-define(CLIENT_LOGGING_FILE, "ClientLog.txt").
-define(REDAKTEUR_ATOM, redakteur).
-define(LESER_ATOM, leser).
-define(RECHNER_NAME, 'rechner@123').





timestamp_to_millis({MegaSecs, Secs, MicroSecs}) ->
  (MegaSecs * 1000000 + Secs) * 1000 + round(MicroSecs / 1000).


is_time_over(0, _) ->
  true;
is_time_over(Start, Lifetime) ->
  (timestamp_to_millis(erlang:now()) - timestamp_to_millis(Start)) >= Lifetime * 1000.



switchRoles(?REDAKTEUR_ATOM) ->
  ?LESER_ATOM;
switchRoles(?LESER_ATOM) ->
  ?REDAKTEUR_ATOM.



fireAction({redakteur, Servername, Servernode}, Interval, Flag) ->
  sendMSG(Servername, Servernode, 0, Interval, Flag);
fireAction({leser, Servername, Servernode}, _, _) ->
  getMSG(Servername, Servernode).


%start()

%% Definition: Startet einen neuen Client-Prozess der Nachrichten an den (laufenden) Server senden kann und Nachrichten abrufen kann.

%pre: keine
%post: Ein neuer Prozess wurde gestartet, der mit dem Server kommunizieren kann
%return: client started als Atom sonst eine sinnvolle Error-Meldung

start() ->

  % TODO: Register node for communication!
  {Clients, Lifetime, Servername, Servernode, Sendinterval} = readConfig(),
  % Range = lists:seq(1, Clients),
  %ists:foreach(fun(X) ->

  spawn(loop(Lifetime, Servername, Servernode, Sendinterval, client1))
% spawn(loop(Lifetime, Servername, Servernode, Sendinterval, client2)),
% spawn(loop(Lifetime, Servername, Servernode, Sendinterval, client3)),
% spawn(loop(Lifetime, Servername, Servernode, Sendinterval, client4)),
% spawn(loop(Lifetime, Servername, Servernode, Sendinterval, client5))
.


% readConfig()

%% Definition: Vor dem Start des Client-Prozesses muss die Konfigurationsdatei (siehe Vorlage) des Clients ausgelesen werden.
%% Das Modul „werkzeug.erl“ vom Professor wird hierfür verwendet.

% pre: Die Datei „client.cfg“ ist vorhanden%
% post: Die Datei wurde erfolgreich ausgelesen und die erforderlichen Werte zurückgeliefert
% return: {Clients, Lifetime, Servername, Servernode, Sendinterval}

readConfig() ->
  {ok, Configfile} = file:consult("../client.cfg"),

  {ok, Clients} = werkzeug:get_config_value(clients, Configfile),
  {ok, Lifetime} = werkzeug:get_config_value(lifetime, Configfile),
  {ok, Servername} = werkzeug:get_config_value(servername, Configfile),
  {ok, Servernode} = werkzeug:get_config_value(nodename, Configfile),
  {ok, Sendinterval} = werkzeug:get_config_value(sendeintervall, Configfile),

  {Clients, Lifetime, Servername, Servernode, Sendinterval}.




loop(Lifetime, Servername, Servernode, Sendinterval, ClientName) ->
  % registriere den Prozess mit dem Erlang Prozess

  erlang:register(ClientName, self()),

  werkzeug:logging(?CLIENT_LOGGING_FILE, "Der Client:" ++
    werkzeug:to_String(ClientName) ++ " wurde registriert. /n"),


  loop(Lifetime, Servername, Servernode, Sendinterval, erlang:now(), 0, ?REDAKTEUR_ATOM, false).



loop(Lifetime, Servername, Servernode, Sendinterval, StartTime, TransmittedNumber, Role, INNRflag) ->

  case not is_time_over(StartTime, Lifetime) of
    true ->
      if TransmittedNumber == 4 ->
        NewRole = switchRoles(Role),
        werkzeug:logging(?CLIENT_LOGGING_FILE,
          "Client has switched role from:" ++
            werkzeug:to_String(Role) ++
            "| To NewRole:" ++
            werkzeug:to_String(NewRole)
            ++ "\n"),
        NewInterval = changeSendInterval(Sendinterval),
        werkzeug:logging(?CLIENT_LOGGING_FILE,
          "New Sendinterval is been created From:" ++
            werkzeug:to_String(Sendinterval) ++
            "| To NewRole:" ++
            werkzeug:to_String(NewInterval)
            ++ "\n"),
        loop(Lifetime, Servername, Servernode, NewInterval, StartTime, 0, NewRole, false);
        true ->
          werkzeug:logging(?CLIENT_LOGGING_FILE,
            "Client is before fired Action for Role of:" ++
              werkzeug:to_String(Role) ++ "\n"),
          ActionReturn = fireAction({Role, Servername, Servernode}, Sendinterval, INNRflag),
          werkzeug:logging(?CLIENT_LOGGING_FILE,
            "Client is fired Action for Role of:" ++
              werkzeug:to_String(Role) ++
              "| With ActionReturn:" ++
              werkzeug:to_String(ActionReturn)
              ++ "\n"),
          case erlang:is_tuple(ActionReturn) of
            true ->
              loop(Lifetime, Servername, Servernode, Sendinterval, StartTime, TransmittedNumber + 1, Role, INNRflag);
            false ->
              loop(Lifetime, Servername, Servernode, Sendinterval, StartTime, 0, switchRoles(Role), false)
          end
      end;
    false ->
      werkzeug:logging(?CLIENT_LOGGING_FILE, "ClientID-X Lifetime is over - terminating at" ++ werkzeug:to_String(now()) ++ "\n"),
      erlang:exit("Lifetime is over")
  end.





changeSendInterval(Sendinterval) ->
  werkzeug:logging(?CLIENT_LOGGING_FILE, "Trying to change Sendinterval for:" ++ werkzeug:to_String(Sendinterval) ++ "\n"),

  Probability = random:uniform(),
  HalfInterval = Sendinterval / 2,
  if Probability > 0.5 ->
    Sendinterval + HalfInterval + 2;
    true ->
      (Sendinterval - HalfInterval) + 2
  end.



logIncomeMsg([NNr, Msg, TSclientout, TShbqin, TSdlqin, TSdlqout], TimeStampClIn) ->
  NewMessage = werkzeug:to_String(NNr) ++
    "te_Nachricht. C Out:" ++
    werkzeug:to_String(TSclientout) ++
    "| ; HBQ In:" ++
    werkzeug:to_String(TShbqin) ++
    "| ; DLQ In:" ++
    werkzeug:to_String(TSdlqin) ++
    "| ; DLQ Out:" ++
    werkzeug:to_String(TSdlqout) ++
    "| ; C In:" ++
    werkzeug:to_String(TimeStampClIn) ++
    "| Nachricht:" ++
    werkzeug:to_String(Msg) ++
    "\n",
  werkzeug:logging(?CLIENT_LOGGING_FILE, NewMessage).



getMSG(Servername, Servernode) ->
  {Servername, Servernode} ! {self(), getmessages},

  werkzeug:logging(?CLIENT_LOGGING_FILE, "Client send getmessages  \n"),
  receive
    {reply, [NNr, Msg, TSclientout, TShbqin, TSdlqin, TSdlqout], true} ->
      logIncomeMsg([NNr, Msg, TSclientout, TShbqin, TSdlqin, TSdlqout], erlang:now()),
      werkzeug:logging(?CLIENT_LOGGING_FILE, "Client get message NR:" ++
        werkzeug:to_String(NNr) ++
        "with an repeat flag: true" ++
        "\n"),
      getMSG(Servername, Servernode);
    {reply, [NNr, Msg, TSclientout, TShbqin, TSdlqin, TSdlqout], false} ->
      werkzeug:logging(?CLIENT_LOGGING_FILE, "Client get message NR:" ++
        werkzeug:to_String(NNr) ++
        "with an repeat flag: false" ++
        "\n"),
      logIncomeMsg([NNr, Msg, TSclientout, TShbqin, TSdlqin, TSdlqout], erlang:now()),
      ok
  after ?MAXIMAL_RESPONSE_TIME_BEFORE_ERROR ->
    werkzeug:logging(?CLIENT_LOGGING_FILE, "Leser did not response" ++ werkzeug:to_String(now()))
  end.


askForMSGID(Servername, Servernode) ->
  {Servername, Servernode} ! {self(), getmsgid},
  werkzeug:logging(?CLIENT_LOGGING_FILE, "Client send getmsgid  \n"),
  receive
    {nid, Number} ->
      werkzeug:logging(?CLIENT_LOGGING_FILE, "Client received in getmsgid an nid: " ++ werkzeug:to_String(Number) ++ "\n"),
      Number
  after ?MAXIMAL_RESPONSE_TIME_BEFORE_ERROR ->
    werkzeug:logging(?CLIENT_LOGGING_FILE, "getMSG did not received response frome Server at" ++ werkzeug:to_String(now()))
  end.



sendMSG(Servername, Servernode, TimeLastSending, Interval, INNRflag) ->


  Msg = "Gruppe:" ++
    werkzeug:to_String(?GRUPPE) ++
    "; | Team:" ++
    werkzeug:to_String(?TEAM) ++
    "; | Rechnername:" ++
    werkzeug:to_String(?RECHNER_NAME),
  INNr = askForMSGID(Servername, Servernode),
  timer:sleep(trunc(Interval * 1000)),
  TSClientout = erlang:now(),
  {Servername, Servernode} ! {dropmessage, [INNr, Msg, TSClientout]},
  werkzeug:logging(?CLIENT_LOGGING_FILE, "Client send dropmessage with Message: " ++ werkzeug:to_String([INNr, Msg, TSClientout]) ++ " .\n"),
  TSClientout.
