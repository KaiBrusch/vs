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
-define(CLIENT_LOGGING_FILE, fun() -> werkzeug:message_to_string(erlang:date()) ++ "-ClientLog.txt" end).
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



fireAction({?REDAKTEUR_ATOM, Servername, Servernode}, Interval, Flag) ->
  sendMSG(Servername, Servernode, 0, Interval, Flag);
fireAction({?LESER_ATOM, Servername, Servernode}, Interval, Flag) ->
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

  REGISTER_RESULT = erlang:register(ClientName, self()),
  erlang:display(REGISTER_RESULT),
  erlang:display(Servernode),
  erlang:display(net_adm:ping(Servernode)),

  %% Anders als in dem Entwurf wird die transmittedMsg mit 1 initialisiert um die Verarbeitung in der Loop zu vereinfachen


  loop(Lifetime, Servername, Servernode, Sendinterval, erlang:now(), 1, ?REDAKTEUR_ATOM, false).



loop(Lifetime, Servername, Servernode, Sendinterval, StartTime, TransmittedNumber, Role, INNRflag) ->

  case not is_time_over(StartTime, Lifetime) of
    true ->
      if TransmittedNumber rem 5 == 0 ->
        NewRole = switchRoles(Role),
        NewInterval = changeSendInterval(Sendinterval),
        loop(Lifetime, Servername, Servernode, NewInterval, StartTime, 1, NewRole, false);
        true ->
          ActionReturn = fireAction({Role, Servername, Servernode}, Sendinterval, INNRflag),
          case erlang:is_tuple(ActionReturn) of
            true ->
              loop(Lifetime, Servername, Servernode, Sendinterval, StartTime, TransmittedNumber + 1, Role, INNRflag);
            false ->
              loop(Lifetime, Servername, Servernode, Sendinterval, StartTime, 1, switchRoles(Role), false)
          end
      end;
    false ->
      werkzeug:logging(?CLIENT_LOGGING_FILE, "ClientID-X Lifetime is over - terminating at" ++ werkzeug:to_String(now())),
      erlang:exit("Lifetime is over")
  end.





changeSendInterval(Sendinterval) ->
  Probability = random:uniform(),
  HalfInterval = Sendinterval / 2,
  if Probability > 0.5 ->
    changeSendInterval(Sendinterval + HalfInterval) + 2;
    true ->
      changeSendInterval(Sendinterval - HalfInterval) + 2
  end.



logIncomeMsg([NNr, Msg, TSclientout, TShbqin, TSdlqin, TSdlqout], TimeStampClIn) ->
  %etwa 6te_Nachricht. C Out: 11.11 21:12:58,720|(6); HBQ In: 11.11 21:12:58,720| DLQ In:11.11 21:13:01,880|.*******; C In: 11.11 21:13:07,190|
  NewMessage = NNr ++
    "te_Nachricht. C Out:" ++
    werkzeug:now2UTC(TSclientout) ++
    "| ; HBQ In:" ++
    werkzeug:now2UTC(TShbqin) ++
    "| ; DLQ In:" ++
    werkzeug:now2UTC(TSdlqin) ++
    "| ; DLQ Out:" ++
    werkzeug:now2UTC(TSdlqout) ++
    "| ; C In:" ++
    werkzeug:now2UTC(TimeStampClIn) ++
    "| Nachricht:" ++
    werkzeug:to_String(Msg),
  werkzeug:logging(?CLIENT_LOGGING_FILE, NewMessage).



getMSG(Servername, Servernode) ->
  {Servername, Servernode} ! {self(), getmessages},
  receive
    {reply, [NNr, Msg, TSclientout, TShbqin, TSdlqin, TSdlqout], true} ->
      logIncomeMsg([NNr, Msg, TSclientout, TShbqin, TSdlqin, TSdlqout], erlang:now()),
      getMSG(Servername, Servernode);
    {reply, [NNr, Msg, TSclientout, TShbqin, TSdlqin, TSdlqout], false} ->
      logIncomeMsg([NNr, Msg, TSclientout, TShbqin, TSdlqin, TSdlqout], erlang:now()),
      ok
  after ?MAXIMAL_RESPONSE_TIME_BEFORE_ERROR ->
    werkzeug:logging(?CLIENT_LOGGING_FILE, "Leser did not response" ++ werkzeug:to_String(now()))
  end.


askForMSGID(Servername, Servernode) ->
  {Servername, Servernode} ! {self(), getmsgid},
  receive
    {nid, Number} -> Number
  after ?MAXIMAL_RESPONSE_TIME_BEFORE_ERROR ->
    werkzeug:logging(?CLIENT_LOGGING_FILE, "getMSG did not received response frome Server at" ++ werkzeug:to_String(now()))
  end.



sendMSG(Servername, Servernode, TimeLastSending, Interval, INNRflag) ->
  case is_time_over(TimeLastSending, Interval) of
    true ->
      Msg = "Gruppe:" ++
        werkzeug:to_String(?GRUPPE) ++
        "; | Team:" ++
        werkzeug:to_String(?TEAM) ++
        "; | Rechnername:" ++
        werkzeug:to_String(?RECHNER_NAME),
      INNr = askForMSGID(Servername, Servernode),
      timer:sleep(trunc(Interval * 1000)),
      Flag = INNRflag or is_number(INNr),
      if Flag ->
        TSClientout = erlang:now(),
        {Servername, Servernode} ! {dropmessage, [INNr, Msg, TSClientout]},
        % todo hier kommt ein tupel zurueck, HIIIIIER
        TSClientout;
        true ->
          werkzeug:logging(?CLIENT_LOGGING_FILE, "got an INNr error" ++ werkzeug:to_String(now()))
      end;
    false ->
      werkzeug:logging(?CLIENT_LOGGING_FILE, "Try sendMSG but the interval time is not over " ++ werkzeug:to_String(now()))
  end.

