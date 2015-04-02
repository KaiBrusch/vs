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
-export([initCMEM/2, getClientNNr/2, updateClient/4, delExpiredCl/2]).

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
  CMEM.


% getClientNNr(CMEM, ClientID)

%% Definition: Liefert dem Server die nächste Nachrichtennummer die an die ClientID geschickt werden soll.

% pre: keine
% post: nicht veränderte CMEM, da nur lesend
% return: ClientID als Integer-Wert, wenn nicht vorhanden wird 1 zurückgegeben

getClientNNr(CMEM, ClientID) ->
  1.

% delExpiredCl(CMEM, Clientlifetime)

%%Definition: In dieser Methode werden die Clients gelöscht, welche die Clientlifetime überschritten haben.

%pre: keine
%post: veränderte CMEM
%return: Das Atom ok als Rückgabewert

delExpiredCl(CMEM, Clientlifetime) ->
  ok.




