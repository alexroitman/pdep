
mata(Uno,Otro) :- (odia(Uno,Otro),not(esMasRicoQue(Uno,Otro)),viveEnDreadbury(Uno)).

odia(charles,Odiado):- (viveEnDreadbury(Odiado),(not(odia(agatha,Odiado)),Odiado\=charles)).

odia(agatha,Odiado):- (viveEnDreadbury(Odiado),Odiado\=carnicero,Odiado\=agatha).

odia(carnicero,Odiado):- (odia(agatha,Odiado),Odiado\=carnicero).

esMasRicoQue(agatha,Odiado):-(not(odia(Odiado,carnicero)),viveEnDreadbury(Odiado)).

viveEnDreadbury(agatha).

viveEnDreadbury(carnicero).

viveEnDreadbury(charles).

% odia(_,milhouse).
%  false.
% odia(charles,Alguien).
%  Alguien = agatha ;
%  Alguien = carnicero 
% odia(agatha,Alguien).
%  Alguien = charles.
% odia(Odiador,Odiado).
%  Odiador = charles,
%  Odiado = agatha ;
%  Odiador = charles,
%  Odiado = carnicero ;
%  Odiador = agatha,
%  Odiado = charles ;
%  Odiador = carnicero,
%  Odiado = charles.
% odia(carnicero,_).
%  true.