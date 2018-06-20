--datos del bingo
duenioDelBingo = "Perez"
ultimaBolilla = 17
--datos del jugador
lista = snd
nombre=fst
jugador1  = ("Juan Perez", [5, 23, 35, 17, 56, 80])
--quien gana?
ganador jugador = seis jugador&& contieneBolilla jugador&& esPariente jugador&&minimoInicio jugador&&maximoFinal jugador
seis jugador= length (lista jugador) ==6 
minimoInicio jugador=minimum (lista jugador)== head (lista jugador) 
maximoFinal jugador=  maximum(lista jugador)== last (lista jugador)
esPariente jugador= drop(length (nombre(jugador))- length duenioDelBingo) (nombre jugador)==duenioDelBingo
contieneBolilla jugador  = elem ultimaBolilla (lista jugador)