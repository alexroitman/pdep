import Text.Show.Functions
type Ataque=(Personaje->Personaje)
data Personaje=UnPersonaje{
	nombre::String,
	ataqueFavorito::Ataque,
	elementos::[String],
	energia::Int
} deriving Show

hulk=UnPersonaje "hulk" superFuerza ["pantalones"] 90
thor=UnPersonaje "thor" (relampago 50) ["mjolnir"] 100
viuda=UnPersonaje "viuda negra" artesMarciales [] 90
capitan=UnPersonaje "capitan america" arrojarEscudo ["escudo"] 80
halcon=UnPersonaje "ojo de halcon" arqueria ["arco","flechas"] 70
vision=UnPersonaje "vision" (proyectarRayos 5) ["gema del infinito"] 100
ironMan=UnPersonaje "iron man" (ironia(relampago(-50))) ["armadura","jarvis","plata"] 60
ultron=UnPersonaje "robot ultron" corromperTecnologia [] 100

esRobot personaje= (head.words.nombre) personaje=="robot"
tiene personaje elemento= elem elemento (elementos personaje) 
potencia personaje= length (elementos personaje) * (energia personaje)




identidad personaje=personaje
superFuerza:: Personaje->Personaje
superFuerza personaje=personaje{energia=0}
relampago:: Int->Personaje->Personaje
relampago x personaje= personaje {energia=(energia personaje)-x}
artesMarciales:: Personaje->Personaje
artesMarciales personaje=personaje{ataqueFavorito=identidad}
arrojarEscudo:: Personaje->Personaje 
arrojarEscudo personaje=personaje{elementos=[]}
arqueria::Personaje ->Personaje
arqueria personaje| not (elem "escudo" (elementos personaje))=superFuerza personaje
				  |otherwise=personaje
proyectarRayos::Int->Personaje->Personaje
proyectarRayos x personaje= (relampago (2*x) personaje)
ironia::Ataque->Personaje->Personaje
ironia ataque personaje=ataque personaje
corromperTecnologia personaje=identidad personaje

enfrentamiento personaje1 personaje2=ataquefavorito personaje1 personaje2