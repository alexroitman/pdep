-- declaracion de data--
data Anillo=UnAnillo{peso::Int,frase::String} deriving Show
data Hobbit= UnHobbit {nombre::String,
		estatura::Int,
		salud:: Int,
		fuerza::Int,
		esComarca::Bool,
		anillo::Anillo}deriving Show
--inicializacion de hobbits y anillos--
froddo= UnHobbit{nombre="froddo", estatura=106, salud=10,fuerza=50, esComarca=True,anillo=anilloUnico}
zen= UnHobbit{nombre="zen", estatura=99, salud=25,fuerza=46, esComarca=False,anillo=anilloSuper}
anilloUnico=UnAnillo{peso=12,frase="Un Anillo para gobernarlos a todos. Un Anillo para encontrarlos, un Anillo para atraerlos a todos y atarlos en las tinieblas."}
anilloSuper=UnAnillo{peso=18,frase="Un Anillo para ganar todas las batallas"}
anilloLeal=UnAnillo{peso=30,frase="Este anillo nunca te defraudara"}
--calculo el poder del anillo
poderAnillo anillo= (*(peso anillo)) (length(frase anillo))  --(peso anillo)*length(frase anillo)

--calculo la resistencia
preresistencia hobbit |(esComarca hobbit)==True =(estatura hobbit*salud hobbit)+(fuerza hobbit)- (poderAnillo (anillo hobbit))
		      |(esComarca hobbit)==False =(salud hobbit*fuerza hobbit)- (poderAnillo (anillo hobbit))

resistencia hobbit | (preresistencia hobbit <0) =0
		   | (head.nombre) hobbit=='f'=(+10) (preresistencia hobbit) 
		   | otherwise=preresistencia hobbit
--cambiaranillo de hobbit
cambiarAnillo hobbit anillo= hobbit{anillo=anillo}
--comidas
desayuno hobbit= hobbit{salud= salud hobbit+5,nombre="Errp"++ nombre hobbit}
segundoDesayuno n hobbit=hobbit{fuerza= fuerza hobbit+4*n}
merienda hobbit=(segundoDesayuno 2.desayuno) hobbit
--resistencia despues de merienda
mayorResistenciaPostMerienda hobbit1 hobbit2 |(resistencia.merienda) hobbit1>(resistencia.merienda) hobbit2=nombre hobbit1
					     |(resistencia.merienda) hobbit1<(resistencia.merienda) hobbit2=nombre hobbit2