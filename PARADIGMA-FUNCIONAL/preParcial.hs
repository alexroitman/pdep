
import Text.Show.Functions
type Concepto=(String,Int)
data Ayudante=UnAyudante{nombre::String,conceptos::[Concepto]} deriving Show
guille=UnAyudante "Guille" [("orden superior",6),("expresiones lambda",7),("fold",8)]
elChacal=UnAyudante "El Chacal" [("aplicacion parcial",9),("fold",6),("sinonimos de tipo",7)]
vicky=UnAyudante "Vicky" [("clases de tipo",5),("aplicacion parcial",8),("tuplas",9),("orden superior",8)]
ayudantes=[guille,elChacal,vicky]
cuantosTienen nivel ayudantes = (length.filter (tieneNivel nivel)) ayudantes

tieneNivel niv ayudante = any ((==niv).snd) (conceptos ayudante)
niveles ayudante= map snd (conceptos ayudante)
fueAprendiendo = (estaOrdenado.niveles) 

estaOrdenado [] = True
estaOrdenado [x] = True
estaOrdenado (x:y:xs) = x <= y && estaOrdenado (y:xs) 

gise ayudante=((div.sum.niveles) ayudante) (length (niveles ayudante))
marche ayudante |sabe ayudante "orden superior"=9 
				|otherwise=5
sabe ayudante concepto=elem concepto (map fst (conceptos ayudante))
hernan esBuenDia ayudante |esBuenDia=((+2).length.niveles) ayudante
						 |otherwise= (length.niveles) ayudante

promedioPuntajes jurado ayudante=div (sumaPuntajes jurado ayudante) (length jurado)

sumaPuntajes jurado ayudante=(sum.(map (aplicar ayudante))) jurado
aplicar ayudante jurado=jurado ayudante

esBuenAyudante jurado ayudante= all (>=7) (map (aplicar ayudante )jurado)


maximoAyudanteSegun criterio ayudantes=foldr1 (elMayorEntre criterio) ayudantes
elMayorEntre criterio ayudante1 ayudante2|criterio ayudante1>criterio ayudante2=ayudante1
										 |otherwise=ayudante2
cantTemas ayudante=(length.filter (>7).niveles) ayudante

juezDeterminado ayudante juez=juez ayudante

nota tema ayudante|sabe ayudante tema=(snd.head.filter ((==tema).fst)) (conceptos ayudante)
				  |otherwise=0
