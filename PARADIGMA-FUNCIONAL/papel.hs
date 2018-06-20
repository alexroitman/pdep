{-import Text.Show.Functions
data Ladron=UnLadron{
	nombre::String,
	habilidades::[String],
	armas::[(Rehen->Rehen)]
} deriving Show
data Rehen=UnRehen{
	nombreRehen::String,
	complot::Int,
	miedo::Int,
	plan::(Ladron->Ladron)
}deriving Show
tokio= UnLadron "tokio" ["trabajo psicologico","entrar en moto"] [pistola 9,pistola 9,ametralladoraTokio 30]
profesor= UnLadron "profesor" ["disfrazarse de linyera","disfrazarse de payaso","estar siempre un paso adlante"] []

pablo=UnRehen "pablo" 40 30 esconderse
arturito= UnRehen "arturito" 70 50 (atacar pablo.esconderse)

pistola calibre rehen=rehen{complot=(complot rehen)-5*(calibre),miedo=miedo rehen+3*(length (nombreRehen rehen))} 
ametralladora balas rehen =rehen{complot=div (complot rehen) 2}

esInteligente ladron=((>=2).length.habilidades) ladron 

esconderse ladron=ladron{armas=drop (div ((length.habilidades) ladron) 3) (armas ladron)}
atacar::Rehen->Ladron->Ladron
atacar compa ladron=ladron{armas= drop (div ((length.nombreRehen) compa) 10) (armas ladron)}

funcion cond num lista str=(>str).sum.map (length.num).filter (lista cond)

conseguirArma arma ladron=ladron{armas=(armas ladron)++[arma]}

--intimidar "Disparos" ladron rehen=maximum (map (accion rehen) (armas ladron))

intimidar "Hacerse el malo" rio rehen=rehen{complot=(complot rehen)+20}
intimidar "Hacerse el malo" berlin rehen =rehen{miedo=(miedo rehen)+sumarHabilidades berlin}
intimidar "Hacerse el malo " ladron rehen =rehen{miedo=(miedo rehen)+10}
--sumarHabilidades (x:[])=x
sumarHabilidades ladron=sum (map (length) (habilidades ladron))-}
import Text.Show.Functions

data Ladron = UnLadron {
	nombreL::String,
	habilidades::[String],
	armas:: [Arma]
} deriving Show

type Arma = (Rehen -> Rehen)

data Rehen = UnRehen {
	nombreR::String,
	nivelComplot::Int,
	nivelMiedo::Int,
	plan:: (Ladron -> Ladron)
} deriving Show

pistola::Int -> Rehen -> Rehen
pistola calibre rehen = rehen {nivelComplot = (nivelComplot rehen) - (5 * calibre) , nivelMiedo = (nivelMiedo rehen) + (3 * ((length.nombreR) rehen))}

ametralladora::Int -> Rehen -> Rehen
ametralladora balas rehen = rehen { nivelComplot = div (nivelComplot rehen) 2 , nivelMiedo = (nivelMiedo rehen) + (3 * ((length.nombreR) rehen))}

--disparos (UnLadron _ _ armas) rehen

hacerseElMalo (UnLadron "Berlin" habilidades _) rehen = rehen {nivelMiedo = (nivelMiedo rehen) + sum (map (length) habilidades)}

hacerseElMalo (UnLadron "Rio" habilidades _) rehen = rehen {nivelComplot = (nivelComplot rehen) + 20}

hacerseElMalo (UnLadron _ habilidades _) rehen = rehen {nivelComplot = (nivelComplot rehen) + 10}


tieneLosHuevos (UnRehen _ complot miedo _) = complot > miedo

atacarLadron:: Rehen ->Ladron -> Ladron
atacarLadron compañero ladron = ladron {armas = drop (div ((length.nombreR) compañero) 10) (armas ladron)}

esconderse:: Ladron -> Ladron
esconderse ladron = ladron {armas = drop (div ((length.habilidades) ladron) 1) (armas ladron)}



tokio::Ladron
tokio = UnLadron "Tokio" ["trabajo psicologico", "entrar en moto"] [(pistola 9),(pistola 9),(ametralladora 30)]

profesor::Ladron
profesor = UnLadron "Profe" ["disfrazarse de linyera", "disfrazarse de payaso","estar siempre un paso adelante"] []

pablo::Rehen
pablo = UnRehen "Pablo" 40 30 esconderse

arturito::Rehen
arturito = UnRehen "Artur" 70 50 ((atacarLadron pablo).esconderse)

esInteligente::Ladron->Bool
esInteligente = ((>2).length.habilidades)

conseguirArma::Arma -> Ladron -> Ladron
conseguirArma arma ladron = ladron {armas = (armas ladron) ++ [arma]} 

intimidar :: Ladron -> Rehen -> Rehen
intimidar = hacerseElMalo

calmarLasAguas :: [Rehen] -> [Rehen]
calmarLasAguas = filter ((>60).nivelComplot)

empiezaCon frase palabra = frase == take (length frase) palabra

puedeEscapar :: Ladron -> Bool
puedeEscapar ladron = any (empiezaCon "disfrazarse de") (habilidades ladron)

complotPromedio rehenes = div ((sum.(map nivelComplot)) rehenes) (length rehenes)

miedoPromedio rehenes = div ((sum.(map nivelMiedo)) rehenes) (length rehenes)

cantArmas = (sum.(map (length.armas)))

pintaMal :: [Rehen] -> [Ladron] -> Bool
pintaMal rehenes ladrones = complotPromedio rehenes > (miedoPromedio rehenes) * cantArmas ladrones

modComplot valor rehen = rehen { nivelComplot = (nivelComplot rehen) + valor}

aptosParaEfectuarPlan = filter (tieneLosHuevos)

efectuarPlan ladron plan = plan ladron

seRebelan :: [Rehen] -> Ladron -> Ladron
seRebelan rehenes ladron = foldl efectuarPlan ladron (map plan (aptosParaEfectuarPlan (map (modComplot (-10)) rehenes)))

armarLadrones = map (conseguirArma (ametralladora 45))

rebelionTotal = map (seRebelan todosLosRehenes) where todosLosRehenes = [pablo,arturito]

planValencia :: [Ladron] -> Int
planValencia ladrones = 1000000 * cantArmas ((rebelionTotal.armarLadrones) ladrones)

--Punto 10: No ya que no se podrá efectuar la funcion length en la lista infinita de armas 

--Punto 11: Si ya que la funcion no evalua las habilidades y gracias al Lazy evaluation se podrá ejecutar sin problemas

--funcion :: Ord b => a -> (a -> a) -> (a -> Bool) -> b -> [a] -> Bool
funcion cond num lista str = (> str) . sum . map (length . num) . filter (lista cond)	 