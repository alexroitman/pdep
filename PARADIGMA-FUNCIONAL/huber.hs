import Text.Show.Functions
instance Eq Chofer where
	(==) unChofer otroChofer = (length.viajes) unChofer == (length.viajes) otroChofer
instance Ord Chofer where
	(<=) unChofer otroChofer = (length.viajes) unChofer <= (length.viajes) otroChofer

type Cond=(Viaje->Bool)
data Chofer=UnChofer{
			nombre::String,
			km::Int,
			viajes::[Viaje],
			condicion::Cond
		}deriving (Show)
data Viaje=UnViaje{
	fecha::(Int,Int,Int),
	costo::Int,
	cliente::Cliente
}deriving Show
data Cliente=UnCliente{
	nombreCliente::String,
	direccion::String
}deriving Show
condicionMasDoscientos =((>200).costo)
condicionN n =((>=n).length.nombreCliente.cliente) 
condicionZona zona =((/=zona).direccion.cliente) 
condicionCualquiera viaje=True

lucas=UnCliente "Lucas" "Victoria"
daniel=UnChofer "daniel" 23500 [viajeLucas] (condicionZona "olivos")


nitoInfy=UnChofer "nito Infy" 70000 (repeat (UnViaje (11,3,2017) 50 lucas)) (condicionN 3)


viajeLucas=UnViaje (20,4,2017) 150 lucas
alejandra=UnChofer "alejandra" 180000 [] condicionCualquiera

puedeTomar viaje chofer=(condicion chofer) viaje

liquidacion chofer=sum (map costo (viajes chofer))

aptosParaRealizar viaje choferes=filter (puedeTomar viaje) choferes

seleccionarChofer viaje choferes= (tieneMenosViajes.aptosParaRealizar viaje) choferes 
realizarViaje viaje choferes= (seleccionarChofer viaje choferes) {viajes=viajes (seleccionarChofer viaje choferes)++[viaje]}

tieneMenosViajes (x:[])=x
tieneMenosViajes (x:xs)=min x (tieneMenosViajes xs)   


repetirViaje viaje = viaje : repetirViaje viaje