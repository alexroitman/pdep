
import Text.Show.Functions
import Data.List

--Data
data Micro = UnMicro {
   nombreMicro :: String,
   memoria:: [Int],  --Siendo la orimera parte de la tupla la posicion de memoria, y la segunda el dato en ella.
   acumuladorA:: Int,
   acumuladorB:: Int,
   programCounter:: Integer,
   mensajeError:: String,
   programa :: [(Micro -> Micro)]
} deriving Show

--Procesadores

at8086 :: Micro
at8086 = UnMicro {nombreMicro = "XM 8086" , memoria = [1..20]++replicate 1004 0 , acumuladorA = 0, acumuladorB = 0, programCounter = 0, mensajeError = "", programa = [nop,nop,nop]}


xt8088 :: Micro
xt8088 = UnMicro {nombreMicro = "XM 8088" , memoria = (replicate 1024 0 ) , acumuladorA = 0, acumuladorB = 0, programCounter = 0, mensajeError = "", programa= [] }


fp20 :: Micro
fp20 = UnMicro {nombreMicro = "fp20" , memoria =  replicate 1024 0 , acumuladorA = 7, acumuladorB = 24, programCounter = 0, mensajeError = "", programa= []}

i9 :: Micro
i9 = UnMicro {nombreMicro = "i9" , memoria =  [0,0..] , acumuladorA = 0, acumuladorB = 0, programCounter = 0, mensajeError = "", programa= []}

--Funciones

nop :: Micro -> Micro
nop micro = micro { programCounter = programCounter micro +1}


-- Punto 3.2
-- Se escribira en consola el comando (nop.nop.nop) Micro , utilizando composición de funciones. En este caso el comando será (nop.nop.nop) xt8088 .

--Funciones


lodv:: Int  -> Micro -> Micro
lodv val micro = micro { acumuladorA =  val , programCounter = programCounter micro +1}

swap:: Micro -> Micro
swap micro = micro { acumuladorA= acumuladorB micro , acumuladorB= acumuladorA micro, programCounter = programCounter micro +1}

add:: Micro -> Micro
add micro = micro { acumuladorA = acumuladorA micro  + acumuladorB micro, acumuladorB =0 ,programCounter = programCounter micro +1}

--Punto 3.3
-- Se escribira en consola la el comando (add.lodv 22.swap.lodv 10) micro, en este caso (add.lodv 22.swap.lodv 10) 
suma10y22 micro=(add.lodv 22.swap.lodv 10) micro
--Funciones

divide:: Micro -> Micro
divide micro | acumuladorB micro == 0 = micro {mensajeError = "DIVISION BY ZERO",programCounter = programCounter micro +1 }
            | otherwise = micro { acumuladorA = div (acumuladorA micro) ( acumuladorB micro), acumuladorB=0,  programCounter = programCounter micro +1 }

str::   Int -> Int -> Micro -> Micro
str addr val micro = micro { memoria=take (addr -1) (memoria micro) ++ [val] ++ drop addr (memoria micro), programCounter=programCounter micro +1}

lod::  Int -> Micro -> Micro
lod addr micro =micro {acumuladorA = (memoria micro)!!(addr-1), programCounter = programCounter micro +1}



--Parte 2

cargarPrograma::[(Micro->Micro)]->Micro->Micro
cargarPrograma unPrograma micro=micro{programa=unPrograma}

ejecutarPrograma :: Micro -> Micro
ejecutarPrograma  micro =foldl ejecutarIns micro (programa micro)

ejecutarIns :: Micro->(Micro->Micro)->Micro
ejecutarIns micro ins | mensajeError micro==""=ins micro
					  |	otherwise=micro

ifnz:: [(Micro -> Micro)] -> Micro -> Micro
ifnz programaN micro | (acumuladorA micro == 0) = micro
                     | otherwise = ejecutarPrograma (cargarPrograma programaN micro)

depurar:: [(Micro->Micro)]->Micro->[(Micro->Micro)]
depurar instrucciones micro=filter (esNecesaria micro) instrucciones
esNecesaria micro instruccion=not(((acumuladorA (instruccion micro))==0 && acumuladorB ((instruccion micro)) ==0) && (all (==0) (memoria(instruccion micro))))
					
estaOrdenado:: Micro-> Bool
estaOrdenado micro=  isOrdered (filter (/=0) (memoria micro))

isOrdered:: [Int]-> Bool
isOrdered [] = True
isOrdered (x:[]) = True
isOrdered (x:xs) |x <= (head xs)=isOrdered xs
                 |otherwise = False
{-
PUNTO 6
Al querer listar el micro con la operacion de sumar 10 y 22 comienza a mostrar el micro, pero nunca termina de mostrar su memoria
Lo mismo pasa cuando intenamos preguntar si esta ordenada, nunca lo podremos saber ya que tiene infinitas posiciones y siempre estara 
preguntando por el siguiente a ver si esta ordenado

-}