module Library where
import PdePreludat

data Categoria = Menor | Adulto | Jubilado deriving (Show)

categoria :: Number -> Categoria
categoria edad
    | edad < 18 = Menor
    | edad < 65 = Adulto
    | otherwise = Jubilado

------------------------------------

--Se sabe que un club de fútbol tiene un nombre, cantidad de títulos (todos los títulos valen lo mismo tanto una final intercontinental contra el real madrid como una primera nacional) y sus jugadores. De los jugadores conocemos su nombre, su edad, su valor de mercado. Se pide modelar los equipos, los jugadores y las siguientes funciones:

-- Así se crea un data con "record syntax"
data Club = UnClub {
    nombre :: String,
    cantTitulos :: Number,
    jugadores :: [Jugador]
} deriving (Show)

data Jugador = UnJugador {
    nombreJug :: String,
    edad :: Number,
    valorMercado :: Number
} deriving (Show)

alf :: Jugador
alf = UnJugador { nombreJug="Alf", edad=39, valorMercado=(-4) }

boca :: Club
boca = UnClub { nombre="Boca", cantTitulos=74, jugadores=[alf] }

nacionalPotosi :: Club
nacionalPotosi = UnClub { nombre="Nacional Potosí", cantTitulos=60, jugadores=[] }

-- la función identidad devuelve lo que recibe
identidad :: a -> a -- Variables de tipo. Me permiten que mi función reciba más de un tipo
identidad x = x

-- comprarJugador :: asdfsadf -> Jugador -> asdfasdf

-- 1. esMasGrandeQue recibe dos clubes y dice si el primero es más grande que el segundo. La grandeza de un club se mide en base a cuantos títulos tiene.

esMasGrandeQue :: Club -> Club -> Bool
esMasGrandeQue clubGrande clubChico = cantTitulos clubGrande > cantTitulos clubChico

-- 2. salirCampeon recibe un club y le suma uno a su cantidad de títulos.

-- Como no hay efecto, necesito construir un nuevo club parecido, pero con un título más.
salirCampeon :: Club -> Club
-- salirCampeon club = UnClub { nombre=nombre club, cantTitulos=cantTitulos club + 1, jugadores=jugadores club }
salirCampeon club = club { cantTitulos = cantTitulos club + 1} -- esta línea y la de arriba son lo mismo

-- bla bla bla 

data Figura = Pelota { color :: String } | LogoUTN | MedioFigura deriving (Show)

caja :: Figura -> Figura
caja (Pelota _) = Pelota { color = "Azul" }
caja LogoUTN = Pelota { color = "Rosa" }
caja MedioFigura = Pelota { color = "Celeste" } -- Variable Anonima o Wilcard Pattern


f :: Number -> String
f 2 = "hola"
f 3 = "chau"
f _ = "puedo irme?" -- Variable anónima

legalParaTenis :: Figura -> Bool
legalParaTenis (Pelota "Amarilla") = True
legalParaTenis _ = False