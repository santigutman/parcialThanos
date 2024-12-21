module Library where
import PdePreludat

data Guante = UnGuante{
  material :: String,
  gemas :: [Gema]
} deriving (Show, Eq)

data Personaje = UnPersonaje{
  edad :: Number,
  energia :: Number,
  habilidades :: [Habilidad],
  nombre :: String,
  planetaDondeVive :: String
} deriving (Show, Eq)

type Universo = [Personaje]

type Habilidad = String
type Gema = String

estaCompleto :: Guante -> Bool
estaCompleto = (==6) . length . gemas

materialEsUru :: Guante -> Bool
materialEsUru = (=="uru") . material

ironman :: Personaje
ironman = UnPersonaje{ nombre = "Iron Man" , edad = 40 , energia = 100 , planetaDondeVive = "Tierra" , habilidades = ["volar"] }

hulk :: Personaje
hulk = UnPersonaje{ nombre = "Hulk" , edad = 55 , energia = 200 , planetaDondeVive = "Tierra" , habilidades = ["Enojarse"] }

universo1 = [ironman, hulk]

chasquearGuante :: Guante -> Universo -> Universo
chasquearGuante guante universo 
    |estaCompleto guante && materialEsUru guante =  reducirMitad universo
    |otherwise = universo

reducirMitad :: Universo -> Universo
reducirMitad universo = take (length universo `div` 2) universo 

esAptoParaPendex :: Universo -> Bool
esAptoParaPendex  uni = length (filter (esMenorde 45) uni) >=1

esMenorde :: Number -> Personaje -> Bool
esMenorde edadNecesaria = (<edadNecesaria) . edad 

energiaTotal :: Universo -> Number
energiaTotal universo = foldl (+) 0 (listaEnergias universo)

listaEnergias :: Universo -> [Number]
listaEnergias universo = map energiaDeCadaUno (listaPersonajesConUnaHabilidadMinimo universo)

energiaDeCadaUno :: Personaje -> Number
energiaDeCadaUno = energia

tieneMasDeUnaHabilidad :: Personaje -> Bool
tieneMasDeUnaHabilidad = (>= 1) . length . habilidades

listaPersonajesConUnaHabilidadMinimo :: Universo -> Universo
listaPersonajesConUnaHabilidadMinimo universo = filter tieneMasDeUnaHabilidad universo

mente :: Number -> Personaje -> Personaje
mente valor personaje = personaje{ energia = energia personaje - valor }

alma :: Habilidad -> Personaje -> Personaje
alma habilidad = modificarEnergia (-10) . eliminarHabilidad habilidad 

eliminarHabilidad :: Habilidad -> Personaje -> Personaje
eliminarHabilidad habilidad personaje = personaje {habilidades = filter (/= habilidad) (habilidades personaje)}

modificarEnergia :: Number -> Personaje -> Personaje
modificarEnergia valor personaje = personaje {energia = energia personaje + valor} 

espacio :: String -> Personaje -> Personaje
espacio planeta personaje = personaje {planetaDondeVive = planeta, energia = energia personaje - 20}

poder :: Personaje -> Personaje
poder personaje
  | tiene2HabilidadesOMenos personaje = eliminarTodasLasHabilidades (modificarEnergia (-10) personaje)
  | otherwise = modificarEnergia (-10) personaje

eliminarTodasLasHabilidades :: Personaje -> Personaje
eliminarTodasLasHabilidades personaje = personaje {habilidades = []}

tiene2HabilidadesOMenos :: Personaje -> Bool
tiene2HabilidadesOMenos = (<= 2) . length . habilidades