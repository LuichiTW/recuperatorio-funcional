import Text.Show.Functions

--Punto 1
data Aventurero = UnAventurero {
    nombre :: String,
    carga :: Int,
    salud :: Int,
    tieneCoraje :: Bool,
    criterioSeleccionEncuentros :: Criterio
} deriving (Show)


type Criterio = Aventurero -> Bool

conformista :: Criterio
conformista unAventurero = True

valiente :: Criterio
valiente unAventurero = tieneCoraje unAventurero || salud unAventurero > 50

lightPacker :: Int -> Criterio
lightPacker umbral unAventurero = (<umbral).carga $ unAventurero 

--Punto 2
existeAventureroConNombreLargo :: [Aventurero] -> Bool
existeAventureroConNombreLargo unosAventureros = any (tieneMasCantidadLetras 5) unosAventureros

tieneMasCantidadLetras :: Int -> Aventurero -> Bool
tieneMasCantidadLetras cantidad unAventurero = (>cantidad).length.nombre $ unAventurero 

joaquin = UnAventurero "joaqi" 10 20 True conformista

sumarParesCarga :: [Aventurero] -> Int
sumarParesCarga unosAventureros = sum.cargasPares $ unosAventureros

cargasPares :: [Aventurero] -> [Int]
cargasPares unosAventureros = filter even.map carga $ unosAventureros

--Punto 3
type Encuentro = Personaje -> Aventurero -> Aventurero
data Personaje =
    Curandero
    | Inspirador
    | Embaucador

encuentroPersonaje :: Encuentro
encuentroPersonaje personaje unAventurero = efectoPersonaje personaje.modificarCarga (-) (1) $  unAventurero

modificarCarga :: (Int -> Int -> Int) -> Int -> Aventurero -> Aventurero
modificarCarga modificador numero unAventurero = unAventurero {carga = modificador (carga unAventurero) numero}

efectoPersonaje :: Personaje -> Aventurero -> Aventurero
efectoPersonaje Curandero unAventurero = modificarSaludPorcentual (+) 20.modificarCarga (div) 2 $ unAventurero
efectoPersonaje Inspirador unAventurero = modificarSaludPorcentual (+) 10.modificarCoraje True$ unAventurero
efectoPersonaje Embaucador unAventurero = cambiarCriterio (lightPacker 10).modificarSaludPorcentual (-) 50.modificarCarga (+) 10.modificarCoraje False $ unAventurero

cambiarCriterio :: Criterio -> Aventurero -> Aventurero
cambiarCriterio unCriterio unAventurero = unAventurero {criterioSeleccionEncuentros = unCriterio}

modificarSaludPorcentual :: (Int -> Int -> Int) -> Int -> Aventurero -> Aventurero
modificarSaludPorcentual modificador porcentaje unAventurero = unAventurero {salud= modificador (salud unAventurero) (div (porcentaje * salud unAventurero) 100)}

modificarCoraje :: Bool -> Aventurero -> Aventurero
modificarCoraje valor unAventurero = unAventurero {tieneCoraje = valor}

--Punto 4
queEncuentros :: Aventurero -> [Personaje] -> [Personaje]
queEncuentros _ [] = []
queEncuentros unAventurero (cabeza:cola) 
    |
    |otherwise = []

cumpleEncuentroCriterios :: Aventurero -> Personaje -> Bool
