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
efectoPersonaje Curandero unAventurero = modificarCarga (div) 2 unAventurero
efectoPersonaje Curandero unAventurero = modificarCarga (div) 2 unAventurero

aumentoSaludPorcentual :: Int -> Aventurero -> Aventurero
aumentoSaludPorcentual porcentaje unAventurero = unAventurero {div porcentaje 100