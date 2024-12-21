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
sumarParesCarga unosAventureros = sum.obtenerCargasPares $ unosAventureros

obtenerCargasPares :: [Aventurero] -> [Int]
obtenerCargasPares unosAventureros = filter even.map carga $ unosAventureros

type Encuentro = Aventurero -> Aventurero