import Text.Show.Functions

--Punto 1
data Aventurero = UnAventurero {
    nombre :: String,
    carga :: Int,
    salud :: Int,
    tieneCoraje :: Bool,
    criterioSeleccionEncuentros :: Criterio
}

type Encuentro = Aventurero -> Aventurero
type Criterio = Aventurero -> Bool

conformista :: Criterio
conformista unAventurero = True

valiente :: Criterio
valiente unAventurero = tieneCoraje unAventurero || salud unAventurero > 50

lightPacker :: Int -> Criterio
lightPacker umbral unAventurero = (<umbral).carga $ unAventurero 
