module Lib where
import Text.Show.Functions

laVerdad = True


type Barrio = String
type Mail = String
type Requisito = Depto -> Bool
type Busqueda = [Requisito]

data Depto = Depto { 
  ambientes :: Int,
  superficie :: Int,
  precio :: Int,
  barrio :: Barrio
} deriving (Show, Eq)

data Persona = Persona {
    mail :: Mail,
    busquedas :: [Busqueda]
}

ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) =
  (ordenarSegun criterio . filter (not . criterio x)) xs ++
  [x] ++
  (ordenarSegun criterio . filter (criterio x)) xs

between cotaInferior cotaSuperior valor =
  valor <= cotaSuperior && valor >= cotaInferior

deptosDeEjemplo = [
  Depto 3 80 7500 "Palermo", 
  Depto 1 45 3500 "Villa Urquiza", 
  Depto 2 50 5000 "Palermo", 
  Depto 1 45 5500 "Recoleta"]


-- Punto 1)

mayor :: Ord a => (b -> a) -> b -> b -> Bool
mayor f valor1 valor2 = f valor1 >= f valor2

menor :: Ord a => (b -> a) -> b -> b-> Bool
menor g valor1 valor2 = g valor1 <= g valor2

listaStrings :: [String]
listaStrings = ["Ignacio","Franco","Topy"]

{- Ejemplo con ordenarSegun:

ordenarSegun (mayor length) listaStrings

devuelve : ["Ignacio","Franco","Topy"]

ordenarSegun (menor length) listaStrings

devuelve : ["Topy","Franco","Ignacio"]

-}

-- Punto 2)

ubicadoEn :: [String] -> Depto -> Bool
ubicadoEn listaBarrios departamento = elem (barrio departamento) listaBarrios

cumpleRango :: Ord a => (Depto -> a) -> a -> a -> Depto -> Bool
cumpleRango funcion num1 num2 departamento = between num1 num2 (funcion departamento)

-- Punto 3)

cumpleBusqueda :: Depto -> Busqueda -> Bool
cumpleBusqueda departamento = all (cumpleRequisito departamento)

cumpleRequisito :: Depto -> Requisito -> Bool
cumpleRequisito depto requisito = requisito depto




-- Punto 4)

buscar :: Busqueda -> (Depto -> Depto -> Bool) -> [Depto] -> [Depto]
buscar busqueda criterio listaDeptos = filter (`cumpleBusqueda` busqueda) (ordenarSegun criterio listaDeptos)


--busquedasEjemplo :: Busqueda
busquedasEjemplo = [ubicadoEn ["Palermo","Recoleta"], cumpleRango ambientes 1 2, (<6000).precio]

{- Punto C

    buscar busquedasEjemplo (mayor superficie) listaDeptos
    [Depto {ambientes = 2, superficie = 50, precio = 5000, barrio = "Palermo"},
    Depto {ambientes = 1, superficie = 45, precio = 5500, barrio = "Recoleta"}]
-}

-- Punto 4)

mailsPersonasInteresadas :: Depto -> [Persona] -> [Mail]
mailsPersonasInteresadas depto = map mail. filter (estaInteresada depto)

estaInteresada :: Depto -> Persona -> Bool
estaInteresada depto persona = any (cumpleBusqueda depto) (busquedas persona)

