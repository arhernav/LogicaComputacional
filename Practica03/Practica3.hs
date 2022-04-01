{--
-- Juan Carlos Zenteno Pompa 316251608
-- Hernández Navarro Armando 317340347
--}
module Practica3 where

import Practica2



{----- Formas Normales -----}

-- 1. fnn. Función que devuelve la Forma Normal Negativa de una 
--         proposición.
fnn :: Prop -> Prop
-- Casos base
fnn PTrue = PTrue
fnn PFalse = PFalse
fnn (PVar a) = PVar a
-- Procesamiento de las negaciones
fnn (PNeg (PNeg p1)) = (fnn p1)
fnn (PNeg (POr p1 p2)) = (PAnd (PNeg (fnn p1)) (PNeg (fnn p2)))
fnn (PNeg (PAnd p1 p2)) = (POr (PNeg (fnn p1)) (PNeg (fnn p2)))
-- Propagación de la función. 
fnn (POr p1 p2) = POr (fnn p1) (fnn p2)
fnn (PAnd p1 p2) = PAnd (fnn p1) (fnn p2)
--Eliminación de implicaciones y equivalencias.
fnn (PImpl p1 p2) = fnn (POr (PNeg (fnn p1)) (fnn p2))
fnn (PNeg (PImpl p1 p2)) = fnn (PNeg (POr (PNeg (fnn p1)) (fnn p2)))
fnn (PEquiv p1 p2) = fnn (POr (PAnd p1 p2) (PAnd (PNeg p1) (PNeg p2)))
fnn (PNeg (PEquiv p1 p2)) = fnn (PNeg (POr (PAnd p1 p2) (PAnd (PNeg p1) (PNeg p2))))





-- 2. fnc. Función que devuelve la Forma Normal Conjuntiva de una 
--         proposición.
fnc :: Prop -> Prop
fnc p = error "Sin implementar."






{----- Algoritmo DPLL -----}

-- Definiciones de algunos conceptos.
type Literal = Prop
type Clausula = [Literal]
type Formula = [Clausula]
type Modelo = [Literal]
type Solucion = (Modelo, Formula)


-- 3. unit. Función que aplica la regla unitaria.
unit :: Solucion -> Solucion
unit (m, f) = error "Sin implementar."

-- 4. elim. Función que aplica la regla de eliminación. 
elim :: Solucion -> Solucion
elim (m, f) = error "Sin implementar."

-- 5. red. Función que aplica la regla de reducción.
red :: Solucion -> Solucion
red (m, f) = error "Sin implementar."

-- 6. split. Función que aplica la regla de la partición de una literal.
--            Se debe tomar la primer literal que aparezca en la fórmula.
split :: Solucion -> [Solucion]
split (m, f) = error "Sin implementar."

-- 7. conflict. Función que determina si la Solucion llegó a una contradicción.
conflict :: Solucion -> Bool
conflict (m, f) = error "Sin implementar."

-- 8. success. Función que determina si la fórmula es satisfacible.
success :: Solucion -> Bool
success (m, f) = error "Sin implementar."

--9. appDPLL. Función que aplica las reglas anteriores una vez.
appDPLL :: Solucion -> Solucion
appDPLL (m, f) = error "Sin implementar."



{-- Puntos Extra --}

{--
--dpll. Función que aplica el algoritmo DPLL a una fórmula.
dpll :: Solucion -> Solucion
dpll (m, f) = error "Sin implementar."
--}