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
fnn (PNeg (PVar a)) = PNeg (PVar a)
-- Procesamiento de las negaciones
fnn (PNeg (PNeg p1)) = (fnn p1)
fnn (PNeg (POr p1 p2)) = (PAnd (PNeg (fnn p1)) (PNeg (fnn p2)))
fnn (PNeg (PAnd p1 p2)) = (POr (PNeg (fnn p1)) (PNeg (fnn p2)))
-- Propagación de la función.
fnn (POr p1 p2) = POr (fnn p1) (fnn p2)
fnn (PAnd p1 p2) = PAnd (fnn p1) (fnn p2)
--Eliminación de implicaciones y equivalencias.
fnn (PImpl p1 p2) = fnn (POr (PNeg (p1)) (p2))
fnn (PNeg (PImpl p1 p2)) = (PAnd (fnn p1) (fnn (PNeg (p2))))
fnn (PEquiv p1 p2) = fnn (POr (PAnd p1 p2) (PAnd (PNeg p1) (PNeg p2)))
fnn (PNeg (PEquiv p1 p2)) = fnn (PNeg (POr (PAnd p1 p2) (PAnd (PNeg p1) (PNeg p2))))




-- 2. fnc. Función que devuelve la Forma Normal Conjuntiva de una
--         proposición.
fnc :: Prop -> Prop
fnc PTrue = PTrue
fnc PFalse = PFalse
fnc (PVar a) = PVar a
fnc p = uberdistr (fnn p)


distr :: Prop -> Prop
-- Casos base
distr PTrue = PTrue
distr PFalse = PFalse
distr (PVar p) = PVar p

-- Distribuye la disyunción
distr (POr p (PAnd q1 q2)) = distr (PAnd (distr (POr p q1)) (distr (POr p q2)))
distr (POr (PAnd q1 q2) p) = distr (PAnd (distr (POr p q1)) (distr (POr p q2)))
-- Propagación de la función.
distr (PNeg p) = PNeg (distr p)
distr (POr p q) = POr (distr p) (distr q)
distr (PAnd p q) = PAnd (distr p) (distr q)
distr (PImpl p q) = PImpl (distr p) (distr q)
distr (PEquiv p q) = PEquiv (distr p) (distr q)

--realiza la distribución hasta que no se puede más.
--Se cicla infinitamente si se le pasa una formula con alguna equivalencia o implicación. Úsese con responsabilidad.
uberdistr :: Prop -> Prop
uberdistr p = let prev = distr p in if (isfnc prev) then prev else uberdistr prev

--Regresa si una proposición está en fnc.
isfnc:: Prop -> Bool
-- Casos Base
isfnc PTrue = True
isfnc PFalse = True
isfnc (PVar p) = True
-- Casos donde es falso
isfnc (POr p (PAnd q1 q2)) = False
isfnc (POr (PAnd q1 q2) p) = False
isfnc (PImpl p q) = False
isfnc (PEquiv p q) = False
--Propagación de la función
isfnc (PNeg p) = isfnc p
isfnc (PAnd p q) = (isfnc p)&&(isfnc q)
isfnc (POr p q) = (isfnc p)&&(isfnc q)



{----- Algoritmo DPLL -----}

-- Definiciones de algunos conceptos.
type Literal = Prop
type Clausula = [Literal]
type Formula = [Clausula]
type Modelo = [Literal]
type Solucion = (Modelo, Formula)


-- 3. unit. Función que aplica la regla unitaria.
unit :: Solucion -> Solucion
unit (m, []) = error "Formula vacía. No se puede proceder."
unit (m, x:xs) = let y:ys = x in  (y:m, xs)

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
