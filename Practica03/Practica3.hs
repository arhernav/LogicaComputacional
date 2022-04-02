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
unit (m, f) = let l = getliterals f in if (null (l)) then ([], f) else ((head l):m, removelf (head l) f)
--------------------------------------------Auxiliares de unit-------------------------------------------------------------------------------------------
-- Regresa las literales solitas que tenga la formula.
getliterals :: Formula -> [Literal]
getliterals [] = []
getliterals (x:xs) = let l:ls = x in if (null ls) then l:(getliterals xs) else getliterals xs

--Remueve la literal dada de la formula
removelf :: Literal -> Formula -> Formula
removelf l [] = []
removelf l (x:xs) = let c = removelc l x in if (null c) then removelf l xs else c:(removelf l xs) 

--Remueve la literal dada de la clausula
removelc :: Literal -> Clausula -> Clausula
removelc l [] = []
removelc l (x:xs) = if (equals l x) then removelc l xs else x:(removelc l xs)

-- Regresa si las dos proposiciones dadas son iguales.
equals :: Prop -> Prop -> Bool
-- Casos Base
equals PTrue PTrue = True
equals PFalse PFalse = True
equals (PVar p) (PVar q) = p == q
-- Propagación de la igualdad
equals (PNeg p) (PNeg q) = equals p q
equals (POr p1 q1) (POr p2 q2) = (equals p1 p2)&&(equals q1 q2)
equals (PAnd p1 q1) (PAnd p2 q2) = (equals p1 p2)&&(equals q1 q2)
equals (PImpl p1 q1) (PImpl p2 q2) = (equals p1 p2)&&(equals q1 q2)
equals (PEquiv p1 q1) (PEquiv p2 q2) = (equals p1 p2)&&(equals q1 q2)
-- Todo lo que no cumpla con lo anterior se considerará falso.
equals p q = False

--Se usan tantas funciones auxiliares para eliminar la literal de la formula, sin importar su ubicación en esta
------------------------------------------------------------------------------------------------------------------------------------------------------------



-- 4. elim. Función que aplica la regla de eliminación.
elim :: Solucion -> Solucion
elim (m, []) = error "Formula vacía. No se puede proceder."
elim (m, f) = (m, removemf m f)
--------------------------------------------Auxiliares de elim-------------------------------------------------------------------------------------------
---Remueve las clausulas que tengan una literal dentro del modelo dado en la formula dada.
removemf :: Modelo -> Formula -> Formula
removemf m [] = []
removemf [] f = f
removemf (l:ls) f = let f2 = removecl l f in removemf ls f2
        
--remueve la clausula que contenga a la literal dada en la formula dada
removecl :: Literal -> Formula -> Formula
removecl l [] = []
removecl l (c:cs) = if (containscl l c) then removecl l cs else c:(removecl l cs)
                        
--Regresa si la clausula contiene a esta literal
containscl :: Literal -> Clausula -> Bool
-- Caso Base
containscl l [] = False
-- Carnita del asunto
containscl l (c:cs) = if (equals l c) then True else containscl l cs

------------------------------------------------------------------------------------------------------------------------------------------------------------


-- 5. red. Función que aplica la regla de reducción.
red :: Solucion -> Solucion
red (m, f) = (m, removecontrarias m f)
--------------------------------------------Auxiliares de red-------------------------------------------------------------------------------------------
--Remueve las literales contrarias a cada literal en el modelo.
removecontrarias :: Modelo -> Formula -> Formula
removecontrarias m [] = []
removecontrarias [] f = f 
removecontrarias (l:ls) f = let contraria = fnn (PNeg l); f2 = removelf contraria f in removecontrarias ls f2
-- removelf está definido en las auxiliares de unit

------------------------------------------------------------------------------------------------------------------------------------------------------------

-- 6. split. Función que aplica la regla de la partición de una literal.
--            Se debe tomar la primer literal que aparezca en la fórmula.
split :: Solucion -> [Solucion]
split (m, f) = let l = getliteral f in [(l:m, f),((PNeg l):m, f)]
--------------------------------------------Auxiliares de split-------------------------------------------------------------------------------------------
-- Regresa la primera literal que se encuentre en la formula.
getliteral :: Formula -> Literal
getliteral [] = error "No se puede sacar una literal de una formula sin literales."
getliteral (c:cs) = if (null c) then getliteral cs else head c
------------------------------------------------------------------------------------------------------------------------------------------------------------

-- 7. conflict. Función que determina si la Solucion llegó a una contradicción.
conflict :: Solucion -> Bool
conflict ([], f) = False
conflict (m, []) = False
conflict (l:ls, f) = if (contains (getliterals f) (fnn (PNeg l))) then True else False
--------------------------------------------Auxiliares de conflict-------------------------------------------------------------------------------------------
-- Regresa si la lista de literales contiene a la literal dada
contains :: [Literal] -> Literal -> Bool
contains [] l = False
contains (l:ls) l1 = if (equals l l1) then True else contains ls l1
------------------------------------------------------------------------------------------------------------------------------------------------------------

-- 8. success. Función que determina si la fórmula es satisfacible.
success :: Solucion -> Bool
success (m, []) = True
success (m, f) = False

--9. appDPLL. Función que aplica las reglas anteriores una vez.
appDPLL :: Solucion -> Solucion
appDPLL s = let (m, f) = s in if (null m) then let (m1, f1) = unit s in if (null m1) then s else (m1, f1)  else red (elim (red s)) 
-- Por cosas como esta Haskell me parece un lenguaje tan feo como pegarle a un padre

{-- Puntos Extra --}

{--
--dpll. Función que aplica el algoritmo DPLL a una fórmula.
dpll :: Solucion -> Solucion
dpll (m, f) = error "Sin implementar."
--}
