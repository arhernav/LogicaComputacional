{--
--
--
--}

module Practica2 where

--Prop. Tipo de datos para proposiciones lógicas.
data Prop = PTrue | PFalse | PVar String | PNeg Prop | POr Prop Prop
                  | PAnd Prop Prop | PImpl Prop Prop | PEquiv Prop Prop

--Estado. Lista de variables asignadas como verdaderas.
type Estado = [String]

--Instancia Show para Prop.
instance Show Prop where
  show PTrue = "T"
  show PFalse = "F"
  show (PVar x) = x
  show (PNeg p) = "¬"++show(p)
  show (POr p1 p2) = "("++show(p1)++" ∨ "++show(p2)++")"
  show (PAnd p1 p2) = "("++show(p1)++" ∧ "++show(p2)++")"
  show (PImpl p1 p2) = show(p1)++"→"++show(p2)
  show (PEquiv p1 p2) = show(p1)++"↔"++show(p2)


-- Funcion auxiliar que regresa el valor de la variable proposicional dada en el estado dado.
value :: Estado -> String -> Bool
value [] p = False
value (x:xs) p = if x==p then True else value xs p

--1. interp. Función que evalua una proposición dado el estado.
interp :: Estado -> Prop -> Bool
interp i PTrue  = True
interp i PFalse  = False
interp i (PVar a) = value i a
interp i (PNeg a) = not (interp i a)
interp i (PAnd a b) = (interp i a) && (interp i b)
interp i (POr a b) = (interp i a) || (interp i b)
interp i (PImpl a b) = (not (interp i a)) || (interp i b)
interp i (PEquiv a b) = interp i a == interp i b


--2. estados. Función que devuelve una lista de todas las combinaciones
-- 				posibles de los estados de una proposición.
estados :: Prop -> [Estado]
--Nótese que debido a que el estado de una variable está definido como la pertenencia a un conjunto,
--todos los estados posibles de una proposición son el conjunto potencia.
estados p = subconj (vars p)

--3. vars. Función que obtiene la lista de todas las variables de una
--			proposición.
vars :: Prop -> [String]
vars PTrue = []
vars PFalse = []
vars (PVar a) = [a]
vars (PNeg a) = vars a
vars (PAnd a b) = vars a ++ vars b
vars (POr a b ) = vars a ++ vars b
vars (PImpl a b) = vars a ++ vars b
vars (PEquiv a b) = vars a ++ vars b

--4. subconj. Función que devuelve el conjunto potencia de una lista.
subconj :: [a] -> [[a]]
subconj [] = [[]]
subconj (x:xs) = let subc = subconj xs in (combine x subc)++subc

--Funcion auxiliar que concatena el elemento dado a cada lista de la lista de las listas.
combine :: a -> [[a]] -> [[a]]
combine e [] = []
combine e (x:xs) = [e:x] ++ combine e xs

--Estas dos funciones auxiliares filtran la lista de estados dada en la proposición dada.

--De la lista de estados dada regresa los estados verdaderos
filterTrue :: [Estado] -> Prop -> [Estado]
filterTrue [] p = []
filterTrue (x:xs) p = if interp x p then x:(filterTrue xs p) else filterTrue xs p

--De la lista de estados dada regresa los estados falsos
filterFalse :: [Estado] -> Prop -> [Estado]
filterFalse [] p = []
filterFalse (x:xs) p = if interp x p then filterFalse xs p else x:(filterFalse xs p)

--5. modelos. Función que devuelve la lista de todos los modelos posibles
-- 				para una proposición.
modelos :: Prop -> [Estado]
modelos p = filterTrue (estados p) p

--6. tautologia. Función que dice si una proposición es tautología.
tautologia :: Prop -> Bool
tautologia p = (filterFalse (estados p) p) == []

--7. satisfen. Función que resuelve si una proposición es satisfacible
-- 				con cierto estado.
satisfen :: Estado -> Prop -> Bool
satisfen e p = interp e p

--8. satisf. Función que resuelve si una proposición es satisfacible.
satisf :: Prop -> Bool
satisf p = (filterTrue (estados p) p) /= []

--9. insatisfen. Función que resuelve si una proposición es insatisfacible
-- 					con cierto estado.
insatisfen :: Estado -> Prop -> Bool
insatisfen e p = (filterFalse (estados p) p) /= []

--10. contrad. Función que dice si una proposición es una contradicción.
contrad :: Prop -> Bool
contrad p = (filterFalse (estados p) p) == (estados p)

--11. equiv. Función que devuelve True si dos proposiciones son equivalentes.
equiv :: Prop -> Prop -> Bool
equiv p1 p2 = (filterFalse (estados p1) p1) == (filterFalse (estados p2) p2)

--12. elimEquiv. Función que elimina las equivalencias lógicas.
elimEquiv :: Prop -> Prop
elimEquiv p = error "Sin implementar."

--13. elimImpl. Función que elimina las implicaciones lógicas.
elimImpl :: Prop -> Prop
elimImpl p = error "Sin implementar."

--14. deMorgan. Función que aplica las leyes de DeMorgan a una proposición.
deMorgan :: Prop -> Prop
deMorgan PTrue = PTrue
deMorgan PFalse = PFalse
deMorgan (PVar a)= PVar a
deMorgan (PNeg(PAnd a b))=POr (PNeg a) (PNeg b)
deMorgan (PNeg(POr a b))= PAnd (PNeg a) (PNeg b)
deMorgan (PNeg a)= PNeg(deMorgan(a))
deMorgan (PAnd (PNeg a) (PNeg b))=PNeg(POr a b)
deMorgan (POr (PNeg a) (PNeg b))= PNeg(PAnd a b)
deMorgan (POr a b)= POr (deMorgan a) (deMorgan b)
deMorgan (PImpl a b)= PImpl (deMorgan a) (deMorgan b)
deMorgan (PEquiv a b)=PEquiv (deMorgan a) (deMorgan b)



{-- Punto extra. Funciones que implementan la satisfacibilidad sobre conjuntos.
--               Deben descomentar el siguiente código.--}

{--
estadosConj :: [Prop] -> [Estado]
modelosConj :: [Prop] -> [Estado]
satisfenConj:: Estado -> [Prop] -> Bool
satisfConj:: [Prop] -> Bool
insatisfenConj:: Estado -> [Prop] -> Bool
insatisfConj:: [Prop] -> Bool

--consecuencia. Función que determina si una proposición es consecuencia
--				del conjunto de premisas.
consecuencia: [Prop] -> Prop -> Bool
consecuencia gamma phi = null [i | i <- estadosConj (phi : gamma),
								satisfenConj i gamma,
								not (satisfen i phi)]

--argCorrecto. Función que determina si un argumento es lógicamente
--				correcto dadas las premisas.
argCorrecto :: [Prop] -> Prop -> Bool
argCorrecto gamma psi = consecuencia gamma psi
--}
