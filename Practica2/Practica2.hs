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


--1. interp. Función que evalua una proposición dado el estado.
interp :: Estado -> Prop -> Bool
interp (xs) PTrue  = True
interp (xs) PFalse  = False
interp [] (PVar a) = False
interp (x:xs) (PVar a) = if a==x then True else interp (xs) (PVar a)
interp (x:xs) (PNeg a) = if (interp (x:xs) a == True) then False else True
interp (x:xs) (PAnd a b) = if ((interp (x:xs) a ) == True)
               && ((interp (x:xs) b )==True) then True else False
interp (x:xs) (POr a b) = if ((interp (x:xs) a ) == False)
               && ((interp (x:xs) b ) == False) then False else True
interp (x:xs) (PImpl a b) = if ((interp (x:xs) a ) == True)
               && ((interp (x:xs) b )== False) then False else True
interp (x:xs) (PEquiv a b) = if ((interp (x:xs) a ) == True)
               && ((interp (x:xs) b ) == True)
               || ((interp (x:xs) a ) == False)
               && ((interp (x:xs) b ) == False)then True else False


--2. estados. Función que devuelve una lista de todas las combinaciones
-- 				posibles de los estados de una proposición.
estados :: Prop -> [Estado]
estados p = error "Sin implementar."

--3. vars. Función que obtiene la lista de todas las variables de una
--			proposición.
vars :: Prop -> [String]
vars PTrue = ["True"]
vars PFalse = ["False"]
vars (PVar a) = [a]
vars (PNeg a) = vars a
vars (PAnd a b) = vars a ++ vars b
vars (POr a b ) = vars a ++ vars b
vars (PImpl a b) = vars(a) ++ vars(b)
vars (PEquiv a b) = vars(a) ++ vars(b)

--4. subconj. Función que devuelve el conjunto potencia de una lista.
subconj :: [a] -> [[a]]
subconj l = error "Sin implementar."

--5. modelos. Función que devuelve la lista de todos los modelos posibles
-- 				para una proposición.
modelos :: Prop -> [Estado]
modelos p = error "Sin implementar."

--6. tautologia. Función que dice si una proposición es tautología.
tautologia :: Prop -> Bool
tautologia p = error "Sin implementar."

--7. satisfen. Función que resuelve si una proposición es satisfacible
-- 				con cierto estado.
satisfen :: Estado -> Prop -> Bool
satisfen e p = error "Sin implementar."

--8. satisf. Función que resuelve si una proposición es satisfacible.
satisf :: Prop -> Bool
satisf p = error "Sin implementar."

--9. insatisfen. Función que resuelve si una proposición es insatisfacible
-- 					con cierto estado.
insatisfen :: Estado -> Prop -> Bool
insatisfen e p = error "Sin implementar."

--10. contrad. Función que dice si una proposición es una contradicción.
contrad :: Prop -> Bool
contrad p = error "Sin implementar."

--11. equiv. Función que devuelve True si dos proposiciones son equivalentes.
equiv :: Prop -> Prop -> Bool
equiv p1 p2 = error "Sin implementar."

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
deMorgan (PAnd (PNeg a) (PNeg b))= PNeg(POr a b)
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
