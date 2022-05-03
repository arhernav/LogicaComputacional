{--
--
--}
module Practica4 where

import Data.List --Unicamente se usa la función sort.

--Definición del tipo de datos para términos.
data Term = V Nombre | F Nombre [Term]

--Definición del tipo de datos para fórmulas.
data Form = NForm | TrueF | FalseF | Pr Nombre [Term] | Eq Term Term | 
            Neg Form | Conj Form Form | Disy Form Form | 
            Imp Form Form | Equi Form Form | All Nombre Form | 
            Ex Nombre Form

type Nombre = String

type Subst = [(Nombre,Term)]


--Instancia Show para Term.
instance Show Term where
  show (V x) = x
  show (F f t) = f ++ "(" ++ show t ++ ")"

--Instancia Show para Form.
instance Show Form where
  show NForm = ""
  show TrueF = "T"
  show FalseF = "F"
  show (Pr p t) = p ++ "(" ++ show t ++ ")"
  show (Eq t1 t2) = "(" ++ show t1 ++ "=" ++ show t2 ++ ")"
  show (Neg f) = "¬" ++ show f
  show (Conj f1 f2) = "(" ++ show f1 ++ " ^ " ++ show f2 ++ ")"
  show (Disy f1 f2) = "(" ++ show f1 ++ " v " ++ show f2 ++ ")"
  show (Imp f1 f2) = "(" ++ show f1 ++ " -> " ++ show f2 ++ ")"
  show (Equi f1 f2) = "(" ++ show f1 ++ " <--> " ++ show f2 ++ ")"
  show (All x f) = "Alle " ++ x ++ " (" ++ show f ++ ")" 
  show (Ex x f) = "Ein " ++ x ++ " (" ++ show f ++ ")"



--alcance. Función que devuelve el alcance de los cuantificadores de
--          una fórmula.
alcance :: Form -> [(Form, Form)]
-- casos base
alcance NForm = []
alcance TrueF = []
alcance FalseF = []
alcance (Pr _ _) = []
alcance (Eq _ _) = []
--casos recursivos
alcance (Neg p) = alcance p
alcance (Conj p q) = (alcance p)++(alcance q)
alcance (Disy p q) = (alcance p)++(alcance q)
alcance (Imp p q) = (alcance p)++(alcance q)
alcance (Equi p q) = (alcance p)++(alcance q)
alcance (All x p) = [(All x NForm, p)]++(alcance p)
alcance (Ex x p) = [(Ex x NForm, p)]++(alcance p)

--bv. Función que devuelve las variables ligadas de una fórmula.
bv :: Form -> [Nombre]
bv f = let alcances = alcance f in elimReps (ligadas alcances)
           
-- Función auxiliar que regresa las variables ligadas de los alcances dados.
ligadas :: [(Form, Form)] -> [Nombre]
ligadas [] = []
ligadas (x:xs) = let (cuant, _) = x in (varCuantificador cuant):(ligadas xs)
  
-- Dado un cuantificador, regresa la variable que cuantifica.
varCuantificador :: Form -> Nombre
varCuantificador (All x _) = x
varCuantificador (Ex  x _) = x
varCuantificador f = error "No es un cuantificador."

-- Dada una lista de nombres, regresa la lista sin elementos repetidos.
elimReps :: [Nombre] -> [Nombre]
elimReps l = let sorted = sort l in compress sorted

-- Tipo de dato que representa un conjunto de variables.
type VSet = [Nombre]

-- Operación de añadir al conjunto. No permite que hayan dos elementos repetidos.
addVSet :: VSet -> Nombre -> VSet 
addVSet [] x = [x]
addVSet (x:xs) y = if x==y then x:xs else x:(addVSet xs y)

--Operación de unión de conjuntos. No permite elementos repetidos.
unionVSet :: VSet -> VSet -> VSet
unionVSet [] s = s
unionVSet s [] = s
unionVSet s (x:xs) = let set = addVSet s x in unionVSet set xs

--fv. Función que devuelve las variables libres de una fórmula.
--fv :: Form -> [Nombre]
-- Lo que hacemos es sacar todas las variables de la lista, luego, la ordenamos. Una vez ordenada 
-- todas las variables repetidas están juntas, por lo que si comprimimos la lista via la función
-- compress, terminamos con una lista sin elementos repetidos, que es funcionalmente idéntica a 
-- un conjunto. Así que, teniendo dos VSets, aplicamos la operación que elimina los elementos de 
-- el primero en el segundo, siendo el primero el conjunto de las variables ligadas. y así, 
-- tenemos nuestra solución. Nota: si x aparece como no ligada y ligada a la vez, esta función no la incluirá.
--fv f = let vars = allVars f; vars_sorted = sort vars; vars_set compress vars_sorted; ligadas = bv f in removeVSet ligadas vars_set

-- Regresa todas las instancias de cada variable en la fórmula.
allVars :: Form -> [Nombre]
-- casos base
allVars NForm = []
allVars TrueF = []
allVars FalseF = []
allVars (Pr _ t) = termVars (F "" t) -- Hacer esto me permite reutilizar el código para contar las variables en un término.
allVars (Eq t1 t2) = (termVars t1)++(termVars t2)
allVars (Neg p) = allVars p
allVars (Conj p q) = (allVars p)++(allVars q)
allVars (Disy p q) = (allVars p)++(allVars q)
allVars (Imp p q) = (allVars p)++(allVars q)
allVars (Equi p q) = (allVars p)++(allVars q)
allVars (All x p) = allVars p
allVars (Ex x p) = allVars p

-- Remueve elementos sucesivos que son iguales. por ejemplo removeSuccesive [a,a] = [a]
compress :: [Nombre] -> [Nombre]
compress [] = []
compress (x:xs) = x:(popEqual x xs)

--Función auxiliar para compress.
popEqual :: Nombre -> [Nombre] -> [Nombre]
popEqual s [] = []
popEqual s (x:xs) = if s==x then popEqual s xs else x:(popEqual x xs)

-- regresa las variables del término dado.
termVars :: Term -> [Nombre]
termVars (V x) = [x]
termVars (F _ []) = []
termVars (F _ (x:xs)) = unionVSet (termVars x) (termVars (F "" xs))

-- regresa si el conjunto dado contiene a la variable dada
vSetHas :: VSet -> Nombre -> Bool
vSetHas [] x = False
vSetHas (x:xs) y = if x==y then True else vSetHas xs y

-- remueve los elementos de el primer conjunto de el segundo.
removeVSet :: VSet -> VSet -> VSet
removeVSet [] s = s
removeVSet e [] = []
removeVSet e (x:xs) = if (vSetHas e x) then removeVSet e xs else x:(removeVSet e xs) 

--sustTerm. Función que realiza la sustitución de variables en un término.
sustTerm :: Term -> Subst -> Term
sustTerm t s = error "Sin implementar."

--sustForm. Función que realiza la sustitución de variables en una 
--          fórmula sin renombramientos.
sustForm :: Form -> Subst -> Form
sustForm f s = error "Sin implementar."

--alphaEq. Función que dice si dos fórmulas son alpha-equivalentes.
alphaEq :: Form -> Form -> Bool
alphaEq f1 f2 = error "Sin implementar."

{-- Puntos Extra
renom :: Form -> Form
renomConj :: Form -> Form
sustFormAlpha :: Form -> Subst -> Form
--}
