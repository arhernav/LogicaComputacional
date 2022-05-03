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

-- Remueve elementos sucesivos que son iguales. por ejemplo removeSuccesive [a,a] = [a]
compress :: [Nombre] -> [Nombre]
compress [] = []
compress (x:xs) = x:(popEqual x xs)


--Función auxiliar para compress.
popEqual :: Nombre -> [Nombre] -> [Nombre]
popEqual s [] = []
popEqual s (x:xs) = if s==x then popEqual s xs else x:(popEqual x xs)


--fv. Función que devuelve las variables libres de una fórmula.
fv :: Form -> [Nombre]
fv f = let vars = fvAux f [] in elimReps vars

-- Hace la mayoría del trabajo. Pero, por desgracia, las variables salen repetidas.
fvAux :: Form -> [Nombre] -> [Nombre]
fvAux NForm  l = []
fvAux TrueF  l = []
fvAux FalseF l = []
fvAux (Pr _ terms)  l = let vars = termVars (F "" terms) in removeVSet l vars --Al reescribirlo como función, podemos reutilizar la función para contar las variables de un predicado.
fvAux (Eq t1 t2) l = let (v1, v2) = (termVars t1, termVars t2) in v1 ++ v2
fvAux (Neg p) l = fvAux p l
fvAux (Conj p q)  l = (fvAux p l)++(fvAux q l)
fvAux (Disy p q) l = (fvAux p l)++(fvAux q l)
fvAux (Imp p q) l = (fvAux p l)++(fvAux q l)
fvAux (Equi p q) l = (fvAux p l)++(fvAux q l)
fvAux (All x p) l = fvAux p (x:l)
fvAux (Ex x p) l = fvAux p (x:l)

-- Tipo de dato que representa un conjunto de variables.
type VSet = [Nombre]

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

-- regresa las variables del término dado.
termVars :: Term -> VSet
termVars (V x) = [x]
termVars (F _ []) = []
termVars (F _ (x:xs)) =  (termVars x) ++ (termVars (F "" xs))

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
