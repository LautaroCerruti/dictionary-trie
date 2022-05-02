{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

-- Trabajo Practico 1
-- Cassinerio Cerruti

data TTree k v = Node k (Maybe v) (TTree k v) (TTree k v) (TTree k v)
               | Leaf k v
               | E
    deriving Show

-- search: Dada una clave y un arbol, devuelve un "Maybe", siendo este un "Just v"
--         en caso de que la clave se encuentre en el arbol, y Nothing en caso contrario.
search :: Ord k => [k] -> TTree k v -> Maybe v
search _ E = Nothing
search [] _ = Nothing
search (x:xs) (Leaf k v) = if xs == [] && x == k then Just v else Nothing
search s@(x:xs) (Node k v l c r) | x < k = search s l
                                 | x > k = search s r
                                 | otherwise = if xs == [] then v
                                                           else search xs c

-- insert: dada una clave, un valor y un arbol, inserta la clave
--         en el arbol con el valor asignado. En caso de ya existir 
--         la misma en el arbol, actualizamos su valor.
insert :: Ord k => [k] -> v -> TTree k v -> TTree k v
insert [] _ t = t
insert (x:xs) vi E | xs == [] = Leaf x vi
                   | otherwise = Node x Nothing E (insert xs vi E) E
insert s@(x:xs) vi (Leaf k v) | x < k = Node k (Just v) (insert s vi E) E E
                              | x > k = Node k (Just v) E E (insert s vi E)
                              | otherwise = if xs == [] then Leaf k vi
                                                        else Node k (Just v) E (insert xs vi E) E
insert s@(x:xs) vi (Node k v l c r) | x < k = Node k v (insert s vi l) c r
                                    | x > k = Node k v l c (insert s vi r)
                                    | otherwise = if xs == [] then Node k (Just vi) l c r
                                                              else Node k v l (insert xs vi c) r

-- getK: devuelve la clave almacenada en un nodo del arbol
getK :: TTree k v -> k
getK (Leaf k _) = k
getK (Node k _ _ _ _) = k

-- merge: Dados 2 arboles, los combina, insertando el primero en el segundo.
merge :: Ord k => TTree k v -> TTree k v -> TTree k v
merge t E = t
merge E t = t
merge t1 (Leaf k2 v2) | (getK t1) < k2 = Node k2 (Just v2) t1 E E
                      | (getK t1) > k2 = Node k2 (Just v2) E E t1
merge t1 (Node k2 v2 l c r) | (getK t1) < k2 = Node k2 v2 (merge t1 l) c r
                            | (getK t1) > k2 = Node k2 v2 l c (merge t1 r)

-- balance: Dado un arbol, balancea el mismo en caso de ser necesario.
--          los desbalanceos contemplados son: 
--          - que quede un nodo sin Hijos donde en el caso que este tenga 
--            un valor asociado lo transformamos en una hoja, en caso contrario
--            lo eliminamos.
--          - que quede un nodo sin hijo central y sin un valor. En este caso 
--            retornaremos como nodo al merge de sus hijos izquierdo y derecho.
balance :: Ord k => TTree k v -> TTree k v
balance (Node k v E E E) = case v of
                            Just vl -> Leaf k vl
                            Nothing -> E
balance t@(Node k v l E r) = case v of
                              Just vl -> t
                              Nothing -> merge l r
balance t = t

-- delete: Dado una lista de k y un arbol, en caso de estar en el arbol la lista, 
--         se la elimina y se balancea el arbol resultante
delete :: Ord k => [k] -> TTree k v -> TTree k v
delete _ E = E
delete [] t = t
delete (x:xs) l@(Leaf k v) = if xs == [] && x == k then E else l
delete s@(x:xs) (Node k v l c r) | x < k = balance (Node k v (delete s l) c r)
                                 | x > k = balance (Node k v l c (delete s r))
                                 | xs == [] = balance (Node k Nothing l c r)
                                 | xs /= [] = balance (Node k v l (delete xs c) r)

-- keys: Dado un arbol, obtiene una lista de sus claves ordenada
keys :: TTree k v -> [[k]]
keys E = []
keys (Leaf k _) = [[k]]
keys (Node k v l c r) = let a = case v of
                                 Just vl -> [[k]]
                                 Nothing -> []
                        in (keys l) ++ a ++ (map (\s -> (k:s)) (keys c))++ (keys r)

class Dic k v d | d -> k v where
    vacio       :: d
    insertar    :: Ord k => k -> v -> d -> d
    buscar      :: Ord k => k -> d -> Maybe v
    eliminar    :: Ord k => k -> d -> d
    claves      :: Ord k => d -> [(k, v)]

instance Ord a => Dic [a] v (TTree a v) where
    vacio = E
    insertar k v d = insert k v d
    buscar k d = search k d
    eliminar k d = delete k d
    claves d = let keyList = keys d
                   getV (Just v) = v
                in zip (keyList) (map (\x -> getV (search x d)) keyList)