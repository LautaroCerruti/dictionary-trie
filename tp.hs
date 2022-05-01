data TTree k v = Node k (Maybe v) (TTree k v) (TTree k v) (TTree k v)
               | Leaf k v
               | E
    deriving Show

t = Node 'r' Nothing E (Node 'e' (Just 16) (Node 'a' Nothing E (Leaf 's' 1) E)
                                           (Node 'o' (Just 2) (Leaf 'd' 9)
                                                              E
                                                              (Leaf 's' 4))
                                           E)
                       (Node 's' Nothing E (Node 'i' (Just 4) (Leaf 'e' 8)
                                                              (Leaf 'n' 7)
                                                              E)
                                           E)

search :: Ord k => [k] -> TTree k v -> Maybe v
search _ E = Nothing
search [] _ = Nothing
search (x:xs) (Leaf k v) = if xs == [] && x == k then Just v else Nothing
search s@(x:xs) (Node k v l c r) | x < k = search s l
                                 | x > k = search s r
                                 | otherwise = if xs == [] then v
                                                           else search xs c

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

getK :: TTree k v -> k
getK (Leaf k _) = k
getK (Node k _ _ _ _) = k

merge :: Ord k => TTree k v -> TTree k v -> TTree k v
merge t E = t
merge E t = t
merge t1 (Leaf k2 v2) | (getK t1) < k2 = Node k2 (Just v2) t1 E E
                      | (getK t1) > k2 = Node k2 (Just v2) E E t1
merge t1 (Node k2 v2 l c r) | (getK t1) < k2 = Node k2 v2 (merge t1 l) c r
                            | (getK t1) > k2 = Node k2 v2 l c (merge t1 r)

balance :: Ord k => TTree k v -> TTree k v
balance (Node k v E E E) = case v of
                            Just vl -> Leaf k vl
                            Nothing -> E
balance t@(Node k v l E r) = case v of
                              Just vl -> t
                              Nothing -> merge l r
balance t = t

delete :: Ord k => [k] -> TTree k v -> TTree k v
delete _ E = E
delete [] t = t
delete (x:xs) (Leaf k v) = if xs == [] && x == k then E else Leaf k v
delete s@(x:xs) (Node k v l c r) | x < k = balance (Node k v (delete s l) c r)
                                 | x > k = balance (Node k v l c (delete s r))
                                 | xs == [] = balance (Node k Nothing l c r)
                                 | xs /= [] = balance (Node k v l (delete xs c) r)

keys :: TTree k v -> [[k]]
keys E = []
keys (Leaf k _) = [[k]]
keys (Node k v l c r) = let a = case v of
                                 Just vl -> [[k]]
                                 Nothing -> []
                        in (keys l) ++ a ++ (map (\s -> (k:s)) (keys c))++ (keys r)