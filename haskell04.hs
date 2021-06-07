-- Prática 04 de Haskell
-- Nome: Julio Cesar Polmann Cuencas

faixaIdoso :: Int -> String
faixaIdoso x
    | x >= 60 && x <= 64 = "IDO64"
    | x >= 65 && x <= 69 = "IDO69"
    | x >= 70 && x <= 74 = "IDO74"
    | x >= 75 && x <= 79 = "IDO79"
    | x >= 80 = "IDO80"
    | otherwise = "ND"

classifIdosos :: [(String,Int)] -> [(String,Int,String)]
classifIdosos tupla = [(nome,idade,faixaIdoso idade) | (nome,idade) <- tupla]

classifIdosos' :: [(String,Int)] -> [(String,Int,String)]
classifIdosos' ltupla = zipWith (\(x,y) z -> (x,y,z)) ltupla (map faixaIdoso (map (\(_,x) -> x) ltupla)) 

strColor :: (Int,Int,Int) -> String
strColor tuple = (\(r,g,b) -> "rgb(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")") tuple

genCircs :: Int -> (Int,Int) -> Int -> [(Int,Int,Int)]
genCircs n (cx,cy) r = [(x,cy,r) | x <- take n [cx+r,cy..]]

genReds :: Int -> [(Int,Int,Int)]
genReds n = [(i*10,0,0) | i <- take n [1,2..]]