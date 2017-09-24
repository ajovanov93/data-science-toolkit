-- This file generates haskell code for ToNamedRecord instances for labels
-- This is the general pattern detected.
{-
instance (ToField value1) => ToNamedRecord (lbl1 := value1) where
    toNamedRecord (lbl1 := value1) = namedRecord [(n !! 0) .= value1]
        where
            n = (map BS.pack) . labels . pure $ (lbl1 := value1)

instance (ToField value1, ToField value2) => ToNamedRecord (lbl1 := value1, lbl2 := value2) where
    toNamedRecord (lbl1 := value1, lbl2 := value2) = namedRecord [(n !! 0) .= value1, (n !! 1) .= value2]
        where
            n = (map BS.pack) . labels . pure $ (lbl1 := value1, lbl2 := value2)

instance (ToField value1, ToField value2, ToField value3) => ToNamedRecord (lbl1 := value1, lbl2 := value2, lbl3 := value3) where
    toNamedRecord (lbl1 := value1, lbl2 := value2, lbl3 := value3) = namedRecord [(n !! 0) .= value1, (n !! 1) .= value2, (n !! 2) .= value3]
        where
            n = (map BS.pack) . labels . pure $ (lbl1 := value1, lbl2 := value2, lbl3 := value3)
-}
import Text.Printf

main = do
    mapM_ (\i -> putStrLn $ make i) [1..24]

make :: Int -> String
make n = output
    where 
        abl xs = (take ((length xs) - 1) xs)
        vals   = map (\i -> "value" ++ (show i)) [1..n]
        tfv    = "(" ++ abl (concatMap (\v -> "ToField " ++ v ++ ",") vals) ++ ")"
        rec    = "(" ++ abl (concatMap (\i -> printf "lbl%d := value%d," i i) [1..n]) ++ ")"
        nrc    = "[" ++ abl (concatMap (\i -> printf "(n !! %d) .= value%d," (i - 1) i) [1..n]) ++ "]"
        fmt    = "instance %s => ToNamedRecord %s where\n\ttoNamedRecord %s = namedRecord %s\n\t\twhere n = (map BS.pack) . labels . pure $ %s"
        output = printf fmt tfv rec rec nrc rec
