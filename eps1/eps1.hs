{--
1)
--}
class Visible a where
    toString :: a -> String
    size :: a -> Int

instance Visible Bool where
    toString True = "True"
    toString _ = "False"
    size _ = 1

instance Visible (a, b) where
    toString (a, b) = "True"
    size (_, _) = 2

instance Visible (a, b, c) where
    toString (a, b, c) = "True"
    size (_, _, _) = 3

-- {--
-- 2)
-- --}
converte :: Int -> String
converte n = show n
instance Visible Int where
    toString n = converte n
    size _ = 0

-- {--
-- 3) compare x y = size x <= size y é numérico
-- --}