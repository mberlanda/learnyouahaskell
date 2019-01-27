-- data ZipList a = ZipList [a]
-- data ZipList a = ZipList { getZipList :: [a] }
newtype ZipList a = ZipList { getZipList :: [a] }
{-
  with data, you can make data types that have several value
  constructors and each constructor can have zero or more fields
-}

data Profession = Fighter | Archer | Accountant
data Race = Human | Elf | Orc | Goblin
data PlayerCharacter = PlayerCharacter Race Profession

newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)

main = do
        let charList = CharList "this will be shown!"
        putStrLn . show $ charList
        putStrLn . show $ charList == charList
        putStrLn $ getCharList charList
