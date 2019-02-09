module K where
  import Control.Monad
  -- You have a knight on a chess board and you want to
  -- find out the positions it can reach in three moves
  type KnightPos = (Int,Int)

  moveKnightM :: KnightPos -> [KnightPos]
  moveKnightM (c,r) = do
    (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
               ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)
               ]
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (c',r')

  moveKnightF :: KnightPos -> [KnightPos]
  moveKnightF (c,r) = filter onBoard
      [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
      ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)
      ]
      where onBoard (c,r) = c `elem` [1..8] && r `elem` [1..8]

  in3 :: KnightPos -> [KnightPos]
  in3 start = do
      first <- moveKnightF start
      second <- moveKnightF first
      moveKnightF second

  in3M :: KnightPos -> [KnightPos]
  in3M start = return start >>= moveKnightF >>= moveKnightF >>= moveKnightF

  canReachIn3 :: KnightPos -> KnightPos -> Bool
  canReachIn3 start end = end `elem` in3 start
