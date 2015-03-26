module Flags(flags) where

import Types

-- Enter flags for grading here.
-- Look in Types.hs for details.
flags' = [ Flag       'a' 10 "This will remove 10 pints"
         , Flag       'b' 20 "This will remove 20 pints"
         , FlagAllPts 'c'    "This will remove all points from question."
         ]

flags :: [Flag]
flags = map addScoreToFlag flags'
    where
      addScoreToFlag (Flag c i n) = Flag c i $ n ++ " " ++ show i ++ "\n"
      addScoreToFlag f@(FlagAllPts c n) = f

