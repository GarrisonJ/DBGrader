module DBGrader.Questions(questions, maxPoints, questionNames) where

import DBGrader.Types
import Data.List

-- Enter questions to be graded here
-- Look in Types.hs for details.
questions :: [Question]
questions = [ ViewQuestion "Question 1"  "hm1" "q1" "SELECT country, avg(salary)\n FROM agent\n GROUP BY country;" 10
            , TableQuestion "Question 2" "hm1" "books" 3 "" 10
            , QueryQuestion "Question 3" "hm1" "SELECT * FROM books NATURAL JOIN authors;" "" 10
            ] 

-- Calculate max score on homework.
maxPoints :: Float
maxPoints = sum $ map points questions

-- Get a list of question names.
questionNames :: String
questionNames = intercalate "," $ map nm questions

