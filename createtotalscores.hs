import Text.CSV
import Data.List
import Data.Function
import Flags
import Questions
import Types

-- CSV file to parse for grades. 
-- The csv columns are as follows:
-- name, view, db, matches, notes, flags, query, errors

csvFile :: String
csvFile = "studentGrades.csv"

main :: IO ()
main = do
  grades <- parseCSVFromFile csvFile
  case grades of
    Left _ -> print $ "Error opening " ++ csvFile
    Right x -> do
        -- Print header
        putStr header
        -- get list of students
        let stugrades = filterBlankLines $ groupByName x
        -- Print row for each student
        mapM_ printStudentGrades stugrades
  where
    groupByName xs = tail $ groupBy ((==) `on` head) xs
    filterBlankLines = filter (\y -> head (head y) /= "") 

-- Given a list of rows containing student solutions, this will print a csv row of 
-- student grades.

printStudentGrades :: [[Field]] -> IO ()
printStudentGrades s = putStr grades
    where
      grades = printCSV [[nme] ++ fgs ++ [score]] ++ "\n" 
      nme    = head $ head s        -- Name of student
      fgs    = map (!!5) s          -- List of flags
      pts    = map points questions -- List of points for each question
      score  = show $ maxPoints + sum (zipWith calcGrade pts fgs)

-- Given points question is worth, and a list of flags, produces amount to subtract from
-- score.

calcGrade :: Float -> String -> Float
calcGrade p nts = sum $ map (noteToGrade p) nts
  where
    noteToGrade :: Float -> Char -> Float
    noteToGrade p c = case find (\f -> flag f==c) flags of
                          Just f@(FlagAllPts _ _) -> -p  -- Subtract all points
                          Just f                  -> subPoints f  
                          Nothing                 -> 0

-- Header of csv file 

header :: String
header = "name," ++ questionNames ++ ",total\n"
