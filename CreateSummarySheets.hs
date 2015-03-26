import Text.CSV
import Data.List
import Data.Function
import DBGrader.Flags
import DBGrader.Questions
import DBGrader.Types
import DBGrader.Config

-- CSV file to parse for grades. 
-- The csv columns are as follows:
-- name, view, db, matches, notes, flags, query, errors
csvFile :: String
csvFile = "studentGrades.csv"

main :: IO ()
main = do
  grades <- parseCSVFromFile csvFile
  case grades of
    Left _ -> print "Error"
    Right x -> do
        -- Get list of student answers
        let stugrades = filterBlankLines $ groupByName x
        -- Create a summary file for each
        mapM_ createFile stugrades
  where
    groupByName xs = tail $ groupBy ((==) `on` head) xs
    filterBlankLines = filter (\y -> head (head y) /= "") 

-- Given a list of answers for a single student, create a summary sheet
-- in ./gradesheets/studentname.hs 
createFile :: [[Field]] -> IO ()
createFile g =  writeFile ("gradesheets/" ++ rmvSpcAndCmm nme ++ ".md") tem
  where
    -- Remove characters we don't want in file name.
    rmvSpcAndCmm = (\\ " ,")
    
    -- Given a character and a list of flags, produces
    -- the note associated with flag.
    getFlagNote :: [Flag] -> Char -> String
    getFlagNote xs c = case find (\(Flag ch _ _) -> ch == c) xs of
                          Nothing -> ""
                          Just (Flag _ _ n) -> n

    -- Given a list of chars produces notes.
    charsToNotes = concatMap (getFlagNote flags)
    -- Name of student
    nme          = head $ head g 
    -- Queries made
    queries      = map (!!5) g
    -- Notes
    nts          = map (charsToNotes . (!!4)) g
    -- Correct Solutions
    sol          = map solution questions
    -- List of "Correct" or "Incorrect". All questions with notes are incorrect.
    cor          = map corOrInc g
    -- Check if question is correct or incorrect
    corOrInc x | x!!4 == "" || x!!4 == " " = "Correct"
               | otherwise                 = "Incorrect"
    -- String to write to file
    tem          = template nme (zipWith3 solutionTemplate sol queries nts) cor

-- Template for printing the correct solution for an incorrect answer.
solutionTemplate :: String -> String -> String -> String 
solutionTemplate cor sol nt = "\n        Notes: \n" ++ nt ++ "\n" ++
                              "\n        Your Solution:\n" ++
                              "```SQL\n" ++
                              sol ++ 
                              "\n```\n" ++
                              "        Possible Solution:\n" ++
                              "```SQL\n" ++
                              cor ++
                              "\n```\n" 

-- Given the name of student, list of questions, and a list of correct or incorrect 
-- strings, this function produces a the final string to print to file.
template :: String -> [String] -> [String] -> String
template nme ques cor = summarySheetHeader ++
                "\n Student: " ++ nme ++ 
                concatMap (\x -> quesString x (cor!!(x-1)) (ques!!(x-1))) 
                          [1..length ques]
          where 
            quesString :: Int -> String -> String -> String
            quesString n c qe = show n ++ ". " ++ c ++ "\n" ++ ifIncorrect c qe
            ifIncorrect coi s | coi == "Correct" = "\n"
                              | otherwise        = s
