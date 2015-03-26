import Database.HDBC 
import Database.HDBC.PostgreSQL
import Control.Monad
import Text.CSV
import DBGrader.Questions
import DBGrader.Types
import DBGrader.Config
import DBGrader.Students

main :: IO ()
main = do
    -- Print header
    printLine "name"      -- Name of student.
              "question"  -- Question name (E.g. q1, q1, q3...).
              "db"        -- Database queried .
              "matches"   -- Did the students answer match the given solution.
              "notes"     -- Notes from the solution checker.
              "flags"     -- Where to input flags for grading.
              "query"     -- If this question involved defining a view, the view 
                          -- definition will be put here.
              "errors"    -- Errors received while trying to find solution, usually 
                          -- this means the database or view did not exist.
    -- Print student solutions for every student.
    mapM_ getStudentSolutions students

getStudentSolutions :: Student -> IO ()
getStudentSolutions s = catchSql (getStudentSolutions' s) $ catcherror (name s)
  where
    catcherror _ _ = putStrLn "\n" -- If we catch an error, just skip
    getStudentSolutions' :: Student -> IO ()
    getStudentSolutions' s = do
        conn <- connectPostgreSQL $ connectInfoFromStudent s
        mapM_ (getStudentGrade conn s) questions
        printLine "" "" "" "" "" "" "" "" -- Add blank line between students
        disconnect conn

-- Add a line to CSV output.
printLine :: String -> String -> String -> String -> String -> String -> String -> String -> IO ()
printLine name view db matches notes flags query errors = 
                putStrLn $ printCSV [[name, view, db, matches, notes, flags, query, errors]]

getStudentGrade :: Connection -> Student -> Question -> IO ()
getStudentGrade conn s q = case q of
      -- Depending on the type of question extract information from database 
      -- and print results. To understand question types see Types.hs
      ViewQuestion{} -> do
        -- Prepare query to get view definition.
        getViewDef <- prepare conn $ "select pg_get_viewdef('" ++ view q ++ "',true);"

        -- Run query to get view definition
        catchSql (executeSQL getViewDef) (catcherror s q conn "")

        -- Fetch rows to get view definition
        viewDef <- fetchAllRows getViewDef

        -- If viewDef is null then an error occurred.
        let def = if null viewDef
                    then "Error"
                    else fromSql $ (head . head) viewDef 

        -- Prepare query to check if answer matches solution
        stmt <- prepare conn $ differenceBetweenQuerys (view q) (solution q)

        -- Run query to check solution
        (n,error) <- catchSql (executeSQL stmt) (catcherror s q conn def)

        -- Fetch rows from query
        results <- fetchAllRows stmt
        
        -- Solution was correct if no rows were returned.
        let correct = null results 
        
        if n==errorCode || def == "Error"
            then printLine (name s) (nm q) (dbname s) "Error" "" "" "" error
            else printLine (name s) 
                           (view q) 
                           (dbname s) 
                           (show (correct && def/="Error" )) 
                           "" 
                           "" 
                           def 
                           "None"

      TableQuestion{} -> do
        -- Query to count number of rows in table
        let qur = "SELECT COUNT(*) FROM " ++ schema q ++ "." ++ table q ++ ";"

        -- Prepare query
        stmt <- prepare conn qur

        -- Run query
        (n,error) <- catchSql (executeSQL stmt) (catcherror s q conn "")

        -- Get rows from query, this should be a count of rows
        results <- fetchAllRows stmt
      
        if n == errorCode
          then printLine (name s) (nm q) (dbname s) "Error" "" "" "" error
          else let correct = if null results
                              then "Error"
                              else show $ fromSql ((head . head) results) >= rows q
                in printLine (name s) 
                             (nm q) 
                             (dbname s) 
                             correct 
                             ("Rows returned:" ++ fromSql ((head . head) results))
                             "" 
                             qur 
                             ""
        
      QueryQuestion{} -> do 
        -- Prepare query 
        stmt <- prepare conn $ query q
        
        -- Run query
        (n,error) <- catchSql (executeSQL stmt) (catcherror s q conn "")

        -- Get rows from query
        results <- fetchAllRows stmt

        if n == errorCode
          then printLine (name s) (nm q) (dbname s) "Error" "" "" "" error
          else let len     = show $ length results
                   correct = show $ not $ null results 
               in printLine (name s) 
                            (nm q) 
                            (dbname s) 
                            correct 
                            ("Rows returned:" ++ len)
                            "" 
                            (query q) 
                            ""
  where
    executeSQL sql = execute sql [] >>= \i -> return (i, "")
    errorCode = 37707 -- This is an arbitrary int to denote an error
    catcherror st qe con ans ex = do
        rollback con -- Rollback connection
        return (errorCode, show (seErrorMsg ex))  -- Return error code

    -- Takes two sql queries and returns a new sql query that will return zero rows 
    -- if queries are equal. If the queries return different columns then running the 
    -- produced query will cause a error at runtime.
    differenceBetweenQuerys :: SQLQuery -> SQLQuery -> SQLQuery
    differenceBetweenQuerys query1 query2 = "(SELECT * FROM " 
                                            ++ init query1 -- Remove ; from sql query
                                            ++ " EXCEPT SELECT * FROM ("  
                                            ++ init query2 
                                            ++ ") AS a) "
                                            ++ "UNION ALL"
                                            ++ " (SELECT * FROM (" 
                                            ++ init query2 
                                            ++ ") AS b EXCEPT SELECT * FROM " 
                                            ++ init query1 
                                            ++ ");"
