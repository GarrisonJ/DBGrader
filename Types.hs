module Types where

-- A view question asks a student to defined a view.
-- Example:
--    Define a view that returns all agents from the agent table. Name the view q1.
--
-- A table question asks a student to create a table
-- Example:
--    Create a table named "books", insert at least 4 rows.
--
-- A query question asks a student to define their database in such a way that a certain query will return some value. 
-- Example:
--    Insert rows into the tables "books" and "authors" such that the following query will return nonempty
--    SELECT * FROM books NATURAL JOIN authors;
data Question = ViewQuestion  { nm       :: String -- Name of question (Eg. Question 1)
                              , schema   :: String -- Schema to find view
                              , view     :: String -- View name 
                              , solution :: SQLQuery -- Example solution 
                              , points   :: Float  -- Points question is worth.
                              } 
              | TableQuestion { nm       :: String -- Name of question
                              , schema   :: String -- Schema to find table
                              , table    :: String -- Table name
                              , rows     :: Int    -- The amount of rows that should be present in table
                              , solution :: String -- 
                              , points   :: Float  -- Points question is worth
                              }
              | QueryQuestion { nm       :: String -- Name of question
                              , schema   :: String -- Schema to run query
                              , query    :: String -- Query to run
                              , solution :: String -- 
                              , points   :: Float  -- Points question is worth
                              }
              deriving (Show)

-- Type synonym for a sql query.
type SQLQuery = String

data Student = Student { name   :: String -- Student name
                       , dbname :: String -- Student's database name
                       } deriving (Ord, Eq, Show)

data Flag = Flag { flag      :: Char   -- Character to denote flag
                 , subPoints :: Float  -- Amount of points to subtract
                 , flagNote  :: String -- Explanation for student
                 } 
          | FlagAllPts 
                 { flag     :: Char   -- Character to denote flag
                 , flagNote :: String -- Explanation for student
                 } 

data Config = Config { hostname :: String -- Hostname of database
                     , database :: String -- Database for logging in
                     , username :: String -- Username for logging in
                     , password :: String -- Password for logging in
                     , conninfo :: String -- Connection string, needed for HDBC
                     }
