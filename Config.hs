module Config( connectInfoFromStudent
             , summarySheetHeader) where

import Types

-- These credentials need to provide access to all student databases
tahost, tauser, tapassword :: String
tahost     = "localhost"  -- Database host
tauser     = "garrison"   -- Username
tapassword = "password"   -- Password

-- Header for summary sheet
summarySheetHeader :: String
summarySheetHeader = "# Example header" ++ "\n" ++
                     "## Modify this header in Config.hs.\n" ++
                     "\n"

-- Given a student, returns connection string.
connectInfoFromStudent :: Student -> String
connectInfoFromStudent s = "host=" ++ tahost ++ " dbname=" ++ dbname s ++ " user=" ++ tauser ++ " password=" ++ tapassword
