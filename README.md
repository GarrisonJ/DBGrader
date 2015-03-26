Step 0: Add specific homework information

    Add students to Students.hs
    Add questions to Questions.hs
    Add credentials to Credentials.hs
    Addb flags to Flags.hs

Step 1: Get student solutions from database

    runhaskell GetStudentAnswers.hs > studentGrades.csv

Step 2: Grade homework 

    Grade homework by adding flags to 'flags' column in studentGrades.csv

Step 3: Calculate scores 

    runhaskell createtotalscores.hs > studentTotalScores.csv

Step 4: Create summary sheets to send to student
    
    mkdir gradesheets
    runhaskell createSummarySheets.hs 

Step 5:

    cd gradesheets
    make 
