- Step 0: Assign homework

    Homework can consist of the following tasks:  
      1. Define views  
         Example: Define a view that returns all agents from the agent table. Name the view q1.

      2. Create tables / add rows to tables  
         Example: Create a table named "books" and insert at least 4 rows.

      3. Modify database in such a way that a certain query produces a certain output.  
         Example: Insert rows into the tables "books" and "authors" such that the following query is nonempty:
                  SELECT * FROM books NATURAL JOIN authors;

- Step 1: Add specific homework information

    - Add students to DBGrader/Students.hs
    - Add questions to DBGrader/Questions.hs
    - Add credentials to DBGrader/Config.hs
    - Add flags to DBGrader/Flags.hs

    Note: DBGrader/Types.hs has more information about defining values.

- Step 2: Get student solutions from database
    ```
    $ runhaskell GetStudentAnswers.hs > studentGrades.csv
    ```
- Step 3: Grade homework

    Grade homework by adding flags to the 'flags' column in studentGrades.csv
    Remeber to define flags in DBGrader/Flags.hs

- Step 4: Calculate scores
    ```
    $ runhaskell createtotalscores.hs > studentTotalScores.csv
    ```
- Step 5: Create a summary sheet for each student

    The following command will produce a summary sheet for each student. The
    sheets will be placed in the ./gradesheets folder.
    ```
    $ runhaskell createSummarySheets.hs
    ```
- Step 6: Compile summary sheets to PDF

    Summary sheets are created in the markdown format. They should be compiled to
    pdf before sending them to students. If pandoc is installed you can compile them
    with the given makefile.
    ```
    $ cd gradesheets
    $ make
    ```
