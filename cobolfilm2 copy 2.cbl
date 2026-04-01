       IDENTIFICATION DIVISION.
       PROGRAM-ID. FilmDB.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT film-file ASSIGN TO "filme.dat"
           ORGANIZATION IS SEQUENTIAL.

           SELECT temp-file ASSIGN TO "temp.dat"
           ORGANIZATION IS SEQUENTIAL.

           SELECT actor-file ASSIGN TO "actors.dat"
           ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD film-file.
       01 film-rec.
           05 film-id   PIC 9(4).
           05 film-name PIC X(20).
           05 film-year PIC 9(4).
           05 film-desc PIC X(20).

       FD temp-file.
       01 temp-rec.
           05 t-id   PIC 9(4).
           05 t-name PIC X(20).
           05 t-year PIC 9(4).
           05 t-desc PIC X(20).

       FD actor-file.
       01 actor-rec.
           05 a-film-id PIC 9(4).
           05 a-name    PIC X(20).

       WORKING-STORAGE SECTION.

       01 choice PIC X.
       01 delete-id PIC 9(4).
       01 last-id PIC 9(4) VALUE 0.

       01 name-in PIC X(20).
       01 year-in PIC X(4).
       01 desc-in PIC X(20).

       01 actor-name PIC X(20).

       PROCEDURE DIVISION.

       MAIN.
           PERFORM UNTIL choice = "9"
               DISPLAY "1 Show 2 Add 3 Delete 4 Add Actor 5 Find Actor 9 Exit"
               ACCEPT choice

               IF choice = "1"
                   PERFORM show-films
               END-IF

               IF choice = "2"
                   PERFORM add-film
               END-IF

               IF choice = "3"
                   PERFORM delete-film
               END-IF

               IF choice = "4"
                   PERFORM add-actor
               END-IF

               IF choice = "5"
                   PERFORM find-by-actor
               END-IF

           END-PERFORM

           STOP RUN.

       show-films.
           OPEN INPUT film-file
           PERFORM UNTIL FALSE
               READ film-file
                   AT END EXIT PERFORM
               END-READ
               DISPLAY film-id " | " film-name " | "
                       film-year " | " film-desc
           END-PERFORM
           CLOSE film-file.

       add-film.
           MOVE 0 TO last-id

           OPEN INPUT film-file
           PERFORM UNTIL FALSE
               READ film-file
                   AT END EXIT PERFORM
               END-READ
               MOVE film-id TO last-id
           END-PERFORM
           CLOSE film-file

           ADD 1 TO last-id

           DISPLAY "Name: " WITH NO ADVANCING
           ACCEPT name-in

           DISPLAY "Year: " WITH NO ADVANCING
           ACCEPT year-in

           DISPLAY "Desc: " WITH NO ADVANCING
           ACCEPT desc-in

           OPEN EXTEND film-file

           MOVE last-id TO film-id
           MOVE name-in TO film-name
           MOVE year-in TO film-year
           MOVE desc-in TO film-desc

           WRITE film-rec
           CLOSE film-file.

       delete-film.
           DISPLAY "ID: " WITH NO ADVANCING
           ACCEPT delete-id

           OPEN INPUT film-file
           OPEN OUTPUT temp-file

           PERFORM UNTIL FALSE
               READ film-file
                   AT END EXIT PERFORM
               END-READ

               IF film-id NOT = delete-id
                   MOVE film-rec TO temp-rec
                   WRITE temp-rec
               END-IF
           END-PERFORM

           CLOSE film-file
           CLOSE temp-file

           CALL "SYSTEM" USING "del filme.dat"
           CALL "SYSTEM" USING "rename temp.dat filme.dat".

       add-actor.
           DISPLAY "Film ID: " WITH NO ADVANCING
           ACCEPT a-film-id

           DISPLAY "Actor Name: " WITH NO ADVANCING
           ACCEPT a-name

           OPEN EXTEND actor-file
           WRITE actor-rec
           CLOSE actor-file.

       find-by-actor.
           DISPLAY "Actor: " WITH NO ADVANCING
           ACCEPT actor-name

           OPEN INPUT actor-file

           PERFORM UNTIL FALSE
               READ actor-file
                   AT END EXIT PERFORM
               END-READ

               IF a-name = actor-name
                   PERFORM show-film-by-id
               END-IF
           END-PERFORM

           CLOSE actor-file.

       show-film-by-id.
           OPEN INPUT film-file

           PERFORM UNTIL FALSE
               READ film-file
                   AT END EXIT PERFORM
               END-READ

               IF film-id = a-film-id
                   DISPLAY film-id " | " film-name " | "
                           film-year " | " film-desc
               END-IF
           END-PERFORM

           CLOSE film-file.