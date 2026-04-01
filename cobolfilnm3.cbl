       IDENTIFICATION DIVISION.
       PROGRAM-ID. FilmDB.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT film-file ASSIGN TO "filme.dat"
               ORGANIZATION IS INDEXED
               RECORD KEY IS film-id
               FILE STATUS IS fs-film.

           SELECT actor-file ASSIGN TO "actors.dat"
               ORGANIZATION IS INDEXED
               RECORD KEY IS a-film-id
               ALTERNATE RECORD KEY IS a-name WITH DUPLICATES
               FILE STATUS IS fs-actor.

       DATA DIVISION.
       FILE SECTION.

       FD film-file.
       01 film-rec.
           05 film-id   PIC 9(4).
           05 film-name PIC X(20).
           05 film-year PIC 9(4).
           05 film-desc PIC X(20).

       FD actor-file.
       01 actor-rec.
           05 a-film-id PIC 9(4).
           05 a-name    PIC X(20).

       WORKING-STORAGE SECTION.
       01 choice        PIC X.
       01 delete-id     PIC 9(4).
       01 last-id       PIC 9(4) VALUE 0.

       01 name-in       PIC X(20).
       01 year-in       PIC 9(4).
       01 desc-in       PIC X(20).
       01 actor-name    PIC X(20).
       01 temp-a-film-id PIC 9(4).
       01 temp-a-name   PIC X(20).

       01 fs-film       PIC XX VALUE "00".
       01 fs-actor      PIC XX VALUE "00".

       PROCEDURE DIVISION.

       MAIN.
           PERFORM UNTIL choice = "9"
               DISPLAY "1 Show 2 Add 3 Delete 4 Add Actor 5 Find Actor 9 Exit"
               ACCEPT choice

               EVALUATE choice
                   WHEN "1" PERFORM show-films
                   WHEN "2" PERFORM add-film
                   WHEN "3" PERFORM delete-film
                   WHEN "4" PERFORM add-actor
                   WHEN "5" PERFORM find-by-actor
               END-EVALUATE
           END-PERFORM
           STOP RUN.

       show-films.
           OPEN INPUT film-file
           MOVE "00" TO fs-film
           PERFORM UNTIL fs-film = "10"
               READ film-file NEXT RECORD
                   AT END MOVE "10" TO fs-film
               END-READ
               IF fs-film NOT = "10"
                   DISPLAY film-id " | " film-name " | " film-year " | " film-desc
               END-IF
           END-PERFORM
           CLOSE film-file.

       add-film.
           OPEN I-O film-file
           MOVE 0 TO last-id
           MOVE "00" TO fs-film

           PERFORM UNTIL fs-film = "10"
               READ film-file NEXT RECORD
                   AT END MOVE "10" TO fs-film
               END-READ
               IF fs-film NOT = "10"
                   MOVE film-id TO last-id
               END-IF
           END-PERFORM

           ADD 1 TO last-id

           DISPLAY "Name: " WITH NO ADVANCING
           ACCEPT name-in
           DISPLAY "Year: " WITH NO ADVANCING
           ACCEPT year-in
           DISPLAY "Desc: " WITH NO ADVANCING
           ACCEPT desc-in

           MOVE last-id TO film-id
           MOVE name-in TO film-name
           MOVE year-in TO film-year
           MOVE desc-in TO film-desc

           WRITE film-rec
           CLOSE film-file.

       delete-film.
           DISPLAY "ID: " WITH NO ADVANCING
           ACCEPT delete-id
           OPEN I-O film-file
           DELETE film-rec
               INVALID KEY DISPLAY "Film ID not found."
           CLOSE film-file.

       add-actor.
           DISPLAY "Film ID: " WITH NO ADVANCING
           ACCEPT temp-a-film-id
           DISPLAY "Actor Name: " WITH NO ADVANCING
           ACCEPT temp-a-name

           OPEN I-O actor-file
           MOVE temp-a-film-id TO a-film-id
           MOVE temp-a-name TO a-name
           WRITE actor-rec
           CLOSE actor-file.

       find-by-actor.
           DISPLAY "Actor: " WITH NO ADVANCING
           ACCEPT actor-name
           OPEN INPUT actor-file
           MOVE "00" TO fs-actor
           PERFORM UNTIL fs-actor = "10"
               READ actor-file NEXT RECORD
                   AT END MOVE "10" TO fs-actor
               END-READ
               IF fs-actor NOT = "10"
                   IF a-name = actor-name
                       MOVE a-film-id TO temp-a-film-id
                       PERFORM show-film-by-id
                   END-IF
               END-IF
           END-PERFORM
           CLOSE actor-file.

       show-film-by-id.
           OPEN INPUT film-file
           READ film-file
               KEY IS temp-a-film-id
               INVALID KEY DISPLAY "Film not found."
           END-READ
           DISPLAY film-id " | " film-name " | " film-year " | " film-desc
           CLOSE film-file.