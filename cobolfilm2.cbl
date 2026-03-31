       IDENTIFICATION DIVISION.
       PROGRAM-ID. FilmDB.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT film-datei ASSIGN TO "filme.dat"
           ORGANIZATION IS SEQUENTIAL
           FILE STATUS IS file-status.

           SELECT temp-datei ASSIGN TO "temp.dat"
           ORGANIZATION IS SEQUENTIAL
           FILE STATUS IS temp-status.

       DATA DIVISION.
       FILE SECTION.

       FD film-datei.
       01 datei-film.
           05 film-id     PIC 9(4).
           05 film-name   PIC X(20).
           05 film-jahr   PIC 9(4).
           05 film-desc   PIC X(20).

       FD temp-datei.
       01 temp-film.
           05 t-id     PIC 9(4).
           05 t-name   PIC X(20).
           05 t-jahr   PIC 9(4).
           05 t-desc   PIC X(20).

       WORKING-STORAGE SECTION.

       01 ws-name   PIC X(20).
       01 ws-desc   PIC X(20).

       01 ws-jahr-input PIC X(4).
       01 ws-jahr PIC 9(4).

       01 file-status PIC XX.
           88 ok VALUE "00".
           88 eof VALUE "10".

       01 temp-status PIC XX.

       01 menu-choice PIC X.
           88 m-show VALUE "1".
           88 m-add  VALUE "2".
           88 m-del  VALUE "3".
           88 m-exit VALUE "9".

       01 last-id PIC 9(4) VALUE ZERO.
       01 delete-id PIC 9(4).

       PROCEDURE DIVISION.

       MAIN.
           DISPLAY "=== Film DB ==="

           PERFORM UNTIL m-exit
               PERFORM menu
               PERFORM handle
           END-PERFORM

           STOP RUN.

       menu.
           DISPLAY " "
           DISPLAY "1 - Anzeigen"
           DISPLAY "2 - Hinzufuegen"
           DISPLAY "3 - Loeschen"
           DISPLAY "9 - Ende"
           ACCEPT menu-choice.

       handle.
           EVALUATE menu-choice
               WHEN "1" PERFORM show-films
               WHEN "2" PERFORM add-film
               WHEN "3" PERFORM delete-film
               WHEN "9" SET m-exit TO TRUE
               WHEN OTHER DISPLAY "Falsche Eingabe"
           END-EVALUATE.

       show-films.
           OPEN INPUT film-datei
           IF ok
               DISPLAY "--- Liste ---"
               PERFORM UNTIL NOT ok
                   READ film-datei
                   IF ok
                       DISPLAY film-id " | "
                               film-name(1:15) " | "
                               film-jahr " | "
                               film-desc(1:15)
                   END-IF
               END-PERFORM
               CLOSE film-datei
           ELSE
               DISPLAY "Datei leer oder nicht gefunden"
           END-IF.

           add-film.
           PERFORM get-id

           DISPLAY "Name: " WITH NO ADVANCING
           ACCEPT ws-name

           *> --- БЕЗПЕЧНИЙ ВВІД РОКУ ---
           MOVE ZERO TO ws-jahr

           PERFORM UNTIL ws-jahr > 0
               DISPLAY "Jahr (nur Zahlen): " WITH NO ADVANCING
               ACCEPT ws-jahr-input

               IF ws-jahr-input NUMERIC
                   MOVE ws-jahr-input TO ws-jahr
               ELSE
                   DISPLAY "Fehler! Nur Zahlen eingeben!"
               END-IF
           END-PERFORM
           *> --- КІНЕЦЬ БЛОКУ ---

           DISPLAY "Beschreibung: " WITH NO ADVANCING
           ACCEPT ws-desc

           OPEN EXTEND film-datei
           IF ok
               MOVE last-id TO film-id
               MOVE ws-name TO film-name
               MOVE ws-jahr TO film-jahr
               MOVE ws-desc TO film-desc

               WRITE datei-film
               DISPLAY "Gespeichert!"
               CLOSE film-datei
           ELSE
               DISPLAY "Fehler beim Oeffnen!"
           END-IF.

              delete-film.

           DISPLAY "ID zum Loeschen: " WITH NO ADVANCING
           ACCEPT delete-id

           OPEN INPUT film-datei
           OPEN OUTPUT temp-datei

           IF ok
               PERFORM UNTIL NOT ok
                   READ film-datei
                   IF ok
                       IF film-id NOT = delete-id
                           MOVE film-id   TO t-id
                           MOVE film-name TO t-name
                           MOVE film-jahr TO t-jahr
                           MOVE film-desc TO t-desc
                           WRITE temp-film
                       END-IF
                   END-IF
               END-PERFORM

               CLOSE film-datei
               CLOSE temp-datei

               *> 🔥 ВАЖЛИВА ЧАСТИНА
               CALL "SYSTEM" USING "del filme.dat"
               CALL "SYSTEM" USING "rename temp.dat filme.dat"

               DISPLAY "Film geloescht!"
           ELSE
               DISPLAY "Fehler!"
           END-IF.

       get-id.
           MOVE ZERO TO last-id

           OPEN INPUT film-datei
           IF ok
               PERFORM UNTIL NOT ok
                   READ film-datei
                   IF ok
                       MOVE film-id TO last-id
                   END-IF
               END-PERFORM
               CLOSE film-datei
           END-IF

           IF last-id = 0
               MOVE 1 TO last-id
           ELSE
               ADD 1 TO last-id
           END-IF.