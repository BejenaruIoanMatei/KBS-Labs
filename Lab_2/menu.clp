; Gestionare studenti in CLIPS

(deftemplate student
   (slot nume)
   (multislot laborator (default (create$)))
   (slot examen (default 0))
   (slot proiect (default 0))
)

(deffacts fapte_initiale
   (meniu)
)

(defrule afiseaza_meniu
   ?a <- (meniu)
   =>
   (retract ?a)
   (printout t crlf "===== MENIU PRINCIPAL =====" crlf)
   (printout t "1. Adauga un student." crlf)
   (printout t "2. Adauga punctaje la laborator pentru un student." crlf)
   (printout t "3. Adauga punctaj la examen pentru un student." crlf)
   (printout t "4. Adauga punctaj la proiect pentru un student." crlf)
   (printout t "5. Afisează studentii promovati si cei nepromovati." crlf)
   (printout t "6. Iesire." crlf)
   (printout t crlf "Dati optiunea: ")
   (assert (optiune (read)))
)

;     Practic se creeaza o variabila 'a' care retine un fapt din baza de cunostinte (adica din fapt_initiale)
; faptul pe care il retine este (meniu), (cauta meniu si il leaga la 'a')
; (retract ?a) elimina faptul pentru a preveni executiile repetate

(defrule adauga_student
   ?a <- (optiune 1)
   =>
   (retract ?a)
   (printout t crlf "=== Adaugare student nou ===" crlf)
   (printout t "Introduceti numele studentului: ")
   (bind ?nume (readline))
   (assert (student (nume ?nume)))
   (printout t "Studentul " ?nume " a fost adaugat cu succes!" crlf crlf)
   (assert (meniu))
)

(defrule adauga_punctaje_laborator
   ?a <- (optiune 2)
   =>
   (retract ?a)
   (printout t crlf "=== Adauagare punctaje laborator ===" crlf)
   (printout t "Introduceti numele studentului: ")
   (bind ?nume (readline))
   
   (if (not (any-factp ((?s student)) (eq ?s:nume ?nume)))
      then
      (printout t "Studentul " ?nume " nu exista!" crlf crlf)
      (assert (meniu))
      else
      (printout t "Introduceti punctajele la laborator (separate prin spatiu): ")
      (bind ?punctaje (explode$ (readline)))
      
      (do-for-all-facts ((?s student)) (eq ?s:nume ?nume)
         (modify ?s (laborator ?punctaje))
      )
      
      (printout t "Punctajele la laborator au fost adaugate cu succes pentru studentul " ?nume "!" crlf crlf)
      (assert (meniu))
   )
)

(defrule adauga_punctaj_examen
   ?a <- (optiune 3)
   =>
   (retract ?a)
   (printout t crlf "=== Adaugare punctaj examen ===" crlf)
   (printout t "Introduceti numele studentului: ")
   (bind ?nume (readline))
   
   (if (not (any-factp ((?s student)) (eq ?s:nume ?nume)))
      then
      (printout t "Studentul " ?nume " nu exista in baza de date!" crlf crlf)
      (assert (meniu))
      else
      (printout t "Introduceti punctajul la examen: ")
      (bind ?punctaj (read))
      
      (do-for-all-facts ((?s student)) (eq ?s:nume ?nume)
         (modify ?s (examen ?punctaj))
      )
      
      (printout t "Punctajul la examen a fost adaugat cu succes pentru studentul " ?nume "!" crlf crlf)
      (assert (meniu))
   )
)

(defrule adauga_punctaj_proiect
   ?a <- (optiune 4)
   =>
   (retract ?a)
   (printout t crlf "=== Adaugare punctaj proiect ===" crlf)
   (printout t "Introduceti numele studentului: ")
   (bind ?nume (readline))
   
   (if (not (any-factp ((?s student)) (eq ?s:nume ?nume)))
      then
      (printout t "Studentul " ?nume " nu exista in baza de date!" crlf crlf)
      (assert (meniu))
      else
      (printout t "Introduceți punctajul la proiect: ")
      (bind ?punctaj (read))
      
      (do-for-all-facts ((?s student)) (eq ?s:nume ?nume)
         (modify ?s (proiect ?punctaj))
      )
      
      (printout t "Punctajul la proiect a fost adăugat cu succes pentru studentul " ?nume "!" crlf crlf)
      (assert (meniu))
   )
)

(deffunction calculeaza-medie-laborator (?punctaje)
   (if (= (length$ ?punctaje) 0)
      then
      (return 0)
      else
      (bind ?suma 0)
      (foreach ?p ?punctaje
         (bind ?suma (+ ?suma ?p))
      )
      (return (/ ?suma (length$ ?punctaje)))
   )
)

; ca promovare:
;   5 lab
;   5 examen
;   5 proiect
; de ex daca am (4.5,5,5) => FALSE
(deffunction este-promovat (?medie-lab ?examen ?proiect)
   (if (and (>= ?medie-lab 5) (>= ?examen 5) (>= ?proiect 5))
      then
      (return TRUE)
      else
      (return FALSE)
   )
)

(defrule afiseaza_studenti_promovati_nepromovati
   ?a <- (optiune 5)
   =>
   (retract ?a)
   (printout t crlf "=== Lista studentilor ===" crlf)
   
   (bind ?exista-studenti FALSE)
   (bind ?promovati 0)
   (bind ?nepromovati 0)
   
   (printout t crlf "STUDENTI PROMOVATI:" crlf)
   (printout t "-----------------" crlf)
   
   (do-for-all-facts ((?s student)) TRUE
      (bind ?exista-studenti TRUE)
      (bind ?medie-lab (calculeaza-medie-laborator ?s:laborator))
      
      (if (este-promovat ?medie-lab ?s:examen ?s:proiect)
         then
         (bind ?promovati (+ ?promovati 1))
         (printout t "Nume: " ?s:nume crlf)
         (printout t "  - Medie laborator: " ?medie-lab crlf)
         (printout t "  - Examen: " ?s:examen crlf)
         (printout t "  - Proiect: " ?s:proiect crlf)
         (printout t crlf)
      )
   )
   
   (if (= ?promovati 0)
      then
      (printout t "Nu exista studenti promovati." crlf)
   )
   
   (printout t crlf "STUDENTI NEPROMOVATI:" crlf)
   (printout t "-------------------" crlf)
   
   (do-for-all-facts ((?s student)) TRUE
      (bind ?medie-lab (calculeaza-medie-laborator ?s:laborator))
      
      (if (not (este-promovat ?medie-lab ?s:examen ?s:proiect))
         then
         (bind ?nepromovati (+ ?nepromovati 1))
         (printout t "Nume: " ?s:nume crlf)
         (printout t "  - Medie laborator: " ?medie-lab crlf)
         (printout t "  - Examen: " ?s:examen crlf)
         (printout t "  - Proiect: " ?s:proiect crlf)
         (printout t crlf)
      )
   )
   
   (if (= ?nepromovati 0)
      then
      (printout t "Nu exista studenti nepromovati." crlf)
   )
   
   (if (not ?exista-studenti)
      then
      (printout t "Nu exista studenti in baza de date!" crlf)
   )
   
   (printout t crlf "Total studenti: " (+ ?promovati ?nepromovati) crlf)
   (printout t "  - Promovati: " ?promovati crlf)
   (printout t "  - Nepromovati: " ?nepromovati crlf crlf)
   
   (assert (meniu))
)

(defrule iesire
   ?a <- (optiune 6)
   =>
   (retract ?a)
   (printout t crlf "La revedere!" crlf)
   (halt)
)

(defrule optiune_invalida
   ?a <- (optiune ?opt)
   (test (or (< ?opt 1) (> ?opt 6)))
   =>
   (retract ?a)
   (printout t crlf "Optiune invalida! Alege o optiune valida (1-6)." crlf crlf)
   (assert (meniu))
)

; De modificat pentru a accepta doar inregistrari cu numar matricol
; Cand sunt doi studenti cu acelasi nume se blocheaza daca incerc alta actiune