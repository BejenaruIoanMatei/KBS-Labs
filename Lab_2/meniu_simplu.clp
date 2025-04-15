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
   (printout t crlf
      "===== MENIU PRINCIPAL =====" crlf
      "1. Adauga student" crlf
      "2. Adauga punctaje laborator" crlf
      "3. Adauga punctaj examen" crlf
      "4. Adauga punctaj proiect" crlf
      "5. Afiseaza promovati/nepromovati" crlf
      "6. Iesire" crlf crlf
      "Dati optiunea: ")
   (assert (optiune (read)))
)

;     Practic se creeaza o variabila simpla 'a' care retine un fapt din baza de cunostinte (adica din fapt_initiale)
; faptul pe care il retine este (meniu), (cauta meniu si il leaga la 'a')
; (retract ?a) elimina faptul pentru a preveni executiile repetate

(defrule adauga_student
   ?a <- (optiune 1)
   =>
   (retract ?a)
   (printout t "Introduceti numele studentului: ")
   (bind ?nume (readline))
   (assert (student (nume ?nume)))
   (printout t "Student adaugat cu succes!" crlf crlf)
   (assert (meniu))
)

; ?x <- ceva = ia un obiect deja existent si ii da bind
; (bind ?x ceva) = creeaza o val noua si o pune in x
; ambele sunt bind uri dar functioneaza diferit cumva

(defrule citire_laborator
  ?a <- (optiune 2)
  ?c <- (student (nume ?nume))
  =>
  (retract ?a)
  (printout t "Dati punctaje pentru " ?nume ": ")
  (modify ?c (laborator (explode$ (readline))))
  (assert (meniu))
)

(defrule citire_examen
  ?a <- (optiune 3)
  ?c <- (student (nume ?nume))
  =>
  (retract ?a)
  (printout t "Dati punctaj pentru " ?nume ": ")
  (modify ?c (examen (read)))
  (assert (meniu))
)

(defrule citire_proiect
  ?a <- (optiune 4)
  ?c <- (student (nume ?nume))
  =>
  (retract ?a)
  (printout t "Dati punctaj pentru " ?nume ": ")
  (modify ?c (proiect (read)))
  (assert (meniu))
)


(deffunction calculeaza-medie-laborator (?note)
   (bind ?suma 0)
   (loop-for-count (?i 1 (length$ ?note))
      (bind ?suma (+ ?suma (nth$ ?i ?note)))
   )
   (if (= (length$ ?note) 0)
      then 0
      else (/ ?suma (length$ ?note))
   )
)

; nth$ ?i ?note = ia elementul cu indexul i din lista (incepand de la 1)

(deffunction este-promovat (?ml ?ex ?pr)
   (and (>= ?ml 5) (>= ?ex 5) (>= ?pr 5))
)

; pentru promovare minim
;   5 ml
;   5 ex
;   5 pr

(defrule afiseaza_studenti
   ?a <- (optiune 5)
   =>
   (retract ?a)
   (printout t crlf "--- LISTA STUDENTI ---" crlf)
   (bind ?promovati 0)
   (bind ?nepromovati 0)

   (do-for-all-facts ((?s student)) TRUE
      (bind ?ml (calculeaza-medie-laborator ?s:laborator))
      (if (este-promovat ?ml ?s:examen ?s:proiect)
         then
         (printout t "[PROMOVAT] " ?s:nume crlf)
         (bind ?promovati (+ ?promovati 1))
         else
         (printout t "[NEPROMOVAT] " ?s:nume crlf)
         (bind ?nepromovati (+ ?nepromovati 1))
      )
   )

   (printout t crlf "Total: " (+ ?promovati ?nepromovati) ", Promovati: " ?promovati ", Nepromovati: " ?nepromovati crlf crlf)
   (assert (meniu))
)

; practic trec prin toate faptele si am TRUE pentru ca nu filtram nimic (vrem toti studentii)

(defrule iesire
   ?a <- (optiune 6)
   =>
   (retract ?a)
   (printout t crlf "La revedere!" crlf)
   (halt)
)

(defrule optiune_invalida
   ?a <- (optiune ?x)
   (test (or (< ?x 1) (> ?x 6)))
   =>
   (retract ?a)
   (printout t "Optiune invalida! Alege intre 1 si 6." crlf)
   (assert (meniu))
)
