(deffacts studenti-din-fisier
   (student (nume "Ion") (laborator 10 9 8) (examen 7) (proiect 9))
   (student (nume "Ana") (laborator 6 7 8) (examen 5) (proiect 10))
   (student (nume "Marin") (laborator 2 3 3) (examen 3) (proiect 4))
   (student (nume "Vlad") (laborator 8 8 8) (examen 3) (proiect 10))
)

; (reset) -> incarca faptele din din baza de cunostinte
; (run) -> executa motorul de reguli, fara run regulile nu se evalueaza si nu se aplica