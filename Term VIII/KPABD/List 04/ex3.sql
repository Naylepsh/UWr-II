SET STATISTICS TIME ON

-- TESTS --
SELECT DISTINCT c.PESEL, c.Nazwisko FROM Egzemplarz e
JOIN Ksiazka k		ON e.Ksiazka_ID    = k.Ksiazka_ID
JOIN Wypozyczenie w ON e.Egzemplarz_ID = w.Egzemplarz_ID
JOIN Czytelnik c	ON c.Czytelnik_ID  = w.Czytelnik_ID
-- 7-9ms

SELECT c.PESEL, c.Nazwisko
FROM Czytelnik c WHERE c.Czytelnik_ID IN
(SELECT w.Czytelnik_ID FROM Wypozyczenie w
	JOIN Egzemplarz e ON e.Egzemplarz_ID = w.Egzemplarz_ID
	JOIN Ksiazka k	  ON e.Ksiazka_ID    = k.Ksiazka_ID)
-- 5-6ms

SELECT c.PESEL, c.Nazwisko
FROM Czytelnik c WHERE c.Czytelnik_ID IN
	(SELECT w.Czytelnik_ID FROM Wypozyczenie w
	WHERE w.Egzemplarz_ID IN 
		(SELECT Egzemplarz_ID FROM Egzemplarz e
		JOIN Ksiazka k ON e.Ksiazka_ID = k.Ksiazka_ID ))
-- 5-6ms

SET STATISTICS TIME OFF