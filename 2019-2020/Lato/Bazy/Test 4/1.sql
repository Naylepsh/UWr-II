-- Dodaj do tabeli comments kolumnę lasteditdate typu timestamp
-- z więzem NOT NULL i domyślną wartością now(). 
-- Wypełnij ją obecnymi wartościami creationdate.
-- • Utwórz tabelę commenthistory z kolumnami id SERIAL PRIMARY KEY, commentid
-- integer, creationdate timestamp oraz text text.

ALTER TABLE comments ADD COLUMN lasteditdate timestamp NOT NULL DEFAULT now();
UPDATE comments SET lasteditdate = creationdate;

CREATE TABLE commenthistory(
  id SERIAL PRIMARY KEY, 
  commentid integer, 
  creationdate timestamp,
  text text
);