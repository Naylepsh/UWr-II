-- Aplikacja obsługująca forum dodaje komentarze za pomocą polecenia
-- INSERT INTO
-- comments(id, postid, score, text, creationdate, userid, userdisplayname)
-- VALUES (...)
-- nie biorąc pod uwagę powyższych zmian, tj. nie ustawia wartości lasteditdate.
-- W efekcie pole to przyjmuje obecnie ustawioną wartość domyślną – now(). 
-- Okazało się, że czasem powoduje to różnicę pomiędzy wartościami lasteditdate oraz
-- creationdate (ustalaną po stronie aplikacji), która jest kłopotliwa (podobno data
-- scientist się zdenerwował).
-- W jaki sposób sprawić, aby wartość lasteditdate dla każdego nowododanego
-- komentarza początkowo była równa creationdate? Oczywiście nie masz dostępu
-- do kodu aplikacji i nie możesz go zmieniać. Napisz odpowiedni kod SQL.

CREATE OR REPLACE FUNCTION baz() RETURNS TRIGGER AS $$
BEGIN
    RETURN (NEW.id, NEW.postid, NEW.score, NEW.text, NEW.creationdate, NEW.userid, NEW.userdisplayname, NEW.creationdate);
END
$$ LANGUAGE plpgsql;

DROP TRIGGER qux ON comments;
CREATE TRIGGER qux BEFORE INSERT ON comments
FOR EACH ROW EXECUTE PROCEDURE baz();

INSERT INTO comments(id, postid, score, text, creationdate, userid, userdisplayname) VALUES (1000042,1,1,'1','2014-11-02 02:05:10.12',1,'1');