-- Napisz wyzwalacz, który zostanie uruchomiony przy każdej próbie
-- wykonania operacji UPDATE na tabeli comments i sprawi, że:
-- • jakiekolwiek zmiany pola creationdate zostaną zignorowane, tzn. po operacji
-- ma pozostać dotychczasowa wartość tego pola,
-- • w przypadku próby zmiany id, postid lub lasteditdate powinien być zgłoszony błąd za pomocą RAISE EXCEPTION,
-- • jeśli operacja zmienia pole text to:
-- – w wyniku tej operacji lasteditdate ma przyjąć wartość now(),
-- – do tabeli commenthistory zostanie dodana krotka z opisem starej wersji: commentid powinna przyjąć wartość 
-- id zmienianego komentarza,creationdate – dotychczasową wartość lasteditdate, a text – dotychczasową wartość text.

CREATE OR REPLACE FUNCTION foo() RETURNS TRIGGER AS
$$
DECLARE
  ad_lasteditdate timestamp without time zone := OLD.lasteditdate;
BEGIN
  IF (OLD.id != NEW.id 
      OR OLD.postid != NEW.postid
      OR OLD.lasteditdate != NEW.lasteditdate)
    THEN RAISE EXCEPTION 'fuck off';
  END IF;
  IF (OLD.text != NEW.text)
    THEN 
    ad_lasteditdate := now();
    INSERT INTO commenthistory(commentid,creationdate,text) VALUES (OLD.id, OLD.lasteditdate, OLD.text);
  END IF;
  RETURN (OLD.id, NEW.postid, NEW.score, NEW.text, OLD.creationdate, NEW.userid, NEW.userdisplayname, ad_lasteditdate);
END
$$ LANGUAGE plpgsql;

DROP TRIGGER bar ON comments;
CREATE TRIGGER bar BEFORE UPDATE ON comments
FOR EACH ROW EXECUTE PROCEDURE foo();

-- UPDATE comments SET text='xs' WHERE id = 267; 