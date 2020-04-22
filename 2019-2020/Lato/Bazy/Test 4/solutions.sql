-- Zadanie 1
ALTER TABLE comments ADD COLUMN lasteditdate timestamp NOT NULL DEFAULT now();
UPDATE comments SET lasteditdate = creationdate;

CREATE TABLE commenthistory(
  id SERIAL PRIMARY KEY, 
  commentid integer, 
  creationdate timestamp,
  text text
);

-- Zadanie 2
CREATE FUNCTION update_comment() RETURNS TRIGGER AS $$
  DECLARE
    editdate timestamp without time zone := OLD.lasteditdate;
  BEGIN
    IF (OLD.id != NEW.id 
      OR OLD.postid != NEW.postid
      OR OLD.lasteditdate != NEW.lasteditdate)
    THEN 
      RAISE EXCEPTION 'Invalid arguments: id/postid/lasteditdate change not allowed';
    END IF;
    IF (OLD.text != NEW.text)
      THEN 
        editdate := now();
        INSERT INTO commenthistory(commentid,creationdate,text) VALUES (OLD.id, OLD.lasteditdate, OLD.text);
    END IF;
    RETURN (OLD.id, NEW.postid, NEW.score, NEW.text, OLD.creationdate, NEW.userid, NEW.userdisplayname, editdate);
  END
$$ LANGUAGE plpgsql;

CREATE TRIGGER update_trigger BEFORE UPDATE ON comments
FOR EACH ROW EXECUTE PROCEDURE update_comment();

-- Zadanie 3
CREATE FUNCTION lasteditdate_change() RETURNS TRIGGER AS $$
BEGIN
    RETURN (
      NEW.id, 
      NEW.postid, 
      NEW.score, 
      NEW.text, 
      NEW.creationdate, 
      NEW.userid, 
      NEW.userdisplayname, 
      NEW.creationdate);
END
$$ LANGUAGE plpgsql;

DROP TRIGGER lasteditdate_trigger ON comments;
CREATE TRIGGER lasteditdate_trigger BEFORE INSERT ON comments
FOR EACH ROW EXECUTE PROCEDURE lasteditdate_change();