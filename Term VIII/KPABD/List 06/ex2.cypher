CREATE (p:Person {name: 'John Doe'}) RETURN p
CREATE (p:Person {name: 'Jane Doe'}) RETURN p

CREATE (m:Movie { title: 'Movie1'}) RETURN m
CREATE (m:Movie { title: 'Movie2'}) RETURN m

MATCH (m:Movie { title: 'Movie1'}) 
SET m.rating = 6,  m.summary = 'Lorem Ipsum'
RETURN m

MATCH
	(p:Person { name: 'John Doe'}),
  (m:Movie { title: 'Movie1'})
CREATE (p)-[r:ACTED_IN]->(m)
RETURN type(r)

MATCH
	(p:Person { name: 'John Doe'}),
  (m:Movie { title: 'Movie2'})
CREATE (p)-[r:ACTED_IN]->(m)
RETURN type(r)
// cmd to check whether relation creation succeeded 
MATCH p=({name: 'John Doe'})-[r:ACTED_IN]->() RETURN p LIMIT 25

MATCH (m:Movie { title: 'Movie1'}) 
SET m.rating = 7
RETURN m

MATCH (p:Person {name: 'John Doe'})-[r:ACTED_IN]->(m:Movie {title: 'Movie2'}) 
DELETE r