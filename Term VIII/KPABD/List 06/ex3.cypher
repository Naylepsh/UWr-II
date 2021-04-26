MATCH (p:Person {name: 'Jack Nicholson'})-[:ACTED_IN]->(m) 
RETURN p, m

MATCH (p:Person {name: 'Rob Reiner'})-[:DIRECTED]->(m)<-[:PRODUCED]-(p:Person) 
RETURN p, m

MATCH (p:Person)
WHERE NOT (p)-[:ACTED_IN]-()
RETURN p

MATCH (p:Person)-[:ACTED_IN]->(m:Movie) 
WITH p, count(m) AS movieCount
WHERE movieCount > 3
RETURN p

MATCH (p:Person)-[:ACTED_IN]->(m:Movie) 
WITH p, count(m) AS movieCount
WHERE movieCount > 6
RETURN p