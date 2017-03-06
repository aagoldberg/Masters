/*
	1. Which destination in the flights database is the furthest distance away, based on information in the flights table.  
    Show the SQL query(s) that support your conclusion.
*/
SELECT 'Problem 1';
SELECT 'The longest distance appears to be from JFK to HNK.  Results have been limited to the top 10';
SELECT origin, dest, MAX(distance) FROM flights GROUP BY dest, origin ORDER BY MAX(distance) DESC LIMIT 10;
/*
	2. What are the different numbers of engines in the planes table? For each number of engines, which aircraft have
    the most number of seats? Show the SQL statement(s) that support your result.
*/
SELECT 'Problem 2';
SELECT 'There are 1, 2, 3, and 4 engines with a maximum number of 16, 400, 379, and 450 seats respectively';
SELECT engines, MAX(seats) FROM planes GROUP BY engines;
/*
	3. Show the total number of flights.
*/
SELECT 'Problem 3';
SELECT Count(*) FROM flights;
/*
	4. Show the total number of flights by airline (carrier).
*/
SELECT 'Problem 4';
SELECT Count(*), carrier FROM flights GROUP BY carrier;
/*
	5. Show all of the airlines, ordered by number of flights in descending order.
*/
SELECT 'Problem 5';
SELECT Count(*), carrier FROM flights GROUP BY carrier ORDER BY Count(*) DESC;
/*
	6. Show only the top 5 airlines, by number of flights, ordered by number of flights in descending order.
*/
SELECT 'Problem 6';
SELECT Count(*), carrier FROM flights GROUP BY carrier ORDER BY Count(*) DESC LIMIT 5;
/*
	7. Show only the top 5 airlines, by number of flights of distance 1,000 miles or greater, 
    ordered by number of flights in descending order.
*/
SELECT 'Problem 7';
SELECT Count(*), carrier FROM flights WHERE distance >= 1000 GROUP BY carrier ORDER BY Count(*) DESC LIMIT 5;
/*
	8. Create a question that (a) uses data from the flights database, and (b) requires aggregation to answer it, and
    write down both the question, and the query that answers the question.
*/
SELECT 'Problem 8';
SELECT 'What are the top 5 destinations with the largest average traveled distance?';
SELECT AVG(distance), dest FROM flights GROUP BY dest ORDER BY AVG(distance) DESC LIMIT 5;