/*
  HOL_SQLJoins.sql
  
  All of the queries use tables in the flights database.  


1. Write a SELECT statement that shows for all of the flights during the period February 14th to February 17th  
   for each of the four planes above: 'N125UW','N848MQ','N328AA','N247JB'
   Your select statement should return the following information:  
   tailnum, flight date information, departure delay, arrival delay, 3 digit destination code
*/

-- 1. Write a SELECT statement that shows for all of the flights during the period February 14th to February 17th  
-- for each of the four planes above: 'N125UW','N848MQ','N328AA','N247JB'
-- Your select statement should return the following information:  
-- tailnum, flight date information, departure delay, arrival delay, 3 digit destination code
SELECT tailnum, year, month, day, dep_delay, arr_delay, dest
FROM flights 
WHERE year = 2013 AND month = 2 AND day BETWEEN 14 AND 17 AND
      tailnum IN ('N125UW','N848MQ','N328AA','N247JB')
ORDER BY tailnum;

/*
2. Add a join clause, to also show destination airport name.  
   How many rows were returned?
*/

SELECT tailnum, year, month, day, dep_delay, arr_delay, dest, airports.name
FROM flights JOIN airports
ON flights.dest = airports.faa
WHERE flights.year = 2013 AND month = 2 AND day BETWEEN 14 AND 17 AND
      flights.tailnum IN ('N125UW','N848MQ','N328AA','N247JB')
ORDER BY flights.tailnum;

-- 3. now use a LEFT JOIN instead.  How many rows were returned?

SELECT tailnum, year, month, day, dep_delay, arr_delay, dest, airports.name
FROM flights LEFT JOIN airports
ON flights.dest = airports.faa
WHERE flights.year = 2013 AND month = 2 AND day BETWEEN 14 AND 17 AND
      flights.tailnum IN ('N125UW','N848MQ','N328AA','N247JB')
ORDER BY flights.tailnum;


-- 4. Add a second left join clause to also show the number of seats in the planes
SELECT flights.tailnum, flights.year, month, day, dep_delay, arr_delay, dest, airports.name, planes.seats
FROM flights
LEFT JOIN airports ON flights.dest = airports.faa 
LEFT JOIN planes ON flights.tailnum = planes.tailnum
WHERE flights.year = 2013 AND month = 2 AND day BETWEEN 14 AND 17 AND
      flights.tailnum IN ('N125UW','N848MQ','N328AA','N247JB')
ORDER BY flights.tailnum;


-- 5. rewrite your query using table aliases
SELECT f.tailnum, f.year, month, day, dep_delay, arr_delay, dest, a.name, p.seats
FROM flights f
LEFT JOIN airports a ON f.dest = a.faa 
LEFT JOIN planes p ON f.tailnum = p.tailnum
WHERE f.year = 2013 AND month = 2 AND day BETWEEN 14 AND 17 AND
      f.tailnum IN ('N125UW','N848MQ','N328AA','N247JB')
ORDER BY f.tailnum;


-- 6. What are the names of the five airports that receive the most flights?  

SELECT airports.name, COUNT(*) FROM flights
INNER JOIN airports
ON flights.dest = airports.faa
GROUP BY airports.name
ORDER BY COUNT(*) DESC LIMIT 5;

/*
   7. What are American Airlines’ destination cities from the New York airports?
   
   This query took about a minute to execute on my Windows laptop.  Consider:  how would you make it run faster?
*/

SELECT DISTINCT airports.name AS airport
 FROM flights JOIN airports
   ON flights.dest = airports.faa
   JOIN airlines
   ON flights.carrier = airlines.carrier
   WHERE airlines.name LIKE 'Am%'
   ORDER BY airport;  









   



