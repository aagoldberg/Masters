/*
  HOL_SQLAggregations.sql
  
  1. What is the average altitude of the three major New York airports?
    remember that you may need to expand the margin in the resultset to see your full result!
*/
SELECT 'Problem 1';
SELECT AVG(alt) FROM airports WHERE faa IN ('EWR','LGA','JFK');

-- or
SELECT AVG(alt) FROM airports WHERE faa = 'EWR' OR faa = 'LGA' OR faa = 'JFK';

-- 2. What is the average altitude for airports grouped by timezone.  Which timezone has the highest altitude?  Why?
SELECT 'Problem 2';
SELECT tz, AVG(alt) 
FROM airports
GROUP BY tz
ORDER BY tz;

-- 3. Which of these four airplanes made the most flights out of New York City airports in 2013?
-- Plane tailnums: 'N125UW','N848MQ','N328AA','N247JB'
SELECT 'Problem 3';
SELECT tailnum, COUNT(*)
  FROM flights WHERE tailnum IN ('N125UW','N848MQ','N328AA','N247JB')
  GROUP BY tailnum
  ORDER BY COUNT(*) DESC;

-- 4. For each of these planes, show the corresponding meta-data (model, manufacturer, engines, etc.) about each plane?  
-- What is surprising about the information returned?  How do you think this could happen?
SELECT 'Problem 4';
SELECT * 
FROM planes
WHERE tailnum IN ('N125UW','N848MQ','N328AA','N247JB');

-- 5. Write a SELECT statement that shows for all of the flights during the period February 14th to February 17th  
-- for each of the four planes above: 'N125UW','N848MQ','N328AA','N247JB'
-- Your select statement should return the following information:  
-- tailnum, flight date information, departure delay, arrival delay, 3 digit destination code
SELECT 'Problem 5';
SELECT tailnum, year, month, day, dep_delay, arr_delay, dest
FROM flights 
WHERE year = 2013 AND month = 2 AND day BETWEEN 14 AND 17 AND
      tailnum IN ('N125UW','N848MQ','N328AA','N247JB')
ORDER BY tailnum;