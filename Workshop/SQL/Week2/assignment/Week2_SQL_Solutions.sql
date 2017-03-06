/*
	Probelm 1 - Create one table to keep track of the videos. This table should include a unique ID, the title of the
    video, the length in minutes, and the URL. Populate the table with at least three related videos from YouTube or
    other publicly available resources\
*/

DROP TABLE IF EXISTS Videos;
DROP TABLE IF EXISTS Reviews;

CREATE TABLE Videos(
	videos_id int PRIMARY KEY,
    title varchar(100) NOT NULL,
    duration DECIMAL(6,2) NOT NULL,
    url text NOT NULL
);

INSERT INTO Videos (videos_id, title, duration, url) Values (1, 'How to tie a tie - Quick and Easy', 1.83, 'https://www.youtube.com/watch?v=9BMhFmNzw-o');
INSERT INTO Videos (videos_id, title, duration, url) Values (2, 'How to make bubbles with soap ,water and glycerin', 2.03, 'https://www.youtube.com/watch?v=abg15FdIpvM');
INSERT INTO Videos (videos_id, title, duration, url) Values (3, 'How to Tie a Bow Tie | Mens Fashion', 2.45, 'https://www.youtube.com/watch?v=wxKA9be_3Gk');

/*
	Problem 2 - Create a second table that provides at least two user reviews for each of
    at least two of the videos. These should be imaginary reviews that include columns for the user’s name
    (“Asher”, “Cyd”, etc.), the rating (which could be NULL, or a number between 0 and 5), and a short text review
    (“Loved it!”). There should be a column that links back to the ID column in the table of videos.
*/

CREATE TABLE Reviews(
	reviews_id int PRIMARY KEY,
    username varchar(30) NOT NULL,
    rating DECIMAL(4, 2),
    review varchar(100) NOT NULL,
    videos_id int NULL REFERENCES Videos
);

DROP TRIGGER IF EXISTS ratingtrigger;
DELIMITER //

CREATE TRIGGER ratingtrigger 
BEFORE INSERT ON Reviews 
FOR EACH ROW BEGIN
	IF new.rating < 0 OR new.rating >5 THEN
		SET new.rating = NULL;
	END IF;
END//

DELIMITER ;

/*
	Note - I realize that if I set the rating field to DECIMAL (3,2)
    it would align more closely with the expected value which would be
    less than 10.  However, since I have set a trigger for values over 5
    I decided to accept these values and reformat them to NULL.  I suppose
    this would vary based on the client's needs.
*/

INSERT INTO Reviews (reviews_id, username, rating, review, videos_id) Values (1, 'User1', 9, 'Very good', 1);
INSERT INTO Reviews (reviews_id, username, rating, review, videos_id) Values (2, 'User2', 2, 'Meh....', 1);
INSERT INTO Reviews (reviews_id, username, rating, review, videos_id) Values (3, 'User3', 1, 'Booo', 1);
INSERT INTO Reviews (reviews_id, username, rating, review, videos_id) Values (4, 'User4', 4, 'Very informative', 2);
INSERT INTO Reviews (reviews_id, username, rating, review, videos_id) Values (5, 'User5', 1, 'Not good', 2);
INSERT INTO Reviews (reviews_id, username, rating, review, videos_id) Values (6, 'User6', NULL, 'Not sure if I liked it', 2);

/*
	Problem 3 - Write a JOIN statement that shows information 
    from both tables.
    
    The following statement returns the average rating for each video
    in descending order.  I made a LEFT JOIN so that all video titles would
    appear even with no ratings.
*/

SELECT v.title as 'Video Title', AVG(r.rating) AS 'Average Rating' FROM Videos v
LEFT JOIN Reviews r ON v.videos_id = r.videos_id
GROUP BY v.title
ORDER BY AVG(r.rating) DESC;