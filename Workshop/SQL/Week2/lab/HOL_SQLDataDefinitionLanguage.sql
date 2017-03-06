/*
 HOL_SQLDataDefinitionLanguage.sql
 */
 
DROP TABLE IF EXISTS tblGroceryList;
DROP TABLE IF EXISTS tblRecipes;
DROP TABLE IF EXISTS tblIngredients;
DROP TABLE IF EXISTS tblCuisine;

CREATE TABLE tblCuisine
(
  cuisine_id int PRIMARY KEY,
  geography varchar(30) NOT NULL
);

INSERT INTO tblCuisine ( cuisine_id, geography ) VALUES ( 1, 'Korean');
INSERT INTO tblCuisine ( cuisine_id, geography ) VALUES ( 2, 'French');
INSERT INTO tblCuisine ( cuisine_id, geography ) VALUES ( 3, 'Italian');
INSERT INTO tblCuisine ( cuisine_id, geography ) VALUES ( 5, 'Morocan');

SELECT * FROM tblCuisine;

CREATE TABLE tblIngredients
(
  ingredient_id int PRIMARY KEY,
  ingredient varchar(30) NOT NULL
);

INSERT INTO tblIngredients ( ingredient_id, ingredient) VALUES ( 1, 'Beef');
INSERT INTO tblIngredients ( ingredient_id, ingredient) VALUES ( 2, 'Tomato Sauce');
INSERT INTO tblIngredients ( ingredient_id, ingredient) VALUES ( 3, 'Snails');
INSERT INTO tblIngredients ( ingredient_id, ingredient) VALUES ( 4, 'Cabbage');
INSERT INTO tblIngredients ( ingredient_id, ingredient) VALUES ( 5, 'Penne');
INSERT INTO tblIngredients ( ingredient_id, ingredient) VALUES ( 6, 'Onions');
INSERT INTO tblIngredients ( ingredient_id, ingredient) VALUES ( 7, 'White beans');
INSERT INTO tblIngredients ( ingredient_id, ingredient) VALUES ( 8, 'Duck');
INSERT INTO tblIngredients ( ingredient_id, ingredient) VALUES ( 9, 'Tomato');
INSERT INTO tblIngredients ( ingredient_id, ingredient) VALUES ( 10, 'Flour');


SELECT * FROM tblIngredients;


CREATE TABLE tblRecipes
(
  recipe_id int PRIMARY KEY,
  recipe varchar(30) NOT NULL,
  cuisine_id int NULL REFERENCES tblCuisine
 );

 INSERT INTO tblRecipes(recipe_id, recipe, cuisine_id) VALUES (1, 'Penne a la vodka', 3);
 INSERT INTO tblRecipes(recipe_id, recipe, cuisine_id) VALUES (2, 'Pizza', NULL);
 INSERT INTO tblRecipes(recipe_id, recipe, cuisine_id) VALUES (3, 'Cassoulet', 2);
 INSERT INTO tblRecipes(recipe_id, recipe, cuisine_id) VALUES (4, 'Bulgogi', 1);
 INSERT INTO tblRecipes(recipe_id, recipe, cuisine_id) VALUES (5, 'Kimchi', 1);
 INSERT INTO tblRecipes(recipe_id, recipe, cuisine_id) VALUES (7, 'Escargot', 2);
     
 SELECT * FROM tblRecipes;

 SELECT geography, recipe
  FROM tblCuisine c INNER JOIN tblRecipes r
  ON c.cuisine_id = r.cuisine_id
ORDER BY geography, recipe;


 CREATE TABLE tblGroceryList
 (
   recipe_id int NOT NULL REFERENCES tblRecipes,
   ingredient_id int NOT NULL REFERENCES tblIngredients,
   quantity int NOT NULL,
   PRIMARY KEY ( recipe_id, ingredient_id)
 );

INSERT INTO tblGroceryList(recipe_id, ingredient_id, quantity) VALUES (4, 6, 2);
INSERT INTO tblGroceryList(recipe_id, ingredient_id, quantity) VALUES (4, 1, 1);
INSERT INTO tblGroceryList(recipe_id, ingredient_id, quantity) VALUES (5, 6, 1);
INSERT INTO tblGroceryList(recipe_id, ingredient_id, quantity) VALUES (5, 4, 3);
INSERT INTO tblGroceryList(recipe_id, ingredient_id, quantity) VALUES (3, 6, 2);
INSERT INTO tblGroceryList(recipe_id, ingredient_id, quantity) VALUES (3, 7, 100);
INSERT INTO tblGroceryList(recipe_id, ingredient_id, quantity) VALUES (3, 8, 2);
INSERT INTO tblGroceryList(recipe_id, ingredient_id, quantity) VALUES (2, 9, 7);
INSERT INTO tblGroceryList(recipe_id, ingredient_id, quantity) VALUES (2, 10, 1);

SELECT * FROM tblGroceryList;

SELECT recipe, ingredient, quantity
  FROM tblRecipes r 
    INNER JOIN tblGroceryList gl ON r.recipe_id = gl.recipe_id
    INNER JOIN tblIngredients i ON gl.ingredient_id = i.ingredient_id
  ORDER BY recipe, ingredient, quantity