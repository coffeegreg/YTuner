SELECT COUNT(DISTINCT language) AS language, COUNT(DISTINCT country) AS country, COUNT(DISTINCT tag) AS tags, COUNT(DISTINCT IDStation) AS popular
FROM @@@AVR;