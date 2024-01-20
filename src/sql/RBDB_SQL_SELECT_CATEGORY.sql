SELECT COUNT(category) OVER (PARTITION BY tc) AS totalcount, IFNULL(NULLIF(TRIM(category),''),'{empty}') AS category, stationcount
FROM (SELECT DISTINCT 1 AS tc, @@@Category AS category, COUNT(DISTINCT IDStation) AS stationcount
	FROM @@@AVR
	GROUP BY category)
ORDER BY category
LIMIT @@@Start,@@@Offset;