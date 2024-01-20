SELECT COUNT(tc) OVER (PARTITION BY tc) AS totalcount, stationuuid, name, url, homepage, favicon, votes, bitrate, IFNULL(NULLIF(TRIM(country),''),'{empty}') AS country, IFNULL(NULLIF(TRIM(language),''),'{empty}') AS language, codec, IFNULL(NULLIF(TRIM(tags),''),'{empty}') AS tags
FROM (SELECT DISTINCT 1 AS tc, stationuuid, name, url, homepage, favicon, votes, bitrate, country, language, codec, group_concat(tag, ',') as tags
	FROM @@@AVR
	WHERE @@@Category
	GROUP BY tc, stationuuid, name, url, homepage, favicon, votes, bitrate, country, language, codec)
ORDER BY @@@Order @@@DirOrder
LIMIT @@@Start,@@@Offset;