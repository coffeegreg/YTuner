SELECT DISTINCT stationuuid, name, url, homepage, favicon, votes, bitrate, IFNULL(NULLIF(TRIM(country),''),'{empty}') AS country, IFNULL(NULLIF(TRIM(language),''),'{empty}') AS language, codec, IFNULL(NULLIF(TRIM(group_concat(tag, ',')),''),'{empty}') AS tags
FROM @@@AVR
WHERE stationuuid LIKE @@@ID
GROUP BY stationuuid, name, url, homepage, favicon, votes, bitrate, country, language, codec
LIMIT 1;