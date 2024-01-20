DROP VIEW IF EXISTS @@@AVR;
CREATE VIEW IF NOT EXISTS @@@AVR AS
SELECT DISTINCT stations.IDStation, stations.stationuuid, stations.name, stations.url, stations.homepage, stations.favicon, stations.votes, stations.bitrate, stations.lastcheckok, countries.Name AS country, languages.Name AS language, codecs.Name as codec, tags.Name AS tag
FROM stations 
    LEFT JOIN countries ON stations.IDCountry = countries.IDCountry 
    LEFT JOIN languages ON stations.IDLanguage = languages.IDLanguage 
    LEFT JOIN codecs ON stations.IDCodec = codecs.IDCodec 
    LEFT JOIN (stationstags INNER JOIN tags ON stationstags.IDTag = tags.IDTag) ON stations.IDStation = stationstags.IDStation 
--Begin AllowedCountries
,
(WITH RECURSIVE items(item,str) AS (
    SELECT NULL,'@@@AllowedCountries'||';'
    UNION ALL 
    SELECT substr(str,0,instr(str,';')),substr(str,instr(str,';')+1)
    FROM items WHERE str!=''
) SELECT item FROM items WHERE item IS NOT NULL) AllowedInCountryList
--End AllowedCountries
--Begin AllowedLanguages
,
(WITH RECURSIVE items(item,str) AS (
    SELECT NULL,'@@@AllowedLanguages'||';'
    UNION ALL 
    SELECT substr(str,0,instr(str,';')),substr(str,instr(str,';')+1)
    FROM items WHERE str!=''
) SELECT item FROM items WHERE item IS NOT NULL) AllowedInLanguageList
--End AllowedLanguages
--Begin AllowedCodecs
,
(WITH RECURSIVE items(item,str) AS (
    SELECT NULL,'@@@AllowedCodecs'||';'
    UNION ALL 
    SELECT substr(str,0,instr(str,';')),substr(str,instr(str,';')+1)
    FROM items WHERE str!=''
) SELECT item FROM items WHERE item IS NOT NULL) AllowedInCodecList
--End AllowedCodecs
--Begin NotAllowedCodecs
,
(WITH RECURSIVE items(item,str) AS (
    SELECT NULL,'@@@NotAllowedCodecs'||';'
    UNION ALL 
    SELECT substr(str,0,instr(str,';')),substr(str,instr(str,';')+1)
    FROM items WHERE str!=''
) SELECT item FROM items WHERE item IS NOT NULL) NotAllowedInCodecList
--End NotAllowedCodecs
--Begin AllowedTags
,
(WITH RECURSIVE items(item,str) AS (
    SELECT NULL,'@@@AllowedTags'||';'
    UNION ALL 
    SELECT substr(str,0,instr(str,';')),substr(str,instr(str,';')+1)
    FROM items WHERE str!=''
) SELECT item FROM items WHERE item IS NOT NULL) AllowedInTagList
--End AllowedTags
--Begin NotAllowedTags
,
(WITH RECURSIVE items(item,str) AS (
    SELECT NULL,'@@@NotAllowedTags'||';'
    UNION ALL 
    SELECT substr(str,0,instr(str,';')),substr(str,instr(str,';')+1)
    FROM items WHERE str!=''
) SELECT item FROM items WHERE item IS NOT NULL) NotAllowedInTagList
--End NotAllowedTags
--Begin NotAllowedInName
,
(WITH RECURSIVE items(item,str) AS (
    SELECT NULL,'@@@NotAllowedInName'||';'
    UNION ALL 
    SELECT substr(str,0,instr(str,';')),substr(str,instr(str,';')+1)
    FROM items WHERE str!=''
) SELECT item FROM items WHERE item IS NOT NULL) NotAllowedInNameList
--End NotAllowedInName
--Begin NotAllowedInURL
,
(WITH RECURSIVE items(item,str) AS (
    SELECT NULL,'@@@NotAllowedInURL'||';'
    UNION ALL 
    SELECT substr(str,0,instr(str,';')),substr(str,instr(str,';')+1)
    FROM items WHERE str!=''
) SELECT item FROM items WHERE item IS NOT NULL) NotAllowedInURLlist
--End NotAllowedInURL
WHERE 1=1
--Begin AllowedCountries
    AND IFNULL(countries.Name,'{empty}') LIKE AllowedInCountryList.item
--End AllowedCountries
--Begin AllowedLanguages
    AND IFNULL(languages.Name,'{empty}') LIKE AllowedInLanguageList.item
--End AllowedLanguages
--Begin AllowedCodecs
    AND IFNULL(codecs.Name,'{empty}') LIKE AllowedInCodecList.item
--End AllowedCodecs
--Begin NotAllowedCodecs
    AND IFNULL(codecs.Name,'{empty}') NOT LIKE NotAllowedInCodecList.item
--End NotAllowedCodecs
--Begin AllowedTags
    AND IFNULL(tags.Name ,'{empty}') LIKE AllowedInTagList.item
--End AllowedTags
--Begin NotAllowedTags
    AND IFNULL(tags.Name ,'{empty}') NOT LIKE NotAllowedInTagList.item
--End NotAllowedTags
--Begin NotAllowedInName
    AND (stations.name NOT LIKE NotAllowedInNameList.item)
--End NotAllowedInName
--Begin NotAllowedInURL
    AND (stations.url NOT LIKE NotAllowedInURLlist.item)
--End NotAllowedInURL
--Begin BitrateMax
    AND IFNULL(stations.bitrate,0)<=@@@BitrateMax
--End BitrateMax
ORDER BY stations.IDStation;