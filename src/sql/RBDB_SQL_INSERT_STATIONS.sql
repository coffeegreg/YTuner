INSERT INTO stations (StationUUID,Name,URL,URL_Resolved,Homepage,Favicon,IDCountry,IDLanguage,Votes,IDCodec,Bitrate,LastCheckOK,LastCheckOKTime,Geo_lat,Geo_long)
SELECT DISTINCT json_extract(value, '$.stationuuid'),
       json_extract(value, '$.name'),
       json_extract(value, '$.url'),
       json_extract(value, '$.url_resolved'),
       json_extract(value, '$.homepage'),
       json_extract(value, '$.favicon'),
       countries.IDCountry,
       languages.IDLanguage,
       json_extract(value, '$.votes'),
       codecs.IDCodec,
       json_extract(value, '$.bitrate'),
       json_extract(value, '$.lastcheckok'),
       json_extract(value, '$.lastcheckoktime'),
       json_extract(value, '$.geo_lat'),
       json_extract(value, '$.geo_long')   
FROM json_each(@@@JSONCONTENTS)
LEFT JOIN countries ON countries.name = json_extract(value, '$.country')
LEFT JOIN languages ON languages.name = json_extract(value, '$.language')
LEFT JOIN codecs ON codecs.name = json_extract(value, '$.codec');
--Next Step--  --!! Do not remove lines like this one !!
CREATE UNIQUE INDEX stations_IDStation_IDX ON stations (IDStation);
CREATE INDEX stations_StationUUID_IDX ON stations (stationuuid);
CREATE INDEX stations_URL_IDX ON stations (url);
CREATE INDEX stations_IDCountry_IDX ON stations (IDCountry);
CREATE INDEX stations_IDLanguage_IDX ON stations (IDLanguage);
CREATE INDEX stations_Votes_IDX ON stations (votes);
CREATE INDEX stations_IDCodec_IDX ON stations (IDCodec);
CREATE INDEX stations_Bitrate_IDX ON stations (bitrate);
CREATE INDEX stations_LastCheckOK_IDX ON stations (lastcheckok);

DROP TABLE IF EXISTS stags;
CREATE TEMP TABLE "stags"(
    [IDStation] INTEGER PRIMARY KEY NOT NULL,
    [Tags] TEXT NULL);
--Next Step--
INSERT INTO stags (IDStation,Tags)
SELECT stations.IDStation, json_extract(value, '$.tags') 
FROM json_each(@@@JSONCONTENTS)
INNER JOIN stations ON stations.stationuuid = json_extract(value, '$.stationuuid');
--Next Step--
CREATE UNIQUE INDEX stags_IDStation_IDX ON stags (IDStation);
CREATE INDEX stags_Tags_IDX ON stags (Tags);

INSERT INTO stationstags (IDStation,IDTag)
SELECT stags.IDStation, Tags.IDTag 
FROM stags, tags
WHERE tags.Name IN (WITH RECURSIVE items(item,str) AS (
    SELECT NULL, stags.Tags||','
    UNION ALL 
    SELECT substr(str,0,instr(str,',')),substr(str,instr(str,',')+1)
	FROM items WHERE str!=''
) SELECT item FROM items WHERE item IS NOT NULL);

DROP INDEX stags_Tags_IDX; 
DROP INDEX stags_IDStation_IDX; 
DROP TABLE stags;

CREATE UNIQUE INDEX stationstags_IDStationTag_IDX ON stationstags (IDStationTag);
CREATE INDEX stationstags_IDStation_IDX ON stationstags (IDStation);
CREATE INDEX stationstags_IDTag_IDX ON stationstags (IDTag);

