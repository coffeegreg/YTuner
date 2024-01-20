-- I don't like all those stations that try to promote and position themselves by adding unnecessary characters to their names.
-- Let's also sort out the names of countries, languages, codecs and tags and remove the unused ones.
-- Basically, Let's clean up this garbage!

PRAGMA temp_store = 2;
DROP TABLE IF EXISTS _TextVariables;
CREATE TEMP TABLE _TextVariables(IDVariable INTEGER PRIMARY KEY, TextValue TEXT);
INSERT INTO _TextVariables (IDVariable,TextValue)
VALUES (1,' ;`;~;!;@;#;$;%;^;&;*;(;);-;_;+;=;";:;?;/;\;?;{;};[;];<;>;|;,;.;'||char(9)||';'||char(10)||';'||char(13)||';'), --Characters to remove from begin and end of station name
       (2,'\t;\n;\r;\b;\f;'||char(9)||';'||char(10)||';'||char(13)||';'||char(39)||';'), --Characters to remove from station name
       (3,'\t;\n;\r;\b;\f;`;~;!;@;#;$;%;^;&;*;(;);-;_;+;=;";:;?;\;?;{;};[;];<;>;|;,;.;'||char(9)||';'||char(10)||';'||char(13)||';'||char(39)||';'), --Characters to remove from country and language names
       (4,'\t;\n;\r;\b;\f;`;~;!;@;#;$;%;^;&;*;(;);_;=;";:;?;\;?;{;};[;];<;>;|;,;.;'||char(9)||';'||char(10)||';'||char(13)||';'||char(39)||';'); --Characters to remove from codec names

UPDATE stations 
SET name = 
(WITH RECURSIVE items(item,str,oper) AS (
    SELECT NULL,REPLACE(hex(zeroblob(LENGTH(_V.TextValue)/2)),'00',_V.TextValue),stations.name
    FROM _TextVariables _V 
    WHERE _V.IDVariable=1
    UNION ALL 
    SELECT substr(str,0,instr(str,';')),substr(str,instr(str,';')+1),LTRIM(oper,substr(str,0,instr(str,';')))
	FROM items
	WHERE str!=''
) SELECT oper FROM items WHERE item IS NOT NULL AND str='' LIMIT 1)
WHERE stations.name NOT GLOB '[a-zA-Z0-9]*';

UPDATE stations 
SET name = 
(WITH RECURSIVE items(item,str,oper) AS (
    SELECT NULL,_V.TextValue,stations.name
    FROM _TextVariables _V 
    WHERE _V.IDVariable=2
    UNION ALL 
    SELECT substr(str,0,instr(str,';')),substr(str,instr(str,';')+1),REPLACE(oper,substr(str,0,instr(str,';')),'')
	FROM items 
	WHERE str!=''
) SELECT oper FROM items WHERE item IS NOT NULL AND str='' LIMIT 1);

DELETE		
FROM stationstags
WHERE IDStation IN (SELECT IDStation
					FROM stations
					WHERE TRIM(name)='' OR TRIM(url)='' OR url NOT LIKE 'http%');

DELETE			
FROM stations
WHERE TRIM(name)='' OR TRIM(url)='' OR url NOT LIKE 'http%';

-- Countries cleanup section --

UPDATE countries 
SET name = 
(WITH RECURSIVE items(item,str,oper) AS (
    SELECT NULL,_V.TextValue,countries.name
    FROM _TextVariables _V 
    WHERE _V.IDVariable=3
    UNION ALL 
    SELECT substr(str,0,instr(str,';')),substr(str,instr(str,';')+1),REPLACE(oper,substr(str,0,instr(str,';')),'')
	FROM items 
	WHERE str!=''
) SELECT oper FROM items WHERE item IS NOT NULL AND str='' LIMIT 1);

DELETE 
FROM countries 
WHERE IDCountry IN (SELECT c.IDCountry 			
					FROM countries c LEFT JOIN stations s on c.IDCountry = s.IDCountry 
					WHERE s.IDStation IS NULL)
	OR TRIM(name)=''
	OR name LIKE 'http%'
	OR name GLOB '[0-9]*';

UPDATE stations 
SET IDCountry = NULL 
WHERE IDCountry NOT IN (SELECT c.IDCountry FROM countries c);

UPDATE stations 
SET IDCountry = (SELECT MIN(c1.IDCountry)
				FROM countries c1
				WHERE c1.Name = c.Name)
FROM countries c 
WHERE c.IDCountry = stations.IDCountry 
	AND stations.IDCountry NOT IN (SELECT MIN(c2.IDCountry)
									FROM countries c2
									GROUP BY c2.Name);

DELETE 
FROM countries 
WHERE IDCountry NOT IN (SELECT MIN(c.IDCountry)
						FROM countries c
						GROUP BY c.Name);
					
-- Languages cleanup section --

UPDATE languages 
SET name = 
(WITH RECURSIVE items(item,str,oper) AS (
    SELECT NULL,_V.TextValue,languages.name
    FROM _TextVariables _V 
    WHERE _V.IDVariable=3
    UNION ALL 
    SELECT substr(str,0,instr(str,';')),substr(str,instr(str,';')+1),REPLACE(oper,substr(str,0,instr(str,';')),'')
	FROM items 
	WHERE str!=''
) SELECT oper FROM items WHERE item IS NOT NULL AND str='' LIMIT 1);

DELETE 
FROM languages 
WHERE IDLanguage IN (SELECT l.IDLanguage 			
					FROM languages l LEFT JOIN stations s on l.IDLanguage = s.IDLanguage 
					WHERE s.IDStation IS NULL)
	OR TRIM(name)=''
	OR name LIKE 'http%'
	OR name GLOB '[0-9]*';

UPDATE stations 
SET IDLanguage = NULL 
WHERE IDLanguage NOT IN (SELECT l.IDLanguage FROM languages l);

UPDATE stations 
SET IDLanguage = (SELECT MIN(l1.IDLanguage)
				FROM languages l1
				WHERE l1.Name = l.Name)
FROM languages l 
WHERE l.IDLanguage = stations.IDLanguage 
	AND stations.IDLanguage NOT IN (SELECT MIN(l2.IDLanguage)
									FROM languages l2
									GROUP BY l2.Name);

DELETE 
FROM languages 
WHERE IDLanguage NOT IN (SELECT MIN(l.IDLanguage)
						FROM languages l
						GROUP BY l.Name);
					
-- Codecs cleanup section --

UPDATE codecs 
SET name = 
(WITH RECURSIVE items(item,str,oper) AS (
    SELECT NULL,_V.TextValue,codecs.name
    FROM _TextVariables _V 
    WHERE _V.IDVariable=4
    UNION ALL 
    SELECT substr(str,0,instr(str,';')),substr(str,instr(str,';')+1),REPLACE(oper,substr(str,0,instr(str,';')),'')
	FROM items 
	WHERE str!=''
) SELECT oper FROM items WHERE item IS NOT NULL AND str='' LIMIT 1);

DELETE 
FROM codecs 
WHERE IDCodec IN (SELECT c.IDCodec
					FROM codecs c LEFT JOIN stations s on c.IDCodec = s.IDCodec 
					WHERE s.IDStation IS NULL)
	OR TRIM(name)=''
	OR name LIKE 'http%';

UPDATE stations 
SET IDCodec = NULL 
WHERE IDCodec NOT IN (SELECT c.IDCodec FROM codecs c);

UPDATE stations 
SET IDCodec = (SELECT MIN(c1.IDCodec)
				FROM codecs c1
				WHERE c1.Name = c.Name)
FROM codecs c 
WHERE c.IDCodec = stations.IDCodec 
	AND stations.IDCodec NOT IN (SELECT MIN(c2.IDCodec)
								FROM codecs c2
								GROUP BY c2.Name);

DELETE 
FROM codecs 
WHERE IDCodec NOT IN (SELECT MIN(c.IDCodec)
						FROM codecs c
						GROUP BY c.Name);
				
-- Tags cleanup section --

UPDATE tags 
SET name = 
(WITH RECURSIVE items(item,str,oper) AS (
    SELECT NULL,_V.TextValue,tags.name
    FROM _TextVariables _V 
    WHERE _V.IDVariable=3
    UNION ALL 
    SELECT substr(str,0,instr(str,';')),substr(str,instr(str,';')+1),REPLACE(oper,substr(str,0,instr(str,';')),'')
	FROM items 
	WHERE str!=''
) SELECT oper FROM items WHERE item IS NOT NULL AND str='' LIMIT 1);
								
DELETE 
FROM stationstags 
WHERE IDTag IN (SELECT IDTag
				FROM tags
				WHERE TRIM(name)=''
					OR name LIKE 'http%');
				
DELETE
FROM tags
WHERE TRIM(name)=''
	OR name LIKE 'http%';

UPDATE stationstags 
SET IDTag = (SELECT MIN(t1.IDTag)
			FROM tags t1
			WHERE t1.Name = t.Name)
FROM tags t 
WHERE t.IDTag = stationstags.IDTag 
	AND stationstags.IDTag NOT IN (SELECT MIN(t2.IDTag)
									FROM tags t2
									GROUP BY t2.Name);

DELETE 
FROM tags 
WHERE IDTag NOT IN (SELECT MIN(t.IDTag)
					FROM tags t
					GROUP BY t.Name);
								
DROP TABLE _TextVariables;

