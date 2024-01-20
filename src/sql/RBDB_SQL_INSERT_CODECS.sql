INSERT INTO codecs(Name)
SELECT DISTINCT json_extract(value, '$.name')
FROM json_each(@@@JSONCONTENTS);