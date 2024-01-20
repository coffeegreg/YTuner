INSERT INTO languages(Name)
SELECT DISTINCT json_extract(value, '$.name')
FROM json_each(@@@JSONCONTENTS);