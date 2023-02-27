use PortfolioProjects;

--Amphibian labels
ALTER TABLE dbo.Amphibians
	ADD Class VARCHAR(20);
UPDATE dbo.Amphibians
SET Class = 'Amphibian';

-- Bird labels
ALTER TABLE dbo.Birds
	ADD Class VARCHAR(20);
UPDATE dbo.Birds
SET Class = 'Bird';

--Mammal labels
ALTER TABLE dbo.Mammals
	ADD Class VARCHAR(20);
UPDATE dbo.Mammals
SET Class = 'Mammal';

--Reptile label
ALTER TABLE dbo.Reptiles
	ADD Class VARCHAR(20);
UPDATE dbo.Reptiles
SET Class = 'Reptile';


-- Concatenate Class Sheets 
SELECT *
	INTO Lost_Species 
	FROM (
	SELECT *
	FROM dbo.Amphibians
UNION 
	SELECT *
	FROM dbo.Mammals
UNION
	SELECT *
	FROM dbo.Birds
UNION
	SELECT *
	FROM dbo.Reptiles 
)a 
;


-- Change Y and N into 1 and 0 in Possibly Extinct column 

SELECT DISTINCT ([Possibly extinct?]), COUNT([Possibly extinct?])
FROM dbo.Lost_Species
GROUP BY [Possibly extinct?];

UPDATE dbo.Lost_Species
	SET [Possibly extinct?] = CASE WHEN [Possibly extinct?] = 'Y' THEN 1
								   WHEN [Possibly extinct?] = 'N' THEN 0
								   ELSE [Possibly extinct?]
								   END
								   ;

-- Check for duplicates
WITH CTE AS(
SELECT *, ROW_NUMBER() OVER( PARTITION BY [Scientific name] ORDER BY [Scientific name] ) row_num
FROM Lost_Species
)

SELECT * 
FROM CTE
WHERE row_num>1 ;
--No Duplicates
