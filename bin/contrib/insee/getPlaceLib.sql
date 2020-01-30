drop function if exists insee.getPlaceLib;

delimiter //
create function insee.getPlaceLib(
 inCode CHAR(5),
 inEffetY CHAR(4),
 inEffetM CHAR(2),
 inEffetD CHAR(2)
)
RETURNS VARCHAR(500)
DETERMINISTIC
READS SQL DATA
BEGIN
	DECLARE res VARCHAR(500);

	IF inEffetY = '0000' THEN
		set inEffetY = '0001';
	END IF;
	IF inEffetM = '00' THEN
		set inEffetM = '01';
	END IF;
	IF inEffetD = '00' THEN
		set inEffetD = '01';
	END IF;

	select Libelle into res
	from PlaceNorme
	where Code = inCode
	  and concat_ws('-', inEffetY,inEffetM,inEffetD) < DateFin
	order by DateFin asc
	limit 1;

	IF res is null THEN
		set res = "";
	END IF;

	return( res );
END//
delimiter ;
