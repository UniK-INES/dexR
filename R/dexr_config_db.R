config_db_paramtables2csv <- function(dexpa) {
	system('psql -U postgres COPY (select * from clickthru.train limit 10;) TO "C:\\me\\psql_test.csv" with CSV')
}