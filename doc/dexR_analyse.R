## ---- eval=FALSE, results="hide", echo=TRUE, messages=FALSE, warning=FALSE----
#  dexpa$sim$starttime_min		<- as.numeric(strptime("30/11/19 12:00", "%d/%m/%y %H:%M"))*1000
#  dexpa$sim$starttime_max		<- as.numeric(strptime("03/12/19 12:00", "%d/%m/%y %H:%M"))*1000

## ---- eval=FALSE, results="hide", echo=TRUE, messages=FALSE, warning=FALSE----
#  dexpa$db$host			<- "localhost"
#  dexpa$db$port			<- "5432"
#  dexpa$db$dbname			<- "enavi"
#  dexpa$db$username		<- "enavi"
#  dexpa$db$password		<- "enavi!"
#  dexpa$db$suname			<- "postgres"
#  dexpa$db$supassword		<- "WiScAn07"

## ---- eval=FALSE, results="hide", echo=TRUE, messages=FALSE, warning=FALSE----
#  dexR::input_db_dumps2db(dexR::create_dexpas(c("id1", "id2"))

## ---- eval=TRUE, results="asis", echo=FALSE, messages=FALSE, warning=FALSE----
dexpas <- dexR::demo_prepare_db4figures()
dexR::hl_figure_clearing_comp_clearingPriceByCTbyProduct(dexpas, filename=NULL)
dexR::input_db_dropdbs(dexpas)

## ---- eval=TRUE, results="asis", echo=FALSE, messages=FALSE, warning=FALSE----
dexpas <- dexR::demo_prepare_db4figures()
dexR::hl_figure_clearing_comp_numConsideredRequests(dexpas, filename=NULL)
dexR::input_db_dropdbs(dexpas)

## ---- eval=TRUE, results="hide", echo=TRUE, messages=FALSE, warning=FALSE-----
dexpas <- dexR::demo_prepare_db4figures()
dexR::hl_figure_requests_numRequests_comp_byStatusByStartT(dexpas, filename=NULL)
dexR::input_db_dropdbs(dexpas)

## ---- eval=FALSE, results="hide", echo=TRUE, messages=FALSE, warning=FALSE----
#  source("/home/USER/dexr/scripts/dexpa-machine_machine.R")
#  hl_reports_comp(dexpa, c("enavi_08-01", "enavi_08-02"))

