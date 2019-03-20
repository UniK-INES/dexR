## ---- eval=FALSE, results="hide", echo=TRUE, messages=FALSE, warning=FALSE----
#  dexpa$db$host			<- "localhost"
#  dexpa$db$port			<- "5432"
#  dexpa$db$dbname			<- "enavi"
#  dexpa$db$username		<- "enavi"
#  dexpa$db$password		<- "enavi!"
#  dexpa$db$suname			<- "postgres"
#  dexpa$db$supassword		<- "WiScAn07"

## ---- eval=TRUE, results="hide", echo=TRUE, messages=FALSE, warning=FALSE----
dexpas <- dexR::demo_prepare_db4figures()
dexR::hl_figure_clearing_comp_clearingPriceByCTbyProduct(dexpas, filename=NULL)
dexR::input_db_dropdbs(dexpas)

## ---- eval=TRUE, results="hide", echo=TRUE, messages=FALSE, warning=FALSE----
dexpas <- dexR::demo_prepare_db4figures()
dexR::hl_figure_clearing_comp_numConsideredRequests(dexpas, filename=NULL)
dexR::input_db_dropdbs(dexpas)

## ---- eval=TRUE, results="hide", echo=TRUE, messages=FALSE, warning=FALSE----
dexpas <- dexR::demo_prepare_db4figures()
dexR::hl_figure_requests_numRequests_comp_byStatusByStartT(dexpas, filename=NULL)
dexR::input_db_dropdbs(dexpas)

