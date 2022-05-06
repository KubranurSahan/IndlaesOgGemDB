# Funktionen bruges til at gemme datasæt i en database

#' Gem datasæt i en database
#' @encoding UTF-8
#'
#' @description Funktionen bruges til at gemme et datasæt i en database
#'
#' @param datasaet en data.frame, datasættet som skal gemmes
#' @param schemaNavn en character string, navnet på det schema datasættet skal gemmes i
#' @param datasaetNavn en character string, navnet som datasættet skal have i databasen. Navnet skal indeholde mindst en underscore "_".
#'     Default er navnet på 'datasaet' og '_df'. Fx. hvis datasaet hedder 'aktivitet' vil datasættet hedde 'aktivitet_df' i databasen.
#' @param databaseNavn en character string, Navnet på databasen som datasættet skal gemmes i. Defaultværdien for variablen er værdien 'minDatabase' fra den globale environment.
#' @param serverNavn en character string, Navnet på serveren. Defaultværdien for variablen er værdien 'minServer' fra den globale environment.
#' @param driverNavn en character string, Navnet på driveren. Defaultværdien for variablen er værdien 'minDriver' fra den globale environment.
#'
# @details
#'
#' @return ingen return
#'
# @seealso
#'
#' @export
#'
# @examples gemDatasaetIDb(datasaet = aktivitet, schemaNavn = "minSchema", datasaetNavn = "aktivitet0120_df")
#'
gemDatasaetIDb <- function(datasaet,
                           schemaNavn,
                           datasaetNavn = NA,
                           databaseNavn = minDatabase,
                           serverNavn = minServer,
                           driverNavn = minDriver,
                           conEncoding = "UTF-8"){

  # Definere navnet på datasættet, hvis denne ikke er angivet
  if(is.na(datasaetNavn)){

    datasaetNavn <- paste0(deparse(substitute(datasaet)), "_df")

  }

  # Pga. en fejl i Id og SQL(som vi brugte før) er navnet på table nød til at indeholde en "_".
  # Tjekker om datasaetNavn indeholder "_"
  if(!grepl("_", datasaetNavn, fixed = TRUE)){
    stop("Navnet på tabellen (datasaetNavn) skal indeholde en underscore '_'.")
  }

  # Opretter forbindelsen
  forb <- dbConnect(odbc(), Driver = driverNavn, Server = serverNavn, Database = databaseNavn, encoding = conEncoding)

  # Henter en liste over schema'er i databasen (både tomme og ikke tomme)
  schemaIDatabasen <- dbGetQuery(forb, "select SCHEMA_NAME from INFORMATION_SCHEMA.SCHEMATA")

  # Melder fejl hvis schema ikke findes i databasen
  if(!(schemaNavn %in% schemaIDatabasen$SCHEMA_NAME)){

    dbDisconnect(forb)
    rm(forb)

    stop(paste0("Den valgte schema '", schemaNavn, "' findes ikke i ", databaseNavn, " - databasen.
       Angiv en eksisterende schema eller opret et schema med det valgte schemanavn og kør koden igen."))

  }

  # Gemmer datasættet i databasen under projekt navnet
  dbWriteTable(forb, Id(schema = schemaNavn, table = datasaetNavn), datasaet, append = TRUE)

  # Afbryder og fjerner forbindelsen
  dbDisconnect(forb)
  rm(forb)

  gc()

}




