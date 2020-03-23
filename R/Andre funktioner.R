# Funktionen tjekker om schema eksisterer i databasen

#' Tjek om schema eksister i databasen
#' @encoding UTF-8
#'
#' @description Funktionen bruges til at tjekke om et schema eksisterer i databasen
#'
#' @param schemaNavn en character string, navnet på det schema der skal tjekkes
#' @param databaseNavn en character string, Navnet på databasen for schema. Defaultværdien for variablen er værdien 'minDatabase' fra den globale environment.
#' @param serverNavn en character string, Navnet på serveren. Defaultværdien for variablen er værdien 'minServer' fra den globale environment.
#' @param driverNavn en character string, Navnet på driveren. Defaultværdien for variablen er værdien 'minDriver' fra den globale environment.
#'
# @details
#'
#' @return Funktionen melder fejl, hvis schema ikke eksisterer i databasen ellers ingen return.
#'
# @seealso
#'
#' @export
#'
# @examples tjekOmSchemaEksistererIDb(schemaNavn = "minSchema")
#'
tjekOmSchemaEksistererIDb <- function(schemaNavn,
                                      databaseNavn = minDatabase,
                                      serverNavn = minServer,
                                      driverNavn = minDriver){

  # Opretter forbindelsen
  forb <- dbConnect(odbc(), Driver = driverNavn, Server = serverNavn, Database = databaseNavn, encoding = "latin1")

  # Henter en liste over schema'er i databasen (både tomme og ikke tomme)
  schemaIDatabasen <- dbGetQuery(forb, "select SCHEMA_NAME from INFORMATION_SCHEMA.SCHEMATA")

  # Afbryder og fjerner forbindelsen
  dbDisconnect(forb)
  rm(forb)

  # Melder fejl hvis schema ikke findes i databasen
  if(!(schemaNavn %in% schemaIDatabasen$SCHEMA_NAME)){

    stop(paste0("Den valgte schema '", schemaNavn, "' findes ikke i ", databaseNavn, " - databasen.
       Angiv en eksisterende schema eller opret et schema med det valgte schemanavn og kør koden igen."))

  }

}
