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



#' Tjek om tabellen eksister i databasen
#' @encoding UTF-8
#'
#'
#' @description Funktionen bruges til at tjekke om en tabel eksisterer under en bestemt schema i databasen
#'
#'
#' @param tabelNavn en character string, navnet på det tabel der skal tjekkes
#' @param schemaNavn en character string, navnet på det schema som tabellen skal findes i
#' @param databaseNavn en character string, Navnet på databasen. Defaultværdien for variablen er værdien 'minDatabase' fra den globale environment.
#' @param serverNavn en character string, Navnet på serveren. Defaultværdien for variablen er værdien 'minServer' fra den globale environment.
#' @param driverNavn en character string, Navnet på driveren. Defaultværdien for variablen er værdien 'minDriver' fra den globale environment.
#'
#' @return TRUE/FALSE, TRUE hvis tabellen findes i databasen
#' @export
#'
# @examples
tjekOmTabelEksistererIDb <- function(tabelNavn,
                                     schemaNavn,
                                     databaseNavn = minDatabase,
                                     serverNavn = minServer,
                                     driverNavn = minDriver){

  # Opretter forbindelsen
  forb <- dbConnect(odbc(), Driver = driverNavn, Server = serverNavn, Database = databaseNavn, encoding = "latin1")

  # Henter en liste over schema'er og tabeller i databasen
  tabellerIDatabasen <- dbGetQuery(forb, "select TABLE_SCHEMA,TABLE_NAME from INFORMATION_SCHEMA.TABLES where TABLE_TYPE = 'BASE TABLE'")

  # Afbryder og fjerner forbindelsen
  dbDisconnect(forb)
  rm(forb)

  # Melder fejl hvis schema ikke findes i databasen
  if(!(tabelNavn %in% tabellerIDatabasen[tabellerIDatabasen$TABLE_SCHEMA == schemaNavn,]$TABLE_NAME)){

    tabelFindes <- FALSE
    # stop(paste0("Tabellen ", tabelNavn, " findes ikke i schema '", schemaNavn, "' i ", databaseNavn, " - databasen."))

  }else{

    tabelFindes <- TRUE

  }

  return(tabelFindes)

}


























