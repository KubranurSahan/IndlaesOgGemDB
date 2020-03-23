### Funktion til indlæsning af datasæt og mulighed for sammenligning for tidligere gemt datasæt fra databasen ###
#' Hent datasæt fra database
#' @encoding UTF-8
#'
#' @description Funktion til at indlæse et datasæt fra database med følgende muligheder udover selve indlæsningen:\cr
#'     \itemize{
#'     \item Mulighed for at definere 'udtraeksDatoTidVaerdi' for at hente en bestemt version af et datasæt, der indeholder flere versioner af samme data.
#'     \item Mulighed for at formatere datasæt med det samme ved angivelse af datasaetFormatFil
#     \item Mulighed for at sammenligne det indlæste data med den version den havde da den blev gemt
#'     }
#'     Funktionen bruger værdier fra global environment til at finde databasen, serveren og driveren, der skal bruges til at hente data fra.
#'
#' @param schemaNavn en character string, Navnet på det schema tabellen ligger i i databasen
#' @param datasaetNavn en character string, Navnet på tabellen, som skal hentes
#' @param datasaetFormatFil en data.frame, formatfilen som fortæller, hvad og hvordan datasættet skal formateres (default værdi er NA - datasættet formateres ikke)
# @param datasaetSummary tidligere gemt summary for datasættet til sammenligning
# @param antalNAs tidligere gemt antal NA for datasættet til sammenligning
#' @param udtraeksDatoTidVaerdi en character string, angiver version for at udvælge en bestemt version af en tabel der opdateres løbende uden erstatning af tidligere versioner af datasættet
#' @param hentForSidsteUdtraeksDatoTidVaerdi TRUE/FALSE værdi, Hvis TRUE, så hentes de rækker af datasættet der svarer til den sidste udtrækdato. Kræver i øvrigt at 'udtraeksDatoTidVaerdi' er NA.
#' @param databaseNavn en character string, Navnet på databasen som datasættet skal hentes fra. Defaultværdien for variablen er værdien 'minDatabase' fra den globale environment.
#' @param serverNavn en character string, Navnet på serveren som datasættet skal hentes fra. Defaultværdien for variablen er værdien 'minServer' fra den globale environment.
#' @param driverNavn en character string, Navnet på driveren som skal bruges til at hente datasættet. Defaultværdien for variablen er værdien 'minDriver' fra den globale environment.
#'
# @details
#'
#' @return Den indlæste datasæt
#'
#' @seealso \code{\link[Formateringsfunktioner]{retOgFormaterDatasaet}} og \code{\link[Formateringsfunktioner]{opsaetFormatFil}}
#'
#' @export
#'
# @examples data <- hentDatasaetFraDatabase(schemaNavn = "mitSchema",datasaetNavn = "mit_data_tabel")
#'
hentDatasaetFraDatabase<-function(schemaNavn,
                                  datasaetNavn,
                                  datasaetFormatFil = NA,
                                  # datasaetSummary = NA,
                                  # antalNAs = NA,
                                  udtraeksDatoTidVaerdi = NA,
                                  hentForSidsteUdtraeksDatoTidVaerdi = FALSE,
                                  databaseNavn = minDatabase,
                                  serverNavn = minServer,
                                  driverNavn = minDriver){

  # Opretter forbindelsen
  forb <- dbConnect(odbc(), Driver = driverNavn, Server = serverNavn, Database = databaseNavn, encoding = "latin1")

  # Henter datasættet
  datasaet <- dbReadTable(forb, Id(schema = schemaNavn, table = datasaetNavn))

  # Afbryder og fjerner forbindelsen
  dbDisconnect(forb)
  rm(forb)

  # Tjekker i tilfælde af at man har valgt at hente rækker med et bestemt eller sidste udtræksdato fra datasættet
  # om udtraeksDato variablen eksisterer
  if(!is.na(udtraeksDatoTidVaerdi) | hentForSidsteUdtraeksDatoTidVaerdi){

    if(!("udtraeksDatoTid" %in% colnames(datasaet))){

      stop("Kan ikke hente datasaettet for den valgte eller sidste udtræksdato,
           da variablen 'udtraeksDatoTid' ikke findes i datasættet.")

    }

  }


  if(!is.na(udtraeksDatoTidVaerdi)){

    # Vælger de rækker i datasættet som passer med udtraeksDatoTidVaerdien
    datasaet <- datasaet %>%
      filter(udtraeksDatoTid == udtraeksDatoTidVaerdi)

  }else if(hentForSidsteUdtraeksDatoTidVaerdi){

    # Henter de rækker i datslttet som passer med den sidste (højeste) udtraeksDatoTid i tabellen
    sidsteDato <- max(datasaet$udtraeksDatoTid, na.rm = TRUE)

    datasaet <- datasaet %>%
      filter(udtraeksDatoTid == sidsteDato)

  }

  # Tjekker om der overhovedet er noget 'tilbage' i tabellen
  if(nrow(datasaet) == 0){

    stop("Datasættet du har valgt at indlæse er tom. Være sikker på at du har angivet de rigtige oplysninger.")

  }

  if(!all(is.na(datasaetFormatFil))){

    # Formatere datasættet på baggrund af den tilhørende formatfil
    datasaet <- retOgFormaterDatasaet(datasaettet = datasaet, datasaetFormatFilen = datasaetFormatFil)

  }

  # if(!is.na(datasaetSummary) & !is.na(antalNAs)){
  #
  #   # Tjekker om summary og antalNAs holder for character vars holder
  #   if(!identical(summary(datasaet), datasaetSummary) | !identical(colSums(is.na(datasaet)), antalNAs)){
  #
  #     cat("Fejl: Problemer med indlæsning af ", datasaetNavn, " fra databasen. Den afviger fra den gemte datasæt.\n", sep = "")
  #     stop(paste0("Fejl: Problemer med indlæsning af ", datasaetNavn, " fra databasen. Den afviger fra den gemte datasæt."))
  #
  #   }
  #
  # }

  return(datasaet)

}
