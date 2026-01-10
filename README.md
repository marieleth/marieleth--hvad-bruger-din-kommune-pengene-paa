# Marie Leth - Hvad bruger din kommune pengene på?

## Projektbeskrivelse
Gennem dette visualiseringsvørktøj kan du sammenligne og danne dig et overblik over kommunale udgifter i danske kommuner. Udgifterne er målt per. indbygger i kroner. Jeg har lavet visualiseringsværktøjet i R Shiny. I appen kan du vælg en eller flere kommuner og et politikområde - og så se udviklingen fra 2016-2024.

Du kan tilgå appen her: https://marieleth.shinyapps.io/hvadbrugerdinkommunepengenepaa/

## Indhold
- Data-mappe: Indeholder rå data downloadet fra Danmarks Statistik opdelt efter årstal
- `app.R.R` : R-koder til appen
- `README.md`: Denne fil
  
## Datagrundlag
Sammenligningsværktøjet bygger på data fra Danmarks Statistik, tabel REGK31 (Kommunernes regnskaber på funktioner - efter område, funktion, dranst, art og prisenhed).
Alle beløb er opgjort i kroner per indbygger i løbende priser.

## Metode
Data viser kommunale udgifter per indbygger fra 2016 til 2024.
Udgifterne er opgjort som nettobeløb.
Beløbene er ikke justeret for inflation og vises i løbende priser.

## Kontakt
Har du spørgsmål til projektet, er du velkommen til at skrive til mig på marieleth.mail@gmail.com 




