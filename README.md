# Simulering av gytebestand og innsig

### Vitenskapelig råd for lakseforvaltning i Norge, http://www.vitenskapsradet.no/

## Oversikt

Dette repositoriet inneholder kode som:

* Simulerer gytebestand og måloppnåelse i de rundt 200 viktigste laksevassdragene i Norge
* Beregner vassdragsspesifikke innsig til de simulerte vassdragene basert på fangststatistikk og måloppnåelse

Repositoriet er organisert med følgende kataloger:

* `data` inneholder rådataene som inngår i simuleringene
* `src` innholder koden som simulerer gytebestand, beregner innsig og lager figurer
* `results` er katalogen hvor resultatfiler og figurer blir lagret 

## Instruksjon

For å kjøre skriptene må programmet R være installert. Nettside: https://www.r-project.org/

Skriptene kjøres i følgende rekkefølge:

* `GBM_simulering.R` kjører gjennom alle simuleringsvassdragene og produserer resultatfiler som brukes til innsig og figurer
* `Innsig_fangstfordeling.R` tar resultat fra simulering, innhenter fangststatistikk fra sjø og fordeler så en innsigsberegning til de ulike vassdragene
* `nye_figurer.R` kjører gjennom alle vassdragene, innhenter innsigs- og simuleringstall og produserer nye figurer som lagres i `results\figurer\`
