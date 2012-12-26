Projekt zaliczeniowy z przedmiotu Algorymty Heurystyczne.
23.12.2012r.
====================================================================================

Temat:
Ewolucja różnicowa w kostce – modyfikacja kierunkowa współczynnika skalującego.
Szczegóły w "SprawozdanieWstepne.pdf"
====================================================================================

Jak uruchomić?
 1. Pobierz R stąd: http://www.r-project.org/
    Np. to: http://r.meteo.uni.wroc.pl/bin/windows/base/R-2.15.2-win.exe
 2. Uruchom i zainstaluj bibliotekę umożliwiającą współpracę z Javą:
    Wpisz w konsoli R: install.package("rJava");
 3. Pobierz i zainstaluj Eclipse for Java.
 4. Zainstaluj plugin http://www.walware.de/it/statet/
    W Eclipse: Help -> Install New Software -> Work with: http://download.walware.de/eclipse-3.8
    Po zainstalowaniu skonfiguruj:
      Window -> Preferences -> StatET -> Run/Debug -> R Environments -> Add
        Location R_HOME -> Try Find Automatically
        Detect Default Properties/Settings
      Run -> Run Configurations -> R Console -> New
        Ustaw Working Directory na ${workspace_loc:/ALHE/src}
        
Jak uruchomić v2.
 http://lukemiller.org/index.php/2010/04/eclipse-and-statet-a-nice-working-environment-for-r/
    