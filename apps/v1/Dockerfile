FROM lc_app_upper_base_04jul2020
WORKDIR /home/shiny
RUN mkdir data
COPY navApp.R .
COPY data/*.RData data/
COPY installPrerequisites.R .
RUN R -f installPrerequisites.R
COPY pkgs/igvShiny_0.99.91.tar.gz .
RUN R CMD INSTALL igvShiny_0.99.91.tar.gz
COPY pkgs/shinyModules_1.0.15.tar.gz .
RUN R CMD INSTALL shinyModules_1.0.15.tar.gz
COPY pkgs/ProteomicsNavigator_1.0.3.tar.gz .
RUN R CMD INSTALL ProteomicsNavigator_1.0.3.tar.gz



