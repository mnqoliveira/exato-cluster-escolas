# School visiting routes

In 2017, I was responsible for choosing which schools in Campinas and neighbor cities would be visited to announce the dates for enrolment in this outreach project in which I worked. I used data from the projects' previous year, to identify which schools the students that took part in the project attended, and official data from Sao Paulo's list of public schools. This list was used to retrieve their locations using an API from Google or Google Maps, and through their location and coordinates, the distance from Unicamp, where the project activities happened, was calculated. These distances and coordinates were also used to generate clusters of schools close to each other, so that visitation schedules could be optimized. Unfortunately, the API I used no longer works as in the script.

--

De modo a selecionar as escolas para divulgação no Projeto Exato, foram processados os dados de inscrição do ano anterior, bem como os dados da Secretaria Estadual de Educação de São Paulo. Deste modo, estavam disponíveis para a equipe as escolas de origem dos alunos que frequentavam o projeto e as distâncias entre as escolas dos municípios da RMC e a Unicamp.