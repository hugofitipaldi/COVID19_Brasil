# Primeiro vamos ver os diferentes dados que temos em mãos em relação ao COVID19 no Brasil

# Lendo as libraries necessárias
library(tidyverse)
library(rio)
library(DT)
library(fs)


# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------
# -------------------------------- FUNÇÕES PRA BAIXAR OS DADOS -----------------------------------
# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------

# John Hopkins
# Função para baixar os dados da John Hopkins atualizados baseado no package coronavirus  (a cada 0.5 horas) 
downloadJH <- function() {
  download.file(
    url      = "https://github.com/RamiKrispin/coronavirus-csv/raw/master/coronavirus_dataset.csv",
    destfile = "data/coronavirus_dataset.csv"
  )
}

updateJH <- function() {
  # Download data from Folkhalsomyndigheten if the data is older than 0.5h
  if (!dir_exists("data")) {
    dir.create('data')
    downloadJH()
  } else if ((!file.exists("data/coronavirus_dataset.csv")) || (as.double(Sys.time() - file_info("data/coronavirus_dataset.csv")$change_time, units = "hours") > 0.5)) {
    downloadJH()
  }
}

# Update with start of app
updateJH()

coronavirus <- rio::import('data/coronavirus_dataset.csv', which = 1)

# Governo Federal
# Função para baixar os dados do Governo Federal atualizados (a cada 0.5 horas)

download_gov <- function() {
  download.file(
    url      = "https://mobileapps.saude.gov.br/esus-vepi/files/unAFkcaNDeXajurGB7LChj8SgQYS2ptm/b350824dbfad17f083e62d4b41e88cb7_Download_COVID19_20200408.csv",
    destfile = "data/b350824dbfad17f083e62d4b41e88cb7_Download_COVID19_20200408.csv"
  )
}


updateData_gov <- function() {
  if (!dir_exists("data")) {
    dir.create('data')
    download_gov()
  } else if ((!file.exists("data/b350824dbfad17f083e62d4b41e88cb7_Download_COVID19_20200408.csv")) || (as.double(Sys.time() - file_info("data/b350824dbfad17f083e62d4b41e88cb7_Download_COVID19_20200408.csv")$change_time, units = "hours") > 5)) {
    download_gov()
  }
}

# Update with start of app
updateData_gov()

# Agora vamos dar uma olhada em todos os dados
covid19_gov <- rio::import("data/b350824dbfad17f083e62d4b41e88cb7_Download_COVID19_20200408.csv")

# Um pouco de Data cleaning com os dados do governo

names(covid19_gov) <- c("Regiao", "Sigla", "Data", "Casos_novos",
                        "Casos_acumulados", "Obitios_novos", "Obitos_acumulados")

covid19_gov$Data <- as.Date(covid19_gov$Data, format = "%d/%m/%Y")
head(covid19_gov)

# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------
# -------------------------------- TRABALHANDO COM OS DADOS --------------------------------------
# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------

# John Hopkins

df <- coronavirus %>%
  # dplyr::filter(date == max(date)) %>%
  dplyr::filter(Country.Region == "Brazil") %>%
  dplyr::group_by(Country.Region, type) %>%
  dplyr::summarise(total = sum(cases)) %>%
  tidyr::pivot_wider(
    names_from = type,
    values_from = total
  ) %>%
  # dplyr::mutate(unrecovered = confirmed - ifelse(is.na(recovered), 0, recovered) - ifelse(is.na(death), 0, death)) %>%
  dplyr::mutate(unrecovered = confirmed - ifelse(is.na(death), 0, death)) %>%
  dplyr::arrange(-confirmed) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(country = dplyr::if_else(Country.Region == "United Arab Emirates", "UAE", Country.Region)) %>%
  dplyr::mutate(country = dplyr::if_else(country == "Mainland China", "China", country)) %>%
  dplyr::mutate(country = dplyr::if_else(country == "North Macedonia", "N.Macedonia", country)) %>%
  dplyr::mutate(country = trimws(country)) %>%
  dplyr::mutate(country = factor(country, levels = country))

df_daily <- coronavirus %>%
  dplyr::filter(Country.Region == "Brazil") %>%
  dplyr::group_by(date, type) %>%
  dplyr::summarise(total = sum(cases, na.rm = TRUE)) %>%
  tidyr::pivot_wider(
    names_from = type,
    values_from = total
  ) %>%
  dplyr::arrange(date) %>%
  dplyr::ungroup() %>%
  #dplyr::mutate(active = confirmed - death - recovered) %>%
  dplyr::mutate(active = confirmed - death) %>%
  dplyr::mutate(
    confirmed_cum = cumsum(confirmed),
    death_cum = cumsum(death),
    # recovered_cum = cumsum(recovered),
    active_cum = cumsum(active)
  )

df_daily$date <- as.Date(df_daily$date, format = "%Y-%m-%d")
df1 <- coronavirus %>% dplyr::filter(date == max(date))

# Alguns plots

#------------------ Parameters ------------------
# Set colors
# https://www.w3.org/TR/css-color-3/#svg-color
confirmed_color <- "blue"
active_color <- "#1f77b4"
recovered_color <- "forestgreen"
death_color <- "red"


plotly::plot_ly(data = df_daily) %>%
  plotly::add_trace(
    x = ~date,
    # y = ~active_cum,
    y = ~confirmed_cum,
    type = "scatter",
    mode = "lines+markers",
    # name = "Active",
    name = "Casos confirmados",
    line = list(color = active_color),
    marker = list(color = active_color)
  ) %>%
  plotly::add_trace(
    x = ~date,
    y = ~death_cum,
    type = "scatter",
    mode = "lines+markers",
    name = "Mortes",
    line = list(color = death_color),
    marker = list(color = death_color)
  ) %>%
  plotly::add_annotations(
    x = as.Date("2020-02-25"),
    y = 1,
    text = paste("Primeiro caso"),
    xref = "x",
    yref = "y",
    arrowhead = 5,
    arrowhead = 3,
    arrowsize = 1,
    showarrow = TRUE,
    ax = -10,
    ay = -90
  ) %>%
  plotly::add_annotations(
    x = as.Date("2020-03-17"),
    y = 3,
    text = paste("Primeira morte"),
    xref = "x",
    yref = "y",
    arrowhead = 5,
    arrowhead = 3,
    arrowsize = 1,
    showarrow = TRUE,
    ax = -90,
    ay = -90
  )  %>%
     plotly::add_annotations(
       x = as.Date("2020-03-21"),
       y = 14,
       text = paste(
         "Medidas de",
         "",
         "Confinamento"
       ),
       xref = "x",
       yref = "y",
       arrowhead = 5,
       arrowhead = 3,
       arrowsize = 1,
       showarrow = TRUE,
       ax = -10,
       ay = -90
     ) %>%

plotly::layout(
  title = "",
  yaxis = list(title = "Número cumulativo de casos e mortes"),
  xaxis = list(title = "Data"),
  legend = list(x = 0.1, y = 0.9),
  hovermode = "compare"
)


# Gráfico com acontecimentos políticos


plotly::plot_ly(data = df_daily) %>%
  plotly::add_trace(
    x = ~date,
    y = ~death_cum,
    type = "scatter",
    mode = "lines+markers",
    name = "Mortes",
    line = list(color = death_color),
    marker = list(color = death_color,
    size = 8)
  ) %>%
  plotly::add_annotations(
    x = as.Date("2020-02-25"),
    y = 1,
    text = paste("<a href='https://saude.estadao.com.br/noticias/geral,primeiro-caso-da-covid-19-no-brasil-e-do-fim-de-janeiro-diz-ministerio-da-saude,70003258394'><br>Primeiro caso</br>de COVID19 no Brasil</a>"),
    xref = "x",
    yref = "y",
    arrowhead = 5,
    arrowhead = 3,
    arrowsize = 1,
    showarrow = TRUE,
    ax = -10,
    ay = -50
  ) %>%
  plotly::add_annotations(
    x = as.Date("2020-03-17"),
    y = 3,
    text = paste("<a href= 'https://www.em.com.br/app/noticia/nacional/2020/04/02/interna_nacional,1135097/primeira-morte-de-covid-19-no-brasil-foi-em-minas-informa-governo.shtml'><>brPrimeira morte</br>por COVID19</a>", sep ='\n'),
    xref = "x",
    yref = "y",
    arrowhead = 5,
    arrowhead = 3,
    arrowsize = 1,
    showarrow = TRUE,
    ax = -10,
    ay = -50
  )  %>%
  plotly::add_annotations(
    x = as.Date("2020-04-16"),
    y = 1924,
    text = paste("<a href = 'https://exame.abril.com.br/brasil/apos-semanas-de-conflitos-bolsonaro-demite-mandetta/'>Bolsonaro demite Mandetta</a>"),
    xref = "x",
    yref = "y",
    arrowhead = 5,
    arrowhead = 3,
    arrowsize = 1,
    showarrow = TRUE,
    ax = -90,
    ay = -90
  ) %>%
  plotly::add_annotations(
    x = as.Date("2020-03-24"),
    y = 46,
    text = paste('<br>Primeiro</br> pronunciamento Bolsonaro:</br> <a href = "https://www1.folha.uol.com.br/poder/2020/03/em-pronunciamento-bolsonaro-critica-fechamento-de-escolas-ataca-governadores-e-culpa-midia.shtml">"gripezinha"</a>'),
    xref = "x",
    yref = "y",
    arrowhead = 5,
    arrowhead = 3,
    arrowsize = 1,
    showarrow = TRUE,
    ax = 30,
    ay = -90
  ) %>%
  plotly::add_annotations(
    x = as.Date("2020-03-10"),
    y = 0,
    text = paste('<br>Bolsonaro sobre COVID19: </br><a href="https://g1.globo.com/politica/noticia/2020/03/10/bolsonaro-diz-que-questao-do-coronavirus-e-muito-mais-fantasia.ghtml">"pequena crise"</a>'),
    xref = "x",
    yref = "y",
    arrowhead = 5,
    arrowhead = 3,
    arrowsize = 1.5,
    showarrow = TRUE,
    ax = -90,
    ay = -90
  ) %>%
  plotly::add_annotations(
    x = as.Date("2020-03-13"),
    y = 0,
    text = paste('<br>Bolsonaro testa positivo</br> para COVID19</br> e depois <a href = "https://politica.estadao.com.br/noticias/geral,bolsonaro-diz-que-2-teste-para-coronavirus-deu-negativo,70003237455">nega</a>'),
    xref = "x",
    yref = "y",
    arrowhead = 5,
    arrowhead = 3,
    arrowsize = 1.5,
    showarrow = TRUE,
    ax = -30,
    ay = -150
  ) %>%
  plotly::add_annotations(
    x = as.Date("2020-03-20"),
    y = 11,
    text = paste('<br><a href = "https://noticias.uol.com.br/politica/ultimas-noticias/2020/03/22/coronavirus-comitiva-jair-bolsonaro-eua-donald-trump-marcelo-thome-rondonia.htm">Número de infectados da comitiva que </br> viajou com Bolsonaro</br> sobe para 23 </a>'),
    xref = "x",
    yref = "y",
    arrowhead = 5,
    arrowhead = 3,
    arrowsize = 1.5,
    showarrow = TRUE,
    ax = -10,
    ay = -200
  ) %>%
  plotly::add_annotations(
    x = as.Date("2020-04-19"),
    y = 2462,
    text = paste('<br><a href="https://www.dn.pt/mundo/bolsonaro-participa-em-manifestacao-contra-o-isolamento-e-a-favor-de-intervencao-militar-12091094.html">Bolsonaro participa em manifestação </br> contra o isolamento e </br> a favor de intervenção militar</a>'),
    xref = "x",
    yref = "y",
    arrowhead = 5,
    arrowhead = 3,
    arrowsize = 1.5,
    showarrow = TRUE,
    ax = -90,
    ay = -90
  ) %>%
  plotly::add_annotations(
    x = as.Date("2020-03-31"),
    y = 201,
    text = paste('<br>Segundo</br> pronunciamento Bolsonaro:</br> <a href = "https://www1.folha.uol.com.br/poder/2020/03/em-novo-pronunciamento-bolsonaro-distorce-oms-e-volta-a-igualar-empregos-e-vidas-diante-do-coronavirus.shtml">críticas ao isolamento</a>'),
    xref = "x",
    yref = "y",
    arrowhead = 5,
    arrowhead = 3,
    arrowsize = 1,
    showarrow = TRUE,
    ax = 160,
    ay = -30
  ) %>%
  plotly::add_annotations(
    x = as.Date("2020-04-08"),
    y = 819,
    text = paste('<br>Terceiro</br> pronunciamento Bolsonaro:</br> <a href = "https://www.correiobraziliense.com.br/app/noticia/politica/2020/04/08/interna_politica,843286/pronunciamento-de-bolsonaro-nesta-quarta-feira-8-4.shtml">atrito com governadores</a>'),
    xref = "x",
    yref = "y",
    arrowhead = 5,
    arrowhead = 3,
    arrowsize = 1,
    showarrow = TRUE,
    ax = -90,
    ay = -90
  ) %>%
  plotly::layout(
    title = "Mortes por COVID19 no Brasil com comentários de Jair Bolsonaro",
    yaxis = list(title = "Número cumulativo de mortes"),
    xaxis = list(title = "Data"),
    legend = list(x = 0.1, y = 0.9),
    hovermode = "compare"
  )




df_daily2 <- df_daily %>%
  filter(date >= as.Date('2020-02-25'))

plotly::plot_ly(data = df_daily2) %>%
  plotly::add_trace(
    x = ~date,
    y = ~death_cum,
    type = "scatter",
    mode = "lines+markers",
    name = "Mortes",
    line = list(color = death_color),
    marker = list(color = death_color,
                  size = 8)
  ) %>%
  plotly::add_annotations(
    x = as.Date("2020-02-25"),
    y = 1,
    text = paste("<a href='https://saude.estadao.com.br/noticias/geral,primeiro-caso-da-covid-19-no-brasil-e-do-fim-de-janeiro-diz-ministerio-da-saude,70003258394'><br>Primeiro caso de</br>COVID19no Brasil</a>"),
    xref = "x",
    yref = "y",
    arrowhead = 5,
    arrowhead = 3,
    arrowsize = 1,
    showarrow = TRUE,
    ax = 10,
    ay = -90
  ) %>%
  plotly::add_annotations(
    x = as.Date("2020-03-17"),
    y = 3,
    text = paste("<a href= 'https://www.em.com.br/app/noticia/nacional/2020/04/02/interna_nacional,1135097/primeira-morte-de-covid-19-no-brasil-foi-em-minas-informa-governo.shtml'><br>Primeira morte por</br>COVID19 no Brasil</a>", sep ='\n'),
    xref = "x",
    yref = "y",
    arrowhead = 5,
    arrowhead = 3,
    arrowsize = 1,
    showarrow = TRUE,
    ax = 0,
    ay = -50
  )  %>%
  plotly::add_annotations(
    x = as.Date("2020-04-16"),
    y = 1924,
    text = paste("<a href = 'https://exame.abril.com.br/brasil/apos-semanas-de-conflitos-bolsonaro-demite-mandetta/'>Bolsonaro demite Mandetta</a>"),
    xref = "x",
    yref = "y",
    arrowhead = 5,
    arrowhead = 3,
    arrowsize = 1,
    showarrow = TRUE,
    ax = -90,
    ay = -90
  ) %>%
  plotly::add_annotations(
    x = as.Date("2020-03-24"),
    y = 46,
    text = paste('<br>Primeiro</br> pronunciamento Bolsonaro:</br> <a href = "https://www1.folha.uol.com.br/poder/2020/03/em-pronunciamento-bolsonaro-critica-fechamento-de-escolas-ataca-governadores-e-culpa-midia.shtml">"gripezinha"</a>'),
    xref = "x",
    yref = "y",
    arrowhead = 5,
    arrowhead = 3,
    arrowsize = 1,
    showarrow = TRUE,
    ax = 10,
    ay = -90
  ) %>%
  plotly::add_annotations(
    x = as.Date("2020-03-10"),
    y = 0,
    text = paste('<br>Bolsonaro sobre COVID19: </br><a href="https://g1.globo.com/politica/noticia/2020/03/10/bolsonaro-diz-que-questao-do-coronavirus-e-muito-mais-fantasia.ghtml">"pequena crise"</a>'),
    xref = "x",
    yref = "y",
    arrowhead = 5,
    arrowhead = 3,
    arrowsize = 1.5,
    showarrow = TRUE,
    ax = -90,
    ay = -45
  ) %>%
  plotly::add_annotations(
    x = as.Date("2020-03-13"),
    y = 0,
    text = paste('<br><a href = https://politica.estadao.com.br/noticias/geral,apos-descartar-coronavirus-bolsonaro-aparece-no-alvorada-vida-segue-normal,70003232060>Bolsonaro afirma que</br>testou negativo</br>para coronavírus</a>'),
    xref = "x",
    yref = "y",
    arrowhead = 5,
    arrowhead = 3,
    arrowsize = 1.5,
    showarrow = TRUE,
    ax = -10,
    ay = -120
  ) %>%
  plotly::add_annotations(
    x = as.Date("2020-03-20"),
    y = 11,
    text = paste('<br><a href = "https://noticias.uol.com.br/politica/ultimas-noticias/2020/03/22/coronavirus-comitiva-jair-bolsonaro-eua-donald-trump-marcelo-thome-rondonia.htm">Número de infectados da comitiva</br>que viajou com Bolsonaro</br> sobe para 23 </a>'),
    xref = "x",
    yref = "y",
    arrowhead = 5,
    arrowhead = 3,
    arrowsize = 1.5,
    showarrow = TRUE,
    ax = 15,
    ay = -200
  ) %>%
  plotly::add_annotations(
    x = as.Date("2020-04-19"),
    y = 2462,
    text = paste('<br><a href="https://www.dn.pt/mundo/bolsonaro-participa-em-manifestacao-contra-o-isolamento-e-a-favor-de-intervencao-militar-12091094.html">Bolsonaro participa em manifestação </br> contra o isolamento e </br> a favor de intervenção militar</a>'),
    xref = "x",
    yref = "y",
    arrowhead = 5,
    arrowhead = 3,
    arrowsize = 1.5,
    showarrow = TRUE,
    ax = -90,
    ay = -90
  ) %>%
  plotly::add_annotations(
    x = as.Date("2020-03-31"),
    y = 201,
    text = paste('<br>Segundo</br> pronunciamento Bolsonaro:</br> <a href = "https://www1.folha.uol.com.br/poder/2020/03/em-novo-pronunciamento-bolsonaro-distorce-oms-e-volta-a-igualar-empregos-e-vidas-diante-do-coronavirus.shtml">críticas ao isolamento</a>'),
    xref = "x",
    yref = "y",
    arrowhead = 5,
    arrowhead = 3,
    arrowsize = 1,
    showarrow = TRUE,
    ax = 160,
    ay = -3
  ) %>%
  plotly::add_annotations(
    x = as.Date("2020-04-08"),
    y = 819,
    text = paste('<br>Terceiro</br> pronunciamento Bolsonaro:</br> <a href = "https://www.correiobraziliense.com.br/app/noticia/politica/2020/04/08/interna_politica,843286/pronunciamento-de-bolsonaro-nesta-quarta-feira-8-4.shtml">atrito com governadores</a>'),
    xref = "x",
    yref = "y",
    arrowhead = 5,
    arrowhead = 3,
    arrowsize = 1,
    showarrow = TRUE,
    ax = -75,
    ay = -110
  ) %>%
  plotly::add_annotations(
    x = as.Date("2020-04-24"),
    y = 3704,
    text = paste('<a href = https://noticias.uol.com.br/saude/ultimas-noticias/redacao/2020/04/24/em-meio-a-pandemia-bolsonaro-da-entrevista-com-aglomeracao-de-ministros.htm><br>Contariando OMS, Bolsonaro discursa </br> ao lado de mininstros promovendo aglomeração</a>'),
    xref = "x",
    yref = "y",
    arrowhead = 5,
    arrowhead = 3,
    arrowsize = 1,
    showarrow = TRUE,
    ax = -150,
    ay = -45
  ) %>%
  plotly::add_annotations(
    x = as.Date("2020-04-23"),
    y = 3331,
    text = paste('<a href = ""><br>Bolsonaro veta projeto que dispensava </br>trabalhadores de justificar faltas por 7 dias'),
    xref = "x",
    yref = "y",
    arrowhead = 5,
    arrowhead = 3,
    arrowsize = 1,
    showarrow = TRUE,
    ax = -145,
    ay = -45
  ) %>%
  plotly::add_annotations(
    x = as.Date("2020-03-12"),
    y = 0,
    text = paste('<br>Membro da comitiva</br>presidencial nos EUA</br><a href = "https://theintercept.com/2020/03/12/wajngarten-coronavirus-covid-bolsonaro/">testa positivo para COVID19</a>'),
    xref = "x",
    yref = "y",
    arrowhead = 5,
    arrowhead = 3,
    arrowsize = 1,
    showarrow = TRUE,
    ax = -150,
    ay = -180
  ) %>%
  plotly::add_annotations(
    x = as.Date("2020-04-25"),
    y = 4057,
    text = paste('<a = href = "https://exame.abril.com.br/brasil/governo-quer-flexibilizar-quarentena-mesmo-sem-ter-dados-sobre-leitos-e-testes/">Governo quer relaxar quarentena</a>'),
    xref = "x",
    yref = "y",
    arrowhead = 5,
    arrowhead = 3,
    arrowsize = 1,
    showarrow = TRUE,
    ax = -90,
    ay = -45
  ) %>%
  plotly::add_annotations(
    x = as.Date("2020-04-12"),
    y = 1223,
    text = paste('<br>Bolsonaro ignora dados:</br><a href = "https://www.em.com.br/app/noticia/politica/2020/04/12/interna_politica,1137976/bolsonaro-ignora-dados-esta-comecando-a-ir-embora-a-questao-do-virus.shtml">"Está começando a ir embora <br>a questão do vírus"</a>'),
    xref = "x",
    yref = "y",
    arrowhead = 5,
    arrowhead = 3,
    arrowsize = 1,
    showarrow = TRUE,
    ax = -100,
    ay = -120
  ) %>%
  plotly::add_annotations(
    x = as.Date("2020-04-11"),
    y = 1124,
    text = paste('<br><a href="https://g1.globo.com/jornal-nacional/noticia/2020/04/11/bolsonaro-descumpre-medidas-de-distanciamento-social-pelo-terceiro-dia-seguido.ghtml">Bolsonaro descumpre medidas</br>de distanciamento social</br>pelo terceiro dia seguido</a>'),
    xref = "x",
    yref = "y",
    arrowhead = 5,
    arrowhead = 3,
    arrowsize = 1,
    showarrow = TRUE,
    ax = 110,
    ay = 65
  ) %>%
  plotly::add_annotations(
    x = as.Date("2020-03-29"),
    y = 136,
    text = paste('<br>Bolsonaro diz cogitar decreto</br> para <a href="https://oglobo.globo.com/brasil/bolsonaro-diz-cogitar-decreto-para-toda-qualquer-profissao-voltar-ao-trabalho-1-24336772">"toda e qualquer profissão</br>voltar ao trabalho</a>”'),
    xref = "x",
    yref = "y",
    arrowhead = 5,
    arrowhead = 3,
    arrowsize = 1,
    showarrow = TRUE,
    ax = 10,
    ay = -130
  ) %>%
  plotly::add_annotations(
    x = as.Date("2020-04-17"),
    y = 2141,
    text = paste('<br>Bolsonaro defende</br>fim do isolamento:<a href ="https://www.correiobraziliense.com.br/app/noticia/politica/2020/04/17/interna_politica,845629/bolsonaro-fim-do-isolamento-se-agravar-vem-para-o-meu-colo.shtml"></br>"Se agravar vem pro meu colo"</a>'),
    xref = "x",
    yref = "y",
    arrowhead = 5,
    arrowhead = 3,
    arrowsize = 1,
    showarrow = TRUE,
    ax = 90,
    ay = 45
  ) %>%
  plotly::add_annotations(
    x = as.Date("2020-04-20"),
    y = 2587,
    text = paste('<br><a href="https://g1.globo.com/politica/noticia/2020/04/20/nao-sou-coveiro-ta-diz-bolsonaro-ao-responder-sobre-mortos-por-coronavirus.ghtml">"Não sou coveiro, ta?"</a></br>diz Bolsonaro ao responder</br>sobre mortos por coronavírus'),
    xref = "x",
    yref = "y",
    arrowhead = 5,
    arrowhead = 3,
    arrowsize = 1,
    showarrow = TRUE,
    ax = 90,
    ay = 45
  ) %>%
  plotly::add_annotations(
    x = as.Date("2020-04-28"),
    y = 5083,
    text = paste('<a href = ""><br>"E daí? Lamento,</br> quer que eu faça o quê?",</br></a>Bolsonaro sobre recorde</br> de mortos por coronavírus'),
    xref = "x",
    yref = "y",
    arrowhead = 5,
    arrowhead = 3,
    arrowsize = 1,
    showarrow = TRUE,
    ax = -120,
    ay = -45
  ) %>%
  plotly::layout(
    title = "Mortes por COVID19 no Brasil e postura de Jair Bolsonaro",
    yaxis = list(title = "Número cumulativo de mortes"),
    xaxis = list(title = "Data"),
    legend = list(x = 0.1, y = 0.9),
    hovermode = "compare"
  )







