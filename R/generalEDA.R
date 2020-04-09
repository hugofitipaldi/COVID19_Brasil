# Primeiro vamos ver os diferentes dados que temos em mãos em relação ao COVID19 no Brasil

# Lendo as libraries necessárias
library(tidyverse)
library(rio)
library(DT)
library(fs)
library(coronavirus)


# John Hopkins
# Função para baixar os dados da John Hopkins atualizados (a cada 0.5 horas)

downloadGithubData <- function() {
  download.file(
    url      = "https://github.com/CSSEGISandData/COVID-19/archive/master.zip",
    destfile = "data/covid19_data.zip"
  )
  
  data_path <- "COVID-19-master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_"
  unzip(
    zipfile   = "data/covid19_data.zip",
    files     = paste0(data_path, c("confirmed_global.csv", "deaths_global.csv", "recovered_global.csv")),
    exdir     = "data",
    junkpaths = T
  )
}


updateData <- function() {
  # Download data from Johns Hopkins (https://github.com/CSSEGISandData/COVID-19) if the data is older than 0.5h
  if (!dir_exists("data")) {
    dir.create('data')
    downloadGithubData()
  } else if ((!file.exists("data/covid19_data.zip")) || (as.double(Sys.time() - file_info("data/covid19_data.zip")$change_time, units = "hours") > 0.5)) {
    downloadGithubData()
  }
}
# Update with start of app
updateData()

# Package coronavirus
data(coronavirus)

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







