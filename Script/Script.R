#Packages do Programa
packages <- c(
  "readr",
  "readxl",
  "janitor",
  "dplyr",
  "forcats",
  "stringr",
  "lubridate",
  "summarytools",
  "magrittr",
  "questionr",
  "knitr",
  "data.table",
  "writexl",
  "modelsummary")
lapply(packages, loadlibrary)

dadosGeraisMA = read.csv("SIVEP_MA.csv",sep = ";", dec = ".", header = T) #Inicialmente, pega todos os dados do Estado
dadosPedreiras <- subset(dadosGeraisMA, ID_MUNICIP == "PEDREIRAS") #É feita a seperação para o estudo de caso em Pedreiras
covidHospital = table(dadosPedreiras$HOSPITAL)
prop.table(dadosPedreiras$CS_RACA)
covid19 = table(dadosPedreiras$CLASSI_FIN)
names(dadosPedreiras) #Estudos das variáveis do DATASET

tabela_Idade = table(dadosPedreiras$NU_IDADE_N) #Cria a tabela dos infectados por Idade
tabela_Sexo = table(dadosPedreiras$CS_SEXO) #Cria a tabela dos infectados por Sexo (Masculino = 431, Feminino = 303)
tabela_Raca = table(dadosPedreiras$CS_RACA) #Cria a tabela dos infectados por Raça

#Abaixo, tabela de Fator de Risco
tabela_FatorDeRisco = table(dadosPedreiras$FATOR_RISC) #Cria a tabela dos infectados que apresentam ou não fator de risco
#Agora são os tipos de Fatores de Risco
tabela_Puerpera = table(dadosPedreiras$PURPERA) #Cria a tabela dos infectados com fator de risco - Puérpera
tabela_Cardiovascular = table(dadosPedreiras$CARDIOPATI) #Cria a tabela dos infectados com fator de risco - Doença Cardiovascular Crônica
tabela_Hematologica = table(dadosPedreiras$HEMATOLOGI) #Cria a tabela dos infectados com fator de risco - Doença Hematológica Crônica
tabela_Down = table(dadosPedreiras$SIND_DOWN) #Cria a tabela dos infectados com fator de risco - Síndrome de Down
tabela_Hepatica = table(dadosPedreiras$HEPATICA) #Cria a tabela dos infectados com fator de risco - Doença Hepática Crônica
tabela_Asma = table(dadosPedreiras$ASMA) #Cria a tabela dos infectados com fator de risco - Asma
tabela_Diabetes = table(dadosPedreiras$DIABETES) #Cria a tabela dos infectados com fator de risco - Diabetes mellitus
tabela_NeuroCronica = table(dadosPedreiras$NEUROLOGIC) #Cria a tabela dos infectados com fator de risco - Doença Neurológica Crônica
tabela_Pneumatopica = table(dadosPedreiras$PNEUMOPATI) #Cria a tabela dos infectados com fator de risco - Outra Pneumatopatia Crônica
tabela_Imunodeficiencia = table(dadosPedreiras$IMUNODEPRE) #Cria a tabela dos infectados com fator de risco - Imunodeficiência
tabela_Renal = table(dadosPedreiras$RENAL) #Cria a tabela dos infectados com fator de risco - Doença Renal
tabela_Obesidade = table(dadosPedreiras$OBESIDADE) #Cria a tabela dos infectados com fator de risco - Obesidade
tabela_Outros_Doencas = table(dadosPedreiras$OUT_MORBI) #Cria a tabela dos infectados com fator de risco - Outras Doenças
is.numeric(tabela_Renal)
#Abaixo, tabela dos tipos de sintomas
tabela_Sintomas_Febre = table(dadosPedreiras$FEBRE) #Cria a tabela para os sintomas de febre 
tabela_Sintomas_Tosse = table(dadosPedreiras$TOSSE) #Cria a tabela para os sintomas de Tosse 
tabela_Sintomas_Garganta = table(dadosPedreiras$GARGANTA) #Cria a tabela para os sintomas de Dor de Garganta
tabela_Sintomas_Dispneia = table(dadosPedreiras$DISPNEIA) #Cria a tabela para os sintomas de Dispneia
tabela_Sintomas_Desconforto = table(dadosPedreiras$DESC_RESP) #Cria a tabela para os sintomas de Desconforto Respiratório
tabela_Sintomas_Saturacao = table(dadosPedreiras$SATURACAO) #Cria a tabela para os sintomas de Saturação de O2 inferior a 95%
tabela_Sintomas_Diarreia = table(dadosPedreiras$DIARREIA) #Cria a tabela para os sintomas de Diarreia
tabela_Sintomas_Vomito = table(dadosPedreiras$VOMITO) #Cria a tabela para os sintomas de Vômito
tabela_Sintomas_DorAbdominal = table(dadosPedreiras$DOR_ABD) #Cria a tabela para os sintomas de Dor Abdominal
tabela_Sintomas_Fadiga = table(dadosPedreiras$FADIGA) #Cria a tabela para os sintomas de Fadiga
tabela_Sintomas_Perda_Olfato = table(dadosPedreiras$PERD_OLFT) #Cria a tabela para os sintomas de Perda de Olfato
tabela_Sintomas_Perda_Paladar = table(dadosPedreiras$PERD_PALA) #Cria a tabela para os sintomas de Perda de Paladar
tabela_Sintomas_Outros_Sintomas = table(dadosPedreiras$OUTRO_SIN) #Cria a tabela para os sintomas que não foram cadastrados


tabela_Vacina = table(dadosPedreiras$VACINA_COV) #Cria a tabela dos que receberam ou não vacina

tabela_Internados_UTI = table(dadosPedreiras$UTI) #Cria a tabela dos internados na UTI ou não
tabela_Internados_Suporte = table(dadosPedreiras$SUPORT_VEN) #Cria a tabela dos internados em UTI que usaram suporte ventilatório


#Os gráficos seguem abaixo
grafico_PorSexo = barplot(tabela_Sexo, #Gráfico de barras para a quantidades de casos por Sexo
                          beside = TRUE,
                          width = 0.5,
                          ylim = c(0, 600),
                          ylab = "Casos de COVID",
                          xlab = "Quantidade por Sexo",
                          col = c("purple", "darkblue"),
                          las = 1,
                          legend = c("Feminino", "Masculino"),
                          names = c("Feminino", "Masculino"),
                          main = "Quantidade de Casos de COVID por Sexo")

grafico_PorIdade = barplot(tabela_Idade, space = c(0.9), #Gráfico de barras para a quantidades de casos por Idade
                           ylab = "Casos de COVID por IDADE",
                           xlab = "Idade",
                           col = c("darkblue"),
                           ylim = c(0, 30),
                           las = 1,
                           main = "Quantidade de Casos de COVID por Idade")

grafico_PorRaca = barplot(tabela_Raca, space = c(0.9), #Gráfico de barras para a quantidade de casos por Raça
                          ylab = "Número de Casos",
                          xlab = "Identificador por Cor ou Raça",
                          col = c("white", "black", "yellow", "brown", "red"),
                          ylim = c(0, 600),
                          legend = c("1 - Branca", "2 - Preta", "3 - Amarela", "4 - Parda", "9 - Ignorado"),
                          names = c("Branca", "Preta", "Amarela", "Parda", "Ignorado"),
                          las = 1,
                          main = "Quantidade de Casos de Covid por Raça")

grafico_FatorDeRisco = barplot(tabela_FatorDeRisco, space = c(0.9), #Gráfico para apresentação de Fator de Risco
                               ylab = "Apresenta Fator de Risco",
                               xlab = "",
                               col = c("green", "red"),
                               ylim = c(0, 900),
                               legend = c("1 - Sim", "2 - Não"),
                               names = c("Sim", "Não"), 
                               las = 1,
                               main = "Apresenta Fator de Risco?")

grafico_FatorDeRisco_Cardiovascular = barplot(tabela_Cardiovascular, space = c(0.9), #Gráfico para Cardiovascular
                                              ylab = "Número de Casos",
                                              xlab = "",
                                              col = c("green", "red"),
                                              ylim = c(0, 200),
                                              legend = c("1 - Sim", "2 - Não"),
                                              names = c("Sim", "Não"),
                                              las = 1,
                                              main = "Doença Cardiovascular Crônica")

grafico_Sintomas_Febre = barplot(tabela_Sintomas_Febre, space = c(0.9), #Gráfico para o sintoma de Febre
                                 ylab = "Quantitativo do Sintoma",
                                 xlab = "",
                                 col = c("green", "red"),
                                 ylim = c(0, 400),
                                 legend = c("1 - Sim", "2 - Não"),
                                 names = c("Sim", "Não"),
                                 las = 1,
                                 main = "Sintoma - Febre")

grafico_Sintomas_Tosse = barplot(tabela_Sintomas_Tosse, space = c(0.9), #Gráfico para o sintoma de Tosse
                                 ylab = "Quantitativo do Sintoma",
                                 xlab = "",
                                 col = c("green", "red"),
                                 ylim = c(0, 400),
                                 legend = c("1 - Sim", "2 - Não"),
                                 names = c("Sim", "Não"),
                                 las = 1,
                                 main = "Sintoma - Tosse")

grafico_Sintomas_Garganta = barplot(tabela_Sintomas_Garganta, space = c(0.9), #Gráfico para o sintoma de Dor de Garganta
                                    ylab = "Quantitativo do Sintoma",
                                    xlab = "",
                                    col = c("green", "red"),
                                    ylim = c(0, 400),
                                    legend = c("1 - Sim", "2 - Não"),
                                    names = c("Sim", "Não"),
                                    las = 1,
                                    main = "Sintoma - Dor de Garganta")

grafico_Sintomas_Dispineia = barplot(tabela_Sintomas_Dispneia, space = c(0.9), #Gráfico para o sintoma de Dispineia
                                     ylab = "Quantitativo do Sintoma",
                                     xlab = "",
                                     col = c("green", "red"),
                                     ylim = c(0, 400),
                                     legend = c("1 - Sim", "2 - Não"),
                                     names = c("Sim", "Não"),
                                     las = 1,
                                     main = "Sintoma - Dispineia")

grafico_Sintomas_Desconforto = barplot(tabela_Sintomas_Desconforto, space = c(0.9), #Gráfico para o sintoma de Desconforto Respiratório
                                       ylab = "Quantitativo do Sintoma",
                                       xlab = "",
                                       col = c("green", "red"),
                                       ylim = c(0, 400),
                                       legend = c("1 - Sim", "2 - Não"),
                                       names = c("Sim", "Não"),
                                       las = 1,
                                       main = "Sintoma - Desconforto Respiratório")

grafico_Sintomas_Saturacao = barplot(tabela_Sintomas_Saturacao, space = c(0.9), #Gráfico para o sintoma de Saturação Baixa
                                     ylab = "Quantitativo do Sintoma",
                                     xlab = "",
                                     col = c("green", "red","yellow"),
                                     ylim = c(0, 120),
                                     legend = c("1 - Sim", "2 - Não", "9 - Ignorado"),
                                     names = c("Sim", "Não", "Ignorado"),
                                     las = 1,
                                     main = "Sintoma - Saturação de 02 inferior a 95%")

grafico_Sintomas_Diarreia = barplot(tabela_Sintomas_Diarreia, space = c(0.9), #Gráfico para o sintoma de Diarreia
                                    ylab = "Quantitativo do Sintoma",
                                    xlab = "",
                                    col = c("green", "red","yellow"),
                                    ylim = c(0, 60),
                                    legend = c("1 - Sim", "2 - Não", "9 - Ignorado"),
                                    names = c("Sim", "Não", "Ignorado"),
                                    las = 1,
                                    main = "Sintoma - Diarreia")

grafico_Sintomas_Vomito = barplot(tabela_Sintomas_Vomito, space = c(0.9), #Gráfico para o sintoma de Vômito
                                  ylab = "Quantitativo do Sintoma",
                                  xlab = "",
                                  col = c("green", "red","yellow"),
                                  ylim = c(0, 60),
                                  legend = c("1 - Sim", "2 - Não", "9 - Ignorado"),
                                  names = c("Sim", "Não", "Ignorado"),
                                  las = 1,
                                  main = "Sintoma - Vômito")

grafico_Sintomas_DorAbdominal = barplot(tabela_Sintomas_DorAbdominal, space = c(0.9), #Gráfico para o sintoma de Dor Abdominal
                                        ylab = "Quantitativo do Sintoma",
                                        xlab = "",
                                        col = c("green", "red","yellow"),
                                        ylim = c(0, 60),
                                        legend = c("1 - Sim", "2 - Não", "9 - Ignorado"),
                                        names = c("Sim", "Não", "Ignorado"),
                                        las = 1,
                                        main = "Sintoma - Dor Abdominal")

grafico_Sintomas_Fadiga = barplot(tabela_Sintomas_Fadiga, space = c(0.9), #Gráfico para o sintoma de Fadiga
                                  ylab = "Quantitativo do Sintoma",
                                  xlab = "",
                                  col = c("green", "red"),
                                  ylim = c(0, 100),
                                  legend = c("1 - Sim", "2 - Não"),
                                  names = c("Sim", "Não"),
                                  las = 1,
                                  main = "Sintoma - Fadiga")

grafico_Sintomas_Perda_Olfato = barplot(tabela_Sintomas_Perda_Olfato, space = c(0.9), #Gráfico para o sintoma Perda de Olfato
                                        ylab = "Quantitativo do Sintoma",
                                        xlab = "",
                                        col = c("green", "red","yellow"),
                                        ylim = c(0, 60),
                                        legend = c("1 - Sim", "2 - Não", "9 - Ignorado"),
                                        names = c("Sim", "Não", "Ignorado"),
                                        las = 1,
                                        main = "Sintoma - Perda do Olfato")

grafico_Sintomas_Perda_Paladar = barplot(tabela_Sintomas_Perda_Paladar, space = c(0.9), #Gráfico para o sintoma Perda de Paladar
                                         ylab = "Quantitativo do Sintoma",
                                         xlab = "",
                                         col = c("green", "red","yellow"),
                                         ylim = c(0, 60),
                                         legend = c("1 - Sim", "2 - Não", "9 - Ignorado"),
                                         names = c("Sim", "Não", "Ignorado"),
                                         las = 1,
                                         main = "Sintoma - Perda do Paladar")

grafico_Sintomas_Outros_Sintomas = barplot(tabela_Sintomas_Outros_Sintomas, space = c(0.9), #Gráfico para outros sintomas não cadastrados
                                           ylab = "Quantitativo do Sintoma",
                                           xlab = "",
                                           col = c("green", "red"),
                                           ylim = c(0, 15),
                                           legend = c("1 - Sim", "2 - Não"),
                                           names = c("Sim", "Não"),
                                           las = 1,
                                           main = "Sintoma - Outros Sintomas Foram Apresentados?")

grafico_Vacinados = barplot(tabela_Vacina, space = c(0.9), #Gráfico de Vacinados
                            ylab = "Número de Vacinados",
                            xlab = "",
                            col = c("green", "red", "yellow"),
                            ylim = c(0, 150),
                            legend = c("1 - Sim", "2 - Não", "9 - Ignorado"),
                            names = c("Sim", "Não","Ignorado"),
                            las = 1,
                            main = "Vacina")

grafico_Internados_UTI = barplot(tabela_Internados_UTI, space = c(0.9), #Gráfico de Internados na UTI
                                 ylab = "Número de Internados",
                                 xlab = "",
                                 col = c("green", "red", "yellow"),
                                 ylim = c(0, 200),
                                 legend = c("1 - Sim", "2 - Não", "9 - Ignorado"),
                                 names = c("Sim", "Não","Ignorado"),
                                 las = 1,
                                 main = "Internados na UTI")

grafico_Internados_Suporte = barplot(tabela_Internados_Suporte, space = c(0.9), #Gráfico de Internados na UTI com algum Suporte
                                     ylab = "Número de Internados com Suporte",
                                     xlab = "",
                                     col = c("green", "red", "yellow"),
                                     ylim = c(0, 50),
                                     legend = c("1 - Sim, Invasivo", "2 - Sim, não invasivo", "3 - Não"),
                                     names = c("Sim, Invasivo", "Sim, não invasivo", "Não"),
                                     las = 1,
                                     main = "Internados na UTI que usuaram algum tipo de Suporte")

#Evolução da COVID - LETRA B
#Por Semana - Quantidade de Casos
covidPorSemana = table(dadosPedreiras$HOSPITAL, dadosPedreiras$SEM_PRI)
quantidadeDeCasos <- covidPorSemana[1,]
semana<-c(1:45)
length(sem)
lentgth(porSemana2)
plot(semana, quantidadeDeCasos)

grafico_CovidPorSemana = barplot(covidPorSemana, #Gráfico por Semana
                                 beside = TRUE,
                                 width = 0.7,
                                 ylim = c(0, 40),
                                 ylab = "Quantidade de Casos de COVID",
                                 xlab = "Semanas",
                                 col = c("darkblue"),
                                 las = 1,
                                 main = "Quantidade de Casos por Semana")

#Por tipo - Curado, Óbito, Ignorado
evolucaoCovid = table(dadosPedreiras$EVOLUCAO)

grafico_evoludaoCovid = barplot(obitosCovid,
        beside = TRUE,
        width = 0.7,
        ylim = c(0, 600),
        ylab = "Quantidade por Tipo de Evolução",
        xlab = "",
        col = c("green", "red", "yellow"),
        legend = c("1 - Curado", "2 - Óbito", "9 - Ignorado"),
        names = c("Curado", "Óbito", "Ignorado"),
        las = 1,
        main = "Evolução da COVID")

#Óbitos por Semana
evolucaoCovid_PorSemana = table(dadosPedreiras$EVOLUCAO, dadosPedreiras$SEM_PRI)
quantitativoDaEvolucao <- evolucaoCovid_PorSemana[2,]
semanaEvolucao<-c(1:45)
length(semanaEvolucao)
length(quantitativoDaEvolucao)
plot(semanaEvolucao, quantitativoDaEvolucao)

grafico_Obitos_PorSemana = barplot(quantitativoDaEvolucao,
                                   beside = TRUE,
                                   width = 0.7,
                                   ylim = c(0, 15),
                                   ylab = "Número de Mortes",
                                   xlab = "Semanas",
                                   col = c("darkblue"),
                                   las = 1,
                                   main = "Óbitos por Semana")

#LETRA C
internacaoHospital = table(dadosPedreiras$HOSPITAL)
calculoPor100K = 100000
populacaoPedreiras = 39.153
calculoDeHospitalizaocao = (internacaoHospital / populacaoPedreiras) * calculoPor100K


obitosCalculo = 142
calculoDeObitospor100K = (obitosCalculo / populacaoPedreiras) * calculoPor100K

numeroDeCasos = 728
letalidadeCalculo = (obitosCalculo / numeroDeCasos) * 100
