
# ---------------------------------------
# Ajustado
# VALIDACAO SERIES TEMPORAIS
# ---------------------------------------
 
# ----------------------------------------------------------------------------------------------------------------------------------------

  # PASSO 1 - INSTALANDO PACOTES
  
    library(openxlsx)
    library(tidyverse)
    #library(tseries) 
    #library(urca) 
    library(esquisse) # GERADOR DE CODIGO PARA GGPLOT - facilitador
    library(ggplot2)
    library(ggthemes)
    library(aTSA) # USAR PARA OS TESTES DE ESTACIONARIEDADE

# ----------------------------------------------------------------------------------------------------------------------------------------

  # PASSO 2 - CARREGANDO ARQUIVO
    
    indices <- list()
    
    for (i in 1:2) {
      indices[[i]] <- read.xlsx("dados/times_series.xlsx", sheet = i ,colNames = TRUE) %>%
        rename(data = Data,
               coluna1 = 'coluna1',
               coluna2 = 'coluna2',
               coluna3 = 'coluna3',
               coluna4 = 'coluna4',
               coluna5 = 'coluna5',
               coluna6 = 'coluna6',
               coluna7 = 'coluna7')
    }
    
# ----------------------------------------------------------------------------------------------------------------------------------------
    
  # PASSO 3 - CRIANDO A DIFERENCA ENTRE OS INDICES (ELETROBRAS E CONSTRUCAO)  
    
    diferenca_indices <- indices[[2]][2:length(indices[[2]])] - indices[[1]][2:length(indices[[1]])]
    
# ----------------------------------------------------------------------------------------------------------------------------------------
    
  # PASSO 4 - ANALISE GRAFICA
    
    #SEPARANDO DATA
    
      data <- indices[[1]][1] 
      data <- as.Date(data[,1], origin='1900-01-01')
      data <- as.data.frame(data)
      class(data)
  
    #data$data  #data <- indices[[1]][1] %>%   #mutate(data = as.Date(.$data, origin='1900-01-01') )   #class(data$data)   #data <- indices[[1]][1] %>%   #  mutate(data = as.data.frame(as.Date(.$data, origin='1900-01-01')))
  
    # CRIANDO LISTAS PARA ARMAZENAR AS SERIES E O GRAFICOS
    
      analise_grafica <- list()
      grafico <- list()
      
      vetor_tit <- c("coluna1",
                     "coluna2",
                     "coluna3",
                     "coluna4",
                     "coluna5",
                     "coluna6",
                     "coluna7")
      
      
    # CHUNK PARA GERAR OS GRAFICOS
      for (i in 1:length(diferenca_indices)) {
        analise_grafica [[i]] <- cbind(data,diferenca_indices[,i])
        
        grafico [[i]] <- ggplot(analise_grafica [[i]]) + 
          aes(x = data, y = diferenca_indices[,i]) +
          geom_line(size = 1.38, colour = "#0c4c8a") +
          theme_gdocs()+
          labs(title = paste0(vetor_tit[i]))+
          labs(x = "Data", y = "I(AGIS) - I(OH)") +
          ylim(min(diferenca_indices[,1]) - 0.030, max(diferenca_indices[,1]) + 0.030) +
          #ylim(-0.05, 0.05) +
          theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5))+
          theme(axis.text=element_text(size=16),
                axis.title=element_text(size=16,face="bold"))+
          theme(plot.title = element_text(hjust = 0.5, color = "black"),
                axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                #axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
                axis.title.x = element_blank())
        
        ggsave (grafico [[i]], 
                path = "Gráficos/",
                file = paste0("frequencia", i, ".png"), 
                width = 20, 
                height = 15, 
                units = "cm")
        
      }
      
      
# ----------------------------------------------------------------------------------------------------------------------------------------
      
  # PASSO 5 - AS SERIES SAO ESTACIONARIAS?
      
    # SERIES DITAS ESTACIONARIAS SAO AQUELAS QUE POSSUEM MEDIA E VARIANCIA CONSTANTE NO TEMPO, ALEM DA COVARIANCIA ENTRE DOIS PERIODOS DE TEMPO DEPENDE APENAS DA DEFASAGEM ENTRE OS PERIODOS E NAO O TEMPO EM QUE SE ENCONTRA.
  
    # P-VALOR < 0.05 - MENOR O P VALOR, MENOR A CHANCE DE SE ERRAR AO REJEITAR A HIPOTESE NULA
    adf_teste <- list()
    pp_teste <- list()
    
    # KPSS TEST P-VALOR > 0.1 - HIPOTESE NULA INVERTIDA
    kpss_teste <- list()
  
    
    adf_test <- sapply(diferenca_indices,stationary.test,method = "adf") # GERANDO VETORES E NAO LISTAS
    pp_test <- lapply(diferenca_indices,stationary.test,method = "pp")
    kpss_test <- lapply(diferenca_indices,stationary.test,method = "kpss") 

    # Aqui poderia ser retirado todos os valores dos testes e extraídos para o xlsx, mas será retirado apenas a estatística feita com lag 0.
    # Bastaria adf_teste[[i]] <- data.frame(adf_test[[i]],adf_test[[i]])
    
    ordem_adf <- c(1,4,7,10,13,16,19)
    
    for (i in 1:length(diferenca_indices)) {
      
      adf_teste[[i]] <- data.frame(adf_test[[ordem_adf[i]]][4],adf_test[[ordem_adf[i]]][7])
      pp_teste[[i]] <- data.frame(pp_test[[i]][4],pp_test[[i]][7])
      kpss_teste[[i]] <- data.frame(kpss_test[[i]][4],kpss_test[[i]][7])
      
    }
    
    # GERANDO SAIDAS EM .XLSX
   
    write.xlsx(adf_teste,"Output/ADF.xlsx", sheetName = c("coluna1",
                                                     "coluna2",
                                                     "coluna3",
                                                     "coluna4",
                                                     "coluna5",
                                                     "coluna6",
                                                     "coluna7"))
    
    write.xlsx(pp_teste,"Output/PP.xlsx", sheetName = c("coluna1",
                                                        "coluna2",
                                                        "coluna3",
                                                        "coluna4",
                                                        "coluna5",
                                                        "coluna6",
                                                        "coluna7"))
    
    write.xlsx(kpss_teste,"Output/KPSS.xlsx", sheetName = c("coluna1",
                                                            "coluna2",
                                                            "coluna3",
                                                            "coluna4",
                                                            "coluna5",
                                                            "coluna6",
                                                            "coluna7"))
    
# ----------------------------------------------------------------------------------------------------------------------------------------    
    
  # PASSO 6 - RESIDUOS SEGUEM PROCESSO DE RUIDO BRANCO? 
    
    # UMA SERIE E GERADA POR UM PROCESSO ESTOCASTICO DE RUIDO BRANCO (RANDOM WALK OU PROCESSO RANDOMICO OU PASSEIO ALEATORIO) QUANDO FORMADO POR UMA SEQUENCIA DE VARIAVEIS RANDOMICAS I.I.D (INDPENDENTE E IDENTICAMENTE DISTRUIBUIDA)
    # RUIDO BRANCO - MEDIA DOS ERROS E IGUAL A ZERO A VARIANCIA DOS ERROS E CONSTANTE, ERROS NAO CORRELACIONADOS
    
    # --------------- TESTE DE BARTLETT
    
    # TESTE PARA VERIFICAR RUIDO BRANCO
    
    # P-VALOR > 0.05 INDICA QUE NAO SE PODE REJEITAR A HIPOTESE QUE A VARIANCIA E IGUAL AO LONGO DA AMOSTRA
 
    bart <- data.frame()
    
    for (i in 2:length(indices[[1]])) {
      
      eletr <- data.frame(coluna1 = indices[[1]][i], elab = "ELE") 
      
      names (eletr) <- c("serviço","elab") 
      
      const <- data.frame(coluna1 = indices[[2]][i],elab = "CONST")
      
      names (const) <- c("serviço","elab")
      
      final <- rbind(eletr,const)
      
      bart_teste <- bartlett.test(serviço ~ elab, final)
      
      bart[i-1,1] <- vetor_tit[i-1]
      bart[i-1,2] <- bart_teste[1]
      bart[i-1,3] <- bart_teste[3]
      
    }
  
    write.xlsx(bart,"Output/teste_bart.xlsx")
  
    
  # --------------- TESTE DE PORTMANTEAU 
    
  # AUTOCORRELACAO ENTRE OS RESIDUOS  
  #TESTES PADROES DE PORTMANTEU SAO LJUNG-BOX E BOX-PIERCE
  # P-VALOR MENOR QUE ALFA SIGNIFICA REJEITAR A HIPOTESE NULA
  # P-VALOR MAIOR NAO REJEITA A HIPOTESE NULA
  
  # TESTE DE PORTMANTEAU - LJUNG BOX E BOX PIERCE
  # A HIPOTESE NULA E QUE OS RESIDUOS SEGUEM UM PROCESSO DE RUIDO BRANCO - A SERIE E FORMADA POR UM PROCESSO DE RUIDO BRANCO
  # VALORES DE P MAIORES QUE .05 SIGNIFICA DIZER QUE NAO E POSSIVEL REJEITAR A HIPOTESE NULA DE INDEPENDENCIA DAS SERIES, O QUE E INTERESSANTE, POIS INDICA UM RUIDO BRANCO
  
  
    
  # -------------------------- LJUNG BOX 
    
  # AUTOCORRELACAO ENTRE OS RESIDUOS  
  #ESTATISTICA QUI-QUADRADO
  
  teste_port <- data.frame()
  
  for (i in 1: length(diferenca_indices)) {
    
    Box_test <- Box.test(diferenca_indices[[i]], lag = 10, type = "Ljung-Box")
    
    teste_port [i,1] <- vetor_tit[i]
    teste_port [i,2] <- Box_test[1]
    teste_port [i,3] <- Box_test[3]
    
  }
  
  write.xlsx(teste_port,"Output/teste_ljung_box.xlsx")

  # -------------------------- BOX PIERCE - AUTOCORRELACAO ENTRE OS RESIDUOS
  
  
  teste_box_pierce <- data.frame()
  
  for (i in 1: length(diferenca_indices)) {
    
    Box__pierce_test <- Box.test(diferenca_indices[[i]], lag = 10, type = "Box-Pierce")
    
    teste_box_pierce [i,1] <- vetor_tit[i]
    teste_box_pierce [i,2] <- Box__pierce_test[1]
    teste_box_pierce [i,3] <- Box__pierce_test[3]
    
  }
  
  write.xlsx(teste_box_pierce,"Output/teste_box_pierce.xlsx")
  
  
  
  
# ----------------------------------------------------------------------------------------------------------------------------------------
 






