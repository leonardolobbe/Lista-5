equire(dplyr)
library(readr)
library(stringr)
require(tidyverse)
library(tibble)

dados <- readr::read_csv("Dados/chocolate.csv.gz")
str(dados)
#1)

##a)

numero_de_paises <- dados %>%
  summarise(num_paises = n_distinct(origem_cacau))
numero_de_paises
view(dados)
sum(dados$origem_cacau)

#b)

dados_filtrados <- dados %>%
  filter(ingredientes >= 3)
dados_filtrados %>% 
  glimpse()
count(dados_filtrados)

#c)

numero_de_chocolates_5_ingredientes <- dados %>%
  filter(str_count(ingredientes, ",") + 1 == 5) %>%  
  nrow()
numero_chocolates_5

#d)

numero_de_caracteristicas <- dados %>%
  filter(str_count(caracteristicas, ",") + 1 >= 4) %>%  
  nrow()
numero_de_caracteristicas

#e)

qtd_sal = dados %>% 
  filter(str_detect(ingredientes, "Sa")) %>%
  summarise(total = n())
qtd_sal

#f)

qtd_baunilha = dados %>% 
  filter(str_detect(ingredientes, "V")) %>%
  summarise(total = n())
qtd_baunilha

#g)

qtd_lecitina_baunilha <- dados %>%
  filter(str_detect(ingredientes, "L") & str_detect(ingredientes, "V")) %>%
  summarise(count = n())
qtd_lecitina_baunilha


######


#2)
artdt = readr::read_csv("Dados/Art.csv.gz")
art_moma =  readr::read_csv("Dados/Art_Moma.csv.gz")
view(art_moma)
view(artdt)
view(art2)


#a)Qual a média de exposições realizadas pelo MoMA e pelo Whitney por ano?

media_Whitney = mean(art_moma$whitney_count_to_year)
media_Moma = mean(art_moma$moma_count_to_year)
media_Moma
media_Whitney

#b)Qual a média de exposições realizadas pelo MoMA e pelo Whitney por ano para artistas de raça não branca?

art2 = inner_join(art_moma,artdt, by= "artist_unique_id")
not_white =  art2 %>%
  filter(artist_race_nwi == "Non-White")

media_total_exposicoes <- not_white %>%
  summarise(
    media_total_moma = mean(moma_count_to_year, na.rm = TRUE),
    media_total_whitney = mean(whitney_count_to_year, na.rm = TRUE)
  )
media_total_exposicoes

#c)Quais os quatro artistas com mais exposições realizadas pelo MoMA?

top_4_artist_moma = art2 %>%
  group_by(artist_name) %>%
  summarise(top_artist_moma = sum(moma_count_to_year, na.rm = TRUE)) %>%  #
  arrange(desc(top_artist_moma)) %>%  
  slice(1:4)
view(top_4_artist_moma)

#d)Do total de artistas, quantos são homens e quantos são mulheres?

#total de mulheres

artist_female = art2%>%
  group_by(artist_name) %>%
  filter(artist_gender %in% "Female")
nrow(artist_female)

#total de homens 
artist_male = art2%>%
  group_by(artist_name) %>%
  filter(artist_gender %in% "Male")
nrow(artist_male)


#e) Do total de artistas, qual as cinco nacionalidades predominante?

top_5_nacionalidade = art2 %>%
  group_by(artist_nationality) %>%
  summarise(count = n()) %>% 
  arrange(desc(count))  %>%
  slice(1:5)
view(top_5_nacionalidade)

#f) Dos artistas que expuseram no MoMA, quantos aparecem em cada livro? E dos que expuseram no Whitney?

#filtrando artistas que expuseram no MaMA

artist_moma = art2 %>%
  group_by(artist_name) %>%
  filter(moma_count_to_year != 0)
view(artist_moma)


#filtrando por livro

artist_moma_book = artist_moma %>%
  group_by(book) %>%
  summarise(count = n()) 
view(artist_moma_book)


#g)Qual a média de espaço ocupado por página de cada artista?

mean_space = art2 %>%
  group_by(artist_name) %>%
  summarise(media_espaço = mean(space_ratio_per_page_total))
view(mean_space)


######


#3)V
refugiados_Pais
refugiados1

#a)Qual a média de refugiados por país?
media_por_pais <- refugiados1 %>%
  group_by(id_origem) %>%
  summarise(Media_Refugiados = mean(refugiados))
view(media_por_pais)

#b)Quantos refugiados houveram saíndo do Afeganistão em 1990? E a partir de 2000?
#agrupando os refugiados do afeganistão
refugiados_afg = refugiados1 %>%
  filter(id_origem %in% "AFG") 
view(refugiados_afg)

sum_a_partir <- refugiados_afg %>%
  select(ano, refugiados) %>%
  filter(ano == 1990 | ano >= 2000) %>%
  sum(sum_a_partir)

#c)
#FILTRANDO
refugiados_2005 = refugiados1 %>%
  filter(ano == 2005)
#FAZENDO A MATRIZ
matriz_migracao <- refugiados_2005 %>%
  group_by(id_origem, id_destino) %>%  
  summarise(total_refugiados = sum(refugiados)) %>%  
  spread(key = id_destino, value = total_refugiados, fill = 0)
view(matriz_migracao)

#D)Qual o país que mais recebeu refugiados em 2005? E em 2010?

#FILTRANDO REFUGIADOS 2005 E 2010
refugiados_05_10 = refugiados1 %>%
  filter(ano %in% c(2005, 2010)) %>%
  group_by(ano, id_destino) %>%
  summarise(total_refugiados = sum(refugiados, na.rm = TRUE)) %>%
  ungroup()
view(refugiados_05_10)

#VENDO QUAL È O QUE MAIS RECEBEU:
#2005
mais_2005 <- refugiados_05_10 %>%
  filter(ano == 2005) %>%
  arrange(desc(total_refugiados)) %>%  
  slice(1)  

#2010
mais_2010 <- refugiados_05_10 %>%
  filter(ano == 2010) %>%
  arrange(desc(total_refugiados)) %>% 
  slice(1)  

mais_2005
mais_2010

#E)Quantos refugiados os 3 países que mais receberam refugiados em 2010 receberam em 2005?
#pais que mais teve refugiados 2010
top_3_2010 <- refugiados1 %>%
  filter(ano == 2010) %>%  
  group_by(id_destino) %>% 
  summarise(total_refugiados_2010 = sum(refugiados, na.rm = TRUE)) %>% 
  arrange(desc(total_refugiados_2010)) %>% 
  slice(1:3)
view(top_3_2010)


paises_top3 = c("PAK", "IRN", "SYR")

#SOMA DO TOP 3 EM 2005
total_top3 = refugiados_2005 %>%
  filter(id_destino %in% paises_top3)%>%
  group_by(id_destino) %>%
  summarise(sum_top3 = sum(refugiados, na.rm = TRUE))
view(total_top3)


