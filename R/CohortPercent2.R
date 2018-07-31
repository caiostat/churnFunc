CohortPercent2 <- function(df, c, var, cols, cols2){
  x <- df %>%
    mutate(MesCriacao = as.yearmon(DataCriacao)) %>%
    group_by(!!!as_quosure(cols)) %>% dplyr::summarise(ChurnAlvo=n()) %>%
    group_by(!!!as_quosure(cols2)) %>% mutate(Total = sum(ChurnAlvo),Percentual = round((ChurnAlvo/Total)*100,1))

  ggplot(x,aes_string(x = "MesCriacao",y="Percentual",fill=cols[3])) +
    geom_col() +
    facet_grid(reformulate(".",var)) +
    theme_few() +
    theme_few() +
    theme(legend.position = "bottom") +
    geom_text(data = x %>% filter(!! sym(cols[3]) != "Não"), aes(y=100 - Percentual, x=MesCriacao, label= Percentual)) +
    labs(title = paste(c))
  }
