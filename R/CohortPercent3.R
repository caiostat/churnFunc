
CohortPercent3 <- function(df, c, var, cols, cols2){
  x <- df %>% mutate(ChurnVoluntario = ifelse(MotivoCancelamento == "Voluntário", "Sim", "Não"),
                                     ChurnInadimplencia = ifelse(MotivoCancelamento == "Inadimplência", "Sim", "Não"))  %>%
    mutate(MesCriacao = as.yearmon(DataCriacao)) %>%
    group_by(!!!as_quosure(cols)) %>% dplyr::summarise(n=n()) %>%
    group_by(!!!as_quosure(cols2)) %>% mutate(Total = sum(n),Percentual = round((n/Total)*100,1))

  ggplot(x,aes_string(x = "MesCriacao",y="Percentual",fill=cols[3])) +
    geom_col() +
    facet_grid(reformulate(".",var)) +
    theme(legend.position = "bottom") +
    theme_few() +
    geom_text(data = x %>% filter(!! sym(cols[3]) != "Não"), aes(y=100 - Percentual, x=MesCriacao, label= Percentual)) +
    labs(title = paste(c))
}
