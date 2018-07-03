#' @title Functions to help reporting churn
#'
#' @description They let knitr script looks cleaner
#'
#' @param symbol
#'
#' @return NULL
#'
#' @examples
#'
#' @export CohortPercent

CohortPercent <- function(c){
  x <- ApolicesAuto[ApolicesAuto$MotivoCancelamento != c,] %>% mutate(MesCriacao = as.yearmon(DataCriacao)) %>%
    group_by(MesCriacao,CS,CoreChurn) %>% dplyr::summarise(n=n()) %>% group_by(MesCriacao,CS) %>% mutate(Total = sum(n),Percentual = round((n/Total)*100,1))
  ggplot(x %>% filter(MesCriacao>"Dec 2016"),aes(y=Percentual, x=MesCriacao, fill = CoreChurn)) + geom_col() + facet_grid(CS~.) +
    theme_few() +  theme(legend.position = "bottom") +
    geom_text(data = x %>% filter(MesCriacao>"Dec 2016",CoreChurn != "Não Core"),aes(y=100 - Percentual, x=MesCriacao, label= Percentual)) + labs(title = paste(c))
}
