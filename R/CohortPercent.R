#' @title Functions to help reporting churn
#'
#' @description function calculates the churn cohortwise
#'
#' @param c
#'
#' @return A bar graph describing churn patterns, discriminated by c.
#'
#' @examples
#'
#' @export CohortPercent


CohortPercent <- function(df, c, var){
  x <- df %>% filter(MotivoCancelamento %in% c) %>%
    mutate(MesCriacao = as.yearmon(DataCriacao)) %>%
    group_by(MesCriacao, var, CoreChurn) %>% dplyr::summarise(n=n()) %>%
    group_by(MesCriacao, var) %>% mutate(Total = sum(n),Percentual = round((n/Total)*100,1))

    ggplot(x, aes(y=Percentual, x=MesCriacao, fill = CoreChurn)) +
    geom_col() +
    facet_grid(var~.) +
    theme_few() +
    theme(legend.position = "bottom") +
    geom_text(data = x, aes(y=100 - Percentual, x=MesCriacao, label= Percentual)) +
    labs(title = paste(c))
}
