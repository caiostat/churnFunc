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

CohortPercent <- function(df, c, var, cols, cols2){
  x <- df %>% mutate(ChurnAlvo = ifelse(MotivoCancelamento %in% c, "Sim", "Não"))  %>%
    mutate(MesCriacao = as.yearmon(DataCriacao)) %>%
    group_by(!!!as_quosure(cols)) %>% dplyr::summarise(n=n()) %>%
    group_by(!!!as_quosure(cols2)) %>% mutate(Total = sum(n),Percentual = round((n/Total)*100,1))

  ggplot(x, aes(y=Percentual, x=MesCriacao, fill = ChurnAlvo)) +
    geom_col() +
    facet_grid(reformulate(".",var)) +
    theme_few() +
    theme(legend.position = "bottom") +
    geom_text(data = x %>% filter(ChurnAlvo != "Não"), aes(y= Percentual, x=MesCriacao, label= Percentual)) +
    labs(title = paste(c, collapse = " e "))
}


