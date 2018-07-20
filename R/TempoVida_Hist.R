#' @title Functions to help reporting churn
#'
#' @description They let knitr script looks cleaner
#'
#' @param df, c, var.
#'
#' @return An histogram, discriminated by c.
#'
#' @examples
#'
#' @export TempoVida_Hist

TempoVida_Hist <- function(df, c, var){
  ggplot(df %>% filter(!is.na(DataCancelamento), MotivoCancelamento %in% c), aes_string(x="TempoDeVida", fill=var)) +
    geom_histogram(aes(y =..density..),position="identity",  breaks=seq(0, 200, by=10),alpha = .3) +
    theme_few() +
    theme(legend.position = "bottom") +
    labs(title = paste0("Tempo de Vida das Apólices Canceladas- categorias:",c)) +
    scale_y_continuous(labels = percent)
}


