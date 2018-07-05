TempoVida_Hist <- function(c){
  ggplot(ApolicesAuto %>% filter(MesCriacao > "Dec2016", !is.na(DataCancelamento), MotivoCancelamento %in% c), aes(x=TempoDeVida, fill=CS)) +
    geom_histogram(aes(y =..density..),position="identity",  breaks=seq(0, 200, by=10),alpha = .3) +
    theme_few() + theme(legend.position = "bottom") + labs(title = paste0("Tempo de Vida das Apólices Canceladas- categorias:",c)) +
    scale_y_continuous(labels = percent)
}


