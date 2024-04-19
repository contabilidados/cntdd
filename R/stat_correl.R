#' Correlacao de Pearson e Spearman
#'
#' @description
#' Gera matriz de correlacao de Pearson e/ou Spearman com suas respectivas
#' significancias estatisticas
#'
#' @details
#' Informacoes adicionais sobre como usar o pacote, orientamos acessar o menu
#' `cntdd` do Blog do Projeto contabiliDados: <http://contabilidados.com.br>.
#' Ao acessar, fazer busca pelo nome da funcao `utl_tStarSig`
#'
#' Contatos pelo email do Projeto contabiliDados:
#' Email: <contabilidados@@ufersa.edu.br>
#' Siga-nos no Instagram: <https://www.instagram.com/contabilidados> @contabilidados
#'
#' @param dt Um data.frame
#' @param pearsonLower Se `TRUE` ou `T`, a correlacao de Pearson e disposta na diagonal inferior da Matriz e a correlação de Spearman na diagonal superior. Se `FALSE` OU `F`, a disposicao das correlacoes e inversa. (Padrao: `T`)
#' @param ommitLower Se `TRUE` ou `T`, a diagonal INFERIOR e omitida. Se `FALSE` OU `F`, e mostrada. (Padrao: `F`)
#' @param ommitUpper Se `TRUE` ou `T`, a diagonal SUPERIOR e omitida. Se `FALSE` OU `F`, e mostrada. (Padrao: `F`)
#' @param digits Numero de digitos da correlacao (Padrao: `2`)
#' @param decimal Caracter para casa decimal (Padrao: `,`)
#'
#' @importFrom Hmisc rcorr
#' @import dplyr
#' @export

stat_correl <-
  function(
    dt = cntdd::dt_contabil[4:8],
    pearsonLower = T,
    ommitLower = F,
    ommitUpper = F,
    digits = 2,
    decimal = ","
  ){

    df <- na.omit(dt)

    corPearson <-
      rcorr(as.matrix(
        df), type = "pearson")

    pvPearson <-
      ifelse(corPearson$P <= 0.01, "***",
             ifelse(corPearson$P <= 0.05, "**",
                    ifelse(corPearson$P <= 0.10, "*", "")
             )
      ) %>% as.matrix()

    correl1 <-
      paste0(format(round(corPearson$r, digits), nsmall = digits,
                    decimal.mark = decimal), pvPearson) %>%
      matrix(nrow = dim(corPearson$r)[1])

    corSpearman <-
      rcorr(as.matrix(
        df), type = "spearman")

    pvSpearman <-
      ifelse(corSpearman$P <= 0.01, "***",
             ifelse(corSpearman$P <= 0.05, "**",
                    ifelse(corSpearman$P <= 0.10, "*", "")
             )
      ) %>% as.matrix()

    correl2 <-
      paste0(format(round(corSpearman$r, digits), nsmall = digits,
                    decimal.mark = decimal), pvSpearman) %>%
      matrix(nrow = ncol(df))

    matCorrel <- matrix(NA, ncol = ncol(df), nrow = ncol(df))

    diag(matCorrel) <- ""

    if(pearsonLower){
      matCorrel[lower.tri(correl1)] <- correl1[lower.tri(correl1)]
      matCorrel[upper.tri(correl2)] <- correl2[upper.tri(correl2)]
    }else{
      matCorrel[lower.tri(correl2)] <- correl2[lower.tri(correl2)]
      matCorrel[upper.tri(correl1)] <- correl1[upper.tri(correl1)]
    }

    rownames(matCorrel) <- names(df)
    colnames(matCorrel) <- names(df)


    if(ommitLower){
      matCorrel[lower.tri(matCorrel)] <- ""
    }

    if(ommitUpper){
      matCorrel[upper.tri(matCorrel)] <- ""
    }

    result <- as.data.frame(matCorrel)

    return(result)

  }


