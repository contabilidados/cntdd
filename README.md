# cntdd <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
[![R version](https://img.shields.io/badge/R-%3E%3D%203.5-blue)](https://cran.r-project.org/)
[![Functions](https://img.shields.io/badge/functions-60-informational)](https://github.com/contabilidados/cntdd)
[![Last commit](https://img.shields.io/github/last-commit/contabilidados/cntdd)](https://github.com/contabilidados/cntdd/commits)
[![GitHub stars](https://img.shields.io/github/stars/contabilidados/cntdd?style=social)](https://github.com/contabilidados/cntdd/stargazers)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

> **cntdd** é um pacote R desenvolvido pelo projeto **contabiliDados** (UFERSA) com utilitários para pesquisa, ensino e análise em contabilidade e finanças. O pacote reúne funções para cálculo de indicadores econômico-financeiros, análises gráficas, testes estatísticos, manipulação de dados cadastrais e integração com APIs públicas brasileiras.

---

## Instalação

### Versão de desenvolvimento (GitHub)

```r
# install.packages("remotes")
remotes::install_github("contabilidados/cntdd")
```

---

## Visão Geral

O `cntdd` organiza suas funções em quatro grupos principais:

| Prefixo | Grupo | Descrição |
|---------|-------|-----------|
| `ind_` | Indicadores | Cálculo e visualização de índices econômico-financeiros |
| `stat_` | Estatística | Testes estatísticos e medidas descritivas |
| `utl_` | Utilitários | Manipulação de dados, CNPJs e integração com APIs |
| `dt_` | Dados | Datasets de referência incluídos no pacote |

---

## Funções por Grupo

### 📊 Indicadores Econômico-Financeiros (`ind_`)

#### Análises Integradas

| Função | Descrição |
|--------|-----------|
| `ind_analisaDRE()` | Visualiza as etapas do lucro na DRE (Lucro Bruto → EBITDA → EBIT → NOPAT → Lucro Líquido → Lucros Retidos) |
| `ind_analiseCiclo()` | Representa graficamente os ciclos operacional e financeiro |
| `ind_analiseDupont()` | Desmembra o ROE pela análise Dupont (ROE = ROS × Giro × Endividamento) |
| `ind_analiseEfeitoTesoura()` | Analisa o efeito tesoura comparando a NCG e o saldo de tesouraria |
| `ind_analiseVH()` | Realiza análises vertical e horizontal das demonstrações contábeis |

#### Liquidez

| Função | Descrição |
|--------|-----------|
| `ind_liqCorrente()` | Liquidez Corrente (AC / PC) |
| `ind_liqSeca()` | Liquidez Seca ((AC − Estoques) / PC) |
| `ind_liqImediata()` | Liquidez Imediata (Disponível / PC) |
| `ind_liqGeral()` | Liquidez Geral ((AC + RLP) / (PC + PNC)) |

#### Endividamento e Estrutura de Capital

| Função | Descrição |
|--------|-----------|
| `ind_nivelEndividamento()` | Nível de endividamento (AT / PL) |
| `ind_composicaoEndividamento()` | Composição do endividamento (PC / PT) |
| `ind_partCapTerceiros()` | Participação de capital de terceiros (PT / PL) |
| `ind_imobilizacaoPL()` | Imobilização do patrimônio líquido |
| `ind_endividamentoFinanceiro()` | Endividamento financeiro líquido |
| `ind_passivoOneroso()` | Passivo oneroso (dívidas com encargos financeiros) |
| `ind_passivoFinanceiro()` | Passivo financeiro de curto e longo prazo |

#### Rentabilidade

| Função | Descrição |
|--------|-----------|
| `ind_roe()` | ROE — Retorno sobre o Patrimônio Líquido |
| `ind_roa()` | ROA — Retorno sobre o Ativo Total |
| `ind_roic()` | ROIC — Retorno sobre o Capital Investido |
| `ind_ros()` | ROS — Margem Líquida (Retorno sobre as Vendas) |
| `ind_giro()` | Giro do Ativo (Receita / AT) |
| `ind_nopat()` | NOPAT — Lucro Operacional Líquido após Impostos |
| `ind_eva()` | EVA® — Economic Value Added (valor absoluto) |
| `ind_evaPercentual()` | EVA® Percentual em relação ao capital investido |

#### Custo de Capital

| Função | Descrição |
|--------|-----------|
| `ind_wacc()` | WACC — Custo Médio Ponderado de Capital |
| `ind_custoCapital()` | Custo do capital próprio e de terceiros |
| `ind_capitalInvestido()` | Capital Investido total na empresa |
| `ind_capitalOperLiquido()` | Capital Operacional Líquido |
| `ind_capitalCircLiquido()` | Capital Circulante Líquido (CCL = AC − PC) |

#### Análise Dinâmica (Modelo Fleuriet)

| Função | Descrição |
|--------|-----------|
| `ind_necessidadeCapGiro()` | NCG — Necessidade de Capital de Giro |
| `ind_saldoTesouraria()` | Saldo de Tesouraria |
| `ind_ativoCiclico()` | Ativo Cíclico (operacional de curto prazo) |
| `ind_ativoFinanceiro()` | Ativo Financeiro (errático) |
| `ind_passivoCiclico()` | Passivo Cíclico (operacional de curto prazo) |

#### Prazos Médios

| Função | Descrição |
|--------|-----------|
| `ind_pmrv()` | PMRV — Prazo Médio de Recebimento de Vendas |
| `ind_pmre()` | PMRE — Prazo Médio de Renovação de Estoques |
| `ind_pmpc()` | PMPC — Prazo Médio de Pagamento de Compras |
| `ind_cicloOperacional()` | Ciclo Operacional (PMRE + PMRV) |
| `ind_cicloFinanceiro()` | Ciclo Financeiro (CO − PMPC) |

---

### 📐 Estatística (`stat_`)

| Função | Descrição |
|--------|-----------|
| `stat_correl()` | Matriz de correlação de Pearson e/ou Spearman com significâncias |
| `stat_geomMean()` | Média geométrica |
| `stat_meanTest()` | Teste de comparação de médias (paramétrico e não-paramétrico) |
| `stat_testaAssociacao()` | Teste de associação entre variáveis categóricas |
| `stat_testaIndependencia()` | Teste de independência (qui-quadrado) |

---

### 🔧 Utilitários (`utl_`)

| Função | Descrição |
|--------|-----------|
| `utl_consultarCNPJ()` | Consulta dados cadastrais de CNPJs via API `minhareceita.org` |
| `utl_cnpjMascara()` | Aplica máscara de formatação ao CNPJ (XX.XXX.XXX/XXXX-XX) |
| `utl_limpaCNPJ()` | Remove formatação e caracteres especiais de CNPJs |
| `utl_baixaBCB()` | Baixa séries temporais da API do Banco Central do Brasil (SGS/BCB) |
| `utl_ajustaContaInflacao()` | Deflaciona valores de contas contábeis por um índice de inflação |
| `utl_createGroup()` | Cria agrupamentos personalizados em data frames |
| `utl_qdeNAcols()` | Conta a quantidade de valores `NA` por coluna |
| `utl_tableStat()` | Gera tabela de estatísticas descritivas formatada |
| `utl_t_starSig()` | Retorna símbolos de significância estatística (*, **, ***) |
| `robError()` | Tratamento robusto de erros em loops e pipelines |

---

### 📦 Datasets (`dt_`)

| Dataset | Descrição |
|---------|-----------|
| `dt_contabil` | Dados contábeis de exemplo para uso nas funções do pacote |
| `dt_cvmB3` | Cadastro de empresas listadas na B3 conforme a CVM |
| `dt_iseB3` | Empresas integrantes do Índice de Sustentabilidade Empresarial (ISE B3) |
| `dt_meses` | Tabela de referência de meses em português e inglês |
| `dt_ufRegiao` | Tabela de UFs brasileiras com suas respectivas regiões geográficas |

---

## Exemplos de Uso

### Analisar a DRE de uma empresa

```r
library(cntdd)

ind_analisaDRE(
  empresa    = "Empresa XYZ",
  periodo    = 2023,
  receita    = 800,
  custo      = 200,
  despSemDep = 150,
  depreciacao = 50,
  juros      = 30,
  ircsll     = 80,
  dividendos = 40
)
```

### Calcular e visualizar o ciclo financeiro

```r
ind_analiseCiclo(
  periodo        = 2021:2023,
  pmrv           = c(60, 65, 75),
  pmre           = c(45, 40, 20),
  pmpc           = c(50, 55, 100),
  periodoGrafico = 2023
)
```

### Análise Dupont

```r
ind_analiseDupont(
  intervalo         = 2020:2023,
  patrimonioLiquido = c(200, 210, 225, 240),
  ativoTotal        = c(500, 520, 540, 560),
  receita           = c(800, 820, 850, 900),
  lucroLiquido      = c(40, 45, 50, 55),
  apenasDados       = FALSE
)
```

### Baixar série do Banco Central (IPCA)

```r
# Código 433 = IPCA (índice geral)
ipca <- utl_baixaBCB(codSerie = 433, inicio = "01/01/2015", fim = "31/12/2023")
head(ipca)
```

### Consultar CNPJ

```r
utl_consultarCNPJ(cnpj_lista = "24529265000140")
```

### Deflacionar valores contábeis

```r
utl_ajustaContaInflacao(
  periodo        = 2019:2023,
  vrConta        = c(100, 110, 120, 130, 140),
  indiceInflacao = c(4.31, 4.52, 10.06, 5.79, 4.62),
  periodoBase    = 2023,
  apenasVetor    = TRUE
)
```

### Matriz de correlação

```r
stat_correl(
  dt           = dt_contabil[4:8],
  pearsonLower = TRUE,
  digits       = 2,
  decimal      = ","
)
```

---

## Dependências

O `cntdd` importa os seguintes pacotes:

`dplyr`, `ggplot2`, `tidyr`, `scales`, `lubridate`, `stringr`, `Hmisc`, `jsonlite`, `rstatix`, `nortest`, `sandwich`, `stats`, `CGPfunctions`, `ggpubr`, `ggrepel`, `pbapply`, `purrr`, `parallel`

---

## Sobre o Projeto contabiliDados

O **contabiliDados** é um projeto de pesquisa e extensão vinculado à [Universidade Federal Rural do Semi-Árido (UFERSA)](https://ufersa.edu.br/), com foco em análise de dados aplicada à contabilidade e finanças. O projeto explora técnicas de KDD (*Knowledge Discovery in Databases*) para transformar dados contábeis brutos em informações úteis ao processo decisório.

- 🌐 Blog: <https://contabilidados.com.br/>
- 📧 Email: <contabilidados@ufersa.edu.br>
- 📸 Instagram: [@contabilidados](https://www.instagram.com/contabilidados)
- 🐛 Reporte problemas: <https://github.com/contabilidados/cntdd/issues>

---

## Citação

Se você usar o `cntdd` em publicações científicas, por favor cite:

```bibtex
@misc{cntdd,
  author = {{contabiliDados - UFERSA}},
  title  = {cntdd: Utilitários para pesquisa em contabilidade e finanças},
  year   = {2024},
  url    = {https://github.com/contabilidados/cntdd}
}
```

---

## Licença

MIT © contabiliDados / UFERSA. Veja o arquivo [LICENSE](LICENSE) para detalhes.
