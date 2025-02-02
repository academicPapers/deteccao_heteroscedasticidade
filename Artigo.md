---
title: "Erros heteroscedástico-consistentes"
author:
- Luiz Fernando Palin Droubi^[SPU/SC, luiz.droubi@planejamento.gov.br]
- Norberto Hochheim^[UFSC, hochheim@gmail.com]
- Willian Zonato^[SPU/SC, willian.zonato@planejamento.gov.br]
date: "09/06/2019"
output:
  html_document:
    fig_caption: yes
    keep_md: yes
  word_document: default
  pdf_document:
    includes:
      in_header: preamble.tex
    keep_tex: yes
    latex_engine: xelatex
    number_sections: yes
    toc: no
classoption: a4paper, 12pt
documentclass: article
geometry: 
- left=3cm,right=2cm,top=1.9cm,bottom=1.5cm
- headheight=75pt
- includeheadfoot
link-citations: yes
linkcolor: red
urlcolor: magenta
citecolor: green
csl: ABNT_UFPR_2011-Mendeley.csl
subtitle: Teoria e simulações
bibliography: bibliography.bib
---



# INTRODUÇÃO

A homoscedasticidade é uma  das hipóteses que devem ser necessariamente verificadas na inferência clássica. A ausência da homoscedasticidade, ou seja, a heteroscedasticidade, no entanto, não invalida completamente um modelo de regressão linear, pois a heteroscedasticidade se relaciona apenas aos erros do modelo, o que afeta o cômputo dos intervalos de confiança das previsões e dos coeficientes do modelo, mas não invalida os valores centrais dos mesmos, ou seja, mesmo que um modelo contenha erros heteroscedásticos, este modelo pode ser utilizado para fazer previsões, desde que não se esteja interessado nos seus intervalos de confiança.
Na Engenharia de Avaliações, contudo, frequentemente ou quase sempre, existe interesse em avaliar o grau de precisão do modelo, ou mesmo o grau de fundamentação de um laudo. Dado que estes enquadramentos normativos são feitos com a utilização dos intervalos de confiança das estimativas, ou com os p-valores dos testes de hipótese realizado pelos regressores, a utilização de modelos com erros heteroscedásticos nesta área torna-se complicada, o que não significa que isto seja impossível.
Infelizmente a NBR14.653-2 não aborda este assunto, mas acredita-se que, mesmo assim, é possível se utilizar de modelos heteroscedásticos e ainda estar de acordo com a norma. Demonstrar como isso pode ser feito é o objetivo deste artigo.

# DESENVOLVIMENTO E FUNDAMENTAÇÃO

## A Hipótese de homoscedasticidade

Segundo Long [-@Long, 5], na regressão linear, a variância do estimador MQO $\hat{\beta}$ é:

$$var(\hat{\beta})=(X'X)^{-1}X' \Omega X(X'X)^{-1}$$

A adoção da hipótese da homoscedasticidade na inferência clássica, em conjunto com a hipótese da independência dos erros, pode ser resumida na equação abaixo:

$$\Omega_{MQO}=\sigma^2I$$
onde $\sigma^2$ é a variância dos erros do modelo, suposta constante, e $I$ é a matriz identidade $n \text{x}n$, onde $n$ é o número de dados do modelo. A matriz $\Omega$ assim obtida, pode-se demonstrar, simplifica demasiado o cálculo da matriz de variância-covariância dos coeficientes de estimação, da maneira a seguir:

$$var (\hat{\beta})=\sigma^2(X'X)^{-1}$$

## Detecção da heteroscedasticidade

Diversos testes foram desenvolvidos visando a detecção da heteroscedasticidade, dentro os quais citamos o teste de Goldfeld-Quandt [@GQ], o teste de Park [-@Park], o teste de Glejser [-@glejser], o teste de Breusch-Pagan [@BP] e o teste de White [-@white1980].

Deve-se salientar que nenhum dos testes mencionados pode ser considerado melhor do que o outro, porque, conforme será visto, cada um deles testa uma hipótese em particular para a estrutura do erro. O teste de Breusch-Pagan, por exemplo, assume que a estrutura da variância do termo de erro seja linear, sendo muito eficiente para detectar este tipo de heteroscedasticidade. Se, no entanto, se estiver lidando com uma estrutura não-linear do termo de erro, este teste não será eficaz, sendo o teste de White melhor para estes casos.

### Teste de Goldfeld-Quandt

O teste de Goldfeld-Quandt [@GQ] é aplicável a grandes amostras e consiste em omitir algumas amostras do modelo e comparar a estrutura da variância do termo de erro em diferentes subgrupos. 

### Teste de Park

O teste de Park [-@Park] assume que a variância do termo de erro é proporcional ao quadrado da variável independente. Desta maneira, o teste de Park consiste em estimar os resíduos da equação de regressão e então fazer uso de uma regressão destes resíduos em relação à variável dependente, da seguinte maneira:

$$ln(u_i^2) = ln(\sigma^2) + \beta ln(X_i) + v_i$$
Se o coeficiente $\beta$ for significante, é indicativo da presença de heteroscedasticidade.]

### Teste de Breusch-Pagan

O teste de Breusch-Pagan [@BP] consiste em assumir que a estrutura da variância do termo de erro seja linear em função das variáveis independentes do modelo. Assim, o método consiste em estimar a variância do erro à partir do quadrado dos resíduos da regressão linear e efetuar então uma regressão auxiliar deste termo em função dos regressores do mesmo modelo. Calcula-se o valor do coeficiente de determinação deste modelo auxiliar e aplica-se o teste do $\chi^2$ para obter a sua significância. @koenker1981 propõe um ajuste ao teste original, fazendo uso dos resíduos studantizados.

### Teste de White

O teste de White [-@white1980] consiste numa aplicação do teste de Breusch-Pagan, porém com a adição dos termos quadráticos e de interação do modelo original. Obviamente, o teste de White necessita de um número maios de graus de liberdade no modelo para ter eficácio, haja vista que a regressão auxiliar necessitará de diversos graus de liberdade.

## Alternativas para contornar a heteroscedasticidade

Ocorre que apenas raramente os dados do mundo real podem ser considerados homoscedásticos e independentes, o que faz com que procedimentos especiais tenham que ser tomados para a utilização do estimador MQO. A tentativa de se contornar a heteroscedasticidade pode ser feita de diversas formas, algumas mais trabalhosas, outras mais complexas, outras causando maior distorção ao modelo. 

## Transformação da variável dependente

Na Engenharia de Avaliações, a primeira e mais usual forma de se contornar a heteroscedasticidade é através da aplicação de transformações à variável dependente. No entanto, esta não é a única alternativa e a transformação dos dados pode causar distorções significativas, o que pode ser um problema, especialmente considerando o problema da retransformação das variáveis transformadas.

Segundo @matloff2017, se $\varphi(x)$ é uma função convexa, então a desigualdade de Jensen se exprime na seguinte desigualdade:

$\varphi \left(\mathbb{E} [X]\right)\leq \mathbb{E} \left[\varphi (X)\right].$

## Outros estimadores

Outra forma de contornar a heteroscedasticidade é através da utilização de outros estimadores, como o estimador dos mínimos quadrados ponderados. Este método consiste na adoção de um vetor de pesos conveninente escolhido e aplicado a cada uma das observações, de maneira que se elimine a heteroscedasticidade do modelo. O inconveniente deste método está na determinação do vetor de pesos a ser utilizado, o que não é trivial.

## Estimação heteroscedástico-consistente da matriz de variância-covariância

Uma maneira mais interessante de se detectar a heteroscedasticidade é através da estimação da matriz de variância-covariância dos coeficientes heteroscedástico-consistente, desfazendo-se assim, da hipótese da homoscedasticidade.

Existem diversas maneiras de se estimar a matriz heteroscedástico-consistente, sendo que este método foi originalmente concebido por White [-@white1980], da seguinte forma:

$$\text{HC0}=(X'X)^{-1}X' \hat{\Omega} X(X'X)^{-1} = (X'X)^{-1}X' diag[e_i^2] X(X'X)^{-1}$$
Onde $diag[e_i^2]$ é a matriz diagonal em que os elementos da diagonal são compostos pelo vetor do erros amostrais elevado ao quadrado.

Posteriormente diversas pequenas modificações foram propostas ao método, como a correção dos resíduos pelos graus de liberdade ($n/(n-k)$) e outras.

# ESTUDO DE CASO

## Exemplo 1

<img src="images/hetero-1.png" width="90%" style="display: block; margin: auto;" />

### Tesde de Breusch-Pagan


```r
bptest(fit, studentize = FALSE)
```

```
## 
## 	Breusch-Pagan test
## 
## data:  fit
## BP = 40.275, df = 1, p-value = 2.206e-10
```


### Teste de Breusch-Pagan studantizado (Koenker)


```r
bptest(fit)
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  fit
## BP = 18.864, df = 1, p-value = 1.404e-05
```

### Tesde de Goldfeld-Quandt


```r
gqtest(fit)
```

```
## 
## 	Goldfeld-Quandt test
## 
## data:  fit
## GQ = 6.9269, df1 = 48, df2 = 48, p-value = 2.203e-10
## alternative hypothesis: variance increases from segment 1 to 2
```

### Teste de White


```r
bptest(fit, ~X + I(X^2))
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  fit
## BP = 21.665, df = 2, p-value = 1.975e-05
```

## Exemplo 2

<img src="images/hetero2-1.png" width="90%" style="display: block; margin: auto;" />

### Tesde de Breusch-Pagan


```r
bptest(fit2, studentize = FALSE)
```

```
## 
## 	Breusch-Pagan test
## 
## data:  fit2
## BP = 71.838, df = 1, p-value < 2.2e-16
```


### Teste de Breusch-Pagan studantizado


```r
bptest(fit2)
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  fit2
## BP = 20.962, df = 1, p-value = 4.684e-06
```

### Tesde de Goldfeld-Quandt


```r
gqtest(fit2)
```

```
## 
## 	Goldfeld-Quandt test
## 
## data:  fit2
## GQ = 31.975, df1 = 48, df2 = 48, p-value < 2.2e-16
## alternative hypothesis: variance increases from segment 1 to 2
```

### Teste de White


```r
bptest(fit2, ~X + I(X^2))
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  fit2
## BP = 30.676, df = 2, p-value = 2.182e-07
```

## Exemplo 3

<img src="images/hetero3-1.png" width="90%" style="display: block; margin: auto;" />



### Tesde de Breusch-Pagan


```r
bptest(fit3, studentize = FALSE)
```

```
## 
## 	Breusch-Pagan test
## 
## data:  fit3
## BP = 0.068957, df = 1, p-value = 0.7929
```


### Teste de Breusch-Pagan studantizado


```r
bptest(fit3)
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  fit3
## BP = 0.021291, df = 1, p-value = 0.884
```

### Tesde de Goldfeld-Quandt


```r
gqtest(fit3)
```

```
## 
## 	Goldfeld-Quandt test
## 
## data:  fit3
## GQ = 1.0325, df1 = 99, df2 = 98, p-value = 0.4373
## alternative hypothesis: variance increases from segment 1 to 2
```

### Teste de White


```r
bptest(fit3, ~X + I(X^2))
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  fit3
## BP = 50.283, df = 2, p-value = 1.205e-11
```

# REFERÊNCIAS
