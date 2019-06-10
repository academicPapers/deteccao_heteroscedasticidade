
```{r}
dados <- centro_2015@data
dados$padrao <- factor(dados$padrao, levels = c("alto", "baixo", "medio"))
```

```{r}
fit <- lm(valor ~ I(area_total^2)  + quartos + suites +
            garagens + dist_b_mar + padrao, data = dados)
```

```{r}
summary(fit)
```

```{r, results='hide', out.width = '33%', fig.show='hold', fig.cap="Gráficos básicos do modelo"}
mplot(fit, level = 0.8)
```

```{r}
confint(fit, level = .8)
```

```{r}
fit1 <- update(fit, subset = -c(34, 39))
```

```{r}
summary(fit1)
```

```{r, out.width = '33%', fig.show='hold', fig.cap="Gráficos básicos do modelo"}
mplot(fit1, which = 1:7, level = .80)
```

```{r}
coeftest(fit1, vcov. = vcovHC, type = "HC0")
```

```{r}
lmtest::coefci(fit1, vcov. = vcovHC, type = "HC0", level = 0.80)
```


```{r}
(p <- predict.lm(fit1, predict.df = dados[52,], level = .80))
((p$ci.upper - p$ci.lower)/p$predicted.value)
```

