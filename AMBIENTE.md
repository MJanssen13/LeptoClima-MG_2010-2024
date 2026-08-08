# Ambiente de execucao

Versoes efetivamente utilizadas para produzir os resultados do estudo.
Registrado automaticamente; nao editar a mao.

- R: R version 4.6.0 (2026-04-24 ucrt)
- Plataforma: x86_64-w64-mingw32

## Pacotes

| Pacote | Versao |
|---|---|
| cowplot | 1.2.0 |
| dplyr | 1.2.1 |
| exactextractr | 0.10.1 |
| flextable | 0.9.12 |
| forcats | 1.0.1 |
| geepack | 1.3.13 |
| geobr | 2.0.0 |
| ggplot2 | 4.0.3 |
| ggspatial | 1.1.10 |
| grid | 4.6.0 |
| lmtest | 0.9.40 |
| lubridate | 1.9.5 |
| MASS | 7.3.65 |
| officer | 0.7.5 |
| patchwork | 1.3.2 |
| png | 0.1.9 |
| read.dbc | 1.2.0 |
| readxl | 1.4.5 |
| remotes | 2.5.0 |
| sandwich | 3.1.1 |
| scales | 1.4.0 |
| sf | 1.1.1 |
| sidrar | 0.2.9 |
| spatialreg | 1.4.3 |
| spdep | 1.4.2 |
| stats | 4.6.0 |
| terra | 1.9.27 |
| tidyr | 1.3.2 |

## Observacao sobre reprodutibilidade

Os scripts instalam pacotes ausentes em tempo de execucao
(`if (!requireNamespace(p)) install.packages(p)`), o que traz a versao
corrente do CRAN e nao a registrada acima. Para reproducao fiel, instale
as versoes desta tabela antes de executar o pipeline.
