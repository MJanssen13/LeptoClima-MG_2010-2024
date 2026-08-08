# RELATÓRIO EDITORIAL CONSOLIDADO
## Manuscrito RESS — "Fatores climáticos e socioambientais associados à incidência de leptospirose: estudo ecológico, Minas Gerais, 2010-2024"
Base: `D:\Pesquisas\Leptospirose\Resultados\Para envio\MANUSCRITO_RESS.docx`, `_rev2\manuscrito.txt`, `_rev2\modelo.txt`, `_rev2\tabelas.txt`, `_rev2\figuras.txt` e instruções on-line da RESS.

---

## 1. VEREDITO GERAL: **AJUSTES MAIORES** (não submeter na forma atual)

O manuscrito **não está pronto**, mas está longe de exigir refazer a pesquisa. Justificativa em três pontos:

**(a) Nenhum achado atinge a análise.** Foi verificado, rodando o próprio banco depositado, que as estatísticas centrais estão corretas: I de Moran bruto (0,2988 e 0,3382), I de Moran log (0,2337 e 0,2884), resíduos (0,1328 e 0,2146), escore de Heidke (0,083), sensibilidade e especificidade do alerta. Não há número inventado nem erro de cálculo. **Nenhuma correção deste relatório exige reanálise.**

**(b) Há, porém, erros objetivos de texto que contradizem os próprios dados.** O mais grave: "a associação estadual foi moderada (τ 0,45; p<0,001) e **mais intensa** na Metropolitana de Belo Horizonte (τ 0,39)... Sul/Sudoeste (τ 0,32)... Zona da Mata (τ 0,30)" — os três valores são **menores** que 0,45. A isso somam-se seis métricas do alerta reportadas com uma casa decimal que não corresponde ao resultado (46,0% em vez de 46,5%; 90,0% em vez de 89,5%; 29,0% em vez de 28,6%), um denominador mal atribuído, um I de Moran comparado entre escalas diferentes sem aviso, e um parágrafo inteiro de Resultados (validação preditiva) **sem método correspondente**. São itens que um parecerista atento detecta e que, isolados, sustentam devolução.

**(c) Três regras imperativas do modelo oficial estão descumpridas de forma verificável na triagem editorial:** limitações fora do 2º parágrafo da Discussão (estão no 7º de 8), Figura 2 citada antes da Figura 1, e siglas não explicadas no resumo.

**Custo estimado de conformidade: 1 a 2 dias de trabalho de redação e copidesque**, sem tocar em código nem em dados. Restrição operacional real: o corpo tem **3.458 palavras** (limite 3.500), ou seja, **margem de 42 palavras**, enquanto as correções obrigatórias somam cerca de **+200 palavras** — ver o orçamento na seção 5.

---

## 2. TABELA PRIORIZADA DE CORREÇÕES

Gravidade: **IMP** = impeditivo (erro factual ou regra imperativa; corrigir obrigatoriamente) · **MAI** = maior · **MEN** = menor/copidesque · **OPC** = opcional.
Custo: **min** = minutos · **bx** = baixo (até ~30 min) · **md** = médio (remanejar texto).
"Achados" remete à numeração da verificação adversarial.

### 2.1 Impeditivos

| Nº | Grav. | Custo | O que corrigir | Onde | Correção exata |
|---|---|---|---|---|---|
| 1 | IMP | min | Frase afirma o contrário dos dados: os τ mesorregionais citados são **menores** que o τ estadual da mesma frase | Resultados, 4º parágrafo (manuscrito.txt L77) e Tabela 1 | Trocar por: "Na série suavizada por quatro semanas, a associação estadual foi moderada (τ 0,45; p<0,001); entre as mesorregiões, os maiores valores ocorreram na Metropolitana de Belo Horizonte (τ 0,39), no Sul/Sudoeste de Minas (τ 0,32) e na Zona da Mata (τ 0,30)". Além disso, o τ 0,45 **não consta da Tabela 1** (não há linha "Minas Gerais"): ou acrescentar a linha estadual (semanal 0,32; 4 semanas 0,45), ou retirar a chamada "(Tabela 1)" desse valor. [45] |
| 2 | IMP | min | Seis métricas do alerta com decimal espúrio ",0" que não corresponde ao valor calculado | Resultados, último parágrafo (L85) | 46,0%→**46,5%**; 90,0%→**89,5%**; 10,0%→**10,5%**; 86,0%→**85,9%**; 29,0%→**28,6%**; "45,0% a 49,0%"→"**45,4% a 49,2%**". (Alternativa aceitável: reportar todos como inteiros; nunca com ",0".) [53] |
| 3 | IMP | bx | Parágrafo inteiro de Resultados sem método correspondente (STROBE 12a): não há definição de "semanas de maior incidência", partição treino/teste, gatilho, métricas nem n | Métodos estatísticos (L67) e Resultados (L85) | Acrescentar aos Métodos: "A validação preditiva fora da amostra usou treino até 2019 e teste de 2020 a 2024, em 12 mesorregiões × 261 semanas (n=3.133 região-semanas). Definiu-se semana de maior incidência como aquela com casos confirmados acima do percentil 80 da própria mesorregião no treino (185 semanas no teste). O gatilho foi a chuva acumulada em quatro semanas acima do percentil 80 (ou 90) climatológico da mesorregião, estimado apenas no treino, emitido duas semanas antes. Reportaram-se sensibilidade, especificidade, valor preditivo positivo e escore de habilidade de Heidke." Nos Resultados, **suprimir uma das duas métricas complementares** (taxa de falso-alarme = 1 − VPP; manter apenas o VPP) e informar o n do teste. [49] |
| 4 | IMP | bx | I de Moran comparado entre escalas diferentes sem declarar: 0,30/0,34 (escala original) no 5º parágrafo e "reduziu-se de 0,23" (escala log) no 6º — o leitor não tem como reconciliar | Resultados, 5º e 6º parágrafos (L79, L81); Métodos estatísticos | Não alterar números. No 5º: "I de Moran da incidência acumulada (escala original): 0,30 nos confirmados e 0,34 nos notificados (p=0,001)". No 6º: "Sobre a incidência log-transformada, o I de Moran passou de 0,23 para 0,13 nos confirmados e de 0,29 para 0,21 nos notificados após residualizar a taxa de confirmação". Acrescentar aos Métodos que a análise de confundimento por acesso ao diagnóstico foi feita em escala logarítmica. Corrigir também a Tabela S6 do repositório, que hoje emparelha bruto (escala original) com ajustado (escala log). [44] |
| 5 | IMP | md | Limitações no 7º de 8 parágrafos da Discussão. Regra literal (modelo, L89): "Apresentar as limitações do estudo, levando em consideração fontes potenciais de viés ou imprecisão **no segundo parágrafo**", reforçada em L91: "Após esclarecer as limitações da pesquisa, o texto da discussão deve ser guiado pelos achados" | Discussão (L101 → posição 2) | Mover integralmente o parágrafo "Entre as limitações, destacam-se..." (128 palavras) para a **2ª posição**, logo após o parágrafo-síntese. Levar para ele a ressalva "sensível à especificação do modelo" retirada do 1º parágrafo (item 16). O parágrafo de viés de averiguação (hoje 6º, 226 palavras) **pode permanecer** onde está — é discussão substantiva de achado, e sua fusão não é exigida pelo modelo; se permanecer, acrescentar às limitações a oração "inclui-se o possível viés de averiguação, discutido adiante". Manter o 8º parágrafo (89 palavras) como conclusão. [3, 20, 56] |
| 6 | IMP | md | Ilustrações citadas fora de ordem (Figura 2 antes da Figura 1) e texto circular (espacial → temporal → espacial). Regras: instruções on-line, "devem ser citadas no texto e enumeradas sequencialmente"; modelo L79, "sequência que evolua linearmente seguindo a ordem das ilustrações, evitar texto circular" | Resultados, parágrafos 3 a 5 (L75, L77, L79) | **Rota barata:** renumerar — o mapa de incidência acumulada passa a **Figura 1** e a série mensal a **Figura 2** (ajustar os dois arquivos de títulos e os nomes dos .png em `Resultados\Para envio`). **Rota melhor:** reordenar os Resultados para caracterização → tendência e sazonalidade (**citar aqui a figura da série mensal**, que hoje não é citada em nenhum parágrafo sobre sazonalidade) → associação temporal (Tabela 1; Tabela 2) → distribuição espacial (mapa) → aglomerados. Em qualquer rota, consolidar cada ilustração em um único parágrafo. [7, 19, 59] |
| 7 | IMP | bx | Siglas não explicadas no resumo (IC95%, IRR, I). Regras: modelo L11 — "sem siglas ou explicar siglas consagradas na primeira menção"; instruções — "devem ser explicadas na primeira menção **no resumo**, texto completo e cada tabela ou figura" | Resumo (L7) | **Solução que economiza palavras** (a margem de 6 palavras do resumo não comporta as expansões, que custam ~8-13): suprimir as siglas — "incidência acumulada de 11,1/100 mil habitantes (intervalo de confiança de 95% de 10,7 a 11,6)"; "a incidência foi 36,0% maior (razão de taxas de incidência 1,36)"; "o índice de Moran foi 0,30". Se optar por manter as siglas, expandir na primeira menção e **compensar encurtando a Conclusão do resumo** (hoje 62 palavras em duas orações encadeadas). [8, 16, 62] |

### 2.2 Maiores

| Nº | Grav. | Custo | O que corrigir | Onde | Correção exata |
|---|---|---|---|---|---|
| 8 | MAI | min | Resultado próprio, com valor-p, apresentado **apenas na Discussão** (9,4%; 32,9%; Spearman 0,77; p=0,005) | Discussão, 6º parágrafo (L99) → Resultados, 6º parágrafo (L81) | Transferir a medida: "A proporção de notificações confirmadas variou de 9,4% no Norte de Minas a 32,9% no Sul/Sudoeste de Minas, com correlação de Spearman de 0,77 (p=0,005) com a incidência entre as mesorregiões." Na Discussão, manter só a leitura interpretativa, sem números. [58] |
| 9 | MAI | bx | Afirmações nos Resultados sem dado numérico. Regra literal das instruções: "cada afirmação apresentada nos resultados deve ser acompanhada por dado numérico que a apoie, indicando a ilustração pertinente" | Resultados L73, L75, L79 | (a) "...a Metropolitana de Belo Horizonte, **com o maior número absoluto de casos (4.487 notificados; 655 confirmados)**, apresentou incidência de 9,9/100 mil habitantes"; (b) "com pico em janeiro e menor frequência em julho" — informar os percentuais e citar a figura da série mensal; (c) "A análise bivariada local evidenciou a coincidência espacial..." — informar o I bivariado da precipitação (0,24) e o número de municípios por quadrante significativo. [61] |
| 10 | MAI | min | Afirmação do resumo sem dado e sobre variável **não medida** ("municípios rurais"): o estudo não tem variável de ruralidade; a variável correlata medida (densidade demográfica) tem associação **positiva** (I 0,14) | Resumo (L7) | Substituir "com maior risco em municípios rurais menos vulneráveis" por: "e positivamente à densidade demográfica (I 0,14; p=0,001)". A leitura sobre transmissão rural já está na Discussão, corretamente qualificada como hipótese. [64] |
| 11 | MAI | md | Literatura citada sem caracterizar o estudo nem confrontar com os achados. Regra literal (modelo L91): "caracterizar o estudo citado (delineamento, pessoa, tempo e lugar)... ou evidenciar as discrepâncias". **Caso mais grave:** a ref. 25 (Teles et al., BMC Public Health 2023, RS 2010-2019, 4.760 casos) encontrou associação **positiva** entre vulnerabilidade social e incidência — direção **oposta** à deste estudo (I −0,20; β −0,26) — e o manuscrito apenas diz que o tema "também foi examinado" | Discussão, parágrafos 3, 5 e 6 (L93, L97, L99): refs. [25], [26], [27], [28] | Ref. 25: "**Em sentido oposto**, análise por regressão espacial dos municípios do Rio Grande do Sul, entre 2010 e 2019 (4.760 casos confirmados), encontrou associação **positiva** entre vulnerabilidade social e incidência [25]; a divergência pode decorrer do contraste climático interno a Minas Gerais, em que as áreas de menor vulnerabilidade coincidem com as mais úmidas, e da diferença entre os índices empregados." Aplicar o mesmo tratamento a [26], [27] (informar o desempenho preditivo do sistema argentino frente à sensibilidade de 46,5% e VPP de 10,5% aqui obtidos) e [28]. **Se não houver palavras para confrontar, suprimir a citação** em vez de mantê-la como mera menção. [39, 57] |
| 12 | MAI | bx | Ref. 14 funde dois produtos distintos do IBGE em título inexistente e com ano (2023) que não pode sustentar a estimativa de 2024 (divulgada em 29/8/2024). Afeta a rastreabilidade do **denominador de todas as incidências** | Referência 14 (L141) e Métodos, Variáveis e fontes (L49) | Substituir por: "Instituto Brasileiro de Geografia e Estatística. Estimativas da população residente para os municípios e para as unidades da federação brasileiros com data de referência em 1º de julho de 2024 [Internet]. Rio de Janeiro: IBGE; 2024 [cited 2026 Aug 7]. Available from: https://www.ibge.gov.br/estatisticas/sociais/populacao/9103-estimativas-de-populacao.html". Nos Métodos, discriminar as fontes efetivamente usadas (estimativas anuais; Censos 2010 e 2022; tabelas SIDRA de saneamento), **citando as tabelas SIDRA no corpo do texto** para não estourar o limite de 30 referências. [36] |
| 13 | MAI | bx | Títulos das ilustrações **divergem** entre o manuscrito e os arquivos de tabelas/figuras, inclusive no conteúdo e no n | `MANUSCRITO_RESS.docx` (L177-185), `Tabelas.docx`, `Figuras.docx` | Adotar **um único arquivo-fonte** e replicar. Decisões: Figura 3 → "**n=853 municípios**" nas duas versões (é a unidade mapeada); Figura 1 → "precipitação" nas duas; Tabela 1 → preservar "incidência acumulada (casos/100 mil habitantes; IC95%)" e o mesmo separador do n nas duas; Tabela 2 → adotar o título de `Tabelas.docx`, que cobre as **três** medidas realmente apresentadas (o do manuscrito omite a coluna "Erro espacial multivariável"). Acrescentar "(Tabela 2)" ao parágrafo do modelo de erro espacial nos Resultados, hoje sem chamada. [5, 22, 48, 54] |
| 14 | MAI | bx | Formato dos títulos: travessão no rótulo e vírgula antes do local, em vez de ponto e ponto seguido. Regras (modelo L141/L143 e instruções): "Após **ponto seguido**, informar o local, ano(s) e total de participantes"; exemplo "Tabela 3. Razões de prevalências (RP)... Local, ano (n=xx)" | As 5 ilustrações, nos 3 arquivos | Localizar-e-substituir "Tabela 1 – " → "Tabela 1. " (idem 2, e Figuras 1-3), e trocar a vírgula que antecede "Minas Gerais" por ponto seguido. Modelo: "Tabela 1. Casos notificados e confirmados de leptospirose, incidência acumulada (casos/100 mil habitantes; IC95%) e correlação de Kendall entre a incidência semanal e a precipitação, por mesorregião. Minas Gerais, 2010-2024 (n=11.542 notificados; n=2.314 confirmados)". **Manter a ausência de ponto final**, que já está correta nas cinco. [1, 21] |
| 15 | MAI | bx | Interpretação, conclusão e justificativa de método dentro dos Resultados. Regras: modelo L81 "apresentar dados, evitar extrapolações ou interpretações"; instruções "não faça inferências, interpretações ou comparações com a literatura" | Resultados L77 e L81 | (a) "os intervalos alargaram-se **sem alterar as conclusões**" → "os intervalos foram **mais amplos**"; (b) "ambas mantiveram **efeito independente**" → "No modelo com precipitação e temperatura, cada desvio-padrão de precipitação associou-se a incidência 36,0% maior..."; (c) mover para os Métodos a justificativa "para avaliar o confundimento por acesso ao diagnóstico", iniciando o parágrafo pelo dado. [12, 60] |
| 16 | MAI | min | 1º parágrafo da Discussão informa o que se fez. Regra literal (modelo L87): "não informar o que fez, interpretar o que encontrou" | Discussão, 1º parágrafo (L89) | Retirar "mesmo após o ajuste pela temperatura e pela sazonalidade" e "ainda que sensível à especificação do modelo" e reescrever em chave interpretativa: "Períodos mais chuvosos foram consistentemente seguidos de aumento da leptospirose em Minas Gerais, e as áreas mais úmidas do estado concentraram as maiores incidências, ainda que o padrão espacial se mostre menos estável que o temporal." Levar a ressalva metodológica para o parágrafo de limitações (que passa a ser o 2º). [13, 68] |

### 2.3 Menores / copidesque

| Nº | Grav. | Custo | O que corrigir | Onde | Correção exata |
|---|---|---|---|---|---|
| 17 | MEN | min | Nota da Tabela 2 sem "Nota(s):", sem letras sobrescritas e sem separação por ponto e vírgula (modelo L145 — as três exigências descumpridas) | `Tabelas.docx`, abaixo da Tabela 2 | "Notas: ᵃ p<0,001 para todas as razões de taxas de incidência; ᵇ p=0,001 para todos os coeficientes de Moran bivariado; ᶜ p=0,757 nos casos confirmados e p=0,830 nos notificados", ancorando ᵃ no cabeçalho da razão de taxas, ᵇ no cabeçalho de Moran e ᶜ na linha do abastecimento de água. **Não é necessário criar nota para a Tabela 1** (o modelo não obriga a existência de notas). [4, 26] |
| 18 | MEN | min | Dados numéricos alinhados à esquerda. Regra literal (modelo L155): "Alinhe os dados numéricos à direita" | `Tabelas.docx`: **todas** as células de dados da Tabela 2; colunas "Semanal, τ" e "4 semanas, τ" da Tabela 1 | Aplicar alinhamento à direita; manter à esquerda apenas as colunas "Mesorregião" e "Variável". [9] |
| 19 | MEN | bx | Cabeçalhos sem estatística/unidade e siglas não explicadas na ilustração (modelo L151/L153) | `Tabelas.docx`, cabeçalhos e linhas | Tabela 1: "Incidência/100 mil hab. (IC95%)" e "τ (valor-p)". Tabela 2: "Moran" → "**I de Moran bivariado**"; explicar IVS, IC95% e IRR no título (preferencial) ou em nota. Agrupar as três linhas de saneamento sob "**Saneamento (proporção de domicílios)**" e renomeá-las "Esgotamento sanitário adequado", "Abastecimento de água pela rede geral", "Coleta de lixo" — resolve de uma vez a unidade da linha, a hierarquia e a compatibilidade com os Métodos. [10, 23, 28, 29] |
| 20 | MEN | min | Notação de intervalos inconsistente na coluna "Erro espacial". Regra literal das instruções: "Separar intervalos por ponto e vírgula, e com espaço entre a pontuação e numeral subsequente: (IC95% 1,14; 2,23)" | `Tabelas.docx`, Tabela 2 | **Obrigatório:** "0,25 (0,07; 0,43)"; "−0,15 (−0,31; 0,00)"; "0,14 (0,01; 0,26)"; "−0,26 (−0,40; −0,11)". **Recomendado, sem base normativa:** remover os sinais "+", uniformizar o sinal negativo (usar sempre U+2212, hoje misturado com hífen na mesma linha) e grafar "0,00" em vez de "-0,00" na Tabela 1 (Noroeste de Minas). [11, 25] |
| 21 | MEN | min | Detalhes metodológicos nos títulos (modelo L141: "Não incluir detalhes metodológicos... do título") | Títulos da Tabela 1 e da Tabela 2, nos 2 arquivos | Retirar **apenas** "da regressão Binomial Negativa" e "com defasagem de duas semanas"/"defasada em duas semanas". **Manter** "razão de taxas de incidência", "correlação de Kendall" e "I de Moran bivariado" — são nomes de medida, cuja identificação o próprio modelo exige, e o exemplo oficial admite "Razões de prevalências (RP) brutas e ajustadas". Para a terceira medida, usar o cabeçalho "Coeficiente ajustado (IC95%)" com nota remetendo ao modelo de erro espacial descrito nos Métodos. [6, 21] |
| 22 | MEN | min | Percentuais sem a casa decimal exigida. Regra: "percentual 1 casa decimal" | 3 ocorrências: resumo e 5º parágrafo dos Resultados | "36%" → "**36,0%**" e "25%" → "**25,0%**". (Os 14 demais percentuais já estão corretos; "95%" e "5%" de confiança/significância não são percentuais de resultado.) [24] |
| 23 | MEN | min | "p<0,001" para estatísticas de Moran, inatingível por 999 permutações (piso 0,001) — os valores vêm de teste assintótico não declarado | Métodos estatísticos (L65) e Resultados (L81, L83) | Acrescentar aos Métodos: "A significância do I de Moran global foi avaliada por 999 permutações; para os testes sobre a incidência log-transformada, os resíduos e os indicadores locais submetidos à correção por comparações múltiplas, empregou-se o teste assintótico sob normalidade." Alternativa: uniformizar tudo em permutações e trocar "p<0,001" por "p=0,001". (Os "p<0,001" de Kendall são legítimos.) [27, 50] |
| 24 | MEN | min | Denominador da hospitalização mal atribuído (78,0% é 1.768/2.267, não sobre 2.314; sobre 2.314 seria 76,4%) | Resultados, 1º parágrafo (L71) | "a hospitalização ocorreu em 78,0% dos casos com informação (1.768/2.267)". Os demais percentuais descritivos podem permanecer: a Tabela suplementar 1 já traz a coluna "Registros com informação" e a nota sobre ausência de imputação. [52] |
| 25 | MEN | min | Tendência anual anuncia incidência mas apresenta contagens, e o ano citado (2020) é o de **pico**, não o terminal da série | Resultados, 2º parágrafo (L73) | "A incidência anual aumentou ao longo do período (Mann-Kendall τ 0,56; p=0,003), de X,X/100 mil habitantes em 2010 a Y,Y/100 mil habitantes em 2024, com pico em 2020 (216 casos)." Os valores estão no objeto `ac` do script `04_caracterizacao_tabelas.R`. Conferir a coerência com a menção à queda de 2021 nas limitações. [51] |
| 26 | MEN | min | "resultados **consistentes** nos casos notificados" para IRR 1,18 (1,16; 1,21) vs 1,36 (1,31; 1,41) — intervalos disjuntos | Resultados, 4º parágrafo (L77) e resumo | "com associações **na mesma direção, porém de menor magnitude**, nos casos notificados (Tabela 2)". No resumo, explicitar o desfecho: "nos casos confirmados, cada desvio-padrão...". Não repetir os IRR dos notificados no corpo (o modelo pede evitar repetição texto-ilustração). [46] |
| 27 | MEN | min | "54 municípios" sem indicar desfecho nem categorias (reconcilia, mas o leitor não descobre) | Resultados, 5º parágrafo (L79) | "Nos casos confirmados, 54 municípios apresentaram indicador local significativo (40 Alto-Alto e 14 Baixo-Alto); sob correção de Benjamini-Hochberg esse total reduziu-se a 23." Se quiser reportar os notificados: 88 significativos (61 Alto-Alto, 14 Baixo-Baixo, 13 Baixo-Alto). [47] |
| 28 | MEN | bx | Referências fora do padrão Vancouver/MEDLINE | Refs. 6, 7, 8, 10, 12, 13, 15, 20, 21, 24, 28 | **Ref. 12:** restituir "no período de" — "A influência da variabilidade da precipitação no padrão de distribuição dos casos de leptospirose em Minas Gerais, no período de 1998-2012". **Ref. 20:** ano incompatível (R 4.6.0 é de 24/4/2026) → "R Core Team. R: a language and environment for statistical computing [Internet]. Vienna: R Foundation for Statistical Computing; **2026** [cited 2026 Aug 7]. Available from: https://www.R-project.org/". **Ref. 15:** "...geobr: loads shapefiles of official spatial data sets of Brazil [Internet]. 2019 [cited 2026 Aug 7]. Available from: https://github.com/ipeaGIT/geobr" (**não** acrescentar "et al." — só dois autores formais no CRAN). **Ref. 21:** ver risco residual R1 antes de mexer. **Nomes MEDLINE:** "Martins MHDM"; "Latorre MDRDO"; "de Oliveira D"; "de Souza KOC"; "Wunder EA **Jr**"; "de Oliveira LM" e "de Masi E" — padronizar partículas em toda a lista (o manuscrito já grafa "Nery N Jr" nas refs. 4 e 8). **Ref. 13:** o STROBE não tem referência própria → "O relato seguiu a recomendação RECORD, extensão do STROBE para estudos com dados de saúde coletados rotineiramente [13]" (evita abrir a 31ª referência). **Ref. 9** (revisão sistemática internacional) não sustenta "estudos em centros urbanos... correlações fracas a moderadas" → deslocar: "...com correlações fracas a moderadas [7,8]. A magnitude dessa associação varia conforme características geográficas, climáticas e socioeconômicas locais [5,7,**9**]." [31, 34, 35, 37, 38, 40, 41, 42, 43] |
| 29 | MEN | bx | Quadro "Aspectos éticos" no layout de pesquisa **com** apreciação (tabela 4×2 com "Item/Informação" e linha de TCLE), quando o modelo prevê moldura de célula única para pesquisa **sem** apreciação (modelo L53-59) | `MANUSCRITO_RESS.docx`, única `<w:tbl>` | Converter em moldura de célula única, "Aspectos éticos" centralizado no topo e frase única: "Pesquisa não submetida a Comitê de Ética em Pesquisa por utilizar exclusivamente dados secundários, agregados, de domínio público e sem variáveis que permitam identificar os participantes, situação dispensada de apreciação nos termos das Resoluções CNS nº 466/2012 e nº 510/2016; não se aplica registro de consentimento livre e esclarecido." O conteúdo substantivo já está correto — trata-se só de reformatar. [2] |
| 30 | MEN | min | Siglas dispensáveis e estrangeirismo | Métodos L37, L63, L65 | "Reportou-se a razão de taxas de incidência (**RTI**)" — suprimir "incidence rate ratio". Se trocar IRR por RTI, aplicar às 11 ocorrências **e** aos 2 cabeçalhos da Tabela 2 (senão cria a inconsistência do item 13). Apagar "(LISA)", mantendo "Os indicadores locais de associação espacial identificaram aglomerados...". Grafar por extenso na forma usada pela própria revista: "Strengthening the Reporting of Observational Studies in Epidemiology (STROBE)" e "REporting of Studies Conducted using Observational Routinely-collected health Data (RECORD)" — custo ~18 palavras. [17, 18, 65] |
| 31 | MEN | bx | Estilo: topicalização, voz passiva, frases longas | Resultados L71, L79; Métodos L63-67; Resumo | Regra literal das instruções: "evite anunciar o tema no início da frase... prefira 'As mulheres foram maioria' a 'Em relação ao sexo, as mulheres foram maioria'". → "Declararam-se brancos 51,2% dos casos e pretos ou pardos, 48,1%." Idem "Entre os indicadores de saneamento, houve associação..." (que deve ainda **explicitar a direção**, não apenas informar que houve). Converter as ~8 apassivadoras dos Métodos ("Reportou-se", "aplicou-se", "Ajustou-se", "adotando-se") em construções diretas. Fatiar a frase de Métodos do resumo (82 palavras) em orações de uma ideia cada — **sem** ultrapassar 250 palavras. [63, 66, 67] |
| 32 | MEN | min | Versão do produto climático não declarada | Métodos, Variáveis e fontes (L49) | Após "[16]", acrescentar "(versão v_3.2.4)". A ref. 16 permanece inalterada: é a citação canônica do BR-DWGD e a v_3.2.4 cobre 2001-2025, ou seja, **todo** o período do estudo. [33] |

### 2.4 Opcionais

| Nº | Grav. | Custo | O que corrigir | Onde | Correção exata |
|---|---|---|---|---|---|
| 33 | OPC | min | Ausência da seção "Registro do protocolo" — **não é violação** (a exigência é condicionada a revisões sistemáticas e ensaios clínicos), apenas defesa contra triagem automatizada por seção faltante | Entre "Disponibilidade de dados" e "Uso de inteligência artificial generativa" | "Registro do protocolo: não se aplica; não houve registro prévio de protocolo, por se tratar de estudo ecológico com dados secundários de acesso público." ~20 palavras, **fora** do limite de 3.500. [30] |
| 34 | OPC | bx | Tópicos dos Métodos não reproduzem integralmente as denominações do STROBE (regra **recomendatória**: "Recomenda-se estruturar de acordo com os tópicos previstos no guia") | Métodos | Se houver folga de palavras: desdobrar "Variáveis e fontes de dados" em "Variáveis" e "Fontes de dados/mensuração"; renomear "Vieses" → "Viés"; criar "Variáveis quantitativas" reunindo o que já está escrito (padronização por desvio-padrão, densidade em escala log, soma móvel de 4 semanas, defasagem de 2 semanas). Custo ≈ 3 subtítulos. [32] |
| 35 | OPC | min | Descasamento de escala: a figura da série **mensal** é citada como suporte de correlações **semanais** com soma móvel de 4 semanas — e não é citada no parágrafo de sazonalidade, que é o que ela de fato ilustra | Resultados L73 e L77 | Já resolvido se adotada a rota melhor do item 6. Caso contrário: mover a chamada da figura para o parágrafo da sazonalidade. Sem base normativa obrigatória. [55] |

---

## 3. O QUE JÁ ESTÁ EM CONFORMIDADE

Verificado item a item, não é preciso mexer:

**Limites formais — todos cumpridos, dois deles no limite exato**
- Corpo da Introdução ao fim da Discussão: **3.458 palavras** (limite 3.500). Distribuição: Introdução 307, Métodos 1.123, Resultados 950, Discussão 1.074.
- Resumo: **244 palavras** (limite 250), estruturado com os quatro rótulos do modelo (Objetivo, Métodos, Resultados, Conclusão), com objetivo idêntico ao do corpo.
- **Exatamente 5 ilustrações** (2 tabelas + 3 figuras) e **exatamente 30 referências** — ambos os tetos da modalidade Artigo original.
- 5 palavras-chave (Leptospirose; Análise Espacial; Chuva; Incidência; Estudos Ecológicos).

**Referências**
- Sistema numérico Vancouver **entre colchetes**, em ordem de citação, sequencial de [1] a [30], sem lacunas.
- DOI presente em todas as referências que o possuem; periódicos com abreviação NLM correta.
- Ref. 16 (BR-DWGD) e Ref. 17 (Atlas do Ipea) estão **corretas** como estão: a versão v_3.2.4 do produto climático cobre 2001-2025, portanto não há lacuna 2021-2024; e o Atlas é monografia impressa (ISBN 9788578112554, 77 p.), não exigindo "[Internet]" nem data de acesso.

**Estrutura e seções obrigatórias**
- Ordem das seções conforme o modelo, incluindo "Disponibilidade de dados", que cumpre integralmente a exigência tríplice: declaração + **link do repositório** + citação no corpo + referência na lista.
- Declaração de uso de IA generativa presente, com os cuidados de acurácia de citações e originalidade exigidos pelo modelo.
- Aspectos éticos: o requisito **substantivo** está cumprido — a justificativa da ausência de apreciação cita as Resoluções CNS nº 466/2012 e nº 510/2016 e o TCLE. Só o layout precisa de ajuste (item 29).
- Métodos com subtópicos nomeados de guia de redação (Delineamento, Contexto, Participantes, Variáveis e fontes de dados, Vieses, Tamanho do estudo, Métodos estatísticos).
- Último parágrafo da Discussão curto (**89 palavras**) e conclusivo, conforme "O último parágrafo (curto e conciso) deve apresentar a conclusão".
- Primeiro parágrafo da Discussão cumpre metade da regra: **não repete números e não cita ilustrações**.

**Ilustrações**
- Nenhum dos 5 títulos termina com ponto — exigência expressa do modelo, cumprida.
- Dados complementares em coluna única, no formato pedido pelo modelo: "Incidência (IC95%)", "IRR (IC95%)".
- Sem colunas recíprocas redundantes.
- Tabela 1: colunas de N e Incidência já alinhadas à direita.

**Números e estilo**
- Casas decimais corretas em 14 dos 16 percentuais de resultado (20,0%; 82,0%; 76,4%; 51,2%; 48,1%; 9,8%; 42,6%; 32,7%; 26,7%; 25,3%; 9,4%; 32,9%...), medidas de associação com 2 casas e valores-p com 3.
- Nenhuma das expressões vedadas pelas instruções ("cerca de", "aproximadamente", "respectivamente", "Nesse sentido", "No que se refere à", "Contudo").
- As 783 semanas epidemiológicas dos Métodos estão corretas (2011, 2016 e 2022 com 53 semanas).

**Reprodutibilidade — verificada, não apenas alegada**
- Rodando o banco depositado, reproduziram-se: I de Moran bruto (0,2988/0,3382), log (0,2337/0,2884) e residualizado (0,1328/0,2146); a distribuição LISA (40 Alto-Alto + 14 Baixo-Alto = 54; 23 sob Benjamini-Hochberg); e **todas as métricas do alerta** — sensibilidade 46,5%, especificidade 75,1%, VPP 10,5% e HSS 0,083 a partir de TP=86, FP=734, FN=99, TN=2.214 (n=3.133 região-semanas). **O escore de Heidke e a coerência entre FAR e VPP estão corretos**; a suspeita de inconsistência entre essas métricas foi verificada e é improcedente.
- Coerência interna entre texto e Tabela 2 confirmada por reconstrução dos valores-p a partir dos IC (β 0,25 → p≈0,006; β −0,26 → p≈0,0004; β 3,56 com EP 2,05 → p≈0,083).
- Tabela suplementar 1 traz coluna explícita "Registros com informação" com todos os denominadores efetivos e nota sobre ausência de imputação — a promessa dos Métodos ("com os denominadores efetivos explicitados") está cumprida, salvo a única frase do item 24.

---

## 4. RISCOS RESIDUAIS — DEPENDEM DE DECISÃO DO AUTOR

**R1. Anonimato do repositório × completude da referência 21 (decisão com conflito interno).**
O DOI 10.5281/zenodo.21294415, citado em "Disponibilidade de dados", **resolve publicamente para os criadores** "Janssen, Miguel Antonio; Resende, Eduardo Soares; Fernandes, Ana Paula" (confirmado na API DataCite). A RESS usa avaliação duplo-cega. Porém: (i) a lista taxativa de itens vedados no manuscrito é "Nome e ORCID iD; Afiliação; Créditos de autoria; Financiamento; Agradecimentos; Conflito de interesses" — **não inclui link de repositório**; (ii) o modelo **ordena** incluir "o link do repositório, com a devida citação no corpo do manuscrito"; (iii) hoje a ref. 21 está sem autores, o que preserva a cegueira. Há portanto **conflito direto** entre o item 28 (completar a autoria da ref. 21, exigência Vancouver) e a proteção do duplo-cego.
**Recomendação:** manter a seção e o link; **blindar o depósito** durante a avaliação (anonymous review link do Zenodo, ou versão do registro sem os nomes dos criadores); **manter a ref. 21 sem autoria até a aceitação** e completá-la depois. Plano B, a combinar com o editor: "o DOI foi informado ao editor" — mas isso contraria a literalidade do modelo. A remoção do acrônimo "LeptoClima-MG" é prudencial e opcional.

**R2. Rota de correção da ordem das ilustrações (item 6).**
Renumerar é barato mas obriga a renomear os arquivos de imagem em `Resultados\Para envio` e reeditar os dois arquivos de títulos, com risco de desalinhamento; reordenar os Resultados é mais caro, mas resolve simultaneamente a ordem, o texto circular e o parágrafo de sazonalidade órfão de ilustração. **Escolher uma e aplicá-la por inteiro** — meia correção deixa o pacote pior do que está.

**R3. Orçamento de palavras (ver seção 5).** As correções obrigatórias somam ~200 palavras contra margem de 42. O autor precisa decidir **o que cortar**. Isso não é opção: sem decisão explícita, o manuscrito estoura o limite de 3.500 e é devolvido na triagem.

**R4. Destino do parágrafo de validação preditiva.** Alternativa ao item 3: deslocar toda a validação do alerta para o material suplementar, mantendo nos Resultados uma frase-síntese. Economiza ~90 palavras nos Métodos e ~60 nos Resultados, mas enfraquece uma das duas "questões em aberto" anunciadas na Introdução ("se alertas climáticos têm capacidade preditiva"). Decisão de mérito, não de conformidade.

**R5. IRR ou RTI.** Se optar por RTI (mais coerente com a expansão em português e com o exemplo do modelo), a troca precisa ser **completa**: 11 ocorrências no texto, 1 no resumo e 2 cabeçalhos da Tabela 2. Meia troca gera inconsistência pior que o estrangeirismo. Não é obrigatório: IRR é sigla consagrada, e as instruções admitem siglas consagradas.

**R6. Linha "Minas Gerais" na Tabela 1.** Acrescentá-la resolve o item 1 e dá suporte ao τ 0,45, mas a Tabela 1 já tem 12 linhas de dados e 6 colunas; a alternativa (retirar a chamada "(Tabela 1)") é gratuita. Decisão do autor.

**R7. Sem folga estrutural.** Ilustrações (5/5) e referências (30/30) estão no teto. Qualquer pedido do parecerista por nova figura ou nova referência exigirá remover uma existente — vale antecipar quais são as candidatas a sacrifício (por exemplo, a ref. 9, cujo encaixe já é frouxo, e a ref. 29).

---

## 5. ORÇAMENTO DE PALAVRAS (obrigatório resolver antes de submeter)

| | Palavras |
|---|---|
| Corpo atual (Introdução → fim da Discussão) | **3.458** |
| Limite | 3.500 |
| **Margem** | **+42** |

**Acréscimos exigidos pelas correções obrigatórias**

| Item | Acréscimo |
|---|---|
| 3 — Métodos da validação preditiva | +85 |
| 11 — Caracterizar/confrontar [25], [26], [27], [28] | +50 |
| 23 — Declarar o teste assintótico | +30 |
| 9 — Números nos Resultados | +25 |
| 4 — Escala logarítmica do Moran | +20 |
| 30 — STROBE e RECORD por extenso | +18 |
| 32 — Versão v_3.2.4 | +3 |
| **Total** | **≈ +231** |

**Déficit a compensar: ≈ 190 palavras.** Candidatos verificados, por tamanho:

| Trecho | Palavras | Observação |
|---|---|---|
| Discussão, 6º parágrafo (viés de averiguação) | 226 | O mais longo do manuscrito; comporta corte de 60-80 sem perda de conteúdo, sobretudo após o item 8 transferir os números para os Resultados |
| Resultados, 5º parágrafo (Moran/LISA) | 221 | Repete valores já na Tabela 2, contra "evitar excesso de repetição entre texto do resultado e ilustrações" — 50-60 palavras recuperáveis |
| Discussão, 3º parágrafo (IVS) | 164 | 20-30 palavras |
| Resultados, 4º parágrafo (Kendall/Binomial) | 168 | O item 26 já elimina repetição; 15-20 palavras |
| Métodos, "Vieses" | 89 | Duplica o parágrafo de limitações da Discussão; comporta 30-40 palavras de corte mantendo o tópico STROBE |

Cortes disponíveis: **≈ 200 palavras**, suficientes. O item 33 (Registro do protocolo, ~20 palavras) fica **fora** da contagem, pois esta vai apenas da Introdução ao fim da Discussão.

---

## 6. ORDEM DE EXECUÇÃO SUGERIDA

1. **Bloco de 15 minutos, sem risco:** itens 1, 2, 22, 24, 25, 26, 27, 32 (erros factuais e numéricos — os de maior retorno por minuto gasto).
2. **Bloco de formatação de ilustrações, isolado nos 3 arquivos:** itens 13, 14, 17, 18, 19, 20, 21 — fazer de uma vez só, a partir de um **único arquivo-fonte de títulos**, para não recriar divergências.
3. **Bloco de referências:** itens 12 e 28 (ver R1 antes de tocar na ref. 21).
4. **Bloco de reescrita, com controle de palavras aberto:** itens 3, 4, 5, 6, 7, 8, 9, 10, 11, 15, 16, 23, 30, 31 — aplicar os cortes da seção 5 **na mesma passagem**, recontando ao final.
5. **Conferência final:** recontar Introdução→Discussão (≤3.500), resumo (≤250), ilustrações (5), referências (30), ordem de citação das ilustrações e ordem numérica das referências.