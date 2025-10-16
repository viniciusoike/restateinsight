# Writing Guidelines - restateinsight

**Last Updated:** 2025-10-15

This document outlines writing style guidelines for blog posts in both Portuguese and English. These guidelines are based on analysis of published posts and aim to maintain consistency while improving clarity and engagement.

---

## Table of Contents

1. [General Principles](#general-principles)
2. [Post Structure](#post-structure)
3. [Portuguese Writing Style](#portuguese-writing-style)
4. [English Writing Style](#english-writing-style)
5. [Code Presentation](#code-presentation)
6. [Mathematical Content](#mathematical-content)
7. [Data Visualization](#data-visualization)
8. [Common Pitfalls to Avoid](#common-pitfalls-to-avoid)
9. [Post Type Templates](#post-type-templates)
10. [Pre-Publication Checklist](#pre-publication-checklist)

---

## General Principles

### Core Philosophy

1. **Context before complexity** - Always explain "what" and "why" before "how"
2. **Progressive disclosure** - Start simple, gradually increase detail
3. **Practical over theoretical** - Use real examples, preferably Brazilian data
4. **Accessible rigor** - Be technically accurate but explain concepts clearly
5. **Show, then explain** - Demonstrate with examples before diving into theory

### Audience

- **Primary**: Data scientists, economists, R users interested in Brazil
- **Secondary**: Students learning data visualization and statistics
- **Assumption**: Basic R knowledge, but explain specialized concepts

### Tone

- **Formal but approachable** - Academic tone without being dry
- **Direct and concise** - Avoid unnecessary verbosity
- **Informative** - Focus on teaching, not impressing
- **Honest about limitations** - Point out caveats and edge cases

---

## Post Structure

### Standard Post Hierarchy

```markdown
---
title: "Clear, Descriptive Title"
date: "YYYY-MM-DD"
categories: ['category1', 'category2', 'category3']
description: "One-sentence summary that appears in listings"
image: "/static/images/thumbnails/image_name.png"
execute:
  warning: false
  message: false
---

# Main Title (H1)

[Opening paragraph: 2-3 sentences introducing the topic and its relevance]

## Context/Background (H2)

[Explain what the topic is and why it matters]

## Main Content (H2)

### Subsection (H3)

[Progressive explanations with code and visualizations]

## Conclusion/Summary (H2)

[Key takeaways and practical implications]

## Related Posts (H2)

- [Link to related post 1](url)
- [Link to related post 2](url)
```

### Introduction Patterns

**Good introduction structure:**
1. **Hook** - What is the topic? (1 sentence)
2. **Relevance** - Why does it matter? (1-2 sentences)
3. **Preview** - What will be covered? (1 sentence)

**Example (PT):**
```
O filtro de médias móveis serve para suavizar séries de tempo e encontrar
tendências nos dados. Este filtro é bastante simples e pode ser escalado
com facilidade para lidar com múltiplas séries de tempo. Neste post, vamos
demonstrar como aplicar médias móveis usando R.
```

**Example (EN):**
```
Demographic pyramids are widely used in demographic analysis to visualize
the structure of a population, providing valuable insights into trends like
population growth, aging, and gender distribution. In this tutorial, we will
demonstrate how to create demographic pyramids using R and the WPP 2024 dataset.
```

### Section Guidelines

**Use descriptive headers** - Headers should clearly indicate what's in each section
- ✅ "Estimating the Model"
- ✅ "Cleaning the Data"
- ❌ "Step 3"
- ❌ "Results"

**Paragraph length** - Keep paragraphs to 3-5 sentences maximum

**Transitions** - Use transitional phrases between sections:
- PT: "Agora vamos...", "O próximo passo é...", "Como vimos..."
- EN: "Now let's...", "The next step is...", "As we saw..."

---

## Portuguese Writing Style

### Tone and Voice

**Formal Academic Portuguese** with accessibility:
- Use 3rd person or 1st person plural ("vamos") for tutorials
- Avoid excessive formality - write as you would explain to a colleague
- Use technical terms but always define them first

### Sentence Structure

**Keep sentences concise:**
- ✅ "O PIR é uma razão simples entre o preço dos imóveis e a renda das famílias."
- ❌ "O PIR, que é um indicador bastante utilizado na literatura internacional para mensuração da acessibilidade financeira à moradia, consiste numa razão entre o preço dos imóveis e a renda das famílias."

**Break complex ideas into multiple sentences:**
```
O Brasil está num momento singular na sua transição demográfica. Ao mesmo
tempo em que a população está envelhecendo e menos pessoas estão nascendo,
temos a maior geração de jovens da nossa história. Com menos pessoas nascendo,
o país enfrenta o desafio de aproveitar ao máximo esta janela de oportunidade
demográfica.
```

### Common Phrases

**Introducing concepts:**
- "O conceito de X pode ser definido como..."
- "Como o nome sugere..."
- "Em termos simples..."
- "Grosso modo..."

**Providing context:**
- "Tipicamente..."
- "Na prática..."
- "Via de regra..."
- "Em geral..."

**Explaining steps:**
- "O primeiro passo é..."
- "Agora vamos..."
- "Para isso..."
- "Desta forma..."

**Pointing out caveats:**
- "Vale notar que..."
- "É importante lembrar que..."
- "Contudo..."
- "Evidentemente..."

### Lists and Enumeration

**Use numbered lists for sequential steps:**
```markdown
1. Usando um teste de raiz unitária, identifica se a série é não-estacionária.
2. Se a série for não-estacionária, tira a primeira diferença da série.
3. Avalia-se a FAC e FACP para propor um modelo de "ordem máxima".
```

**Use bullet points for non-sequential information:**
```markdown
- Média constante
- Variância finita e constante
- Autocovariância que depende somente do tamanho do intervalo
```

### Footnotes

Use footnotes for:
- Data source details
- Additional technical references
- Clarifications that would disrupt flow
- Alternative explanations

```markdown
O PIR indica, grosso modo, a quantidade de "anos de trabalho" que uma família
típica precisa investir[^1].

[^1]: Esta é uma simplificação. Na prática, famílias compram imóveis com
financiamento, o que altera significativamente o cálculo.
```

---

## English Writing Style

### Tone and Voice

**Professional but conversational:**
- Use 2nd person ("you") for tutorials to engage readers
- Use 1st person plural ("we") for demonstrations
- Be direct and clear

### Sentence Structure

**Favor active voice:**
- ✅ "We will demonstrate how to create demographic pyramids"
- ❌ "Demographic pyramids will be demonstrated"

**Front-load important information:**
- ✅ "Punchcard plots show the occurrence of paired discrete variables."
- ❌ "The occurrence of paired discrete variables can be shown using punchcard plots."

### Common Phrases

**Introducing concepts:**
- "A [concept] is a..."
- "Put simply..."
- "In essence..."
- "At its core..."

**Providing context:**
- "Typically..."
- "In practice..."
- "Generally speaking..."
- "As a rule..."

**Explaining steps:**
- "The first step is..."
- "Now let's..."
- "To do this..."
- "This allows us to..."

**Pointing out caveats:**
- "It's important to note that..."
- "However..."
- "Keep in mind that..."
- "Bear in mind..."

### Progressive Tutorial Language

Use progression markers:
- "The simplest way..."
- "A more sophisticated approach..."
- "For advanced users..."
- "Finally, we can..."

Example:
```markdown
The simplest way to make a punchcard plot is to use the `geom_count` function.

For more control, we can manually count occurrences and use `geom_point`.

Finally, for complex visualizations, we can add color mappings and custom scales.
```

---

## Code Presentation

### Code Block Principles

1. **Always explain before showing** - Never drop code without context
2. **Incremental complexity** - Show simple version first, add features gradually
3. **Complete examples** - Code should be runnable as-is
4. **Comment sparingly** - Use prose above/below instead of inline comments
5. **Show output** - Include results, plots, or tables after code

### Code Introduction Patterns

**Good code introductions:**
```markdown
O código abaixo importa os dados e calcula a média móvel:
```

```markdown
The following code creates a basic population pyramid:
```

```markdown
Para calcular o PIR, usamos a seguinte fórmula implementada em R:
```

### Code Chunk Options

**Standard setup for tutorials:**
```r
#| eval: true
#| echo: true
#| warning: false
#| message: false
```

**For output display:**
```r
#| fig-width: 8
#| fig-height: 6
#| fig-align: "center"
#| out-width: "90%"
```

**For code folding (when showing alternative approaches):**
```r
#| code-fold: true
```

### Progressive Code Examples

Show evolution of code:

```markdown
### The basics

O exemplo mais simples calcula uma média móvel de ordem 3:

```{r}
ma3 <- stats::filter(x, filter = rep(1/3, 3))
```

### Adding complexity

Para múltiplas séries, podemos usar `dplyr`:

```{r}
series |>
  group_by(series_id) |>
  mutate(trend = roll_mean(value, n = 12))
```
```

### Package Loading

**Load all packages at the start:**
```r
library(ggplot2)
library(dplyr)
library(sf)
```

**Use `import::from()` for single functions to avoid conflicts:**
```r
import::from(sidrar, get_sidra)
```

**Mention package installation:**
```r
# To install wpp2024:
# remotes::install_github("PPgp/wpp2024")
```

---

## Mathematical Content

### Formula Presentation

**Always explain in prose before showing formula:**
```markdown
O PIR é uma razão simples entre o preço médio dos imóveis e a renda anual
das famílias:

$$
\text{PIR} = \frac{\text{Preços}}{\text{Renda Anual}}
$$
```

**Define all variables:**
```markdown
$$
z_t = \sum_{j = -k}^{k}a_{j}y_{t+j}
$$

onde $z_t$ é a série filtrada, $y_t$ é a série original, $a_j$ são os pesos
e $k$ define a janela temporal.
```

### Mathematical Boxes

Use callout boxes or margin notes for definitions:

```markdown
::: {.callout-note}
## Definição: Estacionaridade

Uma série estacionária possui:

1. Média constante
2. Variância finita e constante
3. Autocovariância que depende apenas do intervalo temporal
:::
```

### Margin Notes (for Quarto)

```markdown
::: {.column-margin}
**Definição: PIR**

O Price-Income Ratio indica quantos anos de renda são necessários para
comprar um imóvel típico.
:::
```

---

## Data Visualization

### Narration Before Visualization

**Always set up what the reader will see:**

```markdown
O gráfico abaixo mostra a evolução do PIR em São Paulo por região.
Regiões centrais como Jardins apresentam os valores mais elevados,
indicando menor acessibilidade.
```

```markdown
The following visualization shows the age distribution across countries.
Japan exhibits the most aged population structure, while Pakistan shows
a young, growing population.
```

### After Visualization

**Interpret key patterns:**
```markdown
Como se vê no mapa, há uma clara divisão regional: o Sul e Sudeste
apresentam índices de envelhecimento mais elevados, enquanto o Norte
mantém população mais jovem.
```

### Data Source Attribution

**Always cite data sources:**
```markdown
- Dados: [Censo 2022 - IBGE](https://censo2022.ibge.gov.br/)
- Tipografia: [Open Sans](https://fonts.google.com/specimen/Open+Sans)
- Paleta: `viridis` package
```

### Color Choices

**Justify color decisions when relevant:**
```markdown
Regarding color choices for gender representation, modern data visualization
practices avoid traditional stereotypes. Following DataWrapper's recommendations,
we use green for males and purple for females.
```

**For choropleth maps, explain scale:**
```markdown
O mapa utiliza quebras de Jenks para discretizar os valores, agrupando
municípios com características similares.
```

---

## Common Pitfalls to Avoid

### ❌ Don't

1. **Assume knowledge** - Always define specialized terms
2. **Use jargon without explanation** - Explain acronyms at first use
3. **Write long paragraphs** - Keep to 3-5 sentences
4. **Show code without context** - Always explain what code does
5. **Skip data sources** - Always cite where data came from
6. **Use emojis casually** - Maintain professional tone
7. **Leave conclusions vague** - Be specific about takeaways
8. **Ignore limitations** - Point out caveats and edge cases

### ✅ Do

1. **Define before using** - "O PIR (Price-Income Ratio) é..."
2. **Provide examples** - Use concrete, relatable examples
3. **Break into chunks** - Use sections and subsections liberally
4. **Show output** - Include plots, tables, and results
5. **Credit sources** - Link to data sources and references
6. **Be honest** - "É difícil apurar a qualidade dos dados..."
7. **Summarize findings** - Clear conclusions with actionable insights
8. **Link related content** - Connect to other relevant posts

---

## Post Type Templates

### Tutorial Post (How-To)

```markdown
---
title: "[Action] in R" or "Como [Action] no R"
categories: ['tutorial-R', 'data-visualization', 'ggplot2']
---

# [Topic]

[1-2 paragraph introduction explaining what will be covered and why it's useful]

## Prerequisites

```{r}
library(package1)
library(package2)
```

## The Basics

[Simplest possible example with explanation]

## Adding Complexity

[Progressive enhancement of the basic example]

## Practical Example

[Real-world application with Brazilian data]

## Summary

[Key takeaways and when to use this technique]

## Related Posts

- [Link to related tutorial]
```

### Data Analysis Post

```markdown
---
title: "[Topic] no Brasil" or "[Topic] in Brazil"
categories: ['data-visualization', 'brazil', 'economics']
---

# [Topic]

[Introduction: What is being analyzed and why it matters]

## Context

[Background on the topic and data source]

## Data

[Description of data and any cleaning needed]

## Analysis

[Main findings with visualizations]

## Key Findings

- Finding 1
- Finding 2
- Finding 3

## Implications

[What do these findings mean? What should readers take away?]

## Data Sources

- [Source 1](url)
- [Source 2](url)
```

### Visualization Showcase

```markdown
---
title: "Visualizing [Topic]"
categories: ['data-visualization', 'ggplot2', 'maps']
image: "/static/images/thumbnails/viz_name.png"
---

# [Topic]

[Brief introduction to the visualization]

## The Visualization

[Main plot with detailed explanation]

## Design Choices

- **Color palette**: [Why this palette]
- **Typography**: [Font choices and reasoning]
- **Layout**: [Why this arrangement]

## Code

[Complete code to reproduce the visualization]

## Technical Notes

[Any caveats, alternative approaches, or edge cases]
```

### Econometric/Statistical Post

```markdown
---
title: "[Method] no R"
categories: ['econometria', 'time-series', 'tutorial-R']
---

# [Method]

## Theory

[Mathematical foundation with formulas explained]

## Implementation

[Step-by-step coding example]

## Diagnostics

[How to check if the method worked correctly]

## Practical Example

[Real application with interpretation]

## Summary

[When to use this method and its limitations]
```

---

## Pre-Publication Checklist

### Content Review

- [ ] **Title is descriptive and accurate**
- [ ] **Description/summary is one clear sentence**
- [ ] **Introduction explains what, why, and preview**
- [ ] **All technical terms are defined**
- [ ] **Formulas have variable definitions**
- [ ] **Code has prose explanations**
- [ ] **All code chunks run without errors**
- [ ] **Visualizations have clear captions**
- [ ] **Data sources are cited**
- [ ] **Conclusion summarizes key takeaways**

### Style Review

- [ ] **Paragraphs are 3-5 sentences**
- [ ] **Headers are descriptive**
- [ ] **Transitions between sections are smooth**
- [ ] **Tone is consistent (formal but accessible)**
- [ ] **No unnecessary jargon**
- [ ] **Active voice used where possible**
- [ ] **Portuguese or English is consistent throughout**

### Technical Review

- [ ] **All packages are loaded at the start**
- [ ] **File paths use `here::here()`**
- [ ] **Figures have appropriate size settings**
- [ ] **Code follows progressive complexity**
- [ ] **No hardcoded values without explanation**
- [ ] **Warning/message suppression is justified**

### Metadata Review

- [ ] **Categories are relevant and consistent**
- [ ] **Date is correct**
- [ ] **Thumbnail image exists and displays**
- [ ] **Execute options are set appropriately**
- [ ] **Draft status is set to `false`**

### Related Content

- [ ] **Links to related posts are included**
- [ ] **References are properly formatted**
- [ ] **External links open correctly**

---

## Quick Reference

### Good Opening Lines

**PT:**
- "Neste post, vou demonstrar..."
- "O conceito de [X] é fundamental para..."
- "[Technique] serve para..."
- "Uma das questões mais importantes em [topic] é..."

**EN:**
- "In this tutorial, we'll explore..."
- "Understanding [X] is essential for..."
- "[Technique] provides a way to..."
- "One of the key challenges in [topic] is..."

### Good Transitions

**PT:**
- "Agora vamos..."
- "O próximo passo é..."
- "Como vimos anteriormente..."
- "Para tornar o exemplo mais realista..."

**EN:**
- "Now let's..."
- "The next step is..."
- "As we saw earlier..."
- "To make this more practical..."

### Good Conclusions

**PT:**
- "Neste post, demonstramos como..."
- "As principais conclusões são..."
- "Este método é útil quando..."
- "Vale ressaltar que..."

**EN:**
- "In this post, we demonstrated how to..."
- "The key takeaways are..."
- "This method is useful when..."
- "It's important to note that..."

---

## Version History

- **2025-10-15**: Initial version based on analysis of published posts
