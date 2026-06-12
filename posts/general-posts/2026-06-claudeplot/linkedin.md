# LinkedIn posts for claudeplot

Suggested image: `README-bar-1.png` or `README-palettes-1.png` (in this folder).

---

## English

🆕 New R package: claudeplot — built by Claude, about Claude, in (almost) a single prompt.

On the day Anthropic released Claude Fable 5, I ran an experiment: could the new model build a complete, real R package end to end?

The result is claudeplot, a ggplot2 extension that brings Anthropic's visual language to R:

📊 theme_claude(), a clean, publication-ready theme
🎨 12 palettes (qualitative, sequential, diverging) with discrete and continuous scales
🔤 Anthropic's recommended open fonts (Poppins + Lora) bundled and auto-registered

What impressed me wasn't the theme itself — I've written those by hand before. It was everything around it, which Claude also handled: font licensing checks, graceful fallbacks, testthat + vdiffr visual tests, a vignette, a pkgdown site, and a clean pass on R CMD check --as-cran with zero notes.

The whole thing came from one long prompt plus a few good references (Anthropic's published brand guidelines and some of my earlier theme packages). The lesson: the model multiplies the quality of what you hand it.

It's experimental and unofficial — a demonstration, not a battle-tested library. But it's real, documented, and installable:

pak::pak("viniciusoike/claudeplot")

🔗 GitHub: https://github.com/viniciusoike/claudeplot
📖 Docs: https://viniciusoike.github.io/claudeplot/

#RStats #DataVisualization #ggplot2 #AI #Claude #DataScience

---

## Português

🆕 Novo pacote de R: claudeplot — feito pelo Claude, sobre o Claude, em (quase) um único prompt.

No dia em que a Anthropic lançou o Claude Fable 5, fiz um experimento: será que o novo modelo consegue construir um pacote de R completo e real, de ponta a ponta?

O resultado é o claudeplot, uma extensão do ggplot2 que traz a linguagem visual da Anthropic para o R:

📊 theme_claude(), um tema limpo e pronto para publicação
🎨 12 paletas (qualitativas, sequenciais e divergentes) com escalas discretas e contínuas
🔤 As fontes abertas recomendadas pela Anthropic (Poppins + Lora) embutidas e registradas automaticamente

O que me impressionou não foi o tema em si — já escrevi vários à mão. Foi tudo o que vem ao redor, e que o Claude também resolveu: verificação de licença das fontes, fallbacks elegantes, testes com testthat + vdiffr, vignette, site pkgdown e aprovação no R CMD check --as-cran com zero notes.

Tudo isso saiu de um prompt longo e algumas boas referências (as diretrizes de marca publicadas pela Anthropic e alguns dos meus pacotes de temas anteriores). A lição: o modelo multiplica a qualidade do que você entrega a ele.

É um pacote experimental e não oficial — uma demonstração, não uma biblioteca testada em batalha. Mas é real, documentado e instalável:

pak::pak("viniciusoike/claudeplot")

🔗 GitHub: https://github.com/viniciusoike/claudeplot
📖 Documentação: https://viniciusoike.github.io/claudeplot/

#RStats #DataVisualization #ggplot2 #IA #Claude #DataScience
