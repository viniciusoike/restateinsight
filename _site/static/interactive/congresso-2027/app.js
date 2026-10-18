// One hemicycle per chamber, drawn from seat positions computed in R.
// - Seats are coloured by ideology, gender or race; holdover senators
//   (elected in 2022) are hollow.
// - Hovering a seat fills the member card; clicking pins it.
// - The state filter, the search box and hovering a legend item or a party
//   on the scale dim every seat that does not match.

const { updated, chambers, categories, parties, members } = window.CONGRESSO;

const state = {
  chamber: "camara",
  color: "category",
  uf: "",
  query: "",
  highlight: null, // function(member) -> bool, from legend or scale hover
  pinned: null,
};

// pt-BR formatting ---------------------------------------------------------

const locale = d3.formatLocale({
  decimal: ",",
  thousands: ".",
  grouping: [3],
  currency: ["R$ ", ""],
});
const fmtInt = locale.format(",d");
const fmtPct = locale.format(".1%");
const fmtScore = locale.format(".1f");

// Colours ------------------------------------------------------------------

const grey = "#B2AEA8";
const categoryColor = Object.fromEntries(categories.map((c) => [c.category, c.color]));

const palettes = {
  category: {
    label: "Ideologia",
    missing: "Sem partido",
    key: (d) => d.category,
    domain: categories.map((c) => c.category),
    color: (k) => categoryColor[k] ?? grey,
  },
  gender: {
    label: "Gênero",
    missing: "Sem informação",
    key: (d) => d.gender,
    domain: ["Feminino", "Masculino"],
    color: (k) => ({ Feminino: "#CF7126", Masculino: "#1E3A5F" })[k] ?? grey,
  },
  race: {
    label: "Cor/raça",
    missing: "Sem informação",
    key: (d) => d.race,
    domain: ["Branca", "Parda", "Preta", "Indígena", "Amarela"],
    color: (k) =>
      ({
        Branca: "#1E3A5F",
        Parda: "#CF7126",
        Preta: "#00605E",
        Indígena: "#D2A745",
        Amarela: "#407D51",
      })[k] ?? grey,
  },
};

const isHoldover = (d) => d.term === "2023–2031";
const fill = (d) => palettes[state.color].color(palettes[state.color].key(d));
const normalize = (s) =>
  (s ?? "").normalize("NFD").replace(/[̀-ͯ]/g, "").toLowerCase();

const current = () => members.filter((d) => d.chamber === state.chamber);

function matches(d) {
  if (state.uf && d.uf !== state.uf) return false;
  if (state.query) {
    const hay = normalize(`${d.name} ${d.full_name} ${d.label} ${d.party}`);
    if (!hay.includes(state.query)) return false;
  }
  if (state.highlight && !state.highlight(d)) return false;
  return true;
}

// Hemicycle ----------------------------------------------------------------

const width = 760;
const height = 430;
const radius = 340;
const cx = width / 2;
const cy = 380;
// Side padding so end-anchored party labels on the left fit inside the
// viewBox when the page is narrow or embedded in an iframe.
const padX = 80;

const svg = d3
  .select("#hemicycle")
  .append("svg")
  .attr("viewBox", [-padX, 0, width + 2 * padX, height])
  .attr("role", "img");

const gLabels = svg.append("g");
const gSeats = svg.append("g");
const total = svg.append("text").attr("class", "total").attr("x", cx).attr("y", cy - 18).attr("text-anchor", "middle");
svg.append("text").attr("class", "total-sub").attr("x", cx).attr("y", cy + 4).attr("text-anchor", "middle").text("cadeiras");

// Seat radius follows the spacing between rows, so both chambers fill the
// same arc.
const seatRadius = { camara: 6.6, senado: 15 };

function drawHemicycle() {
  const dat = current();
  const r = seatRadius[state.chamber];
  svg.attr("aria-label", `${chambers[state.chamber].label}: ${dat.length} cadeiras`);
  total.text(dat.length);

  gSeats
    .selectAll("circle")
    .data(dat, (d) => d.id)
    .join("circle")
    .attr("class", "seat")
    .attr("cx", (d) => cx + d.x * radius)
    .attr("cy", (d) => cy - d.y * radius)
    .attr("r", (d) => (isHoldover(d) ? r * 0.72 : r))
    .attr("fill", (d) => (isHoldover(d) ? "var(--bg)" : fill(d)))
    .attr("stroke", (d) => (isHoldover(d) ? fill(d) : null))
    .attr("stroke-width", (d) => (isHoldover(d) ? r * 0.38 : null))
    .attr("tabindex", 0)
    .attr("aria-label", (d) => `${d.name}, ${d.label}, ${d.uf}`)
    .on("mouseenter focus", (event, d) => {
      if (!state.pinned) renderCard(d);
    })
    .on("mouseleave blur", () => {
      if (!state.pinned) renderCard(null);
    })
    .on("click", (event, d) => {
      state.pinned = state.pinned?.id === d.id ? null : d;
      renderCard(state.pinned ?? d);
      updateDim();
    });

  // Party labels outside the arc, at the middle of each wedge.
  const minSeats = state.chamber === "camara" ? 10 : 3;
  const wedges = d3
    .rollups(
      dat,
      (v) => ({ n: v.length, angle: d3.mean(v, (d) => Math.atan2(d.y, d.x)) }),
      (d) => d.label,
    )
    .filter(([, v]) => v.n >= minSeats);

  gLabels
    .selectAll("text")
    .data(wedges, ([k]) => k)
    .join("text")
    .attr("class", "wedge-label")
    .attr("x", ([, v]) => cx + Math.cos(v.angle) * (radius + 16))
    .attr("y", ([, v]) => cy - Math.sin(v.angle) * (radius + 16))
    .attr("text-anchor", ([, v]) => (Math.cos(v.angle) > 0.15 ? "start" : Math.cos(v.angle) < -0.15 ? "end" : "middle"))
    .attr("dominant-baseline", "middle")
    .text(([k, v]) => `${k} ${v.n}`);

  updateDim();
}

function updateDim() {
  gSeats
    .selectAll("circle")
    .classed("dim", (d) => !matches(d))
    .classed("pinned", (d) => state.pinned?.id === d.id);
}

// Legend -------------------------------------------------------------------

function drawLegend() {
  const pal = palettes[state.color];
  const counts = d3.rollup(current(), (v) => v.length, pal.key);
  const items = [...pal.domain];
  if ([...counts.keys()].some((k) => !pal.domain.includes(k))) items.push(null);

  d3.select("#legend")
    .selectAll(".legend-item")
    .data(items, (k) => k)
    .join((enter) => {
      const item = enter.append("span").attr("class", "legend-item");
      item.append("span").attr("class", "sw");
      item.append("span").attr("class", "k");
      item.append("span").attr("class", "n");
      return item;
    })
    .classed("empty", (k) => !counts.get(k))
    .on("mouseenter", (event, k) => {
      state.highlight = (d) => (pal.domain.includes(pal.key(d)) ? pal.key(d) : null) === k;
      updateDim();
    })
    .on("mouseleave", () => {
      state.highlight = null;
      updateDim();
    })
    .call((item) => item.select(".sw").style("background", (k) => pal.color(k)))
    .call((item) => item.select(".k").text((k) => k ?? pal.missing))
    .call((item) =>
      item.select(".n").text((k) => {
        const n = k === null
          ? d3.sum([...counts].filter(([key]) => !pal.domain.includes(key)), ([, v]) => v)
          : counts.get(k) ?? 0;
        return n ? fmtInt(n) : "nenhum";
      }),
    );

  const notes = [];
  if (state.chamber === "senado") notes.push("Círculos vazados: senadores eleitos em 2022, com mandato até 2031.");
  if (state.chamber === "senado" && state.color === "race") notes.push("O Senado não publica cor/raça; os 27 eleitos em 2022 aparecem em cinza.");
  d3.select("#legend-note").text(notes.join(" "));
}

// Member card --------------------------------------------------------------

function renderCard(d) {
  const card = d3.select("#card");
  card.selectAll("*").remove();
  if (!d) {
    card.append("p").attr("class", "eyebrow").text(chambers[state.chamber].label);
    card
      .append("p")
      .attr("class", "empty")
      .text("Passe o mouse sobre uma cadeira para ver o parlamentar. Clique para fixar.");
    return;
  }

  const head = card.append("div").attr("class", "head");
  const initials = d.name
    .split(" ")
    .filter((w) => /^[A-ZÀ-Ú]/.test(w))
    .map((w) => w[0])
    .slice(0, 2)
    .join("");
  const avatar = head
    .append("div")
    .attr("class", "avatar")
    .style("background", categoryColor[d.category] ?? grey)
    .text(initials);
  if (d.photo) {
    avatar
      .append("img")
      .attr("src", d.photo)
      .attr("alt", `Foto de ${d.name}`)
      .on("error", function () {
        d3.select(this).remove();
      });
  }
  const who = head.append("div");
  who.append("p").attr("class", "name").text(d.name);
  if (d.full_name && d.full_name !== d.name) who.append("p").attr("class", "full").text(d.full_name);
  const tag = who.append("span").attr("class", "tag");
  tag.append("span").attr("class", "sw").style("background", categoryColor[d.category] ?? grey);
  tag.append("span").text(`${d.label} · ${d.uf}`);

  const rows = [];
  if (d.score != null) rows.push(["Ideologia", `${d.category} (${fmtScore(d.score)})`]);
  rows.push(["Mandato", d.term]);
  rows.push(["Eleição", d.elected_by]);
  if (d.votes != null) {
    rows.push(["Votos", fmtInt(d.votes)]);
    const place = `${d.rank}º mais votado em ${d.uf}`;
    rows.push([
      "Posição",
      d.chamber === "camara" ? `${place} (${fmtPct(d.share)} dos votos nominais)` : place,
    ]);
  }
  if (d.federation) rows.push(["Federação", d.federation]);
  if (d.coalition) rows.push(["Coligação", d.coalition]);
  if (d.age != null) rows.push(["Idade em 2027", `${d.age} anos`]);
  if (d.gender) rows.push(["Gênero", d.gender]);
  if (d.race) rows.push(["Cor/raça", d.race]);
  if (d.education) rows.push(["Escolaridade", d.education]);
  if (d.occupation) rows.push(["Ocupação", d.occupation]);

  const dl = card.append("dl");
  for (const [k, v] of rows) {
    dl.append("dt").text(k);
    dl.append("dd").text(v);
  }

  const actions = card.append("div").attr("class", "actions");
  if (d.page) actions.append("a").attr("href", d.page).attr("target", "_blank").attr("rel", "noopener").text("Página no Senado");
  if (state.pinned?.id === d.id) {
    actions
      .append("button")
      .text("Desafixar")
      .on("click", () => {
        state.pinned = null;
        renderCard(null);
        updateDim();
      });
  }
}

// Ideological scale --------------------------------------------------------

// One row per party with seats in the selected chamber, in score order: the
// label on the left, a dot at the party's score sized by its seats, over
// bands for the seven categories.
const bands = [
  [0, 1.5],
  [1.5, 3],
  [3, 4.5],
  [4.5, 5.5],
  [5.5, 7],
  [7, 8.5],
  [8.5, 10],
];
const scaleWidth = 1000;
const labelWidth = 140;
const rowHeight = 26;
const scaleTop = 34;
const x = d3.scaleLinear([0, 10], [labelWidth + 10, scaleWidth - 20]);

const scaleSvg = d3
  .select("#scale")
  .append("svg")
  .attr("role", "img")
  .attr("aria-label", "Posição ideológica de cada partido na escala de 0 a 10");

const gBands = scaleSvg.append("g");
const gBandLabels = scaleSvg.append("g");
const gAxis = scaleSvg.append("g").attr("class", "axis");
const gParties = scaleSvg.append("g");

function drawScale() {
  const counts = d3.rollup(current(), (v) => v.length, (d) => d.party);
  const dat = parties
    .map((p) => ({ ...p, n: counts.get(p.party) ?? 0 }))
    .filter((p) => p.n > 0);
  const innerHeight = dat.length * rowHeight;
  const height = scaleTop + innerHeight + 30;
  const maxR = rowHeight * 0.75;
  const size = d3.scaleSqrt([0, d3.max(dat, (p) => p.n)], [2.5, maxR]);
  const y = (i) => scaleTop + i * rowHeight + rowHeight / 2;

  scaleSvg.attr("viewBox", [0, 0, scaleWidth, height]);

  gBands
    .selectAll("rect")
    .data(bands)
    .join("rect")
    .attr("x", ([a]) => x(a))
    .attr("width", ([a, b]) => x(b) - x(a))
    .attr("y", scaleTop - 4)
    .attr("height", innerHeight + 8)
    .attr("fill", (d, i) => categories[i].color)
    .attr("opacity", 0.12);

  gBandLabels
    .selectAll("text")
    .data(bands)
    .join("text")
    .attr("class", "band-label")
    .attr("x", ([a, b]) => (x(a) + x(b)) / 2)
    .attr("y", scaleTop - 14)
    .attr("text-anchor", "middle")
    .text((d, i) => categories[i].category.replace("Extrema ", "Extr. "));

  gAxis
    .attr("transform", `translate(0,${scaleTop + innerHeight + 6})`)
    .call(d3.axisBottom(x).ticks(10).tickFormat(locale.format("d")).tickSize(4))
    .call((g) => g.select(".domain").attr("stroke", "#CED0D4"))
    .call((g) => g.selectAll("line").attr("stroke", "#CED0D4"));

  const rows = gParties
    .selectAll(".party")
    .data(dat, (p) => p.party)
    .join((enter) => {
      const e = enter.append("g").attr("class", "party");
      e.append("rect").attr("class", "hit");
      e.append("line");
      e.append("circle");
      e.append("text").attr("class", "party-label");
      e.append("text").attr("class", "party-score");
      return e;
    })
    .attr("transform", (p, i) => `translate(0,${y(i)})`)
    .on("mouseenter", function (event, p) {
      state.highlight = (d) => d.party === p.party;
      updateDim();
      gParties.selectAll(".party").attr("opacity", (q) => (q.party === p.party ? 1 : 0.35));
    })
    .on("mouseleave", () => {
      state.highlight = null;
      updateDim();
      gParties.selectAll(".party").attr("opacity", 1);
    });

  rows
    .select(".hit")
    .attr("x", 0)
    .attr("y", -rowHeight / 2)
    .attr("width", scaleWidth)
    .attr("height", rowHeight)
    .attr("fill", "transparent");

  rows
    .select("line")
    .attr("x1", x(0))
    .attr("x2", (p) => x(p.score))
    .attr("stroke", "#CED0D4")
    .attr("stroke-dasharray", "2 3");

  rows
    .select("circle")
    .attr("cx", (p) => x(p.score))
    .attr("r", (p) => size(p.n))
    .attr("fill", (p) => categoryColor[p.category])
    .attr("fill-opacity", 0.9);

  rows
    .select(".party-label")
    .attr("x", labelWidth)
    .attr("text-anchor", "end")
    .attr("dominant-baseline", "middle")
    .text((p) => `${p.label} · ${p.n}`);

  rows
    .select(".party-score")
    .attr("x", (p) => x(p.score) + size(p.n) + 6)
    .attr("dominant-baseline", "middle")
    .text((p) => fmtScore(p.score) + (p.rule === "survey" ? "" : "*"));
}

// Blocs and thresholds -----------------------------------------------------

function drawBlocs() {
  const dat = current();
  const { thresholds, seats } = chambers[state.chamber];
  const left = dat.filter((d) => d.score != null && d.score < 4.5).length;
  const right = dat.filter((d) => d.score != null && d.score > 5.5).length;
  const blocs = [
    { title: "Extrema esquerda a centro-esquerda", n: left, note: "partidos com nota abaixo de 4,5" },
    { title: "Centro-direita a extrema direita", n: right, note: "partidos com nota acima de 5,5" },
  ];

  const cards = d3
    .select("#blocs")
    .selectAll(".bloc")
    .data(blocs)
    .join((enter) => {
      const c = enter.append("div").attr("class", "bloc");
      c.append("div").attr("class", "eyebrow");
      c.append("div").attr("class", "big");
      c.append("p");
      c.append("ul").attr("class", "thresholds");
      return c;
    });

  cards.select(".eyebrow").text((b) => b.title);
  cards.select(".big").text((b) => `${fmtInt(b.n)} de ${seats}`);
  cards.select("p").text((b) => `${fmtPct(b.n / seats)} das cadeiras, ${b.note}.`);
  cards
    .select(".thresholds")
    .selectAll("li")
    .data((b) => thresholds.map((t) => ({ ...t, n: b.n })))
    .join("li")
    .html(
      (t) =>
        `<span>${t.label}: ${t.votes} <span class="no">· ${t.note}</span></span>` +
        `<span class="${t.n >= t.votes ? "ok" : "no"}">${t.n >= t.votes ? "alcança" : `faltam ${t.votes - t.n}`}</span>`,
    );
}

// Controls -----------------------------------------------------------------

function render() {
  drawHemicycle();
  drawLegend();
  drawScale();
  drawBlocs();
  renderCard(state.pinned);
}

d3.selectAll('input[name="chamber"]').on("change", function () {
  state.chamber = this.value;
  state.pinned = null;
  render();
});

d3.selectAll('input[name="color"]').on("change", function () {
  state.color = this.value;
  drawHemicycle();
  drawLegend();
});

const ufs = [...new Set(members.map((d) => d.uf))].sort();
d3.select("#uf")
  .selectAll("option.uf")
  .data(ufs)
  .join("option")
  .attr("class", "uf")
  .attr("value", (d) => d)
  .text((d) => d);

d3.select("#uf").on("change", function () {
  state.uf = this.value;
  updateDim();
});

d3.select("#search").on("input", function () {
  state.query = normalize(this.value.trim());
  updateDim();
});

d3.select("#footer").text(
  `Fontes: TSE (candidatos e votação 2026), Senado Federal (senadores em exercício), Bolognesi et al. (2025). Dados atualizados em ${new Date(`${updated}T12:00:00`).toLocaleDateString("pt-BR")}. EKIO.`,
);

render();
