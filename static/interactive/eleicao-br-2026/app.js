// Map of the 2026 presidential election by municipality, drawn from the
// projected coordinates computed in R.
// - The round switch recolours every municipality for the 1st or 2nd round,
//   or for the change in Lula's share between them.
// - The view switch morphs the choropleth into proportional circles and
//   back: each municipality first becomes a circle of its own area, then
//   grows or shrinks to the size of its valid votes while moving to a spot
//   where it does not overlap its neighbours.
// - Hovering or tapping a municipality fills the tooltip.
// - Choosing a state zooms to it and dims the rest; a municipality can then
//   be chosen too. Clicking a legend bin hides or shows that bin.

const { totals, cities, names, municipalities, states } = window.ELEICAO_BR;

// `metric` is "share" for Lula's share in `round`, or "change" for the change
// in that share from the 1st to the 2nd round; the change view keeps `round`
// at 2 for circle sizes and the count. `uf` is a lower-case state code, or ""
// for the whole country. `city` is an IBGE code inside that state, or 0.
const state = {
  round: 2,
  metric: "share",
  view: "map",
  uf: "",
  city: 0,
  hiddenBins: new Set(),
};

// pt-BR formatting ---------------------------------------------------------

const locale = d3.formatLocale({ decimal: ",", thousands: ".", grouping: [3], currency: ["R$ ", ""] });
const fmtInt = locale.format(",d");
const fmtPct = locale.format(".1f");
const fmtSigned = locale.format("+.1f");
const fmtShort = (n) => (n >= 1e6 ? `${locale.format(".1~f")(n / 1e6)} mi` : n >= 1e3 ? `${fmtInt(n / 1e3)} mil` : fmtInt(n));

// Colours ------------------------------------------------------------------

const lulaColor = "#AF4942";
const flavioColor = "#3A71A8";
const otherColor = "#B2AEA8";
const landColor = "#E6E5E0";
const pendingColor = "#CFCCC4";

// Bins of Lula's share of valid votes, as in the São Paulo and Porto Alegre
// maps.
const bins = [
  { label: "< 35", color: "#2E5485" },
  { label: "35–40", color: "#3A71A8" },
  { label: "40–45", color: "#82B5DA" },
  { label: "45–50", color: "#B0D6F0" },
  { label: "50–55", color: "#FBC0B6" },
  { label: "55–60", color: "#E6968B" },
  { label: "60–65", color: "#AF4942" },
  { label: "≥ 65", color: "#89322F" },
];
const binOf = d3.scaleThreshold().domain([35, 40, 45, 50, 55, 60, 65]).range(d3.range(bins.length));

// Bins of the change in Lula's share from the 1st to the 2nd round, in
// points. The share fell in only a handful of municipalities, so every fall
// shares one bin.
const changeBins = [
  { label: "< 0", color: "#3A71A8" },
  { label: "0–3", color: "#FCE3DD" },
  { label: "3–4", color: "#FBC0B6" },
  { label: "4–5", color: "#EDA095" },
  { label: "5–6", color: "#D97B6F" },
  { label: "6–7", color: "#C25E54" },
  { label: "7–8", color: "#A2443D" },
  { label: "≥ 8", color: "#7A2C29" },
];
const changeBinOf = d3.scaleThreshold().domain([0, 3, 4, 5, 6, 7, 8]).range(d3.range(changeBins.length));

// Data ---------------------------------------------------------------------

function result(lula, flavio, other, counted) {
  const valid = lula + flavio + other;
  const share = valid ? (100 * lula) / valid : NaN;
  return { lula, flavio, other, valid, share, counted, bin: valid ? binOf(share) : null };
}

const nameOf = new Map(names.map((d) => [d.code, d]));
for (const d of cities) {
  d.name = nameOf.get(d.code).name;
  d.uf = nameOf.get(d.code).uf.toLowerCase();
  d.r = {
    1: result(d.lula_1, d.flavio_1, d.other_1, d.counted_1),
    2: result(d.lula_2, d.flavio_2, 0, d.counted_2),
  };
  d.change = d.r[1].valid && d.r[2].valid ? d.r[2].share - d.r[1].share : NaN;
  d.changeBin = Number.isNaN(d.change) ? null : changeBinOf(d.change);
}
const cityByCode = new Map(cities.map((d) => [d.code, d]));
const cur = (d) => d.r[state.round];

// Official totals for the country (with voters abroad) and each state.
const scopeTotals = d3.rollup(
  totals,
  ([t]) => ({ ...result(t.lula, t.flavio, t.other, t.counted), updated: t.updated }),
  (t) => t.round,
  (t) => t.scope
);
const scopePair = (scope) => ({ 1: scopeTotals.get(1).get(scope), 2: scopeTotals.get(2).get(scope) });
const changeOf = (pair) => pair[2].share - pair[1].share;

const ufName = {
  ac: "Acre", al: "Alagoas", ap: "Amapá", am: "Amazonas", ba: "Bahia", ce: "Ceará",
  df: "Distrito Federal", es: "Espírito Santo", go: "Goiás", ma: "Maranhão",
  mt: "Mato Grosso", ms: "Mato Grosso do Sul", mg: "Minas Gerais", pa: "Pará",
  pb: "Paraíba", pr: "Paraná", pe: "Pernambuco", pi: "Piauí", rj: "Rio de Janeiro",
  rn: "Rio Grande do Norte", rs: "Rio Grande do Sul", ro: "Rondônia", rr: "Roraima",
  sc: "Santa Catarina", sp: "São Paulo", se: "Sergipe", to: "Tocantins",
};
const byName = (a, b) => a.localeCompare(b, "pt-BR");

// Map ----------------------------------------------------------------------

const width = 760;
const projection = d3.geoIdentity().reflectY(true).fitWidth(width, municipalities);
const path = d3.geoPath(projection);
const height = Math.ceil(path.bounds(municipalities)[1][1]);

// Every feature carries its city row, its dot anchor and the radius of a
// circle with its own area, where the morph starts and ends.
const features = municipalities.features.filter((f) => cityByCode.has(f.properties.code));
for (const f of features) {
  const d = cityByCode.get(f.properties.code);
  f.city = d;
  [d.px, d.py] = projection([d.x, d.y]);
  d.areaRadius = Math.sqrt(path.area(f) / Math.PI);
}

// Dot radii are in pixels at the initial zoom. On zoom they grow with the
// square root of the scale, slower than the map, so dense regions pull
// apart. The domain spans both rounds, so a round switch never rescales.
const maxValid = d3.max(cities, (d) => Math.max(d.r[1].valid, d.r[2].valid));
const radius = d3.scaleSqrt().domain([0, maxValid]).range([0, 26]);

const svg = d3
  .select("#map")
  .insert("svg", ":first-child")
  .attr("viewBox", [0, 0, width, height])
  .attr("role", "img")
  .attr("aria-label", "Mapa do Brasil por município, colorido pela fatia de Lula nos votos válidos.");

const layer = svg.append("g");

const munis = layer
  .append("g")
  .selectAll("path")
  .data(features)
  .join("path")
  .attr("class", "muni")
  .attr("d", path);

const stateLines = layer
  .append("g")
  .selectAll("path")
  .data(states.features)
  .join("path")
  .attr("class", "state")
  .attr("d", path);

// Large circles first, so small ones stay visible on top.
const dotData = cities
  .filter((d) => d.px !== undefined)
  .sort((a, b) => Math.max(b.r[1].valid, b.r[2].valid) - Math.max(a.r[1].valid, a.r[2].valid));

const dots = layer
  .append("g")
  .selectAll("circle")
  .data(dotData)
  .join("circle")
  .attr("class", "dot")
  .attr("cx", (d) => d.px)
  .attr("cy", (d) => d.py)
  .attr("display", "none");

// Zoom ---------------------------------------------------------------------

const maxZoom = 40;
let zoomK = 1;
let morphing = false;

// On touch screens one finger scrolls the page and two fingers pan or zoom
// the map, so the map never traps the scroll. Embedded in another page, the
// wheel zooms only with Ctrl or ⌘, so it can scroll past the map; trackpad
// pinches arrive as Ctrl + wheel and still zoom.
const embedded = window.self !== window.top;
if (embedded) {
  d3.select("#zoom-hint").text("Para aproximar, use Ctrl ou ⌘ com a roda do mouse, ou dois dedos no mapa.");
}

const zoom = d3
  .zoom()
  .filter((event) => {
    if (event.type === "touchstart") return event.touches.length > 1;
    if (event.type === "wheel") return !embedded || event.ctrlKey || event.metaKey;
    return !event.ctrlKey && !event.button;
  })
  // d3's default wheel step, except that Ctrl boosts only trackpad pinches,
  // which send small steps; a mouse wheel with Ctrl or ⌘ zooms at the usual pace.
  .wheelDelta((event) => {
    const pinch = event.ctrlKey && Math.abs(event.deltaY) < 50;
    const unit = event.deltaMode === 1 ? 0.05 : event.deltaMode ? 1 : 0.002;
    return -event.deltaY * unit * (pinch ? 10 : 1);
  })
  .scaleExtent([1, maxZoom])
  .translateExtent([[0, 0], [width, height]])
  .on("zoom", (event) => {
    zoomK = event.transform.k;
    layer.attr("transform", event.transform);
    strokes();
    if (!morphing) dots.attr("r", dotRadius);
    hideTooltip();
    d3.select("#reset").property("disabled", zoomK === 1 && !state.uf);
  });

svg.call(zoom).on("dblclick.zoom", null);

const dotRadius = (d) => radius(cur(d).valid) / Math.sqrt(zoomK);

// In the circles view each circle sits near, not on, its municipality: a
// force layout pushes circles apart until none overlap and pulls each one
// back toward its own point. Collisions use the larger round at the initial
// zoom; zooming in shrinks circles relative to the map, so they stay apart.
// The layout takes a second or two, so it runs in small chunks after the
// page loads; opening the circles view earlier finishes it on the spot.
const layoutTicks = 300;
const layoutNodes = dotData.map((d) => ({ d, x: d.px, y: d.py }));
const layout = d3
  .forceSimulation(layoutNodes)
  .force("x", d3.forceX((n) => n.d.px).strength(0.1))
  .force("y", d3.forceY((n) => n.d.py).strength(0.1))
  .force("collide", d3.forceCollide((n) => radius(Math.max(n.d.r[1].valid, n.d.r[2].valid)) + 0.3))
  .stop();
let ticksDone = 0;

function layoutDots(ticks = layoutTicks) {
  const n = Math.min(ticks, layoutTicks - ticksDone);
  if (n <= 0) return;
  layout.tick(n);
  ticksDone += n;
  for (const node of layoutNodes) [node.d.dx, node.d.dy] = [node.x, node.y];
}

function layoutInChunks() {
  layoutDots(10);
  if (ticksDone < layoutTicks) setTimeout(layoutInChunks, 0);
}
setTimeout(layoutInChunks, 300);

// Each municipality is outlined in its own colour, which covers the hairline
// gaps between neighbours in the simplified grid.
function strokes() {
  munis.attr("stroke-width", 0.5 / zoomK);
  stateLines.attr("stroke-width", 1 / zoomK);
  dots.attr("stroke-width", 0.4 / zoomK);
}

function zoomTo(feature) {
  let transform = d3.zoomIdentity;
  if (feature) {
    const [[x0, y0], [x1, y1]] = path.bounds(feature);
    const k = Math.min(maxZoom, 0.85 / Math.max((x1 - x0) / width, (y1 - y0) / height));
    transform = d3.zoomIdentity
      .translate(width / 2, height / 2)
      .scale(k)
      .translate(-(x0 + x1) / 2, -(y0 + y1) / 2);
  }
  svg.transition().duration(700).call(zoom.transform, transform);
}

// Colours ------------------------------------------------------------------

const changing = () => state.metric === "change";
const binsNow = () => (changing() ? changeBins : bins);
// null when the municipality has no counted votes for the current metric.
const binOfCity = (d) => (changing() ? d.changeBin : cur(d).bin);
const hidden = (d) => binOfCity(d) !== null && state.hiddenBins.has(binOfCity(d));
const fillOf = (d) => {
  const bin = binOfCity(d);
  if (bin === null) return pendingColor;
  return hidden(d) ? landColor : binsNow()[bin].color;
};
const muniFill = (f) => fillOf(f.city);

// Choropleth to circles and back ------------------------------------------

// Into circles: each circle appears on top of its municipality with the
// same area while the shapes fade to plain land, then resizes to its votes.
// Back to the map, the steps run in reverse.
function morph(view) {
  if (view === state.view) return;
  state.view = view;
  d3.selectAll("#views button").attr("aria-pressed", function () {
    return this.dataset.view === view;
  });
  d3.select("#map").classed("dots-mode", view === "dots");
  d3.select("#sizes").property("hidden", view !== "dots");
  d3.select("#sizes-title").property("hidden", view !== "dots");
  hideTooltip();
  morphing = true;
  const ease = d3.easeCubicInOut;
  if (view === "dots") {
    layoutDots();
    dots
      .interrupt()
      .attr("display", (d) => (hidden(d) ? "none" : null))
      .attr("fill", fillOf)
      .attr("cx", (d) => d.px)
      .attr("cy", (d) => d.py)
      .attr("r", (d) => d.areaRadius);
    munis.interrupt().transition().duration(400).attr("fill", landColor).attr("stroke", landColor);
    dots
      .transition()
      .delay(350)
      .duration(1100)
      .ease(ease)
      .attr("r", dotRadius)
      .attr("cx", (d) => d.dx)
      .attr("cy", (d) => d.dy)
      .end()
      .catch(() => {})
      .finally(() => (morphing = false));
  } else {
    dots
      .interrupt()
      .transition()
      .duration(1100)
      .ease(ease)
      .attr("r", (d) => d.areaRadius)
      .attr("cx", (d) => d.px)
      .attr("cy", (d) => d.py)
      .end()
      .catch(() => {})
      .finally(() => {
        morphing = false;
        if (state.view !== "map") return;
        munis.transition().duration(400).attr("fill", muniFill).attr("stroke", muniFill);
        dots.transition().delay(300).attr("display", "none");
      });
  }
  update();
}

d3.selectAll("#views button").on("click", function () {
  morph(this.dataset.view);
});

// Tooltip ------------------------------------------------------------------

const tooltip = d3.select("#tooltip");
const mapBox = document.getElementById("map");

const updatedText = (t) => (t.updated ? t.updated.slice(0, 5) + " às " + t.updated.slice(11, 16) : "");

// The share of sections counted, while a count is still running.
function countLine(t) {
  if (t.counted >= 100) return "";
  if (t.counted === 0) return `<p class="count">Apuração ainda não começou</p>`;
  const when = t.updated ? `, atualizado em ${updatedText(t)}` : "";
  return `<p class="count">${fmtPct(t.counted)}% das seções apuradas${when}</p>`;
}

// Candidate rows and a stacked bar; "Outros" appears only in the 1st round.
function duel(t) {
  if (!t.valid) return "";
  const rows = [
    { name: "Lula", color: lulaColor, votes: t.lula },
    { name: "Flávio Bolsonaro", color: flavioColor, votes: t.flavio },
  ];
  if (t.other > 0) rows.push({ name: "Outros", color: otherColor, votes: t.other });
  for (const row of rows) row.pct = (100 * row.votes) / t.valid;
  return `
    <div class="bar">
      ${rows.map((row) => `<span style="width:${row.pct}%;background:${row.color}"></span>`).join("")}
    </div>
    <div class="duel">
      ${rows
        .map(
          (row) => `
      <span class="cand"><span class="sw" style="background:${row.color}"></span>${row.name}</span>
      <span class="pct">${fmtPct(row.pct)}%</span>
      <span class="votes">${fmtInt(row.votes)} votos</span>`
        )
        .join("")}
    </div>`;
}

// Shares in both rounds and their change, for the change view. "Outros"
// ran only in the 1st round.
function shift(t1, t2) {
  if (!t1.valid || !t2.valid) return "";
  const pct = (votes, t) => (100 * votes) / t.valid;
  const rows = [
    { name: "Lula", color: lulaColor, a: pct(t1.lula, t1), b: pct(t2.lula, t2) },
    { name: "Flávio Bolsonaro", color: flavioColor, a: pct(t1.flavio, t1), b: pct(t2.flavio, t2) },
    { name: "Outros", color: otherColor, a: pct(t1.other, t1), b: null },
  ];
  return `
    <div class="shift">
      <span></span><span class="head">1º turno</span><span class="head">2º turno</span><span class="head">Var.</span>
      ${rows
        .map(
          (row) => `
      <span class="cand"><span class="sw" style="background:${row.color}"></span>${row.name}</span>
      <span>${fmtPct(row.a)}%</span>
      <span>${row.b === null ? "–" : `${fmtPct(row.b)}%`}</span>
      <span class="pct">${fmtSigned((row.b ?? 0) - row.a)}</span>`
        )
        .join("")}
    </div>`;
}

let hovered = null;

// Reclassing every shape is costly, so it runs only when the hovered
// municipality changes; the tooltip itself follows every move.
function showTooltip(event, d) {
  if (hovered !== d) {
    hovered = d;
    munis.classed("hover", (f) => f.city === d);
    dots.classed("hover", (c) => c === d);
  }
  const t = cur(d);
  tooltip.html(`
    <p class="name">${d.name}</p>
    <p class="sub">${ufName[d.uf]} · ${fmtInt(t.valid)} votos válidos no ${state.round}º turno</p>
    ${changing() ? shift(d.r[1], d.r[2]) : duel(t)}
    ${countLine(t)}`);
  const [mx, my] = d3.pointer(event, mapBox);
  const box = mapBox.getBoundingClientRect();
  const tip = tooltip.node();
  const left = mx + 16 + tip.offsetWidth > box.width ? mx - 16 - tip.offsetWidth : mx + 16;
  const top = Math.min(Math.max(my - 20, 0), box.height - tip.offsetHeight);
  tooltip.style("left", `${Math.max(left, 0)}px`).style("top", `${top}px`).classed("on", true);
}

function hideTooltip() {
  if (hovered !== null) {
    munis.classed("hover", false);
    dots.classed("hover", false);
  }
  hovered = null;
  tooltip.classed("on", false);
}

const leave = (event) => {
  if (event.pointerType === "mouse") hideTooltip();
};
munis.on("pointerenter pointermove", (event, f) => showTooltip(event, f.city)).on("pointerleave", leave);
dots.on("pointerenter pointermove", showTooltip).on("pointerleave", leave);
svg.on("pointerdown", (event) => {
  if (!["path", "circle"].includes(event.target.tagName)) hideTooltip();
});

// Summary card -------------------------------------------------------------

const pointsDiff = (a, b, what) => {
  const diff = a - b;
  const abs = Math.abs(diff);
  return ` · ${fmtPct(abs)} ponto${abs >= 2 ? "s" : ""} ${diff >= 0 ? "acima" : "abaixo"} ${what}`;
};

function cardBody(t, vsText) {
  if (!t.valid) return `<p class="big-sub">Sem votos apurados no ${state.round}º turno.</p>${countLine(t)}`;
  return `
      <p class="big">${fmtPct(t.share)}%</p>
      <p class="big-sub">dos votos válidos em Lula no ${state.round}º turno${vsText}</p>
      ${duel(t)}
      ${countLine(t)}
      <p class="meta">${fmtInt(t.valid)} votos válidos</p>`;
}

function changeBody(pair, vsText) {
  const [t1, t2] = [pair[1], pair[2]];
  if (!t1.valid || !t2.valid) return `<p class="big-sub">Sem votos apurados no 2º turno.</p>${countLine(t2)}`;
  return `
      <p class="big">${fmtSigned(changeOf(pair))}</p>
      <p class="big-sub">pontos na fatia de Lula nos votos válidos, do 1º para o 2º turno${vsText}</p>
      ${shift(t1, t2)}
      ${countLine(t2)}`;
}

// Each scope compares with the one above it: a municipality with its state,
// a state with the country.
function renderSummary() {
  let eyebrow = "Brasil";
  let title = "Todo o país";
  let pair = scopePair("br");
  let parent = null;
  let what = "";
  if (state.city) {
    const d = cityByCode.get(state.city);
    [eyebrow, title, pair, parent, what] = [`Município · ${ufName[d.uf]}`, d.name, d.r, d.uf, "do estado"];
  } else if (state.uf) {
    [eyebrow, title, pair, parent, what] = ["Estado", ufName[state.uf], scopePair(state.uf), "br", "do país"];
  }
  const value = (p) => (changing() ? changeOf(p) : p[state.round].share);
  const vsText = parent && !Number.isNaN(value(pair)) ? pointsDiff(value(pair), value(scopePair(parent)), what) : "";
  const body = changing() ? changeBody(pair, vsText) : cardBody(pair[state.round], vsText);
  d3.select("#summary").html(`<div class="eyebrow">${eyebrow}</div><h2>${title}</h2>${body}`);
}

// Legend -------------------------------------------------------------------

// The bins follow the metric; switching metric shows every bin again.
function renderLegend() {
  d3.select("#legend-title").text(
    changing() ? "Variação da fatia de Lula do 1º para o 2º turno (pontos)" : "Votos válidos em Lula (%)"
  );
  d3.select("#bins")
    .selectAll("div")
    .data(binsNow())
    .join("div")
    .attr("class", "bin")
    .attr("role", "button")
    .attr("tabindex", 0)
    .html((d) => `<span style="background:${d.color}"></span>${d.label}`)
    .on("click", (event, d) => toggleBin(binsNow().indexOf(d)))
    .on("keydown", (event, d) => {
      if (event.key === "Enter" || event.key === " ") {
        event.preventDefault();
        toggleBin(binsNow().indexOf(d));
      }
    });
}

function toggleBin(i) {
  if (state.hiddenBins.has(i)) state.hiddenBins.delete(i);
  else state.hiddenBins.add(i);
  paint(false);
  update();
}

// Circle sizes in the legend; the top break is the largest municipality,
// rounded down to the nearest million.
const sizeBreaks = [10000, 100000, 1000000, Math.floor(maxValid / 1e6) * 1e6];
const sizes = d3.select("#sizes").selectAll("div").data(sizeBreaks).join("div");
sizes
  .append("svg")
  .attr("width", (d) => 2 * radius(d) + 2)
  .attr("height", (d) => 2 * radius(d) + 2)
  .append("circle")
  .attr("cx", (d) => radius(d) + 1)
  .attr("cy", (d) => radius(d) + 1)
  .attr("r", (d) => radius(d))
  .attr("fill", otherColor);
sizes.append("span").text((d) => fmtShort(d));

// Hint and method note follow the count.
function renderNotes() {
  const t = scopeTotals.get(2).get("br");
  const status =
    t.counted >= 100
      ? "Apuração do 2º turno concluída."
      : `2º turno com ${fmtPct(t.counted)}% das seções apuradas no país, atualizado em ${updatedText(t)}.`;
  d3.select("#hint").text(`${status} Em cinza, municípios ainda sem votos apurados.`);
  if (t.counted >= 100) {
    d3.select("#count-note").text("Os dois turnos estão totalmente apurados.");
  }
}

// Update -------------------------------------------------------------------

const inScope = (d) => !state.uf || d.uf === state.uf;

function update() {
  munis.classed("dim", (f) => !inScope(f.city));
  dots
    .classed("dim", (d) => !inScope(d))
    .attr("pointer-events", (d) => (hidden(d) ? "none" : null));
  if (state.view === "dots" && !morphing) dots.attr("display", (d) => (hidden(d) ? "none" : null));
  // Selection has its own class, so hover, zoom and morph cannot clear it.
  munis.classed("selected", (f) => f.city.code === state.city);
  dots.classed("selected", (d) => d.code === state.city);
  // Neighbours drawn later would hide the outline, so the selection goes on top.
  munis.filter((f) => f.city.code === state.city).raise();
  dots.filter((d) => d.code === state.city).raise();
  d3.selectAll("#bins .bin").classed("off", (d, i) => state.hiddenBins.has(i)).attr("aria-pressed", (d, i) => !state.hiddenBins.has(i));
  renderSummary();
}

// Colours follow the round; only a switch animates them.
function paint(animate) {
  const t = (sel) => (animate ? sel.transition().duration(500) : sel);
  if (state.view === "map") t(munis).attr("fill", muniFill).attr("stroke", muniFill);
  else munis.attr("fill", landColor).attr("stroke", landColor);
  // One transition for fill and radius: a second one would cancel the first.
  const dotPaint = t(dots).attr("fill", fillOf);
  if (state.view === "dots" && !morphing) dotPaint.attr("r", dotRadius);
  if (state.view === "dots") dots.attr("display", (d) => (hidden(d) ? "none" : null));
}

d3.selectAll("#rounds button").on("click", function () {
  const key = this.dataset.round;
  const metric = key === "change" ? "change" : "share";
  const round = key === "change" ? 2 : Number(key);
  if (metric === state.metric && round === state.round) return;
  if (metric !== state.metric) state.hiddenBins.clear();
  state.metric = metric;
  state.round = round;
  d3.selectAll("#rounds button").attr("aria-pressed", function () {
    return this.dataset.round === key;
  });
  renderLegend();
  paint(true);
  update();
});

// Menus --------------------------------------------------------------------

const ufSelect = d3.select("#uf");
ufSelect
  .selectAll("option.name")
  .data(Object.keys(ufName).sort((a, b) => byName(ufName[a], ufName[b])))
  .join("option")
  .attr("class", "name")
  .attr("value", (d) => d)
  .text((d) => ufName[d]);

const citySelect = d3.select("#city");

function chooseUf(uf) {
  state.uf = uf;
  state.city = 0;
  ufSelect.property("value", uf);
  d3.select("#city-control").property("hidden", !uf);
  citySelect
    .selectAll("option.name")
    .data(uf ? cities.filter((d) => d.uf === uf).sort((a, b) => byName(a.name, b.name)) : [], (d) => d.code)
    .join("option")
    .attr("class", "name")
    .attr("value", (d) => d.code)
    .text((d) => d.name);
  citySelect.property("value", "");
  zoomTo(uf ? states.features.find((f) => f.properties.uf.toLowerCase() === uf) : null);
  update();
}

function chooseCity(code) {
  if (!code) return chooseUf(state.uf);
  const d = cityByCode.get(code);
  if (d.uf !== state.uf) chooseUf(d.uf);
  state.city = code;
  citySelect.property("value", code);
  return update();
}

ufSelect.on("change", (event) => chooseUf(event.target.value));
citySelect.on("change", (event) => chooseCity(Number(event.target.value)));
d3.select("#reset").on("click", () => chooseUf(""));
// A click picks the state first, then the municipality inside it.
const pick = (d) => (state.uf === d.uf ? chooseCity(d.code) : chooseUf(d.uf));
munis.on("click", (event, f) => pick(f.city));
dots.on("click", (event, d) => pick(d));

strokes();
renderLegend();
paint(false);
renderNotes();
update();
