#let grammar(..prods) = {
  let cells = ()
  for (name, var, defn) in prods.pos() {
    cells.push(emph(name))
    cells.push(var)
    if var == none {
      cells.push($|$)
    } else {
      cells.push[::=]
    }
    cells.push(defn)
  }
  grid(
    columns: (auto, auto, auto, auto),
    align: (left, right, right, left),
    column-gutter: 5pt,
    row-gutter: 8pt,
    ..cells)
}

#let judgement(content) = {
  block(stroke: 1pt, inset: 5pt, content)
}

#let rule(..premises, conclusion) = {
  grid(
    columns: (auto),
    rows: (auto, auto),
    row-gutter: (6pt, 4pt),
    premises.pos().join[\ ],
    grid.hline(),
    [], // not sure what this is about
    conclusion)
}
