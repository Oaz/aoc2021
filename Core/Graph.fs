module aoc2021.Core.Graph

type Graph<'NODE when 'NODE: comparison> = Map<'NODE, 'NODE list>
type Path<'NODE> = 'NODE list

type FindAllPathWalkingRules<'HISTORY, 'NODE> =
  { EmptyHistory: 'HISTORY
    Visit: 'HISTORY -> 'NODE -> 'HISTORY
    Accept: 'HISTORY -> 'NODE -> bool }

let findAllPaths
  (graph: Graph<'NODE>)
  (wr: FindAllPathWalkingRules<'HISTORY, 'NODE>)
  (update: 'STATE -> 'HISTORY -> Path<'NODE> -> 'STATE)
  (emptyRecord: 'STATE)
  (startAt: 'NODE)
  (endAt: 'NODE)
  : 'STATE =
  let rec walk (from: 'NODE) (path: Path<'NODE>) (history: 'HISTORY) (state: 'STATE) : 'STATE =
    if from = endAt then
      update state history (List.rev (from :: path))
    else
      let newHistory = wr.Visit history from

      let tryNode (s: 'STATE) (n: 'NODE) : 'STATE =
        if wr.Accept newHistory n then
          walk n (from :: path) newHistory s
        else
          s

      List.fold tryNode state (graph.Item from)

  walk startAt [] wr.EmptyHistory emptyRecord
