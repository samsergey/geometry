failedParams = {}
failedFigures = {}
testCount = 0
failedTests = {}

M = Monoid
C = console

run-tests = (json) ->
  go = (t) ->
    | t.run     => runTest t
    | t.for     => property t
    | t.suite   =>
      C.group("Testing " + t.name) if (t.log and t.name)
      for entry in t.suite
        go entry
      C.groupEnd() if (t.log and t.name) 
    | t.skip    => return
    | otherwise => return

  normalization = 
    foldJSON \name, (M.path ':'), ''
    foldJSON \log, M.or
    foldJSON \number, M.keep, json.number or 100

  normalized = M.fold (M.composition), normalization

  go normalized json


foldJSON = (field, m, mempty) -> 
  empty = m.mempty || mempty
  go = (value) -> (json) -> 
    res = {} <<< json
    newvalue = m.mappend value, (res[field] || empty)
    rec = {}
    if res.suite
      then rec.suite = map (go newvalue), res.suite
      else res.[field] = newvalue
    res <<< rec
    
  (json) -> (go (json[field] || empty)) json

run-test = (test) ->
  unless test.skip
    C.log "Testing #{test.name}..." if test.log
    result = test.run!
    unless result `equal` test.result
      C.groupCollapsed '%cTest %s failed', 'color:red', test.name
      C.log '  expected: %c%s', 'color:blue', test.result.toString!
      C.log '  got:      %c%s', 'color:blue', result.toString!
      C.groupEnd()
    else
      C.log("%cPassed.", 'color:darkgreen') if test.log

  
window <<< { foldJSON, run-tests  }
