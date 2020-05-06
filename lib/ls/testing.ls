M = Monoid
C = console
S = Sequence

#------------------------------------------------------------
failed-params = {}
failed-figures = {}
failed-tests = {}

#------------------------------------------------------------

fold-JSON = (field, m, mempty) -> 
    empty = m.mempty || mempty
    go = (value) -> (json) -> 
        res = {} <<< json
        newvalue = m.mappend value, (res[field] || empty)
        rec = {}
        if res.suite
          then rec.suite = map (go newvalue), res.suite
          else res.[field] = newvalue
        res <<< rec
    
    (json) -> json |> go (json[field] || empty)

#------------------------------------------------------------

run-tests = (json) ->
    for t in json |> normalize-tests |> flatten-tests
        switch
        | t.skip    => continue
        | t.run     => run-test t
        | t.for     => run-property t

normalize-tests = (json) ->
    json |> M.fold do
        M.composition
        * fold-JSON \name, (M.path ':'), ''
          fold-JSON \log, M.or
          fold-JSON \skip, M.replace
          fold-JSON \number, M.keep, json.number or 100

flatten-tests = (json) ->
    | json?.suite => concat-map flatten-tests, json.suite        
    | otherwise => [json]
    
#------------------------------------------------------------

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

#------------------------------------------------------------

run-property = (data) ->
    options = 
        skip: false
        assuming: []
        hold: -> true
        shrinks: 500
        including: []
        number: 123

    options <<< data
    
    unless options.skip
        C.log("Testing #{options.name} ...") if options.log
        res = check-property options

        switch 
        | (res.applied == 0) =>
            C.log("%cNone of samples in test \"%s\" passed assumptions!", 'color:red', options['name']) 
        | res.ok => report-success(res, options)
        | otherwise => report-fail(res, options)

#------------------------------------------------------------

report-success = (res, options) ->
    if options.log

        passed = res.applied/res.samples

        color = switch
          | passed < 0.25 => 'orange'
          | passed < 0.5 => 'goldenrod'
          | otherwise => 'darkgreen'

        C.log('%cOk. Total samples: %d, %cpassed assumptions: %s',
          'color:darkgreen',
          res.samples,
          'color:'+color,
          fmt.percent(passed))

#------------------------------------------------------------

report-fail = (res, options) ->
    augmented-args = options.with or ((...x) -> [...x])
    name = options.name
    params = apply augmented-args, res.sample
    
    C.groupCollapsed "%cProperty #{name} failed!", 'color: red'
    C.log "Counter example (#{res.shrinks} shrinks):"
    for x in params
        C.log '%c'+x.toString(), 'color: blue'
    C.groupEnd()
    failed-params[name] = params
    failed-tests[name] = options
    failed-figures[name] =
        new Group(params .filter(x -> x instanceof Figure))

#------------------------------------------------------------

check-property = (options) ->
    ac = counter: 0
    c = counter: 0
    sc = counter: -2
    augmented-args = options.with or ((...x) -> [...x])
    assertion = options.hold `compose` augmented-args
    assumption = (x) -> apply (conjunction options.assuming), x |> count c
    proposition = (x) -> apply assertion, x |> count ac
    test = assumption `implies` proposition
    obligatory = options.including |> S.list-product
    samples = -> new Arbitrary! .tuple (options.for) .ascending!
    shrinker = samples! .shrink
    simplicity = (r) ->
        if r.sample
          then samples! .simplicity r.sample
          else Infinity
    number = options.number

    run = (seq, ok = true, depth = 0, last) ->
        res = seq .dropWhile test
        switch
        | res.isEmpty                 => {ok : ok, sample : last}
        | !ok and res.tail.isEmpty    => {ok : ok, sample : res.head}
        | otherwise =>
            h = res.head
            sh = shrinker(h).take(options.shrinks)
            switch
            | depth > options.shrinks => {ok : ok, sample : h}
            | sh.isEmpty              => {ok : ok, sample : h}
            | otherwise               => runc sh, false, depth + 1, h

    runc = run |> count sc
    res = runc <| samples! .take number
    counts = 
        samples: c.counter
        applied: ac.counter
        shrinks: sc.counter
    res <<< counts

#------------------------------------------------------------

class Arbitrary  extends Sequence
    (@tailGen, @shrink) ->
        super ...
        @shrink ?= (n) -> new Sequence([n])
        @simplicity = -> Infinity
        @simplest = undefined
        
    @copy = (obj) -> new Arbitrary() <<< obj

    simper = (e, n) -> e .simplicity n
    shrinker = (x, n) -> -> x .shrink n .generator!
           
    @tuple = (xs) ->        
        with (res = S.tuple xs |> Arbitrary.copy)
            ..elements = xs
            ..simplicity = (ns) -> zipWith simper, xs, ns |> norm
            ..simplest = map (.simplest), xs
            ..shrink = (ns) ->
                p = zipWith shrinker, xs, ns |> Gen.product
                new Sequence p .ascendingBy(res.simplicity, res.simplest)
            
    iso: (constr, destr) ->              
        with (res = @apply constr |> Arbitrary.copy)
            ..elements = @elements
            ..simplicity = destr >> @simplicity
            ..simplest = apply constr, map (.simplest) @elements
            ..shrink = destr >> @shrink >> (curry Gen.map) constr
    
    ascending: ->
        @setGen <| Gen.ascendingBy @simplicity, @simplest, @generator!

#------------------------------------------------------------

window <<< { run-tests, Arbitrary }
