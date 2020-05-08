Gen = class
    @destr = (g) ->
        x = g.next!
        if x.done
        then null
        else do
            head: x.value
            tail: g
    
    @rnd = (a, b, f = id) !->*
        loop
            yield f(a + (b - a)*Math.random())

    @iterate = (f, x0) !->* 
        yield x = x0
        loop
            yield x = f(x)

    @bind = (g, proc) !->*
        until (x = g.next!).done
            yield from proc x.value
                
    # Uses thunks to recreate generators
    @product = (gs) !->*
        yield from switch
            | gs.length == 0 => !->*
            | gs.length == 1 =>
                (gs.0)! `@@bind` (x) !->* yield [x]
            | gs.length >= 0 =>
                (gs.0)! `@@bind` (x) !->*
                    yield from (tail gs |> @@product) `@@bind` (xs) !->*
                        yield [x] ++ xs

    @sum = (gs) !->*
        | gs.length == 0 => return
        | gs.length >= 0 =>
            yield from gs.0
            yield from tail gs |> @@sum
    
    @zip = (gs) !->*
        loop 
            res = map (.next!), gs
            break if any (.done), res
            yield map (.value), res
    
    @apply = (f, g) !-->*
        until (x = g.next!).done
            yield apply f, x.value

    @map = (f, g) !-->*
        until (x = g.next!).done
            yield f(x.value)

    @take = (n, g) !-->*
        i = n
        until (i-- <= 0) or ((x = g.next!).done)
            yield x.value
    
    @list = (lst) !->*
        for x in lst
            yield x
    
    @cons = (x, g) !->*
        yield x
        yield from g
    
    @dropWhile = (p, g) !-->*
        until (x = g.next!).done or !(p x.value)
            void
        unless x.done
            yield x.value
            yield from g
    
    @takeWhile = (p, g) !-->*
        while !(x = g.next!).done and p(x.value)
            yield x.value if p x.value
        
    @filter = (p, g) !-->*
        until (x = g.next!).done
            yield x if p x
    
    @tail = (g) !->*
        g.next!
        yield from g

    insert = (lst, x, f = id) ->
        | lst.length == 0  => [x]
        | f(lst.0) >= f(x) => [x] ++ lst
        | otherwise        => [lst.0] ++ insert (tail lst), x, f

    @ascendingBy = (f, x0) -> (g) !->*
        buffer = [x0]
        for from 0 to 32
            x = g.next!
            break if x.done
            buffer = insert buffer, x.value, f
        yield from Gen.list buffer
        until (x = g.next!).done
            buffer = insert buffer, x, f
            yield buffer.shift!
        
############################################################

Sequence = class
    (gen = []) -> 
        g = if Array.isArray(gen)
            then Gen.list(gen)
            else gen
        step = g.next!
        @head = step.value
        @isEmpty = step.done
        @tailGen = -> g

    @iterate = (f, x) -> new Sequence Gen.iterate f, x

    generator: ->
        @head `Gen.cons` @tailGen!

    list: ~ ->
        | @isEmpty => []
        | otherwise => 
            res = [...this.generator()]
            @isEmpty = true
            @head = undefined
            res

    # destructing procedure!
    tail: ~ ->	@setGen @tailGen!

    setGen: (g) ->
        step = g.next!
        with @
            ..head = step.value
            ..isEmpty = step.done
            ..tailGen = -> g

    modGen: (f) -> @setGen <| f @generator!
           
    next: -> 
        step = @tailGen!.next!
        with @
            ..head = step.value
            ..isEmpty = step.done

    take: (n) -> @setGen <| Gen.take n <| @head `Gen.cons` @tailGen!

    @tuple = (seqs) ->
        empty = any (.isEmpty), seqs
        with new Sequence!
            ..isEmpty = empty
            ..head = if empty then undefined else map (.head), seqs
            ..tailGen = -> Gen.zip <| map (.tailGen!), seqs
            ..elements = seqs

    @sum = (seqs) ->
        | seqs.length == 0 =>  new Sequence([])
        | otherwize =>
            s = map (.generator!) seqs
            with new Sequence _ <| Gen.sum s
                ..elements = seqs.0.elements
  
    @list-product = (ls) ->
        | ls.length == 0 => new Sequence([])
        | ls.length >  0 => 
            new Sequence _ <| Gen.product <| map ((l) -> -> Gen.list l), ls

    apply: (f) -> @modGen <| Gen.apply f

    map: (f) -> @modGen <| Gen.map f

    dropWhile: (f) -> @modGen <| Gen.dropWhile f

    takeWhile: (f) -> @modGen <| Gen.takeWhile f

    filter: (p) -> @modGen <| Gen.filter p

    ascendingBy: (f, x0) -> @modGen <| Gen.ascendingBy f, x0

    toString: ->
        if @isEmpty
            then "<empty>"
            else "<#{@head.toString()} ...>"
                                                                        
window <<< {Gen, Sequence}
                        
