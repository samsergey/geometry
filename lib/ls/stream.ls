Stream = class
    (@rest, @buffer = []) ->
        @next if empty @buffer

    empty: ~ -> empty @buffer and @empty-rest
    
    next: ~ ->
        unless (x = @rest.next!).done
            @buffer.push x.value
        @empty-rest = x.done
        @

    head: ~ ->
        unless @empty
            @next if empty @buffer
            @buffer.0
        else
            undefined

    tail: ~ ->
        unless @empty
            @next if empty @buffer
            new Stream @rest, tail @buffer
        else
            undefined

    @fromList = (lst) -> new Stream !->*, lst

    @rnd = (a, b) -> new Stream Gen.rnd(a, b)

    toList: ~ ->
        res = @buffer
        until (x = @rest.next!).done
            res.push x.value
        res

    take: (n) ->
        new Stream Gen.take(n)(@rest) @buffer

    toString: ->
        | @empty => '<empty>'
        | @empty-rest => "<#{@buffer}>"
        | otherwise => "<#{@buffer} ...>"
    

window <<< { Stream }
