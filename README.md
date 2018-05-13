# effects-sequences

An experiment in [extensible effects à la Oleg](http://okmij.org/ftp/Haskell/extensible/), but using richer sequences for the effect context.


## Ideas

### Effect sequences structured as binary trees

Represent the effect sequence with a (type-level) binary tree instead of a (type-level) list. This lets us talk about the effects in isolation more easily by considering an effect within its leaf node; it also turns out to make membership constraints into a special case of a more general subsequence relation.

It’s my hope that this shape will also make it easier for us to eliminate effects somewhere in the middle of the sequence instead of always eliminating them at the head of the list.


### Handling (sub)sequences of effects

List-based effect sequences result in effect handlers generally having the form:

```haskell
handler :: Effect (effect ': effects) a -> Effect effects b
```

That is, they remove `effect` from the sequence by interpreting it into a value of type `b`, possibly making requests of other members of `effects`. There are variations on this—@lexi-lambda’s superb [`freer-simple`][] defines a suite of `reinterpret` handlers which additionally _add_ effects to the sequence—but they generally limit the programmer to handling effects at the head of the list. Because handlers delimit the scope of an effect, i.e. you introduce the capability to perform an effect by introducing its handler, you are equally limited to handling effects in LIFO order—you can’t insert effects into the middle of the sequence.

By virtue of the binary tree structure of the effect sequence, effect handlers in `effects-sequences` have the general form:

```haskell
handler :: Effect effects a -> b
```

This is often seen in the primitive case:

```haskell
handler :: Effect ('S effect) a -> b
```

and the degenerate case:

```haskell
run :: Effect 'Z a -> a
```

which respectively say that when we have a singleton sequence containing `effect`, we can interpret it to produce a `b`, and that when we have handled all of the effects in some (sub)sequence, we can extract a result value.

However, this scales up to larger sequences as well. For example, we could implement a `State` effect with the composition of a `Reader` effect and a `Writer` effect, and the handler for such might look like so:

```haskell
runStateRW :: state -> Effect ('S (Reader state) ':+: 'S (Writer state)) result -> (result, state)
```

which says that (given some initial value), we can interpret the subsequence composed of `Reader` and `Writer` effects for `state` into a pair of result and final state.


[`freer-simple`]: https://github.com/lexi-lambda/freer-simple#readme
