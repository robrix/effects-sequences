# effects-sequences

An experiment in [extensible effects à la Oleg](http://okmij.org/ftp/Haskell/extensible/), but using richer sequences for the effect context.


## Ideas

### Effect sequences structured as binary trees

Represent the effect sequence with a (type-level) binary tree instead of a (type-level) list. This lets us talk about the effects in isolation more easily by considering an effect within its leaf node; it also turns out to make membership constraints into a special case of a more general subsequence relation.

It’s my hope that this shape will also make it easier for us to eliminate effects somewhere in the middle of the sequence instead of always eliminating them at the head of the list.
