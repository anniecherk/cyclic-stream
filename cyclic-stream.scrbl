#lang scribble/base
@(require scribble/manual
          racket/list)

@(define (tuple . args)
   (list "<" (add-between args (list "," 'nbsp)) ">"))

@(define (crossing . content)
  @nested[#:style 'inset
          @tabular[#:style 'block (list (list (apply verbatim content)))]])

@title[#:version "" #:date ""]{Cyclic Streams to Represent Factors and Crossings}

A factor is a list of level. Often, a factor needs to be repeated
indefinitely to generate trials and in combination with other factors.
At the same time, the number of original levels matters for operations
such as Latin Squares crossing. So, we generalize to @deftech{cyclic
streams}, which repeat a set of levels or conditions with a fixed
period.

As an example, repeating a factor's levels in order---which
corresponds to sequential access for generating conditions---is a
stream whose period matches the number of levels in the factor. Random
(without replacement) access for a factor of @math{N} levels
corresponds to a stream where each chunk of @math{N} elements matches
the original levels in some random order, but each chunk of @math{N}
is in a different order; the period is still @math{N}, because each
chunk corresponds to using all of the factor's levels.

@; ----------------------------------------
@section{Randomizations}

We can @deftech{randomize} a stream of period @math{N} by taking each
group of @math{N} elements, shuffling them, and adding the shuffled
elements to a new stream. The period of the resulting stream is the
same as the original.

We can @deftech{randomize with replacement} from a stream of period
@math{N} by taking each group of @math{N} elements, selecting just
one, and adding the selected element new stream. The period of the
resulting stream is 1. Although the period of the stream is infinite
in the sense that it doesn't repeat a pattern, a choice of 1 for the
period reflects the way that the new stream is drawn from an existing
one, and it makes the stream cross in a useful way with other streams.

@; ----------------------------------------
@section{Weights, Harmonics, Rotations, and Truncations}

Given a stream of period @math{N}, we can @deftech{weight} the stream
by repeating each element @math{M} times to produce a stream of period
@math{N·M}. For example, weighting by 3 a stream that produces @tt{A}
and @tt{B} (so the period is 2) produces a stream with period 6:
@tt{A}, @tt{A}, @tt{A}, @tt{B}, @tt{B}, @tt{B}. Randomizing this
stream puts those six elements in random order, and then picks another
random order for the next six elements.

Alternatively, we can take a @deftech{harmonic} of a stream of period
@math{N} by grouping @math{M} cycles together. A harmonic of 3 for the
stream that produces @tt{A} and @tt{B} (so the period is 2) also
produces a stream of period 6: @tt{A}, @tt{B}, @tt{A}, @tt{B}, @tt{A},
@tt{B}. Randomizing this stream produces the same result as
randomizing the variant that is weighted by 3---but the non-randomized
variants can behave differently in other combinations.

We can @deftech{rotate} the stream by @math{M} by moving @math{M}
items within a cycle from the front of the cycle to the end. A
rotation preserves the period of a stream, and just reorders the items
within the stream (so it's of no interest for a randomized stream). A
rotation of a stream that repeats @tt{A}, @tt{B}, @tt{C} by 1 produces
the repeating pattern @tt{B}, @tt{C}, @tt{A}.

We can @deftech{truncate} a stream of period @math{N} to a smaller
period @math{M} by discarding the last @math{N-M} elements of each
group of @math{N}. For example, if we have a large list of words, we
might want to randomize the group and only use a subset before we
start over drawing from the full pool of words; truncating after
randomizing achieves that effect.

@; --------------------------------------------------
@section{Sequences}

Streams can be combined in @deftech{sequence}. The period of the
sequenced streams is the sum of the streams' periods. For example,
sequencing randomized streams @tt{A}--@tt{B}--@tt{C} and
@tt{a}--@tt{b} might produce the following, where the level over each
number @math{i} reflects the level selected for the @math{i}th trial:
@;
@crossing{
   A C B a b    C A B b a

   1 2 3 4 5

                6 7 8 9 10
}
@;
Randomizing after sequencing would mix the uppercase and lowercase
orders together within a group of 5:
@;
@crossing{
   A C B a b    C A B b a

   4 2 1 3 5

               10 6 8 9 7
}
@;
Written so that the selection numbers are in order instead of the
levels, the above sequence is equivalently pictured as
@;
@crossing{
   B C a A b    A a B b C

   1 2 3 4 5

                6 7 8 9 10
}

@; ----------------------------------------
@section{Crossing}

@deftech{Crossing} draws elements from two streams and combines them
into a new stream whose values are tuples of the original values. For
example, crossing a factor with values @tt{A} and @tt{B} with a factor
whose value are @tt{a} and @tt{b} produces a stream whose values are
@tuple[@tt{A} @tt{a}], @tuple[@tt{B} @tt{a}], @tuple[@tt{A} @tt{b}],
and @tuple[@tt{B} @tt{b}]:

@crossing{
   <A,a> <A,b> <B,a> <B,b>
   
    1      2    3     4
}
@;
That same crossing can be pictured using two dimensions:
@;
@crossing{
   A B
   
a  1 3
b  2 4
}
@;
Drawing elements from the first stream would produce @tt{A}, @tt{B},
@tt{A}, @tt{B}, etc., in a repeating pattern. Drawing from the crossing
similarly repeats the overall sequence:
@;
@crossing{
   A B   A B
   
a  1 3
b  2 4

a        5 7
b        6 8
}
@;
Naturally, the period of crossed streams is the product of the crossed
streams' periods. Crossing a 3-element factor with a 2-element factor
is straightforward:
@;
@crossing{
   A B C     A  B  C
   
a  1 2 3
b  4 5 6

a            7  8  9
b           10 11 12
}

The order of elements in a crossing corresponds to a @deftech{blocked}
pattern, as opposed to a randomized pattern. If the crossing result is
randomized, then groups of 4 combinations will be in random order, as
illustrated by having four consecutive numbers grouped below, but
randomly ordered within the group:
@;
@crossing{
   A B   A B
   
b  1 3
a  4 2

a        7 6
b        5 8
}
@;
In contrast, if the two factors are randomized before crossing, then
independent randomizations and @tt{A} and @tt{B} will be paired with
independent randomizations of @tt{a} and @tt{b}, show here by
reordering the levels instead of the selection numbers:
@;
@crossing{
   A B   B A
   
b  1 3
a  2 4

a        5 7
b        6 8
}
@;
Note that blocking causes each set of four elements to have every
possible combination of the factors.

Weighting a stream before crossing makes the stream's elements appear
multiple times in combinations within a single cycle. For example,
weighting and @tt{A}--@tt{B} stream by 2 before crossing with an
@tt{a}--@tt{b} stream produces a stream of period 4:
@;
@crossing{
   A A B B   A  A  B  B
   
a  1 3 5 7
b  2 4 6 8

a             9 11 13 15
b            10 12 14 16
}
@;
Randomizing this stream reorders within each cycle:
@;
@crossing{
   A A B B   A  A  B  B
   
a  6 2 5 8
b  4 1 6 3

a            12 10 13 15
b            11 14  9 16
}


@; --------------------------------------------------
@section{Zips}

A @deftech{zip} combines two streams to form tuples, but it doesn't
``fully cross'' the two streams. Instead, it matches the first
elements of the streams, then the second elements, and so on:

@crossing{
   A B   A B
   
a  1
b    2

a        3 
b          4
}
@;
As this example illustrates, a zip does not necessarily cover all
possible combinations of stream elements.

When the periods of two streams do not match, then a choice has to be
made about how to zip them. One choice is to run through the streams
in order and consider the period of the resulting stream to be the
least-common multiple of the input periods:
@;
@crossing{
   A B C   A B C
   
a  1
b    2

a      3 
b          4

a            5
b              6
}
@;
Another possibility is to have one of the streams determine the
period, and then draw from the other with a consistent alignment (i.e,
consistent position within a cycle):
@;
@crossing{
   A B C   A B C
   
a  1
b    2

a      3
b          

a          4 
b            5

a              6
b
}
@;
With this latter choice, note that @tt{b}s in the second sequence are
sometimes skipped to preserve alignment. Still, this latter kind of
zip is more useful for a design where, for example, each subject
should see @tt{a} paired with only a particular element among @tt{A},
@tt{B}, and @tt{C}.

@; --------------------------------------------------
@section{Latin Squares}

A @deftech{Latin Squares} combination is a set of zips such that all
possible combinations are covered, but each zip has only a subset of
the combinations. One dimension of the Latins Squares combination
determines the number of zips. In the following table, @tt{1.2} means
the first zip's second element:
@;
@crossing{
   A    B      C    D
   
a  1.1  2.1    3.1  4.1
b  4.2  1.2    2.2  3.2

a  3.3  4.3    1.3  2.3
b  2.4  3.4    4.4  1.4
}
@;
Further selections of the Latin Squares combination will maintain the
combinations of each zip:
@;
@crossing{
   A    B      C    D     A    B      C    D
   
a  1.1  2.1    3.1  4.1
b  4.2  1.2    2.2  3.2

a  3.3  4.3    1.3  2.3
b  2.4  3.4    4.4  1.4

a                         1.5  2.5    3.5  4.5
b                         4.6  1.6    2.6  3.6

a                         3.7  4.7    1.7  2.7
b                         2.8  3.8    4.8  1.8
}

When neither stream in a Latin Squares combination has a period that
is a multiple of the other stream's period, then the overall pattern
doesn't follow a diagonal, since the goal is to stay on the same
diagonal within a cycle of the combination. Here's a 2-by-3 example:
@;
@crossing{
   A    B      A   B
   
a  1.1  2.1
b  2.2  1.2
c  1.3  2.3

a              1.4 2.4
b              2.5 1.5
c              1.6 2.6
}
@;
Here's a 3-by-2 example:
@;
@crossing{
   A    B    C      A   B   C
   
a  1.1  2.1  1.3
b  2.3  1.2  2.2

a                   1.4 2.4 1.6
b                   2.6 1.5 2.5
}
@;
As the examples illustrate, a particular zip @math{i} is formed by
first rotating the first stream by @math{i}, and then zipping it with
the other dimension.

A Latin Squares combination of more than two streams takes a
hyper-diagonal through the full crossing. The number of zips is still
determined by the first stream, while the period is the produce of the
remaining streams' periods:
@;
@crossing{
        A          B
     x     y     x    y
a   1.1   2.4   2.1  1.4
b   1.5   2.2   2.5  1.2
c   1.3   2.6   2.3  1.6
}
@;
Compare to crossing a @tt{A}--@tt{B} stream by a @tt{x}--@tt{y} stream
before a Latin Squares combination with an @tt{a}--@tt{b}--@tt{c}
stream:
@;
@crossing{
       A        B
     x   y    x   y
a   1.1 2.1  3.1 4.1
b   4.2 1.2  2.2 3.2
c   3.3 4.3  1.3 2.3
}

@; --------------------------------------------------
@section{Permutations and Flattening}

If we @tech{randomize} the stream @tt{A}--@tt{B}--@tt{C} and take the
first six elements of the stream, we're likely to get two different
orders of the three letters, but it's not guaranteed. For example, the
first six elements of the randomized stream might be @tt{B}, @tt{A},
@tt{C}, @tt{B}, @tt{A}, and @tt{C}. As a balancing constraint on our
design, however, we might want to ensure that six iterations of the
three choices in random order will produce all of the distinct
orderings that are needed, while still choosing randomly among those
orders for the first order.

A @deftech{permutation} of a stream of period @math{N} creates a new
stream whose elements are themselves streams. Each of those streams is
a list of @math{N} elements, and the period of the overall stream is
@math{N!} (since there are @math{N!} permutations of @math{N}
elements). Randomizing the permutations then picks a random order for
the permutations, while ensuring that every permutation is covered by
each group of @math{N!} streams.

A @deftech{flatten} operation can convert the randomly ordered stream
of @math{N!} permutations (each containing @math{N} elements) back to
a stream of period @math{N}. The flattened stream will have the same
structure as a randomized version of the original stream, but it is
guaranteed to be balanced in the sense of trying all possible
``random'' orders before repeating a ``random'' order.

Note that @tech{flatten}ing a @tech{Latin Squares} combination would
effectively result in a full crossing, but with a period that
corresponds to a single diagonal instead of the full crossing's
period.

@; --------------------------------------------------

@;{
 weighting in advance versus re-weight based on derived properties
 filter ?? [like random with replacement: period collapses to 1]
 constrain-order ?? [e.g., how much entry in resulting sequence?]
}
