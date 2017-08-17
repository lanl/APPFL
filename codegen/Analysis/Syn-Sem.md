# Language Syntax and Semantics


Conventions follow that of the Haskell Report:

-----------       -----------
[*pattern*]       Optional
{*pattern*}       Zero or more occurrences
(*pattern*)       Grouping
*pat₁* | *pat₂*    Choice
*script*          A syntax pattern
`fixed width`     Literal characters
`⎵`               A literal space
-----------       -----------

<!--
Unicode Symbol Input reference

𝑃         \mitP
𝐷         \mitD
𝑇         \mitT
𝑉         \mitV
Ƥ         \lthkP
Ɗ         \lthkD
Ɛ         \ltE
Ɓ         \lthkB
Ʋ         \lthkV
Ƙ         \lthkK
ℙ         \BbbP
𝕍         \BbbV
𝔼         \BbbE
𝔻         \BbbD
α         \alpha
τ         \tau
δ         \delta
𝑣         \mitv
⟧         \rBrack
⟦         \lBrack
⎵         \underbracket
↦        \mapsto
≝         \eqdef
⃗         \vec        (combining right arrow above)
-->

--------------------------------------------------------------------------------

Name           Alias       Definition
----------    -------   -- -----------------------------
*program*        𝑃      →  { *datadef* | *valdef* }
*datadef*        𝐷      →  *tycon* {`⎵` *tyvar*} `=` *dcondefs*
*dcondefs*              →  *dcondef* {`|` *dcondef*}
*dcondef*               →  *conid* {`⎵` *type*}
*tycon*          𝑇      →  *conid*
*tyvar*          α      →  *varid*
*type*           τ      →  *tycon* {*type*} | *tyvar*
*valdef*         𝑉      →  *varid* `=` *expr* `;`
*expr*           𝑒       →  *abstraction* | *application* | *literal*
                        |  *varid* | *conid* | *case* | *let*
                        |  `(` *expr* `)`
*abstraction*           →  `\` *varid* `->` *expr*
*application*           →  *expr* `⎵` *expr*
*case*                  →  `case` *expr* `as` *varid* `of` `{` *clauses* `}`
*let*                   →  `let` {*valdef*} `in` *expr*
*literal*        𝑙      →  *integer*
*integer*               →  *number* {*number*}
*clauses*               →  *clause* `;` {[*clause* `;`]}
*clause*         𝐾      →  (*conMatch* | *literal* | `_` ) `->` *expr*
*conMatch*              →  *conid* {`⎵`*varid*}
\
\
\
*conid*           δ     →  *upper* {*idchar*}
*varid*           𝑣     →  *lower* {*idchar*}
*idchar*                →  *upper* | *lower* | *number* | *idsym*
*upper*                 →  `A` | `B` | … | `Y` | `Z`
*lower*                 →  `a` | `b` | … | `y` | `number`
*number*                →  `0` | `1` | … | `8` | `9`
*idsym*                 →  `'` | `_`

--------------------------------------------------------------------------------

## Standard Semantics

This follows the "dynamic" semantics of [KHL91] fairly closely, though
modifications are made to support higher-order semantics. Some of the aliases
above are used for convenience.

-   Bold text indicates a vector.
-   𝐶ᴰ(**𝑥**) is overloaded, denoting the construction of some datatype for
    constructor `D` parameterized by the vector of arguments **𝑥**, and as a
    pattern match on a particular constructor of a sum-type.
-   ρ[`x`↦𝑣] is an augmentation of the environment ρ with the mapping between `x` and 𝑣.
-   ρ(`x`) indicates an indexing operation into the environment, looking up the
    value of `x`.
-   The existence of a function Ɓ which maps the program syntax for literals to
    "real" values is assumed, mapping, for example, the text literal `1` to the
    "real" value 1.

### TODO:
-   Describe *drop* and *lift*.  Need to figure out where *lift* should be
    introduced.  Don't want to over-lift.
-   Literals should really be considered unboxed and treated accordingly (though I may implement some
    kind of sytactic sugar for literal boxed Ints in the parser)

<div><!-- Make the HTML output prettier -->

<style type="text/css" scoped>
table {width: 80%; border: 1px solid black;}
td {padding: 0 0 1em 1em;}
</style>


----------------------------------    ----      -------------------------------------------------
__Semantic Functions__

Ƥ                                      :        *Prog* → *Env*

Ʋ                                      :        *ValDef* → *Env* → *Env*

Ɗ                                      :        *DataDef* → *Env* → *Env*

Ɛ                                      :        *Expr* → *Env* → *Value*

Ƙ                                      :        *Clause* → *Env* → *{Pattern ⟶ Value}*

Ɓ                                      :        *Literal* → *Value*

__Definitions__

Ƥ⟦…,𝑉ᵢ,…,𝐷ⱼ,…  ⟧                       ≝        *fix* (λρ.⋃ᵢⱼ{Ʋ⟦Vᵢ⟧ρ, Ɗ⟦𝐷ⱼ⟧ρ})

Ʋ⟦`x =` 𝑒 ⟧ρ                           ≝        ρ[`x` ↦ Ɛ⟦𝑒⟧ρ]

Ɗ⟦𝑇 **α** `=` … `|` `Dᵢ` **τ** `|` …⟧  ≝       ρ ∪ {…, `Dᵢ` ↦ **λ𝑣**.(𝐶ᵢ(**𝑣**)), …}

                                                **λ𝑣** is used here as shorthand to denote a (still curried)
                                                function of the same arity as that of the data constructor
                                                𝐶ᵢ.


Ɛ⟦𝑙⟧ρ                                  ≝        (Ɓ⟦𝑙⟧)
--
Ɛ⟦`x`⟧ρ                                ≝        ρ(`x`)

Ɛ⟦𝑒₁𝑒₂⟧ρ                               ≝        *case* Ɛ⟦𝑒₁⟧ρ *in* \
                                                ⊥ ⟶ ⊥              \
                                                (*f*) ⟶ *f* Ɛ⟦𝑒₂⟧ρ

Ɛ⟦`\`**`x`** `->` 𝑒⟧ρ                  ≝        (λ𝑣.Ɛ⟦𝑒⟧ρ[`x` ↦ 𝑣])

Ɛ⟦`case` 𝑒 `as x of` …`;`𝐾ᵢ`;`…⟧ρ      ≝        *case* Ɛ⟦𝑒⟧ρ *in*  ⋃ᵢ(Ƙ⟦𝐾ᵢ⟧ρ[`x`↦Ɛ⟦𝑒⟧ρ]) ∪ {⊥ ⟶ ⊥}

Ɛ⟦`let`…, 𝑉ᵢ, … `in` 𝑒⟧ρ               ≝        Ɛ⟦𝑒⟧ρ∪(*fix*(λρ.{…,Ʋ⟦𝑉ᵢ⟧ρ,…}))

Ƙ⟦𝑙 `->` 𝑒⟧ρ                           ≝        {Ɓ⟦l⟧ ⟶ Ɛ⟦𝑒⟧ρ}

Ƙ⟦`D`**`x`** `->` 𝑒⟧ρ                  ≝        {𝐶ᴰ(**𝑣**) ⟶ Ɛ⟦𝑒⟧ρ[`xᵢ`↦𝑣ᵢ]}

Ƙ⟦`_ ->` 𝑒⟧ρ                           ≝        {*otherwise* ⟶ Ɛ⟦𝑒⟧ρ}

----------------------------------    ----      -------------------------------------------------

</div>

## Abstract Semantics

Combining the ideas from the High-fidelity, higher-order portion (sec. 5) of
"Strictness Analysis in 4D" and the partial projections introduced in
"Representing Demand by Partial Projections".

In the former, Projection Transformers (functions from projections to
projections; "PTs") are the base abstract value in the first-order case.  In
higher order, the base abstract value is a tuple of backward and forward
abstractions.  The backward abstraction is just a Projection Transformer: a
function that yields a safe projection given a context. The forward abstraction
is the necessary addition to deal with the higher-order nature of the language.
Consider the simple higher order function `($)` possibly defined in Haskell as
`\f v -> f v`.  Where `($)` is applied, we care about the demand on the
arguments given to it, which is a function of its definition.  But its
definition uses its first argument as a function, applying it to the second
argument. This can express some demand on that second argument.  We need to know
what kind of demand that is, which is where the forward abstraction comes into
play: A function from abstract value to abstract value.



Projections as presented in "Projections for Strictness Analysis" require
lifting the already lifted domains one more to encode "simple" strictness
(Launchbury's claim in the Partial Projection paper).  When values were
unacceptable to a projection, they mapped to ABORT (here, ⇓).  Strict
projections mapped ⊥ to ⇓.  This extra lifting makes the semantics a bit messier
to define.  Launchbury does away with this, showing how partial projections on
the singly-lifted domains are isomorphic to total projections on the
doubly-lifted domains.  For this to hold, any partial projection may only be
undefined on "some lower portion of its domain".  Strictness is then
characterized by partial projections that are undefined on a non-empty set of
values.  The base four-point domain of total and partial projections are here
for comparison.

-----------------------------------------------------------
Projection Name           Total Def            Partial Def
----------------          ----------           ------------
Ide                       Ide x = x            Ide x = x

Str                       Str ⊥ = ⇓\           Str ⊥ = \<UNDEF\>\
                          Str x = x            Str x = x

Abs                       Abs ⇓ = ⇓\           Abs x = ⊥
                          Abs x = ⊥

Fail                      Fail x = ⇓           Fail x = \<UNDEF\>
----------------          ----------           ------------


## Abstract Semantics ##


Some thoughts for Future David:

The factored domains from the 4D paper make sense, but I don't immediately see
how to get strictness information about nested contexts.  For example,

```haskell
groups xs ns = 
   let take n ys = case n of
             0 -> []
             _ -> case ys of
                   [] -> []
                   h:t -> h : take (n - 1) t
       drop n ys = case n of
             0 -> ys
             _ -> case ys of
                   [] -> []
                   h:t -> drop (n - 1) t
   in case xs of 
       [] -> []
       _  -> case ns of
              [] -> [xs]
              n:t -> take n xs : groups (drop n xs) t
```

It would be good to know here that `take` and `drop` are strict in their first
argument in any strict context but not strict in their second.  I *think* this
is doable without changing the 4D approach (other than to fit the language) just
by saving information as successive fixed points are found, but I'm hesititant
to commit to this.

The alternative would be accumulating the abstract values, as in the partial
projections paper.  I'm leaning towards this, but haven't nailed down what the
abstract values would look like (or the rules).

----------------------------------     ----      -------------------------------------------------
__Semantic Functions__

Ƥ#                                      :

Ʋ#                                      :

Ɗ#                                      :

Ɛ#                                      :

Ƙ#                                      :

Ɓ#                                      :

__Definitions__

Ƥ#⟦…,𝑉ᵢ,…,𝐷ⱼ,…  ⟧                       ≝        *fix* (λρ.⋃ᵢⱼ{Ʋ#⟦Vᵢ⟧ρ, Ɗ#⟦𝐷ⱼ⟧ρ})

Ʋ#⟦`x =` 𝑒 ⟧ρ                           ≝        ρ[`x` ↦ Ɛ#⟦𝑒⟧ρ]

Ɗ#⟦𝑇 **α** `=` … `|` `Dᵢ` **τ** `|` …⟧  ≝

Ɛ#⟦𝑙⟧ρ                                  ≝

Ɛ#⟦`x`⟧ρ                                ≝

Ɛ#⟦𝑒₁𝑒₂⟧ρ                               ≝

Ɛ#⟦`\`**`x`** `->` 𝑒⟧ρ                  ≝

Ɛ#⟦`case` 𝑒 `as x of` …`;`𝐾ᵢ`;`…⟧ρ      ≝

Ɛ#⟦`let`…, 𝑉ᵢ, … `in` 𝑒⟧ρ               ≝

Ƙ#⟦𝑙 `->` 𝑒⟧ρ                           ≝

Ƙ#⟦`D`**`x`** `->` 𝑒⟧ρ                  ≝

Ƙ#⟦`_ ->` 𝑒⟧ρ                           ≝

----------------------------------     ----      -------------------------------------------------


