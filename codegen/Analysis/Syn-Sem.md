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
*abstraction*           →  `\` *varid* {`⎵`*varid*} `->` *expr*
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
*upper*                 →  `A` | `B` | ⋯ | `Y` | `Z`
*lower*                 →  `a` | `b` | ⋯ | `y` | `z`
*number*                →  `0` | `1` | ⋯ | `8` | `9`
*idsym*                 →  `'` | `_`

--------------------------------------------------------------------------------

## Standard Semantics

This follows the "dynamic" semantics of [KHL91] fairly closely, though
modifications are made to support higher-order semantics. Some of the aliases
above are used for convenience.

-   Bold text indicates a vector, e.g. the **𝑣** parameters in a lambda abstraction.
-   𝐶ᴰ(**𝑥**) is overloaded, denoting the construction of some datatype for
    constructor `D` parameterized by the vector of arguments **𝑥**, and as a
    pattern match on a particular constructor of a sum-type.
-   ρ[`x`↦𝑣] is an augmentation of the environment ρ with the mapping between `x` and 𝑣.
-   ρ(`x`) indicates an indexing operation into the environment, looking up the
    value of `x`.
-   The existence of a function Ɓ which maps the program syntax for literals to
    "real" values is assumed, mapping, for example, the text literal `1` to the
    "real" value 1.
-   TODO: Describe *drop* and *lift*.  Need to figure out where *lift* should be
    introduced.  Don't want to over-lift.

<div><!-- Make the HTML output prettier -->
<style type="text/css" scoped>
table {width: 80%; border: 1px solid black;}
td {padding: 0 0 1em 1em;}
</style>


----------------------------------   ----      -------------------------------------------------
Ƥ                                     :        *Prog* → *Env*

Ʋ                                     :        *ValDef* → *Env* → *Env*

Ɗ                                     :        *DataDef* → *Env* → *Env*

Ɛ                                     :        *Expr* → *Env* → *Value*

Ƙ                                     :        *Clause* → *Env* → *{Pattern ⟶ Value}*

Ɓ                                     :        *Literal* → *Value*

Ƥ⟦⋯𝑉ᵢ,⋯,𝐷ⱼ,⋯  ⟧                        ≝         *fix* (λρ.⋃ᵢⱼ{Ʋ⟦Vᵢ⟧ρ, Ɗ⟦𝐷ⱼ⟧ρ})

                                               We need the fixed point both here and in the rule 
                                               for `let` expressions because there is arbitrary 
                                               mutual recursion in the definitions.

Ʋ⟦`x =` 𝑒 ⟧ρ                           ≝         ρ[`x` ↦ Ɛ⟦𝑒⟧ρ]

Ɗ⟦𝑇 **α** `=` ⋯ `|` `D` **τ** `|` ⋯⟧  ≝         ρ ∪ {⋯, `D` ↦ λ**𝑣**.𝐶ᵢ(*lift*(**𝑣**))⟧ρ, ⋯}

Ɛ⟦𝑙⟧ρ                                  ≝         Ɓ⟦𝑙⟧

Ɛ⟦`x`⟧ρ                               ≝         ρ(`x`)

Ɛ⟦𝑒₁𝑒₂⟧ρ                                ≝         *case* Ɛ⟦𝑒₁⟧ρ *in* \
                                               ⊥ ⟶ ⊥ \
                                               *lift*(f) ⟶ f Ɛ⟦𝑒₂⟧ρ

Ɛ⟦`\`**`x`** `->` 𝑒⟧ρ                  ≝         λ**𝑣**.Ɛ⟦𝑒⟧ρ[`xᵢ`↦𝑣ᵢ]

Ɛ⟦`case` 𝑒 `as x of` ⋯`;`𝐾ᵢ`;`⋯⟧ρ      ≝         *case* Ɛ⟦𝑒⟧ρ *in*  ⋃ᵢ(Ƙ⟦𝐾ᵢ⟧ρ[`x`↦Ɛ⟦𝑒⟧ρ]) ∪ {⊥ ⟶ ⊥}

Ɛ⟦`let`⋯, 𝑉ᵢ, ⋯ `in` 𝑒⟧ρ               ≝         Ɛ⟦𝑒⟧ρ∪(*fix*(λρ.{⋯,Ʋ⟦𝑉ᵢ⟧ρ,⋯}))

Ƙ⟦𝑙 `->` 𝑒⟧ρ                           ≝          {Ɓ⟦l⟧ ⟶ Ɛ⟦𝑒⟧ρ}

Ƙ⟦`D`**`x`** `->` 𝑒⟧ρ                 ≝          {𝐶ᴰ(**𝑣**) ⟶ Ɛ⟦𝑒⟧ρ[`xᵢ`↦𝑣ᵢ]}

Ƙ⟦`_ ->` 𝑒⟧ρ                          ≝          {*otherwise* ⟶ Ɛ⟦𝑒⟧ρ}
----------------------------------   ----      -------------------------------------------------

</div>

