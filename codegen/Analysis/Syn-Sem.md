# Language Syntax and Semantics


Syntax conventions follow that of the Haskell Report:

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
⎵        \underbracket
↦        \mapsto
--> 

--------------------------------------------------------------------------------

Name         Alias     Definition
----------   -----  -- -----------------------------
*program*    𝑃      →  { *datadef* | *valdef* }
*datadef*    𝐷       →  *tycon* {`⎵` *tyvar*} `=` *dcondefs*
*dcondefs*          →  *dcondef* {`|` *dcondef*}
*dcondef*           →  *conid* {`⎵` *type*}
*tycon*      𝑇      →  *conid*
*tyvar*      α      →  *varid*
*type*       τ      →  *tycon* {*type*} | *tyvar*
*valdef*     𝑉      →  *varid* `=` *expr* `;`
*expr*       𝑒       →  *abstraction* | *application* | *literal* 
                    |  *varid* | *conid* | *case* | *let*
                    |  `(` *expr* `)`
*abstraction*       →  `\` *varid* {`⎵`*varid*} `->` *expr*
*application*       →  *expr* `⎵` *expr*
*case*              →  `case` *expr* `as` *varid* `of` `{` *clauses* `}`
*let*               →  `let` {*valdef*} `in` *expr*
*literal*           →  *integer*
*integer*           →  *number* {*number*}
*clauses*           →  *clause* `;` {[*clause* `;`]}
*clause*            →  (*conMatch* | *literal* | `_` ) `->` *expr*
*conMatch*          →  *conid* {`⎵`*varid*}
\
\
\
*conid*       δ     →  *upper* {*idchar*}
*varid*       𝑣     →  *lower* {*idchar*}
*idchar*            →  *upper* | *lower* | *number* | *idsym*
*upper*             →  `A` | `B` | ⋯ | `Y` | `Z`
*lower*             →  `a` | `b` | ⋯ | `y` | `z`
*number*            →  `0` | `1` | ⋯ | `8` | `9`
*idsym*             →  `'` | `_`

--------------------------------------------------------------------------------

## Standard Semantics

*This follows the "dynamic" semantics of [KHL91] fairly closely. Some of the
aliases above are used for convenience. Here, single square brackets denote a
list, and double square brackets denote some form of parameterized
interpretation.*




------------------------         --------    ---------------
ℙ                                   :         *Prog* → *Env*
𝕍                                   :         *ValDef* → *Env* → *Env*
𝔻                                   :         *DataDef* → *Env* → *Env*
𝔼                                   :         *Expr* → *Env* → *Value*
ℙ⟦⋯𝑉ᵢ,⋯,𝐷ᵢ,⋯  ⟧                       ≝         *fix* (λρ.⋃ᵢ{𝕍⟦Vᵢ⟧ρ, 𝔻⟦𝐷ᵢ⟧ρ})
𝕍⟦𝑣 `=` 𝑒 ⟧ρ                          ≝         ρ ∪ {𝑣 ↦ 𝔼⟦𝑒⟧ρ}
------                           --------    ---------------



