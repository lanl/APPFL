\documentclass[11pt]{article}

\usepackage[letterpaper, total={6.5in,9in}]{geometry}

\usepackage{listings}
\lstloadlanguages{Haskell}
\lstnewenvironment{code}
    {\lstset{}%
      \csname lst@SetFirstLabel\endcsname}
    {\csname lst@SaveFirstLabel\endcsname}
    \lstset{
      basicstyle=\small\ttfamily,
      flexiblecolumns=false,
      basewidth={0.5em,0.45em},
      literate={+}{{$+$}}1 {/}{{$/$}}1 {*}{{$*$}}1 {=}{{$=$}}1
               {>}{{$>$}}1 {<}{{$<$}}1 {\\}{{$\lambda$}}1
               {\\\\}{{\char`\\\char`\\}}1
               {->}{{$\rightarrow$}}2 {>=}{{$\geq$}}2 {<-}{{$\leftarrow$}}2
               {<=}{{$\leq$}}2 {=>}{{$\Rightarrow$}}2 
               {\ .}{{$\circ$}}2 {\ .\ }{{$\circ$}}2
               {>>}{{>>}}2 {>>=}{{>>=}}2
               {|}{{$\mid$}}1               
    }

\title{Circular Programming Example}
\author{Kei Davis}
\date{April 1, 2015}
\begin{document}
\maketitle

\noindent The classic example of circular programming in a pure non-strict
functional language is the solution to the \emph{repmin} problem:

\begin{quote} 
Given a binary tree $T$\/ with integer values at the leaves,
create a tree of the same shape with all leaf values equal to the minimal leaf
value of $T$ \emph{with only one traversal of} $T$.  
\end{quote}

Here's a simple analog of an example that came up in actual programming.
Suppose you have a recursive \emph{let}, that is, where all of the defined
entities have the others in scope, something like this
\begin{verbatim}
  let { x = x yz yyy
        y = zy x z z
        z = yyz x
      } in e
\end{verbatim}
that we'll encode like this (never mind the \texttt{e}),
\begin{code}
  type Var = Char
  type Obj = String
  defs :: [(Var, Obj)]
  defs = [('x', "yyz x"),
          ('y', "zy x z z"),
          ('z', "x yz")]
\end{code}
Now suppose that the entities' sizes are equal to their length
and that they are stack (actually heap) allocated, one after the next,
that the stack grows upward, and that the entities
are accessed by offsets from a stack pointer \texttt{SP}, e.g., \texttt{z} is
at \texttt{SP(-4)}.

One way to calculate the environment for resolving references on the
right-hand sides (RHSs) is first calculate all the sizes, build the
environment, then do the code generation for the RHSs, which entails
traversing them twice.  When the RHSs are large, or calculating their sizes is
a complex property of their structure this can be rather inefficient both in
terms of run time and coding effort.  Fortuitously a one-pass solution just
falls out by writing the top-level equations in the most natural way, as
follows.

Define the type of environments as a map from variable names
to integers (the sizes of the objects they represent)
\begin{code}
  type Env = [(Var,Int)]
\end{code}
and a simple-minded ``code generator'' that replaces variables in the
objects with stack pointer offsets, textually encoded.  For a single
character that's just
\begin{code}
  cgvar :: Env -> Var -> String
  cgvar env v = case lookup v env of
                  Nothing -> error "env lookup fail"
                  Just i -> "SP(" ++ show i ++ ")"
\end{code}
Now the code for a whole string, a list of variables and spaces.  This will
return a pair:  the size of the object and the generated code.
\begin{code}
  cgobj :: Env -> Obj -> (Int, String)
  cgobj env (' ':e) = let (n, c) = cgobj env e
                      in (n + 1, c)

  cgobj env (v:e) = let (n, c) = cgobj env e
                    in (n + 1, cgvar env v ++ c)

  cgobj env "" = (0, [])
\end{code}
Finally the code generation for recursive let.  There are two things to
observe:  the objects (strings) are traversed only once, and \texttt{env}
is seemingly needed before it is defined.  For a specific instance,
consider that the code generation for \texttt{('x',"x yz yyy")} references the
offset of \texttt{z} before that element is reached by the \texttt{map}.
\begin{code}
  cgletrec defs = let (vars, objs) = unzip defs
                      env = zip vars (scanr (flip (-)) 0 sizes)
                      (sizes, objcodes) = unzip(map (cgobj env) objs)
                  in (env, zip vars objcodes)
\end{code}
To try this just load this document source into ghci and type \texttt{cgletrec defs}.

My observation is that circular programming can look even more like magic
when the plumbing is hidden in a monad and the circular dependencies flow
through that plumbing---here it's quite exposed with all the \emph{map},
\emph{zip}, \emph{unzip}, and pairing/unpairing.  Rhetorical question: is
hiding magic in hidden plumbing good programming practice?
\end{document}
