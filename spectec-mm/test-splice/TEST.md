# Preview

```sh
$ (dune exec ../src/exe-watsup/main.exe -- ../spec/*.watsup -l --splice-latex -p spec-splice-in.tex -w)
== Parsing...
== Elaboration...
== IL Validation...
\documentclass[a4paper]{scrartcl}

\usepackage{amsmath}
\usepackage{amssymb}
\usepackage{color}

\hyphenation{Web-Assembly}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\title{Wasm Formal Semantics}

\begin{document}

\small

\maketitle


\subsection*{Syntax}

$$
\begin{array}{@{}lrrl@{}l@{}}
\mbox{(number type)} & {\mathit{numtype}} &::=& \mathsf{i{\scriptstyle32}} ~|~ \mathsf{i{\scriptstyle64}} ~|~ \mathsf{f{\scriptstyle32}} ~|~ \mathsf{f{\scriptstyle64}} \\
\mbox{(vector type)} & {\mathit{vectype}} &::=& \mathsf{v{\scriptstyle128}} \\
\mbox{(reference type)} & {\mathit{reftype}} &::=& \mathsf{ref}~{\mathit{nul}}~{\mathit{heaptype}} \\
\mbox{(value type)} & {\mathit{valtype}} &::=& {\mathit{numtype}} ~|~ {\mathit{vectype}} ~|~ {\mathit{reftype}} ~|~ \mathsf{bot} \\[0.8ex]
\mbox{(result type)} & {\mathit{resulttype}} &::=& {{\mathit{valtype}}^\ast} \\
\end{array}
$$

$$
\begin{array}{@{}lcl@{}l@{}}
{|\mathsf{i{\scriptstyle32}}|} &=& 32 &  \\[0.8ex]
{|\mathsf{i{\scriptstyle64}}|} &=& 64 &  \\[0.8ex]
{|\mathsf{f{\scriptstyle32}}|} &=& 32 &  \\[0.8ex]
{|\mathsf{f{\scriptstyle64}}|} &=& 64 &  \\[0.8ex]
{|\mathsf{v{\scriptstyle128}}|} &=& 128 &  \\
\end{array}
$$

$$
\begin{array}{@{}lrrl@{}l@{}}
\mbox{(limits)} & {\mathit{limits}} &::=& [{\mathit{u{\scriptstyle32}}} .. {\mathit{u{\scriptstyle32}}}] \\[0.8ex]
\mbox{(global type)} & {\mathit{globaltype}} &::=& {\mathit{mut}}~{\mathit{valtype}} \\
\mbox{(function type)} & {\mathit{functype}} &::=& {\mathit{resulttype}} \rightarrow {\mathit{resulttype}} \\
\mbox{(table type)} & {\mathit{tabletype}} &::=& {\mathit{limits}}~{\mathit{reftype}} \\
\mbox{(memory type)} & {\mathit{memtype}} &::=& {\mathit{limits}}~\mathsf{i{\scriptstyle8}} \\[0.8ex]
{} \\[-2ex]
\mbox{(external type)} & {\mathit{externtype}} &::=& \mathsf{func}~{\mathit{deftype}} ~|~ \mathsf{global}~{\mathit{globaltype}} ~|~ \mathsf{table}~{\mathit{tabletype}} ~|~ \mathsf{mem}~{\mathit{memtype}} \\
\end{array}
$$

$$
\begin{array}{@{}l@{}rrl@{}l@{}}
& {\mathit{instr}} &::=& \mathsf{unreachable} \\ &&|&
\mathsf{nop} \\ &&|&
\mathsf{drop} \\ &&|&
\mathsf{select}~{({{\mathit{valtype}}^\ast})^?} \\ &&|&
\mathsf{block}~{\mathit{blocktype}}~{{\mathit{instr}}^\ast} \\ &&|&
\mathsf{loop}~{\mathit{blocktype}}~{{\mathit{instr}}^\ast} \\ &&|&
\mathsf{if}~{\mathit{blocktype}}~{{\mathit{instr}}^\ast}~\mathsf{else}~{{\mathit{instr}}^\ast} \\ &&|&
\mathsf{br}~{\mathit{labelidx}} \\ &&|&
\mathsf{br\_if}~{\mathit{labelidx}} \\ &&|&
\mathsf{br\_table}~{{\mathit{labelidx}}^\ast}~{\mathit{labelidx}} \\ &&|&
\mathsf{br\_on\_null}~{\mathit{labelidx}} \\ &&|&
\mathsf{br\_on\_non\_null}~{\mathit{labelidx}} \\ &&|&
\mathsf{br\_on\_cast}~{\mathit{labelidx}}~{\mathit{reftype}}~{\mathit{reftype}} \\ &&|&
\mathsf{br\_on\_cast\_fail}~{\mathit{labelidx}}~{\mathit{reftype}}~{\mathit{reftype}} \\ &&|&
\mathsf{call}~{\mathit{funcidx}} \\ &&|&
\mathsf{call\_ref}~{{\mathit{typeidx}}^?} \\ &&|&
\mathsf{call\_indirect}~{\mathit{tableidx}}~{\mathit{typeidx}} \\ &&|&
\mathsf{return} \\ &&|&
\mathsf{return\_call}~{\mathit{funcidx}} \\ &&|&
\mathsf{return\_call\_ref}~{{\mathit{typeidx}}^?} \\ &&|&
\mathsf{return\_call\_indirect}~{\mathit{tableidx}}~{\mathit{typeidx}} \\ &&|&
\mathsf{ref.null}~{\mathit{heaptype}} \\ &&|&
\mathsf{ref.i{\scriptstyle31}} \\ &&|&
\mathsf{ref.func}~{\mathit{funcidx}} \\ &&|&
\mathsf{ref.is\_null} \\ &&|&
\mathsf{ref.as\_non\_null} \\ &&|&
\mathsf{ref.eq} \\ &&|&
\mathsf{ref.test}~{\mathit{reftype}} \\ &&|&
\mathsf{ref.cast}~{\mathit{reftype}} \\ &&|&
... \\
\end{array}
$$

$$
\begin{array}{@{}l@{}rrl@{}l@{}}
& {\mathit{instr}} &::=& ... \\ &&|&
{\mathit{numtype}}.\mathsf{const}~{\mathit{c}}_{{\mathit{numtype}}} \\ &&|&
{\mathit{numtype}} . {\mathit{unop}}_{{\mathit{numtype}}} \\ &&|&
{\mathit{numtype}} . {\mathit{binop}}_{{\mathit{numtype}}} \\ &&|&
{\mathit{numtype}} . {\mathit{testop}}_{{\mathit{numtype}}} \\ &&|&
{\mathit{numtype}} . {\mathit{relop}}_{{\mathit{numtype}}} \\ &&|&
{{\mathit{numtype}}.\mathsf{extend}}{{\mathit{n}}} \\ &&|&
{\mathit{numtype}} . {{{{{\mathit{cvtop}}}{\mathsf{\_}}}{{\mathit{numtype}}}}{\mathsf{\_}}}{{{\mathit{sx}}^?}} \\ &&|&
\mathsf{local.get}~{\mathit{localidx}} \\ &&|&
\mathsf{local.set}~{\mathit{localidx}} \\ &&|&
\mathsf{local.tee}~{\mathit{localidx}} \\ &&|&
\mathsf{global.get}~{\mathit{globalidx}} \\ &&|&
\mathsf{global.set}~{\mathit{globalidx}} \\ &&|&
\mathsf{table.get}~{\mathit{tableidx}} \\ &&|&
\mathsf{table.set}~{\mathit{tableidx}} \\ &&|&
\mathsf{table.size}~{\mathit{tableidx}} \\ &&|&
\mathsf{table.grow}~{\mathit{tableidx}} \\ &&|&
\mathsf{table.fill}~{\mathit{tableidx}} \\ &&|&
\mathsf{table.copy}~{\mathit{tableidx}}~{\mathit{tableidx}} \\ &&|&
\mathsf{table.init}~{\mathit{tableidx}}~{\mathit{elemidx}} \\ &&|&
\mathsf{elem.drop}~{\mathit{elemidx}} \\ &&|&
\mathsf{memory.size}~{\mathit{memidx}} \\ &&|&
\mathsf{memory.grow}~{\mathit{memidx}} \\ &&|&
\mathsf{memory.fill}~{\mathit{memidx}} \\ &&|&
\mathsf{memory.copy}~{\mathit{memidx}}~{\mathit{memidx}} \\ &&|&
\mathsf{memory.init}~{\mathit{memidx}}~{\mathit{dataidx}} \\ &&|&
\mathsf{data.drop}~{\mathit{dataidx}} \\ &&|&
{{\mathit{numtype}}.\mathsf{load}}{{({{{\mathit{n}}}{\mathsf{\_}}}{{\mathit{sx}}})^?}}~{\mathit{memidx}}~{\mathit{memop}} \\ &&|&
{{\mathit{numtype}}.\mathsf{store}}{{{\mathit{n}}^?}}~{\mathit{memidx}}~{\mathit{memop}} \\[0.8ex]
& {\mathit{expr}} &::=& {{\mathit{instr}}^\ast} \\
\end{array}
$$


\subsection*{Typing $\boxed{{\mathit{context}} \vdash {\mathit{instr}} : {\mathit{functype}}}$}

An instruction sequence ${{\mathit{instr}}^\ast}$ is well-typed with an instruction type ${{\mathit{t}}_{{1}}^\ast} \rightarrow {{\mathit{t}}_{{2}}^\ast}$, written ${{\mathit{instr}}^\ast} : {{\mathit{t}}_{{1}}^\ast} \rightarrow {{\mathit{t}}_{{2}}^\ast}$, according to the following rules:

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
}{
{\mathit{C}} \vdash \epsilon : \epsilon \rightarrow (\epsilon)~\epsilon
}
\qquad
\frac{
(({\mathit{C}}.\mathsf{local}[{\mathit{x}}_{{1}}] = {\mathit{init}}~{\mathit{t}}))^\ast
 \qquad
{\mathit{C}'} = {\mathit{C}}[\mathsf{local}[{{\mathit{x}}_{{1}}^\ast}] = {(\mathsf{set}~{\mathit{t}})^\ast}]
 \qquad
{\mathit{C}} \vdash {\mathit{instr}}_{{1}} : {{\mathit{t}}_{{1}}^\ast} \rightarrow ({{\mathit{x}}_{{1}}^\ast})~{{\mathit{t}}_{{2}}^\ast}
 \qquad
{\mathit{C}'} \vdash {{\mathit{instr}}_{{2}}^\ast} : {{\mathit{t}}_{{2}}^\ast} \rightarrow ({{\mathit{x}}_{{2}}^\ast})~{{\mathit{t}}_{{3}}^\ast}
}{
{\mathit{C}} \vdash {\mathit{instr}}_{{1}}~{{\mathit{instr}}_{{2}}^\ast} : {{\mathit{t}}_{{1}}^\ast} \rightarrow ({{\mathit{x}}_{{1}}^\ast}~{{\mathit{x}}_{{2}}^\ast})~{{\mathit{t}}_{{3}}^\ast}
}
\\[3ex]\displaystyle
\frac{
{\mathit{C}} \vdash {{\mathit{instr}}^\ast} : {\mathit{it}}
 \qquad
{\mathit{C}} \vdash {\mathit{it}} \leq {\mathit{it}'}
}{
{\mathit{C}} \vdash {{\mathit{instr}}^\ast} : {\mathit{it}'}
}
\qquad
\frac{
{\mathit{C}} \vdash {{\mathit{instr}}^\ast} : {{\mathit{t}}_{{1}}^\ast} \rightarrow ({{\mathit{x}}^\ast})~{{\mathit{t}}_{{2}}^\ast}
}{
{\mathit{C}} \vdash {{\mathit{instr}}^\ast} : {{\mathit{t}}^\ast}~{{\mathit{t}}_{{1}}^\ast} \rightarrow ({{\mathit{x}}^\ast})~({{\mathit{t}}^\ast}~{{\mathit{t}}_{{2}}^\ast})
}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
}{
{\mathit{C}} \vdash \epsilon : \epsilon \rightarrow (\epsilon)~\epsilon
} \, {[\textsc{\scriptsize T{-}instr*{-}empty}]}
\\[3ex]\displaystyle
\frac{
{\mathit{C}} \vdash {{\mathit{instr}}^\ast} : {{\mathit{t}}_{{1}}^\ast} \rightarrow ({{\mathit{x}}^\ast})~{{\mathit{t}}_{{2}}^\ast}
}{
{\mathit{C}} \vdash {{\mathit{instr}}^\ast} : {{\mathit{t}}^\ast}~{{\mathit{t}}_{{1}}^\ast} \rightarrow ({{\mathit{x}}^\ast})~({{\mathit{t}}^\ast}~{{\mathit{t}}_{{2}}^\ast})
} \, {[\textsc{\scriptsize T{-}instr*{-}frame}]}
\\[3ex]\displaystyle
\frac{
}{
{\mathit{C}} \vdash \epsilon : \epsilon \rightarrow (\epsilon)~\epsilon
} \, {[\textsc{\scriptsize T{-}instr*{-}empty}]}
\qquad
\frac{
{\mathit{C}} \vdash {{\mathit{instr}}^\ast} : {{\mathit{t}}_{{1}}^\ast} \rightarrow ({{\mathit{x}}^\ast})~{{\mathit{t}}_{{2}}^\ast}
}{
{\mathit{C}} \vdash {{\mathit{instr}}^\ast} : {{\mathit{t}}^\ast}~{{\mathit{t}}_{{1}}^\ast} \rightarrow ({{\mathit{x}}^\ast})~({{\mathit{t}}^\ast}~{{\mathit{t}}_{{2}}^\ast})
} \, {[\textsc{\scriptsize T{-}instr*{-}frame}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
}{
{\mathit{C}} \vdash \mathsf{unreachable} : {{\mathit{t}}_{{1}}^\ast} \rightarrow {{\mathit{t}}_{{2}}^\ast}
}
\qquad
\frac{
}{
{\mathit{C}} \vdash \mathsf{nop} : \epsilon \rightarrow \epsilon
}
\qquad
\frac{
}{
{\mathit{C}} \vdash \mathsf{drop} : {\mathit{t}} \rightarrow \epsilon
}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
{\mathit{C}} \vdash {\mathit{bt}} : {{\mathit{t}}_{{1}}^\ast} \rightarrow {{\mathit{t}}_{{2}}^\ast}
 \qquad
{\mathit{C}}, \mathsf{label}~({{\mathit{t}}_{{2}}^\ast}) \vdash {{\mathit{instr}}^\ast} : {{\mathit{t}}_{{1}}^\ast} \rightarrow ({{\mathit{x}}^\ast})~{{\mathit{t}}_{{2}}^\ast}
}{
{\mathit{C}} \vdash \mathsf{block}~{\mathit{bt}}~{{\mathit{instr}}^\ast} : {{\mathit{t}}_{{1}}^\ast} \rightarrow {{\mathit{t}}_{{2}}^\ast}
} \, {[\textsc{\scriptsize T{-}block}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
{\mathit{C}} \vdash {\mathit{bt}} : {{\mathit{t}}_{{1}}^\ast} \rightarrow {{\mathit{t}}_{{2}}^\ast}
 \qquad
{\mathit{C}}, \mathsf{label}~({{\mathit{t}}_{{1}}^\ast}) \vdash {{\mathit{instr}}^\ast} : {{\mathit{t}}_{{1}}^\ast} \rightarrow ({{\mathit{x}}^\ast})~{{\mathit{t}}_{{2}}^\ast}
}{
{\mathit{C}} \vdash \mathsf{loop}~{\mathit{bt}}~{{\mathit{instr}}^\ast} : {{\mathit{t}}_{{1}}^\ast} \rightarrow {{\mathit{t}}_{{2}}^\ast}
} \, {[\textsc{\scriptsize T{-}loop}]}
\qquad
\end{array}
$$

$$
\begin{array}{@{}c@{}}\displaystyle
\frac{
{\mathit{C}} \vdash {\mathit{bt}} : {{\mathit{t}}_{{1}}^\ast} \rightarrow {{\mathit{t}}_{{2}}^\ast}
 \qquad
{\mathit{C}}, \mathsf{label}~({{\mathit{t}}_{{2}}^\ast}) \vdash {{\mathit{instr}}_{{1}}^\ast} : {{\mathit{t}}_{{1}}^\ast} \rightarrow ({{\mathit{x}}_{{1}}^\ast})~{{\mathit{t}}_{{2}}^\ast}
 \qquad
{\mathit{C}}, \mathsf{label}~({{\mathit{t}}_{{2}}^\ast}) \vdash {{\mathit{instr}}_{{2}}^\ast} : {{\mathit{t}}_{{1}}^\ast} \rightarrow ({{\mathit{x}}_{{2}}^\ast})~{{\mathit{t}}_{{2}}^\ast}
}{
{\mathit{C}} \vdash \mathsf{if}~{\mathit{bt}}~{{\mathit{instr}}_{{1}}^\ast}~\mathsf{else}~{{\mathit{instr}}_{{2}}^\ast} : {{\mathit{t}}_{{1}}^\ast}~\mathsf{i{\scriptstyle32}} \rightarrow {{\mathit{t}}_{{2}}^\ast}
} \, {[\textsc{\scriptsize T{-}if}]}
\qquad
\end{array}
$$


\subsection*{Runtime}

$$
\begin{array}{@{}lcl@{}l@{}}
{{\mathrm{default}}}_{}~\mathsf{i{\scriptstyle32}} &=& (\mathsf{i{\scriptstyle32}}.\mathsf{const}~0) &  \\[0.8ex]
{{\mathrm{default}}}_{}~\mathsf{i{\scriptstyle64}} &=& (\mathsf{i{\scriptstyle64}}.\mathsf{const}~0) &  \\[0.8ex]
{{\mathrm{default}}}_{}~\mathsf{f{\scriptstyle32}} &=& (\mathsf{f{\scriptstyle32}}.\mathsf{const}~0) &  \\[0.8ex]
{{\mathrm{default}}}_{}~\mathsf{f{\scriptstyle64}} &=& (\mathsf{f{\scriptstyle64}}.\mathsf{const}~0) &  \\[0.8ex]
{{\mathrm{default}}}_{}~\mathsf{ref}~\mathsf{null}~{\mathit{ht}} &=& (\mathsf{ref.null}~{\mathit{ht}}) &  \\[0.8ex]
{{\mathrm{default}}}_{}~\mathsf{ref}~\epsilon~{\mathit{ht}} &=& \epsilon &  \\
\end{array}
$$

$$
\begin{array}{@{}lcl@{}l@{}}
({\mathit{s}} ; {\mathit{f}}).\mathsf{module}.\mathsf{func} &=& {\mathit{f}}.\mathsf{module}.\mathsf{func} &  \\
({\mathit{s}} ; {\mathit{f}}).\mathsf{func} &=& {\mathit{s}}.\mathsf{func} &  \\[0.8ex]
{({\mathit{s}} ; {\mathit{f}}).\mathsf{func}}{[{\mathit{x}}]} &=& {\mathit{s}}.\mathsf{func}[{\mathit{f}}.\mathsf{module}.\mathsf{func}[{\mathit{x}}]] &  \\
{({\mathit{s}} ; {\mathit{f}}).\mathsf{table}}{[{\mathit{x}}]} &=& {\mathit{s}}.\mathsf{table}[{\mathit{f}}.\mathsf{module}.\mathsf{table}[{\mathit{x}}]] &  \\
\end{array}
$$

$$
\begin{array}{@{}lcl@{}l@{}}
{\mathrm{test}}_{{\mathit{sub}}_{{\mathsf{atom}}_{{22}}}}({\mathit{n}}_{{3}_{{\mathsf{atom}}_{{\mathit{y}}}}}) &=& 0 &  \\
\end{array}
$$
$$
\begin{array}{@{}lcl@{}l@{}}
{{\mathrm{curried}}}_{{\mathit{n}}_{{1}}}({\mathit{n}}_{{2}}) &=& {\mathit{n}}_{{1}} + {\mathit{n}}_{{2}} &  \\
\end{array}
$$

$$
\begin{array}{@{}l@{}rrl@{}l@{}}
& {\mathit{testfuse}} &::=& {\mathsf{ab}}_{{\mathit{nat}}}\,\,{\mathit{nat}}~{\mathit{nat}} \\ &&|&
{\mathsf{cd}}_{{\mathit{nat}}}\,{\mathit{nat}}~{\mathit{nat}} \\ &&|&
{\mathsf{ef\_}}{{\mathit{nat}}}~{\mathit{nat}}~{\mathit{nat}} \\ &&|&
{{\mathsf{gh}}_{{\mathit{nat}}}}{{\mathit{nat}}}~{\mathit{nat}} \\ &&|&
{{\mathsf{ij}}_{{\mathit{nat}}}}{{\mathit{nat}}}~{\mathit{nat}} \\ &&|&
{\mathsf{kl\_ab}}{{\mathit{nat}}}~{\mathit{nat}}~{\mathit{nat}} \\ &&|&
{\mathsf{mn\_}}{\mathsf{ab}}~{\mathit{nat}}~{\mathit{nat}}~{\mathit{nat}} \\ &&|&
{{\mathsf{op\_}}{\mathsf{ab}}}{{\mathit{nat}}}~{\mathit{nat}}~{\mathit{nat}} \\ &&|&
{{\mathsf{qr}}_{{\mathit{nat}}}}{\mathsf{ab}}~{\mathit{nat}}~{\mathit{nat}} \\
\end{array}
$$


\subsection*{Reduction $\boxed{{{{\mathit{instr}}}^\ast} \hookrightarrow {{{\mathit{instr}}}^\ast}}$}

$$
\begin{array}{@{}l@{}lcl@{}l@{}}
& {\mathit{z}} ; {{\mathit{instr}}^\ast} &\hookrightarrow& {\mathit{z}} ; {{\mathit{instr}'}^\ast} &\quad
  \mbox{if}~{{\mathit{instr}}^\ast} \hookrightarrow {{\mathit{instr}'}^\ast} \\[0.8ex]
& {\mathit{z}} ; {{\mathit{instr}}^\ast} &\hookrightarrow& {\mathit{z}} ; {{\mathit{instr}'}^\ast} &\quad
  \mbox{if}~{\mathit{z}} ; {{\mathit{instr}}^\ast} \hookrightarrow {{\mathit{instr}'}^\ast} \\
\end{array}
$$

$$
\begin{array}{@{}l@{}lcl@{}l@{}}
{[\textsc{\scriptsize E{-}block}]} \quad & {\mathit{z}} ; {{\mathit{val}}^{{\mathit{k}}}}~(\mathsf{block}~{\mathit{bt}}~{{\mathit{instr}}^\ast}) &\hookrightarrow& ({{\mathsf{label}}_{{\mathit{n}}}}{\{\epsilon\}}~{{\mathit{val}}^{{\mathit{k}}}}~{{\mathit{instr}}^\ast}) &\quad
  \mbox{if}~{{\mathrm{blocktype}}}_{{\mathit{z}}}({\mathit{bt}}) = {{\mathit{t}}_{{1}}^{{\mathit{k}}}} \rightarrow {{\mathit{t}}_{{2}}^{{\mathit{n}}}} \\
{[\textsc{\scriptsize E{-}loop}]} \quad & {\mathit{z}} ; {{\mathit{val}}^{{\mathit{k}}}}~(\mathsf{loop}~{\mathit{bt}}~{{\mathit{instr}}^\ast}) &\hookrightarrow& ({{\mathsf{label}}_{{\mathit{k}}}}{\{\mathsf{loop}~{\mathit{bt}}~{{\mathit{instr}}^\ast}\}}~{{\mathit{val}}^{{\mathit{k}}}}~{{\mathit{instr}}^\ast}) &\quad
  \mbox{if}~{{\mathrm{blocktype}}}_{{\mathit{z}}}({\mathit{bt}}) = {{\mathit{t}}_{{1}}^{{\mathit{k}}}} \rightarrow {{\mathit{t}}_{{2}}^{{\mathit{n}}}} \\[0.8ex]
{[\textsc{\scriptsize E{-}if{-}true}]} \quad & (\mathsf{i{\scriptstyle32}}.\mathsf{const}~{\mathit{c}})~(\mathsf{if}~{\mathit{bt}}~{{\mathit{instr}}_{{1}}^\ast}~\mathsf{else}~{{\mathit{instr}}_{{2}}^\ast}) &\hookrightarrow& (\mathsf{block}~{\mathit{bt}}~{{\mathit{instr}}_{{1}}^\ast}) &\quad
  \mbox{if}~{\mathit{c}} \neq 0 \\
{[\textsc{\scriptsize E{-}if{-}false}]} \quad & (\mathsf{i{\scriptstyle32}}.\mathsf{const}~{\mathit{c}})~(\mathsf{if}~{\mathit{bt}}~{{\mathit{instr}}_{{1}}^\ast}~\mathsf{else}~{{\mathit{instr}}_{{2}}^\ast}) &\hookrightarrow& (\mathsf{block}~{\mathit{bt}}~{{\mathit{instr}}_{{2}}^\ast}) &\quad
  \mbox{if}~{\mathit{c}} = 0 \\
\end{array}
$$

$$
\begin{array}{@{}l@{}lcl@{}l@{}}
{[\textsc{\scriptsize E{-}if{-}true}]} \quad & (\mathsf{i{\scriptstyle32}}.\mathsf{const}~{\mathit{c}})~(\mathsf{if}~{\mathit{bt}}~{{\mathit{instr}}_{{1}}^\ast}~\mathsf{else}~{{\mathit{instr}}_{{2}}^\ast}) &\hookrightarrow& (\mathsf{block}~{\mathit{bt}}~{{\mathit{instr}}_{{1}}^\ast}) &\quad
  \mbox{if}~{\mathit{c}} \neq 0 \\[0.8ex]
{[\textsc{\scriptsize E{-}if{-}false}]} \quad & (\mathsf{i{\scriptstyle32}}.\mathsf{const}~{\mathit{c}})~(\mathsf{if}~{\mathit{bt}}~{{\mathit{instr}}_{{1}}^\ast}~\mathsf{else}~{{\mathit{instr}}_{{2}}^\ast}) &\hookrightarrow& (\mathsf{block}~{\mathit{bt}}~{{\mathit{instr}}_{{2}}^\ast}) &\quad
  \mbox{if}~{\mathit{c}} = 0 \\
\end{array}
$$

\end{document}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

warning: syntax `E` was never spliced
warning: syntax `absheaptype` was never spliced
warning: syntax `absheaptype` was never spliced
warning: syntax `addr` was never spliced
warning: syntax `addrref` was never spliced
warning: syntax `admininstr` was never spliced
warning: syntax `arrayaddr` was never spliced
warning: syntax `arrayinst` was never spliced
warning: syntax `binopFXX` was never spliced
warning: syntax `binopIXX` was never spliced
warning: syntax `binop_numtype` was never spliced
warning: syntax `blocktype` was never spliced
warning: syntax `byte` was never spliced
warning: syntax `c_numtype` was never spliced
warning: syntax `c_packedtype` was never spliced
warning: syntax `c_vectype` was never spliced
warning: syntax `castop` was never spliced
warning: syntax `char` was never spliced
warning: syntax `code` was never spliced
warning: syntax `comptype` was never spliced
warning: syntax `config` was never spliced
warning: syntax `context` was never spliced
warning: syntax `cvtop` was never spliced
warning: syntax `data` was never spliced
warning: syntax `dataaddr` was never spliced
warning: syntax `dataidx` was never spliced
warning: syntax `datainst` was never spliced
warning: syntax `datamode` was never spliced
warning: syntax `datatype` was never spliced
warning: syntax `deftype` was never spliced
warning: syntax `elem` was never spliced
warning: syntax `elemaddr` was never spliced
warning: syntax `elemidx` was never spliced
warning: syntax `eleminst` was never spliced
warning: syntax `elemmode` was never spliced
warning: syntax `elemtype` was never spliced
warning: syntax `export` was never spliced
warning: syntax `exportinst` was never spliced
warning: syntax `externidx` was never spliced
warning: syntax `externval` was never spliced
warning: syntax `f32` was never spliced
warning: syntax `f64` was never spliced
warning: syntax `fN` was never spliced
warning: syntax `fNmag` was never spliced
warning: syntax `fieldtype` was never spliced
warning: syntax `fieldval` was never spliced
warning: syntax `fin` was never spliced
warning: syntax `fnn` was never spliced
warning: syntax `frame` was never spliced
warning: syntax `func` was never spliced
warning: syntax `funcaddr` was never spliced
warning: syntax `funcidx` was never spliced
warning: syntax `funcinst` was never spliced
warning: syntax `global` was never spliced
warning: syntax `globaladdr` was never spliced
warning: syntax `globalidx` was never spliced
warning: syntax `globalinst` was never spliced
warning: syntax `heaptype` was never spliced
warning: syntax `heaptype` was never spliced
warning: syntax `hostaddr` was never spliced
warning: syntax `iN` was never spliced
warning: syntax `idx` was never spliced
warning: syntax `import` was never spliced
warning: syntax `init` was never spliced
warning: syntax `inn` was never spliced
warning: syntax `instr/heap` was never spliced
warning: syntax `instrtype` was never spliced
warning: syntax `labeladdr` was never spliced
warning: syntax `labelidx` was never spliced
warning: syntax `local` was never spliced
warning: syntax `localidx` was never spliced
warning: syntax `localtype` was never spliced
warning: syntax `m` was never spliced
warning: syntax `mem` was never spliced
warning: syntax `memaddr` was never spliced
warning: syntax `memidx` was never spliced
warning: syntax `memidxop` was never spliced
warning: syntax `meminst` was never spliced
warning: syntax `memop` was never spliced
warning: syntax `module` was never spliced
warning: syntax `moduleinst` was never spliced
warning: syntax `mut` was never spliced
warning: syntax `n` was never spliced
warning: syntax `name` was never spliced
warning: syntax `nul` was never spliced
warning: syntax `num` was never spliced
warning: syntax `oktypeidx` was never spliced
warning: syntax `oktypeidxnat` was never spliced
warning: syntax `packedtype` was never spliced
warning: syntax `packedval` was never spliced
warning: syntax `rectype` was never spliced
warning: syntax `ref` was never spliced
warning: syntax `relopFXX` was never spliced
warning: syntax `relopIXX` was never spliced
warning: syntax `relop_numtype` was never spliced
warning: syntax `result` was never spliced
warning: syntax `s33` was never spliced
warning: syntax `sN` was never spliced
warning: syntax `start` was never spliced
warning: syntax `state` was never spliced
warning: syntax `storagetype` was never spliced
warning: syntax `store` was never spliced
warning: syntax `structaddr` was never spliced
warning: syntax `structinst` was never spliced
warning: syntax `subtype` was never spliced
warning: syntax `subtype` was never spliced
warning: syntax `sx` was never spliced
warning: syntax `table` was never spliced
warning: syntax `tableaddr` was never spliced
warning: syntax `tableidx` was never spliced
warning: syntax `tableinst` was never spliced
warning: syntax `testopFXX` was never spliced
warning: syntax `testopIXX` was never spliced
warning: syntax `testop_numtype` was never spliced
warning: syntax `type` was never spliced
warning: syntax `typeidx` was never spliced
warning: syntax `typevar` was never spliced
warning: syntax `u128` was never spliced
warning: syntax `u31` was never spliced
warning: syntax `u32` was never spliced
warning: syntax `u64` was never spliced
warning: syntax `uN` was never spliced
warning: syntax `unopFXX` was never spliced
warning: syntax `unopIXX` was never spliced
warning: syntax `unop_numtype` was never spliced
warning: syntax `val` was never spliced
warning: grammar `Babsheaptype` was never spliced
warning: grammar `Bblocktype` was never spliced
warning: grammar `Bbyte` was never spliced
warning: grammar `Bcastop` was never spliced
warning: grammar `Bcode` was never spliced
warning: grammar `Bcodesec` was never spliced
warning: grammar `Bcomptype` was never spliced
warning: grammar `Bcustom` was never spliced
warning: grammar `Bcustomsec` was never spliced
warning: grammar `Bdata` was never spliced
warning: grammar `Bdatacnt` was never spliced
warning: grammar `Bdatacntsec` was never spliced
warning: grammar `Bdataidx` was never spliced
warning: grammar `Bdatasec` was never spliced
warning: grammar `Belem` was never spliced
warning: grammar `Belemidx` was never spliced
warning: grammar `Belemkind` was never spliced
warning: grammar `Belemsec` was never spliced
warning: grammar `Bexport` was never spliced
warning: grammar `Bexportsec` was never spliced
warning: grammar `Bexpr` was never spliced
warning: grammar `Bexternidx` was never spliced
warning: grammar `Bexterntype` was never spliced
warning: grammar `Bf` was never spliced
warning: grammar `Bf32` was never spliced
warning: grammar `Bf64` was never spliced
warning: grammar `Bfieldtype` was never spliced
warning: grammar `Bfunc` was never spliced
warning: grammar `Bfuncidx` was never spliced
warning: grammar `Bfuncsec` was never spliced
warning: grammar `Bglobal` was never spliced
warning: grammar `Bglobalidx` was never spliced
warning: grammar `Bglobalsec` was never spliced
warning: grammar `Bglobaltype` was never spliced
warning: grammar `Bheaptype` was never spliced
warning: grammar `Bi` was never spliced
warning: grammar `Bimport` was never spliced
warning: grammar `Bimportsec` was never spliced
warning: grammar `Binstr/control` was never spliced
warning: grammar `Binstr/reference` was never spliced
warning: grammar `Binstr/struct` was never spliced
warning: grammar `Binstr/parametric` was never spliced
warning: grammar `Binstr/variable` was never spliced
warning: grammar `Binstr/table` was never spliced
warning: grammar `Binstr/memory` was never spliced
warning: grammar `Binstr/numeric-const` was never spliced
warning: grammar `Binstr/numeric-test-i32` was never spliced
warning: grammar `Binstr/numeric-rel-i32` was never spliced
warning: grammar `Binstr/numeric-test-i64` was never spliced
warning: grammar `Binstr/numeric-rel-i64` was never spliced
warning: grammar `Binstr/numeric-rel-f32` was never spliced
warning: grammar `Binstr/numeric-rel-f64` was never spliced
warning: grammar `Binstr/numeric-un-i32` was never spliced
warning: grammar `Binstr/numeric-bin-i32` was never spliced
warning: grammar `Binstr/numeric-un-i64` was never spliced
warning: grammar `Binstr/numeric-bin-i64` was never spliced
warning: grammar `Binstr/numeric-un-f32` was never spliced
warning: grammar `Binstr/numeric-bin-f32` was never spliced
warning: grammar `Binstr/numeric-un-f64` was never spliced
warning: grammar `Binstr/numeric-bin-f64` was never spliced
warning: grammar `Binstr/numeric-cvt` was never spliced
warning: grammar `Binstr/numeric-extend` was never spliced
warning: grammar `Blabelidx` was never spliced
warning: grammar `Blimits` was never spliced
warning: grammar `Blocalidx` was never spliced
warning: grammar `Blocals` was never spliced
warning: grammar `Bmem` was never spliced
warning: grammar `Bmemidx` was never spliced
warning: grammar `Bmemop` was never spliced
warning: grammar `Bmemsec` was never spliced
warning: grammar `Bmemtype` was never spliced
warning: grammar `Bmodule` was never spliced
warning: grammar `Bmut` was never spliced
warning: grammar `Bname` was never spliced
warning: grammar `Bnumtype` was never spliced
warning: grammar `Bpackedtype` was never spliced
warning: grammar `Brectype` was never spliced
warning: grammar `Breftype` was never spliced
warning: grammar `Bresulttype` was never spliced
warning: grammar `Bs` was never spliced
warning: grammar `Bs33` was never spliced
warning: grammar `Bsection_` was never spliced
warning: grammar `Bstart` was never spliced
warning: grammar `Bstartsec` was never spliced
warning: grammar `Bstoragetype` was never spliced
warning: grammar `Bsubtype` was never spliced
warning: grammar `Btable` was never spliced
warning: grammar `Btableidx` was never spliced
warning: grammar `Btablesec` was never spliced
warning: grammar `Btabletype` was never spliced
warning: grammar `Btype` was never spliced
warning: grammar `Btypeidx` was never spliced
warning: grammar `Btypesec` was never spliced
warning: grammar `Bu` was never spliced
warning: grammar `Bu32` was never spliced
warning: grammar `Bu64` was never spliced
warning: grammar `Bvaltype` was never spliced
warning: grammar `Bvec` was never spliced
warning: grammar `Bvectype` was never spliced
warning: rule `Blocktype_ok/void` was never spliced
warning: rule `Blocktype_ok/result` was never spliced
warning: rule `Blocktype_ok/typeidx` was never spliced
warning: rule `Comptype_ok/struct` was never spliced
warning: rule `Comptype_ok/array` was never spliced
warning: rule `Comptype_ok/func` was never spliced
warning: rule `Comptype_sub/struct` was never spliced
warning: rule `Comptype_sub/array` was never spliced
warning: rule `Comptype_sub/func` was never spliced
warning: rule `Data_ok` was never spliced
warning: rule `Datamode_ok/active` was never spliced
warning: rule `Datamode_ok/passive` was never spliced
warning: rule `Deftype_ok` was never spliced
warning: rule `Deftype_sub/refl` was never spliced
warning: rule `Deftype_sub/super` was never spliced
warning: rule `Elem_ok` was never spliced
warning: rule `Elemmode_ok/active` was never spliced
warning: rule `Elemmode_ok/passive` was never spliced
warning: rule `Elemmode_ok/declare` was never spliced
warning: rule `Eval/done` was never spliced
warning: rule `Eval/step` was never spliced
warning: rule `Eval_expr` was never spliced
warning: rule `Expand` was never spliced
warning: rule `Export_ok` was never spliced
warning: rule `Expr_const` was never spliced
warning: rule `Expr_ok` was never spliced
warning: rule `Expr_ok_const` was never spliced
warning: rule `Externidx_ok/func` was never spliced
warning: rule `Externidx_ok/global` was never spliced
warning: rule `Externidx_ok/table` was never spliced
warning: rule `Externidx_ok/mem` was never spliced
warning: rule `Externtype_ok/func` was never spliced
warning: rule `Externtype_ok/global` was never spliced
warning: rule `Externtype_ok/table` was never spliced
warning: rule `Externtype_ok/mem` was never spliced
warning: rule `Externtype_sub/func` was never spliced
warning: rule `Externtype_sub/global` was never spliced
warning: rule `Externtype_sub/table` was never spliced
warning: rule `Externtype_sub/mem` was never spliced
warning: rule `Fieldtype_ok` was never spliced
warning: rule `Fieldtype_sub/const` was never spliced
warning: rule `Fieldtype_sub/var` was never spliced
warning: rule `Func_ok` was never spliced
warning: rule `Functype_ok` was never spliced
warning: rule `Functype_sub` was never spliced
warning: rule `Global_ok` was never spliced
warning: rule `Globals_ok/empty` was never spliced
warning: rule `Globals_ok/cons` was never spliced
warning: rule `Globaltype_ok` was never spliced
warning: rule `Globaltype_sub/const` was never spliced
warning: rule `Globaltype_sub/var` was never spliced
warning: rule `Heaptype_ok/abs` was never spliced
warning: rule `Heaptype_ok/typeidx` was never spliced
warning: rule `Heaptype_ok/rec` was never spliced
warning: rule `Heaptype_sub/refl` was never spliced
warning: rule `Heaptype_sub/trans` was never spliced
warning: rule `Heaptype_sub/eq-any` was never spliced
warning: rule `Heaptype_sub/i31-eq` was never spliced
warning: rule `Heaptype_sub/struct-eq` was never spliced
warning: rule `Heaptype_sub/array-eq` was never spliced
warning: rule `Heaptype_sub/struct` was never spliced
warning: rule `Heaptype_sub/array` was never spliced
warning: rule `Heaptype_sub/func` was never spliced
warning: rule `Heaptype_sub/def` was never spliced
warning: rule `Heaptype_sub/typeidx-l` was never spliced
warning: rule `Heaptype_sub/typeidx-r` was never spliced
warning: rule `Heaptype_sub/rec` was never spliced
warning: rule `Heaptype_sub/none` was never spliced
warning: rule `Heaptype_sub/nofunc` was never spliced
warning: rule `Heaptype_sub/noextern` was never spliced
warning: rule `Heaptype_sub/bot` was never spliced
warning: rule `Import_ok` was never spliced
warning: rule `Instr_const/const` was never spliced
warning: rule `Instr_const/ref.null` was never spliced
warning: rule `Instr_const/ref.func` was never spliced
warning: rule `Instr_const/global.get` was never spliced
warning: rule `Instr_const/binop` was never spliced
warning: rule `Instr_ok/select-expl` was never spliced
warning: rule `Instr_ok/select-impl` was never spliced
warning: rule `Instr_ok/br` was never spliced
warning: rule `Instr_ok/br_if` was never spliced
warning: rule `Instr_ok/br_table` was never spliced
warning: rule `Instr_ok/br_on_null` was never spliced
warning: rule `Instr_ok/br_on_non_null` was never spliced
warning: rule `Instr_ok/br_on_cast` was never spliced
warning: rule `Instr_ok/br_on_cast_fail` was never spliced
warning: rule `Instr_ok/return` was never spliced
warning: rule `Instr_ok/call` was never spliced
warning: rule `Instr_ok/call_ref` was never spliced
warning: rule `Instr_ok/call_indirect` was never spliced
warning: rule `Instr_ok/return_call` was never spliced
warning: rule `Instr_ok/return_call_ref` was never spliced
warning: rule `Instr_ok/return_call_indirect` was never spliced
warning: rule `Instr_ok/const` was never spliced
warning: rule `Instr_ok/unop` was never spliced
warning: rule `Instr_ok/binop` was never spliced
warning: rule `Instr_ok/testop` was never spliced
warning: rule `Instr_ok/relop` was never spliced
warning: rule `Instr_ok/extend` was never spliced
warning: rule `Instr_ok/reinterpret` was never spliced
warning: rule `Instr_ok/convert-i` was never spliced
warning: rule `Instr_ok/convert-f` was never spliced
warning: rule `Instr_ok/ref.null` was never spliced
warning: rule `Instr_ok/ref.func` was never spliced
warning: rule `Instr_ok/ref.i31` was never spliced
warning: rule `Instr_ok/ref.is_null` was never spliced
warning: rule `Instr_ok/ref.as_non_null` was never spliced
warning: rule `Instr_ok/ref.eq` was never spliced
warning: rule `Instr_ok/ref.test` was never spliced
warning: rule `Instr_ok/ref.cast` was never spliced
warning: rule `Instr_ok/i31.get` was never spliced
warning: rule `Instr_ok/struct.new` was never spliced
warning: rule `Instr_ok/struct.new_default` was never spliced
warning: rule `Instr_ok/struct.get` was never spliced
warning: rule `Instr_ok/struct.set` was never spliced
warning: rule `Instr_ok/array.new` was never spliced
warning: rule `Instr_ok/array.new_default` was never spliced
warning: rule `Instr_ok/array.new_fixed` was never spliced
warning: rule `Instr_ok/array.new_elem` was never spliced
warning: rule `Instr_ok/array.new_data` was never spliced
warning: rule `Instr_ok/array.get` was never spliced
warning: rule `Instr_ok/array.set` was never spliced
warning: rule `Instr_ok/array.len` was never spliced
warning: rule `Instr_ok/array.fill` was never spliced
warning: rule `Instr_ok/array.copy` was never spliced
warning: rule `Instr_ok/array.init_elem` was never spliced
warning: rule `Instr_ok/array.init_data` was never spliced
warning: rule `Instr_ok/extern.convert_any` was never spliced
warning: rule `Instr_ok/any.convert_extern` was never spliced
warning: rule `Instr_ok/local.get` was never spliced
warning: rule `Instr_ok/global.get` was never spliced
warning: rule `Instr_ok/global.set` was never spliced
warning: rule `Instr_ok/table.get` was never spliced
warning: rule `Instr_ok/table.set` was never spliced
warning: rule `Instr_ok/table.size` was never spliced
warning: rule `Instr_ok/table.grow` was never spliced
warning: rule `Instr_ok/table.fill` was never spliced
warning: rule `Instr_ok/table.copy` was never spliced
warning: rule `Instr_ok/table.init` was never spliced
warning: rule `Instr_ok/elem.drop` was never spliced
warning: rule `Instr_ok/memory.size` was never spliced
warning: rule `Instr_ok/memory.grow` was never spliced
warning: rule `Instr_ok/memory.fill` was never spliced
warning: rule `Instr_ok/memory.copy` was never spliced
warning: rule `Instr_ok/memory.init` was never spliced
warning: rule `Instr_ok/data.drop` was never spliced
warning: rule `Instr_ok/load` was never spliced
warning: rule `Instr_ok/store` was never spliced
warning: rule `Instrf_ok/instr` was never spliced
warning: rule `Instrf_ok/local.set` was never spliced
warning: rule `Instrf_ok/local.tee` was never spliced
warning: rule `Instrs_ok/empty` was spliced more than once
warning: rule `Instrs_ok/frame` was spliced more than once
warning: rule `Instrtype_ok` was never spliced
warning: rule `Instrtype_sub` was never spliced
warning: rule `Limits_ok` was never spliced
warning: rule `Limits_sub` was never spliced
warning: rule `Local_ok/set` was never spliced
warning: rule `Local_ok/unset` was never spliced
warning: rule `Mem_ok` was never spliced
warning: rule `Memtype_ok` was never spliced
warning: rule `Memtype_sub` was never spliced
warning: rule `Module_ok` was never spliced
warning: rule `Numtype_ok` was never spliced
warning: rule `Numtype_sub` was never spliced
warning: rule `Packedtype_ok` was never spliced
warning: rule `Packedtype_sub` was never spliced
warning: rule `Rectype_ok/empty` was never spliced
warning: rule `Rectype_ok/cons` was never spliced
warning: rule `Rectype_ok/rec2` was never spliced
warning: rule `Rectype_ok2/empty` was never spliced
warning: rule `Rectype_ok2/cons` was never spliced
warning: rule `Ref_ok/null` was never spliced
warning: rule `Ref_ok/i31` was never spliced
warning: rule `Ref_ok/struct` was never spliced
warning: rule `Ref_ok/array` was never spliced
warning: rule `Ref_ok/func` was never spliced
warning: rule `Ref_ok/host` was never spliced
warning: rule `Ref_ok/extern` was never spliced
warning: rule `Reftype_ok` was never spliced
warning: rule `Reftype_sub/nonnull` was never spliced
warning: rule `Reftype_sub/null` was never spliced
warning: rule `Resulttype_ok` was never spliced
warning: rule `Resulttype_sub` was never spliced
warning: rule `Start_ok` was never spliced
warning: rule `Step/struct.new` was never spliced
warning: rule `Step/struct.set-null` was never spliced
warning: rule `Step/struct.set-struct` was never spliced
warning: rule `Step/array.new_fixed` was never spliced
warning: rule `Step/array.set-null` was never spliced
warning: rule `Step/array.set-oob` was never spliced
warning: rule `Step/array.set-array` was never spliced
warning: rule `Step/local.set` was never spliced
warning: rule `Step/global.set` was never spliced
warning: rule `Step/table.set-oob` was never spliced
warning: rule `Step/table.set-val` was never spliced
warning: rule `Step/table.grow-succeed` was never spliced
warning: rule `Step/table.grow-fail` was never spliced
warning: rule `Step/elem.drop` was never spliced
warning: rule `Step/store-num-oob` was never spliced
warning: rule `Step/store-num-val` was never spliced
warning: rule `Step/store-pack-oob` was never spliced
warning: rule `Step/store-pack-val` was never spliced
warning: rule `Step/memory.grow-succeed` was never spliced
warning: rule `Step/memory.grow-fail` was never spliced
warning: rule `Step/data.drop` was never spliced
warning: rule `Step_pure/unreachable` was never spliced
warning: rule `Step_pure/nop` was never spliced
warning: rule `Step_pure/drop` was never spliced
warning: rule `Step_pure/select-true` was never spliced
warning: rule `Step_pure/select-false` was never spliced
warning: rule `Step_pure/if-true` was spliced more than once
warning: rule `Step_pure/if-false` was spliced more than once
warning: rule `Step_pure/label-vals` was never spliced
warning: rule `Step_pure/br-zero` was never spliced
warning: rule `Step_pure/br-succ` was never spliced
warning: rule `Step_pure/br_if-true` was never spliced
warning: rule `Step_pure/br_if-false` was never spliced
warning: rule `Step_pure/br_table-lt` was never spliced
warning: rule `Step_pure/br_table-ge` was never spliced
warning: rule `Step_pure/br_on_null-null` was never spliced
warning: rule `Step_pure/br_on_null-addr` was never spliced
warning: rule `Step_pure/br_on_non_null-null` was never spliced
warning: rule `Step_pure/br_on_non_null-addr` was never spliced
warning: rule `Step_pure/call_indirect-call` was never spliced
warning: rule `Step_pure/return_call_indirect` was never spliced
warning: rule `Step_pure/frame-vals` was never spliced
warning: rule `Step_pure/return-frame` was never spliced
warning: rule `Step_pure/return-label` was never spliced
warning: rule `Step_pure/unop-val` was never spliced
warning: rule `Step_pure/unop-trap` was never spliced
warning: rule `Step_pure/binop-val` was never spliced
warning: rule `Step_pure/binop-trap` was never spliced
warning: rule `Step_pure/testop` was never spliced
warning: rule `Step_pure/relop` was never spliced
warning: rule `Step_pure/extend` was never spliced
warning: rule `Step_pure/cvtop-val` was never spliced
warning: rule `Step_pure/cvtop-trap` was never spliced
warning: rule `Step_pure/ref.i31` was never spliced
warning: rule `Step_pure/ref.is_null-true` was never spliced
warning: rule `Step_pure/ref.is_null-false` was never spliced
warning: rule `Step_pure/ref.as_non_null-null` was never spliced
warning: rule `Step_pure/ref.as_non_null-addr` was never spliced
warning: rule `Step_pure/ref.eq-null` was never spliced
warning: rule `Step_pure/ref.eq-true` was never spliced
warning: rule `Step_pure/ref.eq-false` was never spliced
warning: rule `Step_pure/i31.get-null` was never spliced
warning: rule `Step_pure/i31.get-num` was never spliced
warning: rule `Step_pure/extern.convert_any-null` was never spliced
warning: rule `Step_pure/extern.convert_any-addr` was never spliced
warning: rule `Step_pure/any.convert_extern-null` was never spliced
warning: rule `Step_pure/any.convert_extern-addr` was never spliced
warning: rule `Step_pure/local.tee` was never spliced
warning: rule `Step_read/br_on_cast-succeed` was never spliced
warning: rule `Step_read/br_on_cast-fail` was never spliced
warning: rule `Step_read/br_on_cast_fail-succeed` was never spliced
warning: rule `Step_read/br_on_cast_fail-fail` was never spliced
warning: rule `Step_read/call` was never spliced
warning: rule `Step_read/call_ref-null` was never spliced
warning: rule `Step_read/call_ref-func` was never spliced
warning: rule `Step_read/return_call` was never spliced
warning: rule `Step_read/return_call_ref-frame-null` was never spliced
warning: rule `Step_read/return_call_ref-frame-addr` was never spliced
warning: rule `Step_read/return_call_ref-label` was never spliced
warning: rule `Step_read/ref.func` was never spliced
warning: rule `Step_read/ref.test-true` was never spliced
warning: rule `Step_read/ref.test-false` was never spliced
warning: rule `Step_read/ref.cast-succeed` was never spliced
warning: rule `Step_read/ref.cast-fail` was never spliced
warning: rule `Step_read/struct.new_default` was never spliced
warning: rule `Step_read/struct.get-null` was never spliced
warning: rule `Step_read/struct.get-struct` was never spliced
warning: rule `Step_read/array.new` was never spliced
warning: rule `Step_read/array.new_default` was never spliced
warning: rule `Step_read/array.new_elem-oob` was never spliced
warning: rule `Step_read/array.new_elem-alloc` was never spliced
warning: rule `Step_read/array.new_data-oob` was never spliced
warning: rule `Step_read/array.new_data-alloc` was never spliced
warning: rule `Step_read/array.get-null` was never spliced
warning: rule `Step_read/array.get-oob` was never spliced
warning: rule `Step_read/array.get-array` was never spliced
warning: rule `Step_read/array.len-null` was never spliced
warning: rule `Step_read/array.len-array` was never spliced
warning: rule `Step_read/array.fill-null` was never spliced
warning: rule `Step_read/array.fill-oob` was never spliced
warning: rule `Step_read/array.fill-zero` was never spliced
warning: rule `Step_read/array.fill-succ` was never spliced
warning: rule `Step_read/array.copy-null1` was never spliced
warning: rule `Step_read/array.copy-null2` was never spliced
warning: rule `Step_read/array.copy-oob1` was never spliced
warning: rule `Step_read/array.copy-oob2` was never spliced
warning: rule `Step_read/array.copy-zero` was never spliced
warning: rule `Step_read/array.copy-le` was never spliced
warning: rule `Step_read/array.copy-gt` was never spliced
warning: rule `Step_read/array.init_elem-null` was never spliced
warning: rule `Step_read/array.init_elem-oob1` was never spliced
warning: rule `Step_read/array.init_elem-oob2` was never spliced
warning: rule `Step_read/array.init_elem-zero` was never spliced
warning: rule `Step_read/array.init_elem-succ` was never spliced
warning: rule `Step_read/array.init_data-null` was never spliced
warning: rule `Step_read/array.init_data-oob1` was never spliced
warning: rule `Step_read/array.init_data-oob2` was never spliced
warning: rule `Step_read/array.init_data-zero` was never spliced
warning: rule `Step_read/array.init_data-succ` was never spliced
warning: rule `Step_read/local.get` was never spliced
warning: rule `Step_read/global.get` was never spliced
warning: rule `Step_read/table.get-oob` was never spliced
warning: rule `Step_read/table.get-val` was never spliced
warning: rule `Step_read/table.size` was never spliced
warning: rule `Step_read/table.fill-oob` was never spliced
warning: rule `Step_read/table.fill-zero` was never spliced
warning: rule `Step_read/table.fill-succ` was never spliced
warning: rule `Step_read/table.copy-oob` was never spliced
warning: rule `Step_read/table.copy-zero` was never spliced
warning: rule `Step_read/table.copy-le` was never spliced
warning: rule `Step_read/table.copy-gt` was never spliced
warning: rule `Step_read/table.init-oob` was never spliced
warning: rule `Step_read/table.init-zero` was never spliced
warning: rule `Step_read/table.init-succ` was never spliced
warning: rule `Step_read/load-num-oob` was never spliced
warning: rule `Step_read/load-num-val` was never spliced
warning: rule `Step_read/load-pack-oob` was never spliced
warning: rule `Step_read/load-pack-val` was never spliced
warning: rule `Step_read/memory.size` was never spliced
warning: rule `Step_read/memory.fill-oob` was never spliced
warning: rule `Step_read/memory.fill-zero` was never spliced
warning: rule `Step_read/memory.fill-succ` was never spliced
warning: rule `Step_read/memory.copy-oob` was never spliced
warning: rule `Step_read/memory.copy-zero` was never spliced
warning: rule `Step_read/memory.copy-le` was never spliced
warning: rule `Step_read/memory.copy-gt` was never spliced
warning: rule `Step_read/memory.init-oob` was never spliced
warning: rule `Step_read/memory.init-zero` was never spliced
warning: rule `Step_read/memory.init-succ` was never spliced
warning: rule `Storagetype_ok/val` was never spliced
warning: rule `Storagetype_ok/packed` was never spliced
warning: rule `Storagetype_sub/val` was never spliced
warning: rule `Storagetype_sub/packed` was never spliced
warning: rule `Subtype_ok` was never spliced
warning: rule `Subtype_ok2` was never spliced
warning: rule `Table_ok` was never spliced
warning: rule `Tabletype_ok` was never spliced
warning: rule `Tabletype_sub` was never spliced
warning: rule `Type_ok` was never spliced
warning: rule `Types_ok/empty` was never spliced
warning: rule `Types_ok/cons` was never spliced
warning: rule `Valtype_ok/num` was never spliced
warning: rule `Valtype_ok/vec` was never spliced
warning: rule `Valtype_ok/ref` was never spliced
warning: rule `Valtype_ok/bot` was never spliced
warning: rule `Valtype_sub/num` was never spliced
warning: rule `Valtype_sub/vec` was never spliced
warning: rule `Valtype_sub/ref` was never spliced
warning: rule `Valtype_sub/bot` was never spliced
warning: rule `Vectype_ok` was never spliced
warning: rule `Vectype_sub` was never spliced
warning: definition `E` was never spliced
warning: definition `Ki` was never spliced
warning: definition `M` was never spliced
warning: definition `allocdata` was never spliced
warning: definition `allocdatas` was never spliced
warning: definition `allocelem` was never spliced
warning: definition `allocelems` was never spliced
warning: definition `allocfunc` was never spliced
warning: definition `allocfuncs` was never spliced
warning: definition `allocglobal` was never spliced
warning: definition `allocglobals` was never spliced
warning: definition `allocmem` was never spliced
warning: definition `allocmems` was never spliced
warning: definition `allocmodule` was never spliced
warning: definition `alloctable` was never spliced
warning: definition `alloctables` was never spliced
warning: definition `alloctypes` was never spliced
warning: definition `arrayinst` was never spliced
warning: definition `before` was never spliced
warning: definition `binop` was never spliced
warning: definition `blocktype` was never spliced
warning: definition `clostype` was never spliced
warning: definition `clostypes` was never spliced
warning: definition `concat_bytes` was never spliced
warning: definition `concat_instr` was never spliced
warning: definition `concat_locals` was never spliced
warning: definition `cvtop` was never spliced
warning: definition `data` was never spliced
warning: definition `datainst` was never spliced
warning: definition `diffrt` was never spliced
warning: definition `elem` was never spliced
warning: definition `eleminst` was never spliced
warning: definition `expanddt` was never spliced
warning: definition `expon` was never spliced
warning: definition `ext` was never spliced
warning: definition `ext_arrayinst` was never spliced
warning: definition `ext_structinst` was never spliced
warning: definition `fNzero` was never spliced
warning: definition `fbytes` was never spliced
warning: definition `frame` was never spliced
warning: definition `free_dataidx_expr` was never spliced
warning: definition `free_dataidx_func` was never spliced
warning: definition `free_dataidx_funcs` was never spliced
warning: definition `free_dataidx_instr` was never spliced
warning: definition `free_dataidx_instrs` was never spliced
warning: definition `funcsxt` was never spliced
warning: definition `funcsxv` was never spliced
warning: definition `global` was never spliced
warning: definition `globalinst` was never spliced
warning: definition `globalsxt` was never spliced
warning: definition `globalsxv` was never spliced
warning: definition `growmemory` was never spliced
warning: definition `growtable` was never spliced
warning: definition `ibytes` was never spliced
warning: definition `idx` was never spliced
warning: definition `in_binop` was never spliced
warning: definition `in_numtype` was never spliced
warning: definition `inst_reftype` was never spliced
warning: definition `instantiate` was never spliced
warning: definition `instexport` was never spliced
warning: definition `invfbytes` was never spliced
warning: definition `invibytes` was never spliced
warning: definition `invoke` was never spliced
warning: definition `invsigned` was never spliced
warning: definition `local` was never spliced
warning: definition `mem` was never spliced
warning: definition `meminst` was never spliced
warning: definition `memop0` was never spliced
warning: definition `memsxt` was never spliced
warning: definition `memsxv` was never spliced
warning: definition `min` was never spliced
warning: definition `moduleinst` was never spliced
warning: definition `ntbytes` was never spliced
warning: definition `packedsize` was never spliced
warning: definition `packval` was never spliced
warning: definition `relop` was never spliced
warning: definition `rolldt` was never spliced
warning: definition `rollrt` was never spliced
warning: definition `rundata` was never spliced
warning: definition `runelem` was never spliced
warning: definition `s33_to_u32` was never spliced
warning: definition `setminus` was never spliced
warning: definition `setminus1` was never spliced
warning: definition `signed` was never spliced
warning: definition `signif` was never spliced
warning: definition `storagesize` was never spliced
warning: definition `store` was never spliced
warning: definition `structinst` was never spliced
warning: definition `subst_all_deftype` was never spliced
warning: definition `subst_all_deftypes` was never spliced
warning: definition `subst_all_reftype` was never spliced
warning: definition `subst_comptype` was never spliced
warning: definition `subst_deftype` was never spliced
warning: definition `subst_externtype` was never spliced
warning: definition `subst_fieldtype` was never spliced
warning: definition `subst_functype` was never spliced
warning: definition `subst_globaltype` was never spliced
warning: definition `subst_heaptype` was never spliced
warning: definition `subst_memtype` was never spliced
warning: definition `subst_numtype` was never spliced
warning: definition `subst_packedtype` was never spliced
warning: definition `subst_rectype` was never spliced
warning: definition `subst_reftype` was never spliced
warning: definition `subst_storagetype` was never spliced
warning: definition `subst_subtype` was never spliced
warning: definition `subst_tabletype` was never spliced
warning: definition `subst_typevar` was never spliced
warning: definition `subst_valtype` was never spliced
warning: definition `subst_vectype` was never spliced
warning: definition `sum` was never spliced
warning: definition `sxfield` was never spliced
warning: definition `tableinst` was never spliced
warning: definition `tablesxt` was never spliced
warning: definition `tablesxv` was never spliced
warning: definition `testop` was never spliced
warning: definition `type` was never spliced
warning: definition `unop` was never spliced
warning: definition `unpacknumtype` was never spliced
warning: definition `unpacktype` was never spliced
warning: definition `unpackval` was never spliced
warning: definition `unrolldt` was never spliced
warning: definition `unrollht` was never spliced
warning: definition `unrollrt` was never spliced
warning: definition `utf8` was never spliced
warning: definition `with_array` was never spliced
warning: definition `with_data` was never spliced
warning: definition `with_elem` was never spliced
warning: definition `with_global` was never spliced
warning: definition `with_local` was never spliced
warning: definition `with_locals` was never spliced
warning: definition `with_mem` was never spliced
warning: definition `with_meminst` was never spliced
warning: definition `with_struct` was never spliced
warning: definition `with_table` was never spliced
warning: definition `with_tableinst` was never spliced
warning: definition `wrap` was never spliced
warning: definition `ztbytes` was never spliced
warning: validation prose `ANY.CONVERT_EXTERN` was never spliced
warning: validation prose `ARRAY.COPY` was never spliced
warning: validation prose `ARRAY.FILL` was never spliced
warning: validation prose `ARRAY.GET` was never spliced
warning: validation prose `ARRAY.INIT_DATA` was never spliced
warning: validation prose `ARRAY.INIT_ELEM` was never spliced
warning: validation prose `ARRAY.LEN` was never spliced
warning: validation prose `ARRAY.NEW` was never spliced
warning: validation prose `ARRAY.NEW_DATA` was never spliced
warning: validation prose `ARRAY.NEW_DEFAULT` was never spliced
warning: validation prose `ARRAY.NEW_ELEM` was never spliced
warning: validation prose `ARRAY.NEW_FIXED` was never spliced
warning: validation prose `ARRAY.SET` was never spliced
warning: validation prose `BINOP` was never spliced
warning: validation prose `BLOCK` was never spliced
warning: validation prose `BR` was never spliced
warning: validation prose `BR_IF` was never spliced
warning: validation prose `BR_ON_CAST` was never spliced
warning: validation prose `BR_ON_CAST_FAIL` was never spliced
warning: validation prose `BR_ON_NON_NULL` was never spliced
warning: validation prose `BR_ON_NULL` was never spliced
warning: validation prose `BR_TABLE` was never spliced
warning: validation prose `CALL` was never spliced
warning: validation prose `CALL_INDIRECT` was never spliced
warning: validation prose `CALL_REF` was never spliced
warning: validation prose `CONST` was never spliced
warning: validation prose `CVTOP` was never spliced
warning: validation prose `DATA.DROP` was never spliced
warning: validation prose `DROP` was never spliced
warning: validation prose `ELEM.DROP` was never spliced
warning: validation prose `EXTEND` was never spliced
warning: validation prose `EXTERN.CONVERT_ANY` was never spliced
warning: validation prose `GLOBAL.GET` was never spliced
warning: validation prose `GLOBAL.SET` was never spliced
warning: validation prose `I31.GET` was never spliced
warning: validation prose `IF` was never spliced
warning: validation prose `LOAD` was never spliced
warning: validation prose `LOCAL.GET` was never spliced
warning: validation prose `LOOP` was never spliced
warning: validation prose `MEMORY.COPY` was never spliced
warning: validation prose `MEMORY.FILL` was never spliced
warning: validation prose `MEMORY.GROW` was never spliced
warning: validation prose `MEMORY.INIT` was never spliced
warning: validation prose `MEMORY.SIZE` was never spliced
warning: validation prose `NOP` was never spliced
warning: validation prose `REF.AS_NON_NULL` was never spliced
warning: validation prose `REF.CAST` was never spliced
warning: validation prose `REF.EQ` was never spliced
warning: validation prose `REF.FUNC` was never spliced
warning: validation prose `REF.I31` was never spliced
warning: validation prose `REF.IS_NULL` was never spliced
warning: validation prose `REF.NULL` was never spliced
warning: validation prose `REF.TEST` was never spliced
warning: validation prose `RELOP` was never spliced
warning: validation prose `RETURN` was never spliced
warning: validation prose `RETURN_CALL` was never spliced
warning: validation prose `RETURN_CALL_INDIRECT` was never spliced
warning: validation prose `RETURN_CALL_REF` was never spliced
warning: validation prose `SELECT` was never spliced
warning: validation prose `STORE` was never spliced
warning: validation prose `STRUCT.GET` was never spliced
warning: validation prose `STRUCT.NEW` was never spliced
warning: validation prose `STRUCT.NEW_DEFAULT` was never spliced
warning: validation prose `STRUCT.SET` was never spliced
warning: validation prose `TABLE.COPY` was never spliced
warning: validation prose `TABLE.FILL` was never spliced
warning: validation prose `TABLE.GET` was never spliced
warning: validation prose `TABLE.GROW` was never spliced
warning: validation prose `TABLE.INIT` was never spliced
warning: validation prose `TABLE.SET` was never spliced
warning: validation prose `TABLE.SIZE` was never spliced
warning: validation prose `TESTOP` was never spliced
warning: validation prose `UNOP` was never spliced
warning: validation prose `UNREACHABLE` was never spliced
== Complete.
```
