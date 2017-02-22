%option main

%START  NORM  VERB  PROG  MATH  SYNTAX
sp			[ \t]*
verb			\n{sp}@{sp}\n
prog			\n{sp}@@@{sp}\n
math			\n{sp}\"{sp}\n
eqn			\n{sp}\"\"{sp}\n
eqqn			\n{sp}\"\"\"{sp}\n
nl			{sp}\n{sp}
subtype			_\{@@@
subst			_\{\\st
%{
#define PUSH		states[top++] =
#define POP		BEGIN states[--top]
%}
%%
			int states[256];
			int top;
			BEGIN NORM;
			top = 0;
<NORM>\\\"		{ printf ("\\\""); }
<NORM>@@		{ printf ("@"); }
<NORM>@			{ printf ("\\mbox{\\tt ");
			  PUSH NORM;  BEGIN VERB; }
<VERB>@@@		{ printf ("}} ");  POP; }
<VERB>@			{ printf ("}");  POP; }
<NORM>{verb}		{ printf ("\n\\vspace{-0.0em}\\begin{flushleft}\n\\mbox{\\tt ");
			  PUSH NORM;  BEGIN VERB; }
<NORM>@@@		{ printf ("{\\makebox{\\subtypesize\\tt ");
			  PUSH NORM;  BEGIN VERB; }
<NORM>{prog}		{ printf ("\n\\begin{flushleft}\n\\vspace{-0.0em}\\makebox{\\progsize\\tt ");
			  PUSH NORM;  BEGIN PROG; }
<NORM>{subtype}		{ printf ("_{\\subtypespace{\\makebox{\\subtypesize\\tt ");
			  PUSH NORM;  BEGIN VERB; }
<MATH>{subtype}		{ printf ("_{\\subtypespace{\\makebox{\\subtypesize\\tt ");
			  PUSH MATH;  BEGIN VERB; }
<NORM>{subst}		{ printf ("_{\\subtypespace\\st"); }
<MATH>{subst}		{ printf ("_{\\subtypespace\\st"); }
<VERB>{verb}		{ printf ("}\\vspace{-0.0em}\n\\end{flushleft}\n");  POP; }
<PROG>{prog}		{ printf ("}\\vspace{-0.0em}\n\\end{flushleft}\n");  POP; }
<VERB>\n		{ printf ("}\\\\{}\n\\mbox{\\tt "); }
<PROG>\n		{ printf ("}\\vspace{-0.0em}\\\\\n\\makebox{\\progsize\\tt "); }
<VERB>" "		{ printf ("\\ "); }
<PROG>" "		{ printf ("\\ "); }
<VERB>@@		{ printf ("@"); }
<PROG>@@		{ printf ("@"); }
<VERB>\#		{ printf ("{\\char'43}"); }
<PROG>\#		{ printf ("{\\char'43}"); }
<VERB>\$		{ printf ("{\\char'44}"); }
<PROG>\$		{ printf ("{\\char'44}"); }
<VERB>\%		{ printf ("{\\char'45}"); }
<PROG>\%		{ printf ("{\\char'45}"); }
<VERB>\&		{ printf ("{\\char'46}"); }
<PROG>\&		{ printf ("{\\char'46}"); }
<VERB>\~		{ printf ("{\\char'176}"); }
<PROG>\~		{ printf ("{\\char'176}"); }
<VERB>\_		{ printf ("{\\char'137}"); }
<PROG>\_		{ printf ("{\\char'137}"); }
<VERB>\^		{ printf ("{\\char'136}"); }
<PROG>\^		{ printf ("{\\char'136}"); }
<VERB>\\		{ printf ("{\\char'134}"); }
<PROG>\\		{ printf ("{\\char'134}"); }
<VERB>\{		{ printf ("{\\char'173}"); }
<PROG>\{		{ printf ("{\\char'173}"); }
<VERB>\}		{ printf ("{\\char'175}"); }
<PROG>\}		{ printf ("{\\char'175}"); }
<NORM>\"{sp}		{ printf ("${\\it ");
			  PUSH NORM;  BEGIN MATH; }
<MATH>{sp}\"		{ printf ("}$"); POP; }
<NORM>{math}{sp}	{ printf ("\n\\[\n\\it ");
			  PUSH NORM;  BEGIN MATH; }
<MATH>{sp}{math}	{ printf ("\n\\]\n"); POP; }
<NORM>{eqn}{sp}		{ printf ("\n\\beqs\n\\it ");
			  PUSH NORM;  BEGIN MATH; }
<NORM>{eqqn}{sp}	{ printf ("\n\\beqqs\n\\it ");
			  PUSH NORM;  BEGIN MATH; }
<MATH>{sp}{eqn}		{ printf ("\n\\eeqs\n"); POP; }
<MATH>{sp}{eqn}{sp}{eqn}{sp}	{ printf ("\n\\eeqs\n\\beqs\n\\it "); }
<MATH>{sp}{eqqn}	{ printf ("\n\\eeqqs\n"); POP; }
<MATH>{sp}{eqqn}{sp}{eqqn}{sp}	{ printf ("\n\\eeqqs\n\\beqqs\n\\it "); }
<MATH>{nl}		{ printf ("\\\\\n\\it "); }
<MATH>{sp}&{sp}		{ printf ("&\\it "); }
<MATH>\\{nl}		{ }
<MATH>\\" "		{ printf ("\\ "); }
<MATH>{sp}		{ printf ("\\ "); }
<MATH>"..."		{ printf ("\\ldots "); }
<MATH>"->"		{ printf ("\\rightarrow "); }
<MATH>"<-"		{ printf ("\\leftarrow "); }
<MATH>"<=>"		{ printf ("\\Leftrightarrow "); }
<MATH>"=>"		{ printf ("\\Rightarrow "); }
<MATH>"<="		{ printf ("\\Leftarrow "); }
<MATH>"<<"		{ printf ("\\approx "); }
<MATH>@@@		{ printf ("{\\makebox{\\subtypesize\\tt ");
			  PUSH MATH;  BEGIN VERB; }
<MATH>@@		{ printf ("@"); }
<MATH>@			{ printf ("\\makebox{\\tt ");
			  PUSH MATH;  BEGIN VERB; }
<MATH>"#1"		{ printf ("#1"); }
<MATH>"#2"		{ printf ("#2"); }
<MATH>"#3"		{ printf ("#3"); }
<MATH>"#4"		{ printf ("#4"); }
<MATH>"#5"		{ printf ("#5"); }
<MATH>"#6"		{ printf ("#6"); }
<MATH>"#7"		{ printf ("#7"); }
<MATH>"#8"		{ printf ("#8"); }
<MATH>"#9"		{ printf ("#9"); }
<MATH>"0"		{ printf ("{\\rm0}"); }
<MATH>"1"		{ printf ("{\\rm1}"); }
<MATH>"2"		{ printf ("{\\rm2}"); }
<MATH>"3"		{ printf ("{\\rm3}"); }
<MATH>"4"		{ printf ("{\\rm4}"); }
<MATH>"5"		{ printf ("{\\rm5}"); }
<MATH>"6"		{ printf ("{\\rm6}"); }
<MATH>"7"		{ printf ("{\\rm7}"); }
<MATH>"8"		{ printf ("{\\rm8}"); }
<MATH>"9"		{ printf ("{\\rm9}"); }

