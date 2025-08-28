\documentclass[twocolumn]{article}

% \usepackage[margin=1cm, papersize={20cm, 32cm}]{geometry}
\usepackage[margin=2cm, a4paper]{geometry}
\usepackage{cuted}          % provides strip environment
\usepackage{syntax}         % BNF code 
\usepackage{enumitem}
\usepackage[document]{ragged2e} % justify text

\setlength{\parskip}{0.8em} % space between paragraphs
\setlist{itemsep=0.1em}     % space between list items
\setlist[itemize]{leftmargin=*, labelindent=\parindent}
\setlist[enumerate]{leftmargin=*, labelindent=\parindent}

%=============================================================================% 
% lhs2tex derivatives                                                         %
%=============================================================================% 
%include polycode.fmt
%options poly
%options ghci -XRankNTypes -fprint-explicit-foralls

%format `AppendSymbol` = "\plus"
%format notImplemented = "\dots"
%format forall         = "\forall"

% ----------------------------------------------------------------------------%
% fix for lhs2tex bug: name endwith underscore will trigger latex errors      %
% ----------------------------------------------------------------------------%
%format tr_            = "\textit{tr_}"
%format td_            = "\textit{td_}"
%format th_            = "\textit{th_}"
%format h1_            = "\textit{h1_}"
%format table_         = "\textit{table_}"
%format class_         = "\textit{class_}"
%format hctraverse_    = "\textit{hctraverse_}"
%=============================================================================% 

\title{A meta standard for standards\\
towards formalising standard definition
}
\date{\today}
\author{Yunshui ZHANG}

\begin{document}

\maketitle

%if False
\begin{code}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Main where

import Meta
import UHOI.Chemical
import UHOI.Measure
import Data.Kind
import Data.Proxy
import Data.SOP
import Data.Type.Show
import Lucid
import GHC.TypeLits

data DbConn = LocalDataBase

readDb :: DbConn -> IO (Maybe a)
readDb _ = pure Nothing

notImplemented :: a
notImplemented = undefined

uniprot :: a -> String
uniprot = undefined

unitDimension :: forall (u :: Unit) . Proxy u -> String
unitDimension = undefined
  
main :: IO ()
main = putStrLn "Hello, UHOI!"
\end{code}
%endif

\section{Introduction}

Recall the problems we are trying to solve:

\begin{enumerate}
  \item How to define (describe) a standard, including 
        item concepts, validation rules, and relationships.
  \item How to enforce the standard in real-world practice.
  \item How to validate the rules defined in the standard.
  \item How to manage the evolution of the standard.
\end{enumerate}

% \lit{nixpkgs}, synt \synt{nixpkgs}

\section {DLS Concepts}

Domain-Specific Languages (DSLs) are specialized programming languages tailored
to a specific application domain. By restricting the language to a particular
domain, we can create more expressive and concise representations of concepts
within that domain. 

A DSL typically consists of two main components: the language constructs and 
the interpreter.
Compared with storing standards as plain text or in document formats or some 
structural data formats, a DSL interpreter can execute the standard providing 
functionalities such as validate rules defined by the standard. It is also superior
to API (Application Programming Interface) based approaches in the terms of 
\begin{enumerate}
  \item polysemic, enables multiple interpretations of the same standard throw multiple 
        interpreters;
  \item extensible, allowing users from 
        sub-domains to define new constructs and interpretations to cope with 
        their specific needs; and
\end{enumerate}

An EDSL (Embedded Domain-Specific Language) is a DSL that is embedded 
in a host language, programmers writing code in the host language but using 
constructs provided by the EDSL to express domain-specific concepts.
EDLS allows us to leverage the host language's syntax and semantics 
while providing additional constructs specific to the domain.

A type level EDSL is a EDSL which expressing domain entities not as values, 
but as types of the host language. Domain entities are represented as types,
allowing us to leverage the host language's type system for compile time 
domain specific constraints enforcement.

% This approach supports
% \begin{enumerate}
%   \item compile time domain-specific constraints enforcement by leveraging 
%         the host language's type checker;
%   \item extensible language constructs, allowing users from sub-domains
%         to define new constructs to cope with their specific needs;
%   \item decouples the language and its interpreter, allowing both 
%         multiple interpretations of the language constructs and 
%         extendable interpreter.
% \end{enumerate}

We propose to use a type level EDSL as an avenue of formalising standard definition.
In the next section, as an example, we present a type level EDSL for human biological data.

\section {A type level EDSL for UHOI}

\cite{uhoi} proposed a framework dubbed UHOI (Uniform Human Biological Data Identifier), 
which aims to provide a unified standard to representing and managing human biological data.

The UHOI consists of several subdomains, each may have multiple features:
\begin{enumerate}
  \item the concept domain ($C\#$), which contains chemical and physical features, etc.
  \item the measure domain ($M\#$), which contains measurement features, such as measure unit.
  \item the feature resource domain ($O\#$), which contains info of data source
  \item etc.
\end{enumerate}

\subsection {The Haskell language and Types in Haskell}
The EDSL uses Haskell as its host language. 
Haskell is a purely functional programming language 
with a sofisticated type system providing utilities to express complicate 
concepts in the type level.


Haskell has 3 levels of terms:
\begin{enumerate}
  \item Terms: the most basic level, representing values.
  \item Types: the next level, representing classifications of terms.
  \item Kinds: the highest level, representing classifications of types.
\end{enumerate}
Kinds can be thought of as types of types, providing a way to classify 
and reason about type-level constructs.

\begin{code}
quoteT    ::  TShow a => Proxy a -> String
quoteT p  =   "\"" ++ showT p ++ "\""
\end{code}

In the above example, \lit{quoteT} is in term level, which has a type 
that maps a \lit{Proxy a} to a \lit{String}, whenever \lit{a} 
is an instance of \lit{TShow}. \lit{TShow} is a type class, living in the 
type level, which has a kind of mapping any type level thing to a \lit{Constraint}. 
A \lit{Constraint} is a special kind, representing a set of conditions that types must satisfy.

We will use \lit{Type}s and \lit{Constraint}s as our DSL's constructs 
to represent concepts in the human biological domain.

\subsection {Language Constructs}

First lets see an example of defining a new concept in our EDSL:
\begin{code}
type ActinLikeProtein9 = 
  Concept "Actin Like Protein 9" 
     [  Chemical     :=  Protein "Actin-Like" 9
     ,  MeasureUnit  :=  Milli Mol :/ Liter
     ]
\end{code}

The concept $Actin Like Protein 9$ is a protein as its chemical feature 
and has a measure unit of $mmol/L$.

Since a concept is a type we can directly use it in type signatures:

\begin{code}
newtype Dose concept = Dose Double
  deriving stock   (Show)
  deriving newtype (Num, Eq, Ord)

actin9Dose :: Dose ActinLikeProtein9
actin9Dose = notImplemented
\end{code}

The type \lit{Dose ActinLikeProtein9} is different from \lit{Dose SomeOtherConcept}, 
the type checker will prevent us from mixing them up, for instance adding $2$ 
doses of different concepts.

\subsection { Extending the language }

We can extend the surface language in $2$ ways
\begin{enumerate}
  \item defining new features, such as \lit{Chemical} and \lit{MeasureUnit}.
  \item defining new values for a feature, such as \lit{Protein "Actin-Like" 9} 
        and \lit{Milli Mol :/ Liter}.
\end{enumerate}

We introduce some kind of type safety by using these 2 rules 
\begin{enumerate}
\item All features must be defined as a type class.
\item All features must implement the \lit{Feature} type class.
\item All legal values of a feature must implement the feature type class.
\end{enumerate}
Thus, the Haskell type checker will enforce that 
\lit{Protein "Actin-Like" 9} is actually a \lit{Chemical}, 
and \lit{Chemical := Milli Mol :/ Liter} will trigger a type error.

\subsubsection {Defining new features}

Lets create a new feature \lit{Doc} which gives a type level documentation of 
the language. Documents must be able to be human readable. Thus, the only 
requirement of a \lit{Doc} instance is that it can be printed, 
besides it must be a type class. 
Also it would be good to generate a real document in some format, such as html.

\begin{code}
class Doc (a :: k) where
  showDoc  :: value a -> String
  htmlDoc  :: value a -> Html ()
  htmlDoc concept = toHtml (showDoc concept)

instance Feature Doc where
  data Doc := doc

instance TShow Doc where
  type ShowT Doc = "Doc"
\end{code}

The \lit{Feature} type class provides some basic functionalities for all features.
You can also see that \lit{:=} is defined with \lit{Feature} making only a feature 
and appear on its left hand side.

\subsubsection {Defining new values for a feature}

Lets create some real documentation types for the \lit{Doc} feature.

\begin{code}
data Description (s :: Symbol)

instance KnownSymbol s => Doc (Description s) where
  showDoc _ = "Description: " ++ symbolVal (Proxy @s)

data RelatedTo concept

instance (TShow concept) => Doc (RelatedTo concept) where
  showDoc _ = "Related to: " ++ showT (Proxy @concept)
\end{code}

and extend the concept definition:

\begin{code}
type ActinLikeProtein9WithDoc = 
  Concept "Actin Like Protein 9" 
     [ Chemical     := Protein "Actin-Like" 9
     , MeasureUnit  := Milli Mol :/ Liter
     , Doc          := Description "A boring protein"
     , Doc          := RelatedTo ActinLikeProtein9
     ]
\end{code}

By embedding documentation in the type, values with type containing the concept 
will carry the documentation with them, making the documentation exposible to 
coding AI agents, such as Copilot and Cursor, which is designed to tracing type 
information.

\subsection {Interpreters --- running Concepts in multiple ways}

There are two technique that can be used to implement interpreters, type classes 
and type families. Type class can be used to map types to values, while type families are type level 
functions that map types to types.

\subsubsection {Generate Documentation}

Since we have already defined the \lit{Doc} feature, it will be better to generate 
a website that documents all the defined concepts in the human biological data domain.
We need an interpreter that maps concepts to a Html file.

The basic idea is to define the interpreter in the following steps 
\begin{enumerate}
\item define a type class that interpretes the instance type;
\item foreach \lit{feat := value} setup, implement the interpreter type class;
\item implement the interpreter for type matches \lit{[f1 := v1, f2 := v2, ...]}
      by combining the interpreters of each \lit{fi := vi};
\item implement the interpreter for the concept type.
\end{enumerate}

Following the above steps, we first define the interpreter type class. 

\begin{code}
class HasHtmlDoc a where
  type HtmlDocRequest a  ::  Type
  defaultHtmlDocRequest  ::  Proxy a 
                         ->  HtmlDocRequest a
  htmlDocResponse        ::  Proxy a 
                         ->  HtmlDocRequest a 
                         ->  Html ()
\end{code}

Then we implement \lit{HasHtmlDoc} for any chemical feature setup, i.e. 
\lit{Chemical := c}.

\begin{code}
instance {-# OVERLAPS #-} Chemical c 
  => HasHtmlDoc (Chemical := c) where
  type HtmlDocRequest (Chemical := c) = ()
  defaultHtmlDocRequest _ = ()
  htmlDocResponse _ _ = tr_ do 
    td_  [class_ "feature"] 
         "Chemical"
    td_  [class_ "feature-value"] 
         (toHtml $ chemicalName (Proxy @c))
\end{code}

Then we implement \lit{HasHtmlDoc} for any measure unit feature setup, i.e. 
\lit{MeasureUnit := u}.

\begin{code}
instance {-# OVERLAPS #-} (MeasureUnit u, TShow u) 
  => HasHtmlDoc (MeasureUnit := u) where
  type HtmlDocRequest (MeasureUnit := u) = ()
  defaultHtmlDocRequest _ = ()
  htmlDocResponse _ _ = tr_ do
    td_  [class_ "feature"] 
         "Measure Unit"
    td_  [class_ "feature-value"] 
         (toHtml $ showT (Proxy @u))
\end{code}

In the 3rd step, we implement the interpreter for any feature setup list type.

\begin{code}
data AsHtmlDocRequest a = 
  HtmlDocRequestWrapper 
  {  typ :: Proxy a
  ,  req :: HtmlDocRequest a
  }

instance All HasHtmlDoc as 
  => HasHtmlDoc as where
  type HtmlDocRequest as = NP AsHtmlDocRequest as
  defaultHtmlDocRequest _ =
    hcmap  (Proxy @HasHtmlDoc) 
           (\p -> HtmlDocRequestWrapper 
                    {  typ = p 
                    ,  req = defaultHtmlDocRequest p
                    }
           )
           (hpure Proxy :: NP Proxy as)
  htmlDocResponse _ = 
    hctraverse_ 
      (Proxy @HasHtmlDoc) 
      (\r -> htmlDocResponse r.typ r.req) 
\end{code}

We can also default default \lit{HasHtmlDoc} interpreters for feature setups, 
the only meaningful implementation is to ommit the feature setup.


\begin{code}
instance {-# OVERLAPPABLE #-} f v => 
  HasHtmlDoc (f := v) where
  type HtmlDocRequest (f := v) = ()
  defaultHtmlDocRequest _ = ()
  htmlDocResponse _ _ = mempty
\end{code}

Its hard to put a full text description in a Html table, the default implementation 
let us just remove the Doc feature from the feature table.

The \lit{OVERLAPPABLE} and \lit{OVERLAPS} tag controls how to selecting from 
multiple matching interpreters(type class instances). Thus the \lit{Chemical := c}
interpreter overrides the more general \lit{f := v} interpreter, and can be 
overridden by more specific one such as a
\lit{Chemical := Protein name version} interpreter.

And last we wrap it up by implementing \lit{HasHtmlDoc} for concept types.

\begin{code}
instance (KnownSymbol name, All HasHtmlDoc fs) 
  => HasHtmlDoc (Concept name fs) where
  type HtmlDocRequest (Concept name fs) = 
    HtmlDocRequest fs
  defaultHtmlDocRequest _ = 
    defaultHtmlDocRequest (Proxy @fs)
  htmlDocResponse _ req = do
    h1_  (toHtml $ symbolVal (Proxy @name)) 
            <> " Documentation"
    table_ [class_ "feature-table"] $ do
      tr_ [class_ "feature-table-header"] $ do 
        th_ "Feature"
        th_ "Value"
      htmlDocResponse (Proxy @fs) req
\end{code}

\subsubsection{A interpreter to gen unique id for concepts}

Interpreters can also be used to gen unique id for concepts, 
and we can have multiple interpreters to exist at the same time 
without interfering with each other.

The \lit{Uid} data type represents the unique id of a concept, feature, 
feature value, or feature setup.

\begin{code}
data Uid meta where
  Feat        ::  String -> Uid "f"
  FeatValue   ::  String -> Uid "v"
  FeatSetup   ::  Uid "f" 
              ->  Uid "v" 
              ->  Uid "f:=v"
  ConceptUid  ::  [Uid "f:=v"] 
              ->  Uid "c"
\end{code}

The \lit{HasUid} is the interpreter type class.

\begin{code}
class HasUid meta a | a -> meta where
  uid :: value a -> Uid meta
\end{code}

Implement interpreter for features.

\begin{code}

instance HasUid "f" Chemical where
  uid _ = Feat "C#"

instance HasUid "f" MeasureUnit where
  uid _ = Feat "M#"
\end{code}

Implement interpreter for feature values.

\begin{code}
instance (KnownSymbol name, KnownNat version)
  => HasUid "v" (Protein name version) where
  uid _ = FeatValue $ uniprot 
             (  "PROTEIN"  :: String 
             ,  symbolVal  (Proxy @name)
             ,  natVal     (Proxy @version)
             )

instance HasUid "v" (u :: Unit) where
  uid _ = FeatValue $ unitDimension (Proxy @u)

\end{code}  
Implement interpreter for feature setups.
\begin{code}
instance (HasUid "f" f, HasUid "v" v)
  => HasUid "f:=v" (f := v) where
  uid _ = FeatSetup  (uid (Proxy @f)) 
                     (uid (Proxy @v))

\end{code}  

Last implement interpreter for concepts.

\begin{code}
instance All (HasUid "f:=v") fs
  => HasUid "c" (Concept n fs)  where
  uid _ = let ps = hpure Proxy :: NP Proxy fs
              fs = hcmap (Proxy @(HasUid "f:=v")) (K . uid) ps
          in  ConceptUid (hcollapse fs)
\end{code}

\end{document}
