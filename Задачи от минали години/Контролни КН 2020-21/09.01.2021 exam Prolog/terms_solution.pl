/** 
 *  Credit to one of the colleagues in our team.
 */

:- use_module(library(clpfd)).
% We shall use the following representation of a closed term:
% 1. T=[c], where [c,0] is a constant symbol
% 2. T=[f|t1,t2,...,t{N}], where [f,N] with N>0 is a functional symbol
% of arrity N and t1 trhough t{N} are representations of closed terms.
%
% The idea is the following. Given a list of generalised functional
% symbols, we shall:
% 1. generate arbitrary (flat) sequences Seq of generalised functional
% symbols
% 2. parse Seq to a desired closed term, i.e.:
% 2a). remove the second coordinates of the pairs.
% 2b). set the "brackets" so that the list complies with the above
% definition of a closed term.
%
% Assuming that point 2 is more chalenging, we start with it. We use a
% mutual recursion to build a term out a sequence/list, Seq, of general
% terms.
%
% build_term(Seq,Term,TailSeq) takes an input sequence Seq, parses the
% shortest prefix of Seq that generates the closed term, Term, and the
% remaining (unparsed) suffix of Seq, TailSeq.
%
% build_seq_term(Seq,N,Terms,TailSeq) takes as input a sequence Seq and
% an integer N, and generates the sequence of N terms in Terms that
% corresponds to an initial segment of Seq and the remaining suffix of
% Seq is generated in TailSeq.
%
% The details are as follows:

build_term([[C,0]|TailSeq],[C],TailSeq).
build_term([[F,N]|Tail],[F|Terms],TailSeq):-N#>0,build_seq_term(Tail,N,Terms,TailSeq).
build_seq_term(Seq,0,[],Seq).
build_seq_term(Seq,N,[T1|Terms],TailSeq):-N#>0,N1#=N-1,build_term(Seq,T1,TailSeq1),build_seq_term(TailSeq1,N1,Terms,TailSeq).

% Now, that we can parse a sequence of generalised functional symbols,
% all we need is given a list of generalised functional symbols, L, to
% generate all finite (non-empty) sequences Seq of items in L and use
% build_term(Seq,Term,TailSeq) to generate the closed term, Term, and
% test whether TailSeq=[], i.e. the entire sequence has been
% successfully parsed.
%
% We generate sequences by:
% 1. generating the length of the sequence, N
% 2. simulate a loop of N iterations where we generate, using member,
% elements of Seq.
%
% We start with 2.
% gen_sequence(L,N,Seq) given a list L and an integer N, generates in
% Seq when resatisfied all the sequences of items in L in Seq.
%
gen_sequence(L,0,[]).
gen_sequence(L,N,[A|Seq]):-N#>0,mem1(A,L),N1#=N-1,gen_sequence(L,N1,Seq).

% mem1(A,L) given L generates in A when resatisfied all the elements
% of L.
mem1(A,[A|_]).
mem1(A,[_|L]):-mem1(A,L).
% Finally, we generate the positive integers. nat(N) generates positive
% integers in N.
nat(1).
nat(N):-nat(N1),N#=N1+1.
% Putting everything together: given L - a list of
% (generalised) functional symbols:
% 1. generate N - a natural number
% 2. generate a sequence, Seq of length N of elements of L
% 3. build a term from the sequence Seq
% 4. Check whether it is valid, by verifying that the sequence has been
% completely parsed.
%
gen_terms(L,Term):-nat(N),gen_sequence(L,N,Seq),build_term(Seq,Term,SeqTail),SeqTail=[].




