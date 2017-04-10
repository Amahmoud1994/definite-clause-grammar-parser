%!sentence(Parse_tree, [the,bat,eats,a,cat], []).
%sentence(Parse_tree, [the,young,boy,put,a,big,box,in,the,empty,room,after,school], []).
%sentence(Parse_tree, [the,old,woman,gave,the,poor,young,man,a,white,envelope,in,the,shed,behind,the,building], []).
%sentence(Parse_tree, [the,pretty,woman,who,worked,with,the,old,man,secretly,gave,me,three,letters], []).
%sentence(Parse_tree, [every,boy,quickly,climbed,some,big,tree], []).

sentence(s(NP,VP)) --> noun_phrase(NP), verb_phrase(VP).
noun_phrase(np(D,N)) --> det(D), noun(N).
noun_phrase(np(D,N,A)) --> det(D), noun(N),adverb(A).

noun_phrase(np(N)) --> noun(N).
noun_phrase(np(D,N,A)) --> adj_phrase(D), noun(N),preposition_phrase(A).
noun_phrase(np(D,A,N,P)) --> det(D), adj_phrase(A), noun(N), preposition_phrase(P).
noun_phrase(np(D,A,N,P)) --> det(D), adj_phrase(A), noun(N), adverb_phrase(P).
noun_phrase(np(D,A,N,P)) --> det(D), adj_phrase(A), noun(N), pronoun_phrase(P).
noun_phrase(np(D,A,N)) --> det(D), adj_phrase(A), noun(N).
noun_phrase(np(D,A,N,NP)) --> det(D), adj_phrase(A), noun(N),noun_phrase(NP).

%noun_phrase(np()) --> [].

verb_phrase(vp(V)) --> verb(V).
verb_phrase(vp(V,A)) --> verb(V),preposition_phrase(A).
verb_phrase(vp(V,A)) --> verb(V),pronoun_phrase(A).
verb_phrase(vp(V,NP)) --> verb(V), noun_phrase(NP).
verb_phrase(vp(V,NP,A)) --> verb(V), noun_phrase(NP),preposition_phrase(A).
%verb_phrase(vp(AP,VP)) --> adverb_phrase(AP),verb_phrase(VP).
%verb_phrase(vp()) --> [].

preposition_phrase(prep(D,N)) --> preposition(D),noun_phrase(N).
preposition_phrase(prep(D)) --> preposition(D).

pronoun_phrase(pron(P,VP)) --> pronoun(P),verb_phrase(VP).
pronoun_phrase(pron(P)) --> pronoun(P).
pronoun_phrase(pron(P,NP)) --> pronoun(P),noun_phrase(NP).

adj_phrase(adj(X)) --> adjective(adj(X)).
adj_phrase(adj(X,Y)) --> adjective(X), adj_phrase(Y).
%adj_phrase(adj()) --> [].

adverb_phrase(adv(X,VP)) --> adverb(adv(X)), verb_phrase(VP).
%adverb_phrase(adv()) --> [].

det(d(the)) --> [the].
det(d(a)) --> [a].
det(d(that)) --> [that].
det(d(these)) --> [these].
det(d(those)) --> [those].
det(d(this)) --> [this].
det(d(every)) --> [every].
det(d(an)) --> [an].
det(d(some)) --> [some].
det(d(three)) --> [three].


adjective(adj(young)) --> [young].
adjective(adj(large)) --> [large].
adjective(adj(empty)) --> [empty].
adjective(adj(old)) --> [old].
adjective(adj(poor)) --> [poor].
adjective(adj(white)) --> [white].
adjective(adj(pretty)) --> [pretty].
adjective(adj(big)) --> [big].
adjective(adj(good)) --> [good].
adjective(adj(bad)) --> [bad].
adjective(adj(great)) --> [great].
adjective(adj(little)) --> [little].
adjective(adj(own)) --> [own].
adjective(adj(other)) --> [other].
adjective(adj(different)) --> [different].
adjective(adj(difficult)) --> [difficult].
adjective(adj(public)) --> [public].
adjective(adj(private)) --> [private].
adjective(adj(few)) --> [few].
adjective(adj(important)) --> [important].


pronoun(pron(who)) --> [who].
pronoun(pron(me)) --> [me].


%preposition(prep(who)) --> [who].
%preposition(prep(me)) --> [me].
preposition(prep(after)) --> [after].
preposition(prep(behind)) --> [behind].
preposition(prep(in)) --> [in].
preposition(prep(with)) --> [with].
preposition(prep(of)) --> [of].
preposition(prep(to)) --> [to].
preposition(prep(across)) --> [across].
preposition(prep(against)) --> [against].
preposition(prep(along)) --> [along].
preposition(prep(above)) --> [above].



adverb(adv(secretly)) --> [secretly].
adverb(adv(not)) --> [not].
adverb(adv(finally)) --> [finally].
adverb(adv(lightly)) --> [lightly].
adverb(adv(beautifully)) --> [beautifully].
adverb(adv(financially)) --> [financially].
adverb(adv(expertly)) --> [expertly].
adverb(adv(uneasily)) --> [uneasily].
adverb(adv(easily)) --> [easily].
adverb(adv(wierdly)) --> [wierdly].
adverb(adv(quickly)) --> [quickly].


noun(n(me)) --> [me].
noun(n(cat)) --> [cat].
noun(n(bat)) --> [bat].
noun(n(boy)) --> [boy].
noun(n(room)) --> [room].
noun(n(school)) --> [school].
noun(n(box)) --> [box].
noun(n(woman)) --> [woman].
noun(n(man)) --> [man].
noun(n(envelope)) --> [envelope].
noun(n(shed)) --> [shed].
noun(n(building)) --> [building].
noun(n(letter)) --> [letter].
noun(n(letters)) --> [letters].
noun(n(paper)) --> [paper].
noun(n(papers)) --> [papers].
noun(n(tree)) --> [tree].
noun(n(trees)) --> [trees].
noun(n(flower)) --> [flower].
noun(n(flowers)) --> [flowers].
noun(n(rocks)) --> [rocks].
noun(n(hand)) --> [hand].
noun(n(hands)) --> [hands].

verb(v(eats)) --> [eats].
verb(v(put)) --> [put].
verb(v(give)) --> [give].
verb(v(gave)) --> [gave].
verb(v(given)) --> [given].
verb(v(climb)) --> [climb].
verb(v(climbed)) --> [climbed].
verb(v(work)) --> [work].
verb(v(worked)) --> [worked].
verb(v(pick)) --> [pick].
verb(v(wrote)) --> [wrote].
verb(v(written)) --> [written].
verb(v(ride)) --> [ride].
verb(v(rode)) --> [rode].
verb(v(ridden)) --> [ridden].
verb(v(play)) --> [play].
verb(v(played)) --> [played].
verb(v(type)) --> [type].
verb(v(typed)) --> [typed].
verb(v(call)) --> [call].
verb(v(called)) --> [called].
verb(v(named)) --> [named].
