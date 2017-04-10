# Definite Clause Grammer Parser
The lexicon should include all the words appearing in the examples commented on top of the file, at least twenty
nouns, at least twenty verbs (with the past tense inflection), at least twenty adjectives, at least
ten adverbs, at least ten prepositions, and at least ten determiners.
A successful parse should result in building a parse tree for the input; make sure your parser
generates such a tree. We linearly represent a parse tree as follows:
a) l, where l is the label of a leaf.
b) p(l1; l2; : : : ; ln), where p is a label of a parent node and li is the ith sub-tree thereof,
where left-to-right order is assumed.
