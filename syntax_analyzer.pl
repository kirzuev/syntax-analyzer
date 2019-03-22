#!/bin/swipl

translate_sentence([], []).
translate_sentence([EngW|EngT], [GerW|GetT]):-
  translate_word(EngW, GerW),
  translate_sentence(EngT, GetT).

sentence(sentence(NounGroupTree, VerbGroupTree), List):-
  noun_group(NounGroupTree, List, List1, Num),
  verb_group(VerbGroupTree, List1, [], Num).

noun_group(noun_group(NounTree), [Noun|List], List, plural):-
  noun(NounTree, Noun, plural),!.
noun_group(noun_group(ArtTree, NounTree), [Art,Noun|List], List, Num):-
  article(ArtTree, Art, Num),
  noun(NounTree, Noun, Num),!.
noun_group(noun_group(ProTree), [Pro|List], List, Num):-
  pronoun(ProTree, Pro, Num),!.
noun_group(noun_group(ArtTree, AdjGroupTree, NounTree), [Det|List], List1, Num):-
  article(ArtTree, Det, Num),
  adjective_group(AdjGroupTree, List, [Noun|List1]),
  noun(NounTree, Noun, Num),!.
noun_group(noun_group(AdjGroupTree, NounTree), List, List1, plural):-
  adjective_group(AdjGroupTree, List, [Noun|List1]),
  noun(NounTree, Noun, plural),!.

article(article(Word), Word, Num):-
  is_article(Word, Num).

adjective_group(adjective_group(Adjectives), List, List1):-
  adjectives(Adjectives, List, List1).

adjectives([], List, List).
adjectives([Adj|Tail], [Word|T], List):-
  adjective(Adj, Word),
  adjectives(Tail, T, List).
adjectives([AdvT,AdjT|Tail], [AdvW,AdjW|T], List):-
  adverb(AdvT, AdvW),
  adjective(AdjT, AdjW),
  adjectives(Tail, T, List).

adjective(adjective(Word), Word):-
  is_adjective(Word).

noun(noun(Word), Word, Num):-
  is_noun(Word, Num).

pronoun(pronoun(Word), Word, Num):-
  is_pronoun(Word, Num).

verb_group(verb_group(VerbTree), [Verb], [], Num):-
  verb(VerbTree, Verb, Num),!.
verb_group(verb_group(AdvTree, VerbTree), [Adv, Verb], [], Num):-
  adverb(AdvTree, Adv),
  verb(VerbTree, Verb, Num),!.
verb_group(verb_group(VerbTree, AddGroupTree), [Verb|List], [], Num):-
  verb(VerbTree, Verb, Num),
  addition_group(AddGroupTree, List, []),!.
verb_group(verb_group(AdvTree, VerbTree, AddGroupTree), [Adv,Verb|List], [], Num):-
  adverb(AdvTree, Adv),
  verb(VerbTree, Verb, Num),
  addition_group(AddGroupTree, List, []), !.

adverb(adverb(Word), Word):-
  is_adverb(Word).

verb(verb(Word), Word, Num):-
  is_verb(Word, Num).

addition_group(addition_group(Additions), List, []):-
  additions(Additions, List, []).

additions([], List, List).
additions([addition(Add)|AddTail], List, []):-
  noun_group(Add, List, List1, _),
  additions(AddTail, List1, []),!.
additions([addition(Prepos,Add)|AddTail], [Word|List], []):-
  preposition(Prepos, Word),
  noun_group(Add, List, List1, _),
  additions(AddTail, List1, []),!.

preposition(preposition(Word), Word):-
  is_preposition(Word).

string_to_tree(String, Tree):-
  string_chars(String, Chars),
  make_atom_list(Chars, List, []),
  sentence(Tree, List).

make_atom_list([], [Atom], Word):-
  atom_chars(Atom, Word).
make_atom_list([' '|T], [Atom|List], Word):-
  atom_chars(Atom, Word),
  make_atom_list(T, List, []),!.
make_atom_list([C|T], List, Word):-
  append(Word, [C], NewWord),
  make_atom_list(T, List, NewWord).

is_article(the, _).
is_article(a, singular).
is_article(an, singular).

is_adjective(beautiful).
is_adjective(short).
is_adjective(next).
is_adjective(clear).
is_adjective(red).
is_adjective(blue).
is_adjective(green).
is_adjective(yellow).
is_adjective(white).
is_adjective(low).
is_adjective(big).
is_adjective(small).
is_adjective(new).
is_adjective(first).
is_adjective(last).
is_adjective(long).
is_adjective(great).
is_adjective(little).
is_adjective(high).
is_adjective(important).
is_adjective(different).
is_adjective(large).
is_adjective(old).
is_adjective(young).
is_adjective(other).

is_noun(cat, singular).
is_noun(cats, plural).
is_noun(dog, singular).
is_noun(dogs, plural).
is_noun(boy, singular).
is_noun(boys, plural).
is_noun(girl, singular).
is_noun(girls, plural).
is_noun(house, singular).
is_noun(houses, plural).
is_noun(people, plural).
is_noun(child, singular).
is_noun(children, plural).
is_noun(man, singular).
is_noun(men, plural).
is_noun(woman, singular).
is_noun(women, plural).
is_noun(animal, singular).
is_noun(animals, plural).
is_noun(book, singular).
is_noun(books, plural).
is_noun(music, singular).
is_noun(horse, singular).
is_noun(horses, plural).
is_noun(bird, singular).
is_noun(birds, plural).
is_noun(meat, singular).
is_noun(food, plural).

is_pronoun(you, _).
is_pronoun(she, singular).
is_pronoun(he, singular).
is_pronoun(it, singular).
is_pronoun(we, plural).
is_pronoun(they, plural).

is_adverb(not).
is_adverb(very).
is_adverb(also).
is_adverb(well).
is_adverb(often).
is_adverb(good).
is_adverb(bad).
is_adverb(fast).
is_adverb(quickly).
is_adverb(never).
is_adverb(actually).
is_adverb(probably).
is_adverb(maybe).
is_adverb(usually).
is_adverb(really).
is_adverb(early).
is_adverb(always).
is_adverb(sometimes).
is_adverb(together).
is_adverb(generally).
is_adverb(especially).
is_adverb(nearly).
is_adverb(normally).
is_adverb(carefully).
is_adverb(greatly).

is_verb(reads, singular).
is_verb(read, plural).
is_verb(eats, singular).
is_verb(eat, plural).
is_verb(runs, singular).
is_verb(run, plural).
is_verb(drinks, singular).
is_verb(drink, plural).
is_verb(walks, singular).
is_verb(walk, plural).
is_verb(has, singular).
is_verb(have, plural).
is_verb(is, singular).
is_verb(are, plural).
is_verb(gets, singular).
is_verb(get, plural).
is_verb(takes, singular).
is_verb(take, plural).
is_verb(sees, singular).
is_verb(see, plural).
is_verb(looks, singular).
is_verb(look, plural).
is_verb(wants, singular).
is_verb(want, plural).
is_verb(gives, singular).
is_verb(give, plural).
is_verb(feels, singular).
is_verb(feel, plural).
is_verb(works, singular).
is_verb(work, plural).

is_preposition(at).
is_preposition(on).
is_preposition(in).
is_preposition(of).
is_preposition(between).
is_preposition(under).
is_preposition(over).
is_preposition(above).
is_preposition(to).
is_preposition(for).
is_preposition(with).
is_preposition(from).
is_preposition(by).
is_preposition(about).
is_preposition(as).
is_preposition(into).
is_preposition(like).
is_preposition(through).
is_preposition(after).
is_preposition(against).
is_preposition(during).
is_preposition(without).
is_preposition(before).
is_preposition(among).

translate_word(the, das).
translate_word(a, ein).
translate_word(an, ein).
translate_word(beautiful, schön).
translate_word(short, kurz).
translate_word(next, nächste).
translate_word(clear, klar).
translate_word(red, rot).
translate_word(blue, blau).
translate_word(green, grün).
translate_word(yellow, gelb).
translate_word(white, weiß).
translate_word(low, niedrig).
translate_word(big, groß).
translate_word(small, klein).
translate_word(new, neu).
translate_word(first, zuerst).
translate_word(last, zuletzt).
translate_word(long, lang).
translate_word(great, großartig).
translate_word(little, wenig).
translate_word(high, hoch).
translate_word(important, wichtig).
translate_word(different, verschieden).
translate_word(large, dick).
translate_word(old, alt).
translate_word(young, jung).
translate_word(other, andere).
translate_word(cat, katze).
translate_word(cats, katzen).
translate_word(dog, hund).
translate_word(dogs, hunde).
translate_word(boy, junge).
translate_word(boys, jungen).
translate_word(girl, mädchen).
translate_word(girls, mädchen).
translate_word(house, haus).
translate_word(houses, häuser).
translate_word(people, personen).
translate_word(child, kind).
translate_word(children, kinder).
translate_word(man, mann).
translate_word(men, männer).
translate_word(woman, frau).
translate_word(women, frauen).
translate_word(animal, tier).
translate_word(animals, tiere).
translate_word(book, buch).
translate_word(books, bücher).
translate_word(music, musik).
translate_word(horse, pferd).
translate_word(horses, pferde).
translate_word(bird, vogel).
translate_word(birds, vögel).
translate_word(meat, fleisch).
translate_word(food, nahrung).
translate_word(you, sie).
translate_word(she, sie).
translate_word(he, er).
translate_word(it, es).
translate_word(we, wir).
translate_word(they, sie).
translate_word(not, nicht).
translate_word(very, sehr).
translate_word(also, auch).
translate_word(well, wohl).
translate_word(often, oft).
translate_word(good, gut).
translate_word(bad, schlecht).
translate_word(fast, schnell).
translate_word(quickly, rasch).
translate_word(never, niemals).
translate_word(actually, eigentlich).
translate_word(probably, wahrscheinlich).
translate_word(maybe, vielleicht).
translate_word(usually, gewöhnlich).
translate_word(really, wirklich).
translate_word(early, früh).
translate_word(always, immer).
translate_word(sometimes, manchmal).
translate_word(together, zusammen).
translate_word(generally, allgemein).
translate_word(especially, besonders).
translate_word(nearly, fast).
translate_word(normally, normalerweise).
translate_word(carefully, sorgfältig).
translate_word(greatly, stark).
translate_word(eats, isst).
translate_word(eat, essen).
translate_word(runs, läuft).
translate_word(run, lauf).
translate_word(drinks, getränke).
translate_word(drink, getränk).
translate_word(walks, geht).
translate_word(walk, gehen).
translate_word(has, hat).
translate_word(have, haben).
translate_word(is, ist).
translate_word(are, sind).
translate_word(gets, bekommt).
translate_word(get, bekommen).
translate_word(takes, nimmt).
translate_word(take, nehmen).
translate_word(sees, sieht).
translate_word(see, siehe).
translate_word(looks, schaut).
translate_word(look, schauen).
translate_word(wants, will).
translate_word(want, wollen).
translate_word(gives, gibt).
translate_word(give, geben).
translate_word(feels, fühlt).
translate_word(feel, fühlen).
translate_word(works, arbeitet).
translate_word(work, arbeit).
translate_word(at, beim).
translate_word(on, auf).
translate_word(in, im).
translate_word(of, von).
translate_word(between, zwischen).
translate_word(under, unter).
translate_word(over, über).
translate_word(above, oben).
translate_word(to, zu).
translate_word(for, zum).
translate_word(with, mit).
translate_word(from, aus).
translate_word(by, durch).
translate_word(about, über).
translate_word(as, zu).
translate_word(into, in).
translate_word(like, ähnlich).
translate_word(through, durch).
translate_word(after, nach).
translate_word(against, gegen).
translate_word(during, während).
translate_word(without, ohne).
translate_word(before, vor).
translate_word(among, unter).
