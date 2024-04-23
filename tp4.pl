:- dynamic rocher/2.
:- dynamic arbre/2.

:- dynamic vache/4.

:- dynamic dimitri/2.

largeur(10).
hauteur(10).

nombre_rochers(4).
nombre_arbres(5).
nombre_vaches(brune,4).
nombre_vaches(simmental,3).
nombre_vaches(alpine_herens,6).

occupe(X,Y) :- rocher(X,Y);
              arbre(X,Y);
             vache(X,Y,_,_);
              dimitri(X,Y).

libre(X,Y) :- repeat, hauteur(H), largeur(L),random(0,L,X), random(0,H,Y),not(occupe(X,Y)),!.

placer_rochers(0).
placer_rochers(N) :- libre(X,Y), assertz(rocher(X,Y)), N1 is N-1,placer_rochers(N1).

placer_arbres(0).
placer_arbres(N) :- libre(X,Y), assertz(arbre(X,Y)), N1 is N-1, placer_arbres(N1).

placer_vaches(_,0).
placer_vaches(Race,N) :- libre(X,Y),assertz(vache(X,Y,Race,vivante)), N1 is N-1, placer_vaches(Race,N1).

placer_dimitri :- libre(X,Y), assertz(dimitri(X,Y)).

vaches(L) :- findall([X,Y], vache(X,Y,_,vivante),L).


creer_zombie :-
  vaches(Vaches),
  random_member(Zombie_position,Vaches),
  nth0(0, Zombie_position,X), nth0(1, Zombie_position,Y),
  retract(vache(X,Y,Race,vivante)),
  assertz(vache(X,Y,Race,zombie)).



question(R) :-
  writeln('Dans quelle direction voulez vous déplacer Dimitri ? :)'),
  read(X),
  (
      X = 'reste', R = reste;
      X = 'nord', R = nord;
      X = 'sud', R = sud;
      X = 'est', R = est;
      X = 'ouest', R = ouest
  ).


zombification :-
  findall([X,Y],vache(X,Y,_,zombie),L),
  coordonnees_victimes(L,C),
  zombification(C).

coordonnees_victimes([],[]).
coordonnees_victimes([[X,Y]|L],C):-
  coordonnees_victimes(L,C1),
  X1 is X+1,X2 is X-1, Y1 is Y+1, Y2 is Y-1,
  C = [[X1,Y],[X2,Y],[X,Y1],[X,Y2]|C1].

zombification([]).
zombification([[X,Y]|L]) :-
  vaches(Vaches),
  (   member([X,Y],Vaches),
      retract(vache(X,Y,Race,vivante)),
      assertz(vache(X,Y,Race,zombie));
      true
  ),
  zombification(L).


deplacement_vaches :-
      findall([X,Y],vache(X,Y,_,_),Vaches),
      deplacement_vaches(Vaches).

deplacement_vaches([]).
deplacement_vaches([[X,Y]|L]):-
      random_member(Direction,[reste,nord,sud,est,ouest]),
      deplacement_vache(X,Y,Direction),
      deplacement_vaches(L).

deplacement_vache(_,_,Direction) :-
  Direction == reste.

deplacement_vache(X,Y,Direction) :-
  Direction == est,
  hauteur(H),
  X1 is X+1,
  (   X1 < H,
      not(occupe(X1,Y)),
      retract(vache(X,Y,Race,Etat)),
      assertz(vache(X1,Y,Race,Etat));
      true
  ).


deplacement_vache(X,Y,Direction) :-
  Direction == ouest,
  X1 is X-1,
 (    X1 >= 0,
      not(occupe(X1,Y)),
      retract(vache(X,Y,Race,Etat)),
      assertz(vache(X1,Y,Race,Etat));
      true
 ).


deplacement_vache(X,Y,Direction) :-
  Direction == sud,
  largeur(L),
  Y1 is Y+1,
  (   Y1 < L,
      not(occupe(X,Y1)),
      retract(vache(X,Y,Race,Etat)),
      assertz(vache(X,Y1,Race,Etat));
      true
  ).



deplacement_vache(X,Y,Direction) :-
  Direction == nord,
  Y1 is Y-1,
  (   Y1 >= 0,
      not(occupe(X,Y1)),
      retract(vache(X,Y,Race,Etat)),
      assertz(vache(X,Y1,Race,Etat));
      true
  ).

deplacement_joueur(Direction) :-
  Direction == reste.

deplacement_joueur(Direction) :-
  Direction == est,
  dimitri(X,Y),
  hauteur(H),
  X1 is X+1,
  (   X1 < H,
      not(occupe(X1,Y)),
      retract(dimitri(X,Y)),
      assertz(dimitri(X1,Y));
      true
  ).


deplacement_joueur(Direction) :-
  Direction == ouest,
  dimitri(X,Y),
  X1 is X-1,
  (   X1 >= 0,
      not(occupe(X1,Y)),
      retract(dimitri(X,Y)),
      assertz(dimitri(X1,Y));
      true
  ).


deplacement_joueur(Direction) :-
  Direction == sud,
  dimitri(X,Y),
  largeur(L),
  Y1 is Y+1,
  (   Y1 < L,
      not(occupe(X,Y1)),
      retract(dimitri(X,Y)),
      assertz(dimitri(X,Y1));
      true
  ).



deplacement_joueur(Direction) :-
  Direction == nord,
  dimitri(X,Y),
  Y1 is Y-1,
  (   Y1 >= 0,
      not(occupe(X,Y1)),
      retract(dimitri(X,Y)),
      assertz(dimitri(X,Y1));
      true
  ).

verification :-
  findall([X,Y],vache(X,Y,_,zombie),Vaches_zombies),
  verification(Vaches_zombies).

verification([]).
verification([[Xv,Yv]|L]):-
  dimitri(Xd,Yd),
  Dxx is Xd-Xv, Dyy is Yd-Yv,
  Dx is abs(Dxx), Dy is abs(Dyy),
  Distance is Dx+Dy, Distance > 1,
  verification(L).


initialisation :-
  nombre_rochers(NR),
  placer_rochers(NR),
  nombre_arbres(NA),
  placer_arbres(NA),
  nombre_vaches(brune, NVB),
  placer_vaches(brune, NVB),
  nombre_vaches(simmental, NVS),
  placer_vaches(simmental, NVS),
  nombre_vaches(alpine_herens, NVH),
  placer_vaches(alpine_herens, NVH),
  placer_dimitri,
  creer_zombie,
  !.

affichage(L, _) :-
  largeur(L),
  nl.

affichage(L, H) :-
  rocher(L, H),
  print('O'),
  L_ is L + 1,
  affichage(L_, H).

affichage(L, H) :-
  arbre(L, H),
  print('T'),
  L_ is L + 1,
  affichage(L_, H).

affichage(L, H) :-
  dimitri(L, H),
  print('D'),
  L_ is L + 1,
  affichage(L_, H).

affichage(L, H) :-
  vache(L, H, brune, vivante),
  print('B'),
  L_ is L + 1,
  affichage(L_, H).
affichage(L, H) :-
  vache(L, H, brune, zombie),
  print('b'),
  L_ is L + 1,
  affichage(L_, H).

affichage(L, H) :-
  vache(L, H, simmental, vivante),
  print('S'),
  L_ is L + 1,
  affichage(L_, H).
affichage(L, H) :-
  vache(L, H, simmental, zombie),
  print('s'),
  L_ is L + 1,
  affichage(L_, H).

affichage(L, H) :-
  vache(L, H, alpine_herens, vivante),
  print('H'),
  L_ is L + 1,
  affichage(L_, H).
affichage(L, H) :-
  vache(L, H, alpine_herens, zombie),
  print('h'),
  L_ is L + 1,
  affichage(L_, H).

affichage(L, H) :-
  \+ occupe(L, H),
  print('.'),
  L_ is L + 1,
  affichage(L_, H).

affichage(H) :-
  hauteur(H).

affichage(H) :-
  hauteur(HMax),
  H < HMax,
  affichage(0, H),
  H_ is H + 1,
  affichage(H_).

affichage :-
  affichage(0),!.



jouer :-
  initialisation,
  tour(0).

tour_(_) :-
  \+ verification,
  write('Dimitri s\'est fait mordre <3'),!.
tour_(N) :-
  verification,
  M is N + 1,
  tour(M).

tour(N) :-
  affichage,
  question(R),
  deplacement_joueur(R),
  deplacement_vaches,
  zombification,
  tour_(N).