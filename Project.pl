/*database of the infected people*/
infected(ardian).
infected(jack).
infected(mike).

/*Two categories of symptoms: serious and mild*/
serious(loss-of-speech-or-movement).
serious(chest-pain-or-pressure).
serious(difficulty-breathing-or-shortness-of-breath).
symptoms(fever).
symptoms(cough).
symptoms(tiredness).
symptoms(sore-throat).
symptoms(headache).
symptoms(loss-of-taste-or-smell).

/*database of the different therapies for each symptom*/
therapy(loss-of-speech-or-movement, amphetamine).
therapy(chest-pain-or-pressure, aspirin).
therapy(difficulty-breathing-or-shortness-of-breath, antibiotics-levaquin).
therapy(fever, tylenol).
therapy(fever, advil).
therapy(cough, dextromethorphan ).
therapy(cough, pholcodine).
therapy(tiredness, dextroamphetamine).
therapy(tiredness, amantadine).
therapy(sore-throat, tylenol).
therapy(sore-throat, advil).
therapy(headache, advil).
therapy(headache, alene).
therapy(loss-of-taste-or-smell, antihistamines).
therapy(loss-of-taste-or-smell, steroid-nasal-spray).

/*The innitial number of symptoms of both categories are zero*/
numbSerious(0).
numbSymptom(0).

:-dynamic name/1, contacts/1, hasitSerious/1, doesnthaveitSerious/1, noContacts/1, hasitSymptom/1, doesnthaveitSymptom/1, numbSerious/1, numbSymptom/1.

/*Thi is the start function that is called first, then it calls other functions until it terminates*/
start:-
	write('Hello I am the virtual Covid-19 checker!'),
	nl,
	write('Can I have your name please: '),
	getName,
	name(Person),
	nl,
	nl,
	write('Welcome '),
	write(Person),
	write('!'),
	nl, 
	write('Did you have contact with someone that has been sick recently? y or n?'),
	contact,
	(contacts(Y) -> 
		alarm;
		noContacts(Y) -> symptom,
	numbSerious(Xs),
	numbSymptom(Ys),
	
	(Xs = 3 ->
		alert-1;
		(Xs = 2 ->
			alert-2;
			(Xs = 1, Ys >= 3 ->
				alert-2;
				(Xs = 1, Ys < 3 ->
					alert-3;
					(Xs = 0, Ys = 0 -> alert-4;
						(Xs = 0, Ys < 3 ->
							alert-3;
							(Xs = 0, Ys >= 3 ->
								alert-2)))))))),
	nl,
	noContacts(Y)-> medications.

/*Different warning messages depending on how the user answered to the symptom questions*/
alert-1:-
		nl,
		nl,
		write('According to the answers you gave, there is a high probability that you have Covid-19,'),
		nl,
		write('please self-isolate and get tested at the nearest hospital ASAP!!!').

alert-2:-
		nl,
		nl,
		write('According to the answers you gave, there is a chance that you have Covid-19, '),
		nl,
		write('please take medications, monitor your health and get tested at the nearest hospital!!!').

alert-3:-
		nl,
		nl,
		write('According to the answers you gave, there is a small probability that you have Covid-19, '),
		nl,
		write('please take medications for cold and monitor your symptoms in the coming days!!!').

alert-4:-
		nl,
		nl,
		write('According to the answers you gave, the chances are minimal that you have Covid-19, '),
		nl,
		write('please monitor your health and symptoms in the coming days, and stay alert for any major changes on your health!!!').
	
alarm:-
	contacts(X), nl, nl,
	write('Since you had contact with '), write(X),
	write(', he has tested positive and you are strongly adviced'),
	nl,
	write(' to self-isolate and get tested to the nearest hospital ASAP!!!').
	
/*Medication treatment suggestions dependong on which symptoms the user has*/

medications:- first, second.

first:-
	forall(hasitSerious(X),
		(nl, write('For '), write(X), write(' you should take '),
		forall(therapy(X, Y),
			(write(Y), write(', '))),
			write('!'))).

second:-
	forall(hasitSymptom(X),
		(nl, write('For '), write(X), write(' you should take one of the following medications: '),
		forall(therapy(X, Y),
			(write(Y), write(', '))),
			write('!'))).
	
/*this is called for all the available serious symptoms and ask the user if they have that symptom, then stores it in the temporary database*/
checkSerious(X):-
		write('Do you have '), write(X), write('? y or n?'), read(Response),
		(Response == y ->
		asserta(hasitSerious(X));
			asserta(doesnthaveitSerious(X))).
			
/*this is called for all the available mild symptoms and ask the user if they have that symptom, then stores it in the temporary database*/			
checkSymptom(X):-
		write('Do you have '), write(X), write('? y or n?'), read(Response),
		(Response == y ->
		asserta(hasitSymptom(X));
			asserta(doesnthaveitSymptom(X))).

/*calls the functions serious and mild*/		
symptom:- serious, mild.
	
/*count number of serious symptoms*/
countSerious:-
		forall(hasitSerious(X),		/*iterate through all symptoms that user has and save it to the numbSerious assertion*/
		(numbSerious(L),
		retract(numbSerious(L)),
		N is L+1,
		asserta(numbSerious(N)))).
		
/*count number of mild symptoms*/
countSymptom:-
	forall(hasitSymptom(X),
		(numbSymptom(L),
		retract(numbSymptom(L)),
		N is L+1,
		asserta(numbSymptom(N)))).
	
/*ask the user if he/she has the serious symptoms*/
serious:-	
	forall(serious(X),
		checkSerious(X)),
		countSerious.
		
/*ask the user if he/she has the mild symptoms*/	
mild:- forall(symptoms(X),
		checkSymptom(X)),
		countSymptom.
		
/*get the name from input*/
getName:-
	read(Response),
	asserta(name(Response)).

/*ask the user if they had contat with a infected person*/
contact:-
	read(Response),
	(Response == y
	->
		write('Who was the person you had contact with?'),read(X), (infected(X) -> asserta(contacts(X)); 
		nl,
		nl,
		write('It looks like we dont have '), write(X), write(' in our database of infected people!'),nl,
		write('Let me ask you some questions:'),nl,nl, asserta(noContacts(X))) ;
		asserta(noContacts(X))).
