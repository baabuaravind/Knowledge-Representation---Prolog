%---------------------------------------------------------------------------------------------------------------------------------
% 		CS 820 - Artificial Intelligence
%-----------------------------------------------------
%                        Assignment 2
%-----------------------------------------------------
% Student Name   : Baabu Aravind Vellaian Selvarajan
% Student Number : 200339484
%-----------------------------------------------------


%------------------------------------------------
% Part 1 : Knowledge repreresentation in PROLOG
%------------------------------------------------


% By personal Characteristics of both male and female : {fe}male_looks(name,height,skincolor,age)

male_looks(paul,tall,brown,old).
male_looks(kevin,average,fair,young).
male_looks(doug,small,brown,old).

female_looks(alice,average,fair,avgage).
female_looks(eva,average,fair,young).
female_looks(lea,small,brown,old).

% By personal interests of both male and female : {fe}male_likes(name,music,reading,sports)

male_likes(paul,classical,adventure,swimming).
male_likes(kevin,rock,science,tennis).
male_likes(doug,jazz,detective,tennis).

female_likes(alice,_,adventure,swimming).
female_likes(eva,rock,science,_).
female_likes(lea,classical,adventure,swimming).

% By qualities in male and female : people_with_quality(name,height,skin color,age)

people_with_quality(paul,tall,_,young).
people_with_quality(kevin,average,fair,young).
people_with_quality(doug,small,fair,avgage).

people_with_quality(alice,tall,brown,avgage).
people_with_quality(eva,average,fair,young).
people_with_quality(lea,average,brown,old).

% By comparing match between both male and female

match_people(X,Y):-male_looks(X,Height,SkinColor,Age),female_looks(Y,_,_,_),people_with_quality(Y,Height,SkinColor,Age).
match_people(X,Y):-male_looks(Y,Height,SkinColor,Age),female_looks(X,_,_,_),people_with_quality(X,Height,SkinColor,Age).
match_tastes(X,Y):-male_likes(X,Music,Read,Sports),female_likes(Y,Music,Read,Sports).
match_tastes(Y,X):-female_likes(Y,Music,Read,Sports),male_likes(X,Music,Read,Sports).
match(X,Y):-match_people(X,Y),match_people(Y,X),match_tastes(X,Y).

%------------------------------------------------------------------------------------------------------------------------------
%------------------------------------------------------------------------------------------------------------------------------

%-------------------------------
% Part 2 : The Analogy Problem
%-------------------------------
% Part 2 - Figure 1 
%--------------------

% Facts for each figure is mentioned as per given in example

figure1(1,center(triangle,square)).
figure1(2,center(circle,triangle)).
figure1(3,center(square,circle)).
figure1(4,center(square,square)).
figure1(5,center(square,triangle)).
figure1(6,center(triangle,circle)).
figure1(7,center(circle,square)).
figure1(8,center(triangle,triangle)).

% about the reverse relation between figures

relations_on_figure1(center(triangle,square),center(square,triangle),invert).
relations_on_figure1(center(circle,triangle),center(triangle,circle),invert).
relations_on_figure1(center(square,circle),center(circle,square),invert).
relations_on_figure1(center(square,square),center(square,square),invert).
relations_on_figure1(center(triangle,triangle),center(triangle,triangle),invert).

% applied conditions and rules - analogy1

analogy1((A,B),(C,X)):-figure1(A,FA),figure1(B,FB),figure1(C,FC),relations_on_figure1(FA,FB,Rel1),relations_on_figure1(FC,FX,Rel1),figure1(X,FX).
analogy1((A,B),(C,X)):-figure1(A,FA),figure1(B,FB),figure1(C,FC),relations_on_figure1(FA,FB,Rel1),relations_on_figure1(FX,FC,Rel1),figure1(X,FX).
analogy1((A,B),(C,X)):-figure1(A,FA),figure1(B,FB),figure1(C,FC),relations_on_figure1(FB,FA,Rel1),relations_on_figure1(FC,FX,Rel1),figure1(X,FX).
analogy1((A,B),(C,X)):-figure1(A,FA),figure1(B,FB),figure1(C,FC),relations_on_figure1(FB,FA,Rel1),relations_on_figure1(FX,FC,Rel1),figure1(X,FX).

%--------------------
% Part 2 - Figure 2
%--------------------

% Facts for each figure is mentioned as per given in example

figure2(1,center(circle)).
figure2(2,left_top(circle)).
figure2(3,right_bot(circle)).
figure2(4,center(square)).
figure2(5,left_top(square)).
figure2(6,right_top(square)).
figure2(7,right_bot(square)).
figure2(8,left_bot(square)).

% About relations between figures 

relations_on_figure2(center(circle),left_top(circle),shiftcentertoleft_top).
relations_on_figure2(center(circle),right_bot(circle),shiftcentertoright_bot).
relations_on_figure2(left_top(circle),right_bot(circle),shiftleft_toptoright_bot).
relations_on_figure2(center(square),left_top(square),shiftcentertoleft_top).
relations_on_figure2(center(square),right_top(square),shiftcentertoright_top).
relations_on_figure2(center(square),right_bot(square),shiftcentertoright_bot).
relations_on_figure2(center(square),left_bot(square),shiftcentertoleft_bot).
relations_on_figure2(left_top(square),right_top(square),shiftleft_toptoright_top).
relations_on_figure2(left_top(square),right_bot(square),shiftleft_toptoright_bot).
relations_on_figure2(left_top(square),left_bot(square),shiftleft_toptoleft_bot).
relations_on_figure2(right_top(square),right_bot(square),shiftright_toptoright_bot).
relations_on_figure2(right_top(square),left_bot(square),shiftright_toptoleft_bot).
relations_on_figure2(right_bot(square),left_bot(square),shiftright_bottoleft_bot).


% applied conditions and rules - analogy2

analogy2((A,B),(C,X)):-figure2(A,FA),figure2(B,FB),figure2(C,FC),relations_on_figure2(FA,FB,Rel2),relations_on_figure2(FC,FX,Rel2),figure2(X,FX).
analogy2((A,B),(C,X)):-figure2(A,FA),figure2(B,FB),figure2(C,FC),relations_on_figure2(FA,FB,Rel2),relations_on_figure2(FX,FC,Rel2),figure2(X,FX).
analogy2((A,B),(C,X)):-figure2(A,FA),figure2(B,FB),figure2(C,FC),relations_on_figure2(FB,FA,Rel2),relations_on_figure2(FX,FC,Rel2),figure2(X,FX).
analogy2((A,B),(C,X)):-figure2(A,FA),figure2(B,FB),figure2(C,FC),relations_on_figure2(FB,FA,Rel2),relations_on_figure2(FC,FX,Rel2),figure2(X,FX).

%------------------------------------------------------------------------------------------------------------------------------
%------------------------------------------------------------------------------------------------------------------------------

%-----------------------------------------
% Part 3 : Prefix and postfix notations
%-----------------------------------------


stack(Top,PostList,[Top|PostList]).

% The preofpost(Original Expression- PostList,Temporary Stack - S ,Output Expression - Res)
% It checks for the items and does the corresponding rule from the 3 rules(integer or atomic)
% It recurresively calls the remaining elements of the stack

preofpost([],TempVar,TempVar).

preofpost([X|PostList],S,Res):-integer(X),stack(X,S,RemainTempStack),preofpost(PostList,RemainTempStack,Res).

preofpost([X|PostList],S,Res):-atomic(X),member(X,[sin,cos,tan,exp,log,sqrt]),stack(Top,StackVariable1,S),stack([X,Top],StackVariable1,RemainTempStack),preofpost(PostList,RemainTempStack,Res).

preofpost([X|PostList],S,Res):-atomic(X),member(X,[+,-,*,/]),stack(Top,StackVariable1,S),stack(TopIndex,StackVariable2,StackVariable1),stack([X,TopIndex,Top],StackVariable2,RemainTempStack),preofpost(PostList,RemainTempStack,Res).

% flatten the resulting list and getting rid of the square brackets

flatten([],[]) :- !.
flatten([Length|IndexLength],Top) :- !,flatten(Length,TempIndex), flatten(IndexLength,Last), append(TempIndex,Last,Top).
flatten(Length,[Length]).

post2pre(PostList,PreList):- preofpost(PostList,[],Res),flatten(Res,PreList).

%------------------------------------------------------------------------------------------------------------------------------
%------------------------------------------------------------------------------------------------------------------------------

%------------------------------------------
% Part 4 : Tree Structures + "Evaluation"
%------------------------------------------

% "Given":
% list of three elements [Left,Test,Right]
% Op is one of "eq", "lt" or "gt"
% Val is the number
% V is the value obtained, X is the value given for evaluation

checkvalue(TempVal,TempVal).

calculateNode(lt,Val,True,False,X,V):-(X < Val,checkvalue(True,V));(X >=  Val,checkvalue(False,V)).
calculateNode(gt,Val,True,False,X,V):-(X > Val,checkvalue(True,V));(X =<  Val,checkvalue(False,V)).
calculateNode(eq,Val,True,False,X,V):-(X=:=Val,checkvalue(True,V));(X =\= Val,checkvalue(False,V)).

calculateSubTree([TempVal1|Op],X,TempVal2):-calculate([TempVal1|Op],X,TempVal2).
calculateSubTree(TempVal1,_,TempVal2):-checkvalue(TempVal1,TempVal2).

calculate([Left,[Op|Op1],Right],X,V):-calculateSubTree(Left,X,True),calculateSubTree(Right,X,False),calculateNode(Op,Op1,True,False,X,V).

evalDT(DT,X,V):-calculate(DT,X,V).

%---------------------------------------------------------------------------------------------------
% References:
% 1. https://www.cs.unm.edu/~luger/ai-final/code/PROLOG.adts.html  (push and pop from stack)
% 2. https://www.cs.unm.edu/~luger/ai-final2/CH3_Abstract%20Data%20Types%20and%20Search.pdf
%---------------------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------------------------------------------------------
%-------------------------------------------------------------------------------------------------------------------------------------
