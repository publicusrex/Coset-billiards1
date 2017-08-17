(* ::Package:: *)

(* ::Title:: *)
(*billiards strategy using basic vector kinetics*)


(* ::Text:: *)
(*	In science or industry you will be tasked with open-ended problems like "understand these phenomena". Let's say you work for a robot-billiards team. Your initial job is to understand the behavior of balls on the pool table (by building a model), and decide what your ideal robot would do, in consultation with the robotics group for a feasibility analysis...*)
(**)
(*[but in school we must skip ahead]*)
(**)
(*	...Soon you realize that pool is a fairly easy game for robots, so you better try to win off the break (i.e., don't let the opposing robot get a chance to play). *)
(*	In this CoSet, you will analyze the break of a game of pool, with the goal of trying and maximize your chances of getting a ball in a pocket.*)


(* ::Section::GrayLevel[1]:: *)
(*Read:: setting up the experimental apparatus / simulation*)


(* ::Text::RGBColor[0.8784696416450558, 0., 0.]:: *)
(*SectionOptions[*)
(*	*)
(*]::*)


(* ::Text:: *)
(*Emphasis[]::*)
(*Sometimes it's best to start with a diagram, so you can visualize your subsequent steps and see that they are correctly modeling your phenomena*)


(* ::Text:: *)
(*Each ball is a Disk[] with unit radius, and we've arranged them as required by the game (*).*)
(*We want the cue ball (ball 1) to be just another ball, except we are allowed to vary its initial position and velocity (by hitting it). These initial states will be represented by parameters v,b,and q.*)
(**)
(** the hexagonal (best possible) packing is required, but in practice there are small gaps*)


(* ::Text::RGBColor[0., 0.31703090891638763`, 0.]:: *)
(*SectionOptions[*)
(*	Storage \[RightArrow] Cloud*)
(*]::*)


v=0;
q=0;
b=0;

pos={
{10 Sin[q]+b Cos[q],-10 Cos[q]+b Sin[q]},
{0,0},
{-1.1,1.1 Sqrt[3]},
{1.1,1.1 Sqrt[3]},
{-2.2,2.2 Sqrt[3]},
{0,2.2 Sqrt[3]},
{2.2,2.2 Sqrt[3]}
};

vel={
{-v Sin[q],v Cos[q]},
{0,0},{0,0},{0,0},
{0,0},{0,0},{0,0}
};

colors={GrayLevel[0.9],RGBColor[0.9,0.9,0],RGBColor[0,0,0.6],RGBColor[0.7,0,0],RGBColor[0.5,0,0.5],RGBColor[0.9,0.3,0],RGBColor[0,0.4,0]};


(* ::Text:: *)
(*The shape of the table is not regulation here, but fairly similar. The "pockets" are represented by a simple red line which, when the center of a ball goes over it, it is considered "in"*)


Graphics[
	{
		Table[Style[Disk[pos[[i]]],colors[[i]]],{i,6+1}], (*balls*)
		Table[Translate[Text[ToString[i]],pos[[i]]],{i,6+1}], (*numbers*)
		Style[Rectangle[{8,-13.1},{12.1,9.1}],GrayLevel[0.8]], (*wall*)
		Style[Line[{{-1,1}+{7,8},{1,-1}+{7,8}}],Red], (*pocket*)
		Style[Line[{{1,1}+{-7,8},{-1,-1}+{-7,8}}],Red]  (*pocket*)
	},
	PlotRange->{{-8,12},{-13,9}},Background->RGBColor[0,0.2,0]
]


(* ::Section::GrayLevel[1]:: *)
(*Problem:: Is there a difference between "contact" vs. a very, very small gap between balls? (after next part: what do you think)*)


(* ::Text::RGBColor[0., 0., 0.47494540352591696`]:: *)
(*SectionOptions[*)
(*	Required \[RightArrow] False*)
(*]::*)


(* ::Section::GrayLevel[1]:: *)
(*Solution:: Is there a difference between "contact" vs. a very, very small gap between balls?*)


(* ::Text::RGBColor[0., 0.3073178857203944, 0.]:: *)
(*SectionOptions[*)
(*	Storage \[RightArrow] Cloud*)
(*]::*)


(* ::Text:: *)
(*The answer is "probably not". *)
(*In reality, there may be different behavior at very close range due to Q.M. (there is no such thing as "contact").*)
(*However this empirically does not seem to matter much.*)
(*In our simulation of balls as solid objects, we do not actually specify what happens upon collision with contacting balls. *)
(*In what follows, you may convince yourself that the limiting behavior as the distance between two balls becomes zero will work, and look realistic. In fact, you can vary the "contact distance" of 2, and see that the behavior is only mildly affected*)


(* ::Section::GrayLevel[1]:: *)
(*Read:: setting up the experimental apparatus / simulation (cont.)*)


(* ::Text::RGBColor[0.3421862652300057, 0., 0.]:: *)
(*SectionOptions[*)
(*	*)
(*]::*)


(* ::Text:: *)
(*Note that the red lines to indicate approximately where the pockets are. *)
(*This is the goal, so I'll make a function to test the positions list for whether any ball-centers have passed the red lines.*)
(*(actually I'm using a rectangle here for the pocket, but it's close to the lines  FIX THIS!!)*)


goalAchievedQ[pos_] := 
(*!VectorQ[pos, #[[2]]<8 || 6.1>Abs[#[[1]]] &]*)
(*For[i=1,i<=Length[pos],i++,
	If[pos[[i]][[2]]<8 || 6.1>Abs[pos[[i]][[1]]],
		True,
		Print["ball "<>ToString[i]<>" is in a pocket!"]; Return[False]
	]
]*)
TrueQ[Scan[ If[pos[[#]][[2]]<8 || 6.1>Abs[pos[[#]][[1]]],
		False,
		Print["ball "<>ToString[#]<>" is in a pocket!!!"]; Return[True]
	]&,
	Range[Length[pos]]
]]


(* ::Input:: *)
(*goalAchievedQ@pos*)


(* ::Input:: *)
(*goalAchievedQ@{{8.955012153947106`,-4.561552073873501`},{0,0},{-33.1`,22.905255888325765`}, {1.1`,1.905255888325765`},{-2.2`,3.81051177665153`},{0,3.81051177665153`},{2.2`,3.81051177665153`}}*)


(* ::Text:: *)
(*Also, if we're done adding to the graphic, lets encapsulate the function.*)
(* Here 'v' (the initial speed), we can show by adding a velocity vector (Arrow[]) to all the cue ball (as well as all the other balls).*)


drawTable[{pos_,vel_},colors_]:=Graphics[
	{
		Table[Style[Disk[pos[[i]]],colors[[i]]],{i,6+1}],
		Table[Translate[Text[ToString[i]],pos[[i]]],{i,6+1}],
		Table[Style[Translate[Arrow[{{0,0},vel[[i]]}],pos[[i]]],Darker[colors[[i]],0.4],Thick],{i,6+1}], (*velocity vectors*)
		Style[Rectangle[{8,-13.1},{12.1,9.1}],GrayLevel[0.8]],
		Style[Line[{{-1,1}+{7,8},{1,-1}+{7,8}}],Red],
		Style[Line[{{1,1}+{-7,8},{-1,-1}+{-7,8}}],Red](*,
		Table[Style[
			Translate[Rectangle[{0,0},{1,(Norm[vel[[i]]]^2)/6}],{9,-12+(Sum[Norm[vel[[j]]]^2,{j,i-1}])/6}],
			Darker[colors[[i]],0.3]
		],{i,6+1}]*)
	},
	PlotRange->{{-8,12},{-13,9}},Background->RGBColor[0,0.2,0]
]


(* ::Input:: *)
(*drawTable[{pos,vel},colors]*)


Manipulate[

	pos={
		{10 Sin[q]+b Cos[q],-10 Cos[q]+b Sin[q]},
		{0,0},
		{-1.1,1.1 Sqrt[3]},
		{1.1,1.1 Sqrt[3]},
		{-2.2,2.2 Sqrt[3]},
		{0,2.2 Sqrt[3]},
		{2.2,2.2 Sqrt[3]}
	};

	vel={
		{-v Sin[q],v Cos[q]},
		{0,0},{0,0},{0,0},
		{0,0},{0,0},{0,0}
	};

	drawTable[{pos,vel},colors]
,

	{q,0,1},
	{b,-1,1},
	{v,0,10}
]


(* ::Section::GrayLevel[1]:: *)
(*Read:: adding dynamics to the simulation*)


(* ::Text::RGBColor[0.5957582943450308, 0., 0.]:: *)
(*SectionOptions[*)
(*	*)
(*]::*)


(* ::Text:: *)
(*Now we need to analyze the dynamics. Fire the cue ball! *)
(**)
(*	We could use a Manipulate or another dynamic or graphical technique, but I'll make an animation, so I can make sure it looks real! The only really tricky part is to say what happens in collisions between balls (collisions with walls are pretty easy: just reverse one of the velocity components). So I'll first try and write a function to update the two velocity vectors in the event of a collision*)


(* ::Text:: *)
(*	Instead of trying to write it from scratch, maybe Solve[] can find a general solution to the problem, if we give it enough equations (we need at least 4 to get the four velocity components out.) *)
(*	At first, it seems like momentum and energy only give us 3 equations, but that's because we haven't used the shape of the balls (non-spheres could bounce off many different ways). Think of the the velocity components along the direction of the line of tangency/contact between the colliding balls. These components don't change, since a horizontal push cannot effect vertical motion (neglecting friction). this tangent vector is the negative reciprocal of the vector (pos1 - pos2). So we find it and write two more equations*)


(* ::Input:: *)
(*collide2[{pos1_,pos2_},{vel1_,vel2_}]:=*)
(*With[{tangvect = Normalize[{-1,1}*Reverse[pos1-pos2]]}, Print[sSolve[*)
(*{*)
(*vel1+vel2 == {newvel1x + newvel2x,newvel1y + newvel2y} (*conservation of momentum - masses are all 1*),*)
(*Norm[vel1]^2/2 +  Norm[vel2]^2/2 == (newvel1x^2 + newvel1y^2)/2 +(newvel2x^2 + newvel2y^2)/2(*conservation of energy*),*)
(**)
(*(*along the tangent line of contact, the speeds of both balls are unchanged*)*)
(*tangvect.vel1 == tangvect.{newvel1x,newvel1x},*)
(*	tangvect.vel2 == tangvect.{newvel2x,newvel2x}*)
(**)
(*},*)
(*{newvel1x,newvel1y,newvel2x,newvel2y}]];*)
(*Solve[*)
(*{*)
(*vel1+vel2 == {newvel1x + newvel2x,newvel1y + newvel2y} (*conservation of momentum - masses are all 1*),*)
(**)
(*Norm[vel1]^2/2 +  Norm[vel2]^2/2 == (newvel1x^2 + newvel1y^2)/2 +(newvel2x^2 + newvel2y^2)/2(*conservation of energy*),*)
(**)
(*(*along the tangent line of contact, the speeds of both balls are unchanged*)*)
(*tangvect.vel1 == tangvect.{newvel1x,newvel1x},*)
(*tangvect.vel2 == tangvect.{newvel2x,newvel2x}*)
(*},*)
(**)
(*{newvel1x,newvel1y,newvel2x,newvel2y}]*)
(*]*)


(* ::Input:: *)
(*collide2[{{p11,p12},{p21,p22}},{{v11,v12},{v21,v22}}]*)


(* ::Input:: *)
(*Reduce[{v11+v21==newvel1x+newvel2x,v12+v22 == newvel1y+newvel2y,1/2 (Abs[v11]^2+Abs[v12]^2)+1/2 (Abs[v21]^2+Abs[v22]^2)==1/2 (newvel1x^2+newvel1y^2)+1/2 (newvel2x^2+newvel2y^2),((-p12+p22) v11)/Sqrt[Abs[p11-p21]^2+Abs[-p12+p22]^2]+((p11-p21) v12)/Sqrt[Abs[p11-p21]^2+Abs[-p12+p22]^2]==(newvel1x (p11-p21))/Sqrt[Abs[p11-p21]^2+Abs[-p12+p22]^2]+(newvel1x (-p12+p22))/Sqrt[Abs[p11-p21]^2+Abs[-p12+p22]^2],((-p12+p22) v21)/Sqrt[Abs[p11-p21]^2+Abs[-p12+p22]^2]+((p11-p21) v22)/Sqrt[Abs[p11-p21]^2+Abs[-p12+p22]^2]==(newvel2x (p11-p21))/Sqrt[Abs[p11-p21]^2+Abs[-p12+p22]^2]+(newvel2x (-p12+p22))/Sqrt[Abs[p11-p21]^2+Abs[-p12+p22]^2]},{newvel1x,newvel1y,newvel2x,newvel2y}]*)


(* ::Input:: *)
(*collide2[{{0,0},{1,0}},{{0,0},{1,-1}}]*)


(* ::Print:: *)
(**)
(*Solve[{{1,-1}=={newvel1x+newvel2x,newvel1y+newvel2y},1==1/2 (newvel1x^2+newvel1y^2)+1/2 (newvel2x^2+newvel2y^2),0==-newvel1x,1==-newvel2x},{newvel1x,newvel1y,newvel2x,newvel2y}] *)


(* ::Section::GrayLevel[1]:: *)
(*Problem:: why are we getting two answers from Solve? and how do we choose the correct one?*)


(* ::Text::RGBColor[0., 0., 0.8864894459422595]:: *)
(*SectionOptions[*)
(*	Required \[RightArrow] True*)
(*]::*)


(* ::Section::GrayLevel[1]:: *)
(*Solution:: why are we getting two answers from Solve? and how do we choose the correct one?*)


(* ::Text::RGBColor[0., 0.4407752384571164, 0.]:: *)
(*SectionOptions[*)
(*	Storage \[RightArrow] Cloud*)
(*]::*)


(* ::Text:: *)
(*When we found the tangent vector, we discarded any info about which ball was on which side. We know only one is possible. *)


(* ::Section::GrayLevel[1]:: *)
(*Read:: *)


(* ::Text:: *)
(*Since Solve did that for us, Here is an equivalent but faster version done by copying the answer from Solve:*)


collide[{pos1_,pos2_},{vel1_,vel2_}]:=
With[{i=1,j=2,poss={pos1,pos2},vels={vel1,vel2}},
(*{
(pos1\[LeftDoubleBracket]2\[RightDoubleBracket]^2 vel1\[LeftDoubleBracket]1\[RightDoubleBracket]+pos2\[LeftDoubleBracket]2\[RightDoubleBracket]^2 vel1\[LeftDoubleBracket]1\[RightDoubleBracket]+(pos1\[LeftDoubleBracket]1\[RightDoubleBracket]-pos2\[LeftDoubleBracket]1\[RightDoubleBracket])^2 vel2\[LeftDoubleBracket]1\[RightDoubleBracket]+pos1\[LeftDoubleBracket]2\[RightDoubleBracket] (-2 pos2\[LeftDoubleBracket]2\[RightDoubleBracket] vel1\[LeftDoubleBracket]1\[RightDoubleBracket]-(pos1\[LeftDoubleBracket]1\[RightDoubleBracket]-pos2\[LeftDoubleBracket]1\[RightDoubleBracket]) (vel1\[LeftDoubleBracket]2\[RightDoubleBracket]-vel2\[LeftDoubleBracket]2\[RightDoubleBracket]))+(pos1\[LeftDoubleBracket]1\[RightDoubleBracket]-pos2\[LeftDoubleBracket]1\[RightDoubleBracket]) pos2\[LeftDoubleBracket]2\[RightDoubleBracket] (vel1\[LeftDoubleBracket]2\[RightDoubleBracket]-vel2\[LeftDoubleBracket]2\[RightDoubleBracket]))/(pos1\[LeftDoubleBracket]1\[RightDoubleBracket]^2+pos1\[LeftDoubleBracket]2\[RightDoubleBracket]^2-2 pos1\[LeftDoubleBracket]1\[RightDoubleBracket] pos2\[LeftDoubleBracket]1\[RightDoubleBracket]+pos2\[LeftDoubleBracket]1\[RightDoubleBracket]^2-2 pos1\[LeftDoubleBracket]2\[RightDoubleBracket] pos2\[LeftDoubleBracket]2\[RightDoubleBracket]+pos2\[LeftDoubleBracket]2\[RightDoubleBracket]^2),
(-pos2\[LeftDoubleBracket]1\[RightDoubleBracket] pos2\[LeftDoubleBracket]2\[RightDoubleBracket] vel1\[LeftDoubleBracket]1\[RightDoubleBracket]+pos1\[LeftDoubleBracket]1\[RightDoubleBracket]^2 vel1\[LeftDoubleBracket]2\[RightDoubleBracket]+pos2\[LeftDoubleBracket]1\[RightDoubleBracket]^2 vel1\[LeftDoubleBracket]2\[RightDoubleBracket]+pos2\[LeftDoubleBracket]1\[RightDoubleBracket] pos2\[LeftDoubleBracket]2\[RightDoubleBracket] vel2\[LeftDoubleBracket]1\[RightDoubleBracket]+pos1\[LeftDoubleBracket]1\[RightDoubleBracket] (-2 pos2\[LeftDoubleBracket]1\[RightDoubleBracket] vel1\[LeftDoubleBracket]2\[RightDoubleBracket]+pos2\[LeftDoubleBracket]2\[RightDoubleBracket] (vel1\[LeftDoubleBracket]1\[RightDoubleBracket]-vel2\[LeftDoubleBracket]1\[RightDoubleBracket])+pos1\[LeftDoubleBracket]2\[RightDoubleBracket] (-vel1\[LeftDoubleBracket]1\[RightDoubleBracket]+vel2\[LeftDoubleBracket]1\[RightDoubleBracket]))+pos1\[LeftDoubleBracket]2\[RightDoubleBracket]^2 vel2\[LeftDoubleBracket]2\[RightDoubleBracket]+pos2\[LeftDoubleBracket]2\[RightDoubleBracket]^2 vel2\[LeftDoubleBracket]2\[RightDoubleBracket]+pos1\[LeftDoubleBracket]2\[RightDoubleBracket] (pos2\[LeftDoubleBracket]1\[RightDoubleBracket] (vel1\[LeftDoubleBracket]1\[RightDoubleBracket]-vel2\[LeftDoubleBracket]1\[RightDoubleBracket])-2 pos2\[LeftDoubleBracket]2\[RightDoubleBracket] vel2\[LeftDoubleBracket]2\[RightDoubleBracket]))/(pos1\[LeftDoubleBracket]1\[RightDoubleBracket]^2+pos1\[LeftDoubleBracket]2\[RightDoubleBracket]^2-2 pos1\[LeftDoubleBracket]1\[RightDoubleBracket] pos2\[LeftDoubleBracket]1\[RightDoubleBracket]+pos2\[LeftDoubleBracket]1\[RightDoubleBracket]^2-2 pos1\[LeftDoubleBracket]2\[RightDoubleBracket] pos2\[LeftDoubleBracket]2\[RightDoubleBracket]+pos2\[LeftDoubleBracket]2\[RightDoubleBracket]^2),
(pos1\[LeftDoubleBracket]1\[RightDoubleBracket]^2 vel1\[LeftDoubleBracket]1\[RightDoubleBracket]+pos2\[LeftDoubleBracket]1\[RightDoubleBracket]^2 vel1\[LeftDoubleBracket]1\[RightDoubleBracket]+(pos1\[LeftDoubleBracket]2\[RightDoubleBracket]-pos2\[LeftDoubleBracket]2\[RightDoubleBracket])^2 vel2\[LeftDoubleBracket]1\[RightDoubleBracket]+pos1\[LeftDoubleBracket]1\[RightDoubleBracket] (-2 pos2\[LeftDoubleBracket]1\[RightDoubleBracket] vel1\[LeftDoubleBracket]1\[RightDoubleBracket]+(pos1\[LeftDoubleBracket]2\[RightDoubleBracket]-pos2\[LeftDoubleBracket]2\[RightDoubleBracket]) (vel1\[LeftDoubleBracket]2\[RightDoubleBracket]-vel2\[LeftDoubleBracket]2\[RightDoubleBracket]))-pos2\[LeftDoubleBracket]1\[RightDoubleBracket] (pos1\[LeftDoubleBracket]2\[RightDoubleBracket]-pos2\[LeftDoubleBracket]2\[RightDoubleBracket]) (vel1\[LeftDoubleBracket]2\[RightDoubleBracket]-vel2\[LeftDoubleBracket]2\[RightDoubleBracket]))/(pos1\[LeftDoubleBracket]1\[RightDoubleBracket]^2+pos1\[LeftDoubleBracket]2\[RightDoubleBracket]^2-2 pos1\[LeftDoubleBracket]1\[RightDoubleBracket] pos2\[LeftDoubleBracket]1\[RightDoubleBracket]+pos2\[LeftDoubleBracket]1\[RightDoubleBracket]^2-2 pos1\[LeftDoubleBracket]2\[RightDoubleBracket] pos2\[LeftDoubleBracket]2\[RightDoubleBracket]+pos2\[LeftDoubleBracket]2\[RightDoubleBracket]^2),
(pos2\[LeftDoubleBracket]1\[RightDoubleBracket] pos2\[LeftDoubleBracket]2\[RightDoubleBracket] vel1\[LeftDoubleBracket]1\[RightDoubleBracket]+pos1\[LeftDoubleBracket]2\[RightDoubleBracket]^2 vel1\[LeftDoubleBracket]2\[RightDoubleBracket]+pos2\[LeftDoubleBracket]2\[RightDoubleBracket]^2 vel1\[LeftDoubleBracket]2\[RightDoubleBracket]-pos2\[LeftDoubleBracket]1\[RightDoubleBracket] pos2\[LeftDoubleBracket]2\[RightDoubleBracket] vel2\[LeftDoubleBracket]1\[RightDoubleBracket]+pos1\[LeftDoubleBracket]2\[RightDoubleBracket] (-2 pos2\[LeftDoubleBracket]2\[RightDoubleBracket] vel1\[LeftDoubleBracket]2\[RightDoubleBracket]+pos2\[LeftDoubleBracket]1\[RightDoubleBracket] (-vel1\[LeftDoubleBracket]1\[RightDoubleBracket]+vel2\[LeftDoubleBracket]1\[RightDoubleBracket]))+pos1\[LeftDoubleBracket]1\[RightDoubleBracket]^2 vel2\[LeftDoubleBracket]2\[RightDoubleBracket]+pos2\[LeftDoubleBracket]1\[RightDoubleBracket]^2 vel2\[LeftDoubleBracket]2\[RightDoubleBracket]+pos1\[LeftDoubleBracket]1\[RightDoubleBracket] (pos1\[LeftDoubleBracket]2\[RightDoubleBracket] (vel1\[LeftDoubleBracket]1\[RightDoubleBracket]-vel2\[LeftDoubleBracket]1\[RightDoubleBracket])+pos2\[LeftDoubleBracket]2\[RightDoubleBracket] (-vel1\[LeftDoubleBracket]1\[RightDoubleBracket]+vel2\[LeftDoubleBracket]1\[RightDoubleBracket])-2 pos2\[LeftDoubleBracket]1\[RightDoubleBracket] vel2\[LeftDoubleBracket]2\[RightDoubleBracket]))/(pos1\[LeftDoubleBracket]1\[RightDoubleBracket]^2+pos1\[LeftDoubleBracket]2\[RightDoubleBracket]^2-2 pos1\[LeftDoubleBracket]1\[RightDoubleBracket] pos2\[LeftDoubleBracket]1\[RightDoubleBracket]+pos2\[LeftDoubleBracket]1\[RightDoubleBracket]^2-2 pos1\[LeftDoubleBracket]2\[RightDoubleBracket] pos2\[LeftDoubleBracket]2\[RightDoubleBracket]+pos2\[LeftDoubleBracket]2\[RightDoubleBracket]^2)
}*){
(poss[[i,2]]^2 vels[[i,1]]+poss[[j,2]]^2 vels[[i,1]]+(poss[[i,1]]-poss[[j,1]])^2 vels[[j,1]]+poss[[i,2]] (-2 poss[[j,2]] vels[[i,1]]-(poss[[i,1]]-poss[[j,1]]) (vels[[i,2]]-vels[[j,2]]))+(poss[[i,1]]-poss[[j,1]]) poss[[j,2]] (vels[[i,2]]-vels[[j,2]]))/(poss[[i,1]]^2+poss[[i,2]]^2-2 poss[[i,1]] poss[[j,1]]+poss[[j,1]]^2-2 poss[[i,2]] poss[[j,2]]+poss[[j,2]]^2),
(-poss[[j,1]] poss[[j,2]] vels[[i,1]]+poss[[i,1]]^2 vels[[i,2]]+poss[[j,1]]^2 vels[[i,2]]+poss[[j,1]] poss[[j,2]] vels[[j,1]]+poss[[i,1]] (-2 poss[[j,1]] vels[[i,2]]+poss[[j,2]] (vels[[i,1]]-vels[[j,1]])+poss[[i,2]] (-vels[[i,1]]+vels[[j,1]]))+poss[[i,2]]^2 vels[[j,2]]+poss[[j,2]]^2 vels[[j,2]]+poss[[i,2]] (poss[[j,1]] (vels[[i,1]]-vels[[j,1]])-2 poss[[j,2]] vels[[j,2]]))/(poss[[i,1]]^2+poss[[i,2]]^2-2 poss[[i,1]] poss[[j,1]]+poss[[j,1]]^2-2 poss[[i,2]] poss[[j,2]]+poss[[j,2]]^2),
(poss[[i,1]]^2 vels[[i,1]]+poss[[j,1]]^2 vels[[i,1]]+(poss[[i,2]]-poss[[j,2]])^2 vels[[j,1]]+poss[[i,1]] (-2 poss[[j,1]] vels[[i,1]]+(poss[[i,2]]-poss[[j,2]]) (vels[[i,2]]-vels[[j,2]]))-poss[[j,1]] (poss[[i,2]]-poss[[j,2]]) (vels[[i,2]]-vels[[j,2]]))/(poss[[i,1]]^2+poss[[i,2]]^2-2 poss[[i,1]] poss[[j,1]]+poss[[j,1]]^2-2 poss[[i,2]] poss[[j,2]]+poss[[j,2]]^2),
(poss[[j,1]] poss[[j,2]] vels[[i,1]]+poss[[i,2]]^2 vels[[i,2]]+poss[[j,2]]^2 vels[[i,2]]-poss[[j,1]] poss[[j,2]] vels[[j,1]]+poss[[i,2]] (-2 poss[[j,2]] vels[[i,2]]+poss[[j,1]] (-vels[[i,1]]+vels[[j,1]]))+poss[[i,1]]^2 vels[[j,2]]+poss[[j,1]]^2 vels[[j,2]]+poss[[i,1]] (poss[[i,2]] (vels[[i,1]]-vels[[j,1]])+poss[[j,2]] (-vels[[i,1]]+vels[[j,1]])-2 poss[[j,1]] vels[[j,2]]))/(poss[[i,1]]^2+poss[[i,2]]^2-2 poss[[i,1]] poss[[j,1]]+poss[[j,1]]^2-2 poss[[i,2]] poss[[j,2]]+poss[[j,2]]^2)
}
];


(* ::Input:: *)
(*collide[{{0,1},{0,2}},{{0,0},{1,-1}}]*)


(* ::Text::RGBColor[0.48006459794424217`, 0., 0.]:: *)
(*SectionOptions[*)
(*	*)
(*]::*)


(* ::Text:: *)
(*Now want to put these collision functions together into a general evolution of the table. A standard "functional" way to do this is to write a function that increments everything one step, and returns the same form as it accepts (the "state variables").*)
(**)
(*For this function onestep[], we also introduce a time step (.03) and boundaries (walls)*)


halfdim={8,12};
center={0,-3};
timeinterval=.03;

onestep[{origpos_, origvel_}]:=
Module[{pos = origpos, vel = origvel}, 

(*if we got one, stop/freeze*)
If[goalAchievedQ[pos], (*Print["yay00"];*)Return[{pos, vel}]];

(*update for collisions*)
Do[If[EuclideanDistance[pos[[i]],pos[[j]]]<2, (*Print[{i,j}];*)
		pos[[i]] = pos[[j]]+(1+Norm[pos[[i]]-pos[[j]]]/2) Normalize[pos[[i]]-pos[[j]]];
		pos[[j]] = pos[[i]]+2 Normalize[pos[[j]]-pos[[i]]];
		{vel[[i,1]],vel[[i,2]],vel[[j,1]],vel[[j,2]]} = collide[{pos[[i]],pos[[j]]},{vel[[i]],vel[[j]]}]
	],
{i,6},{j,i+1,6+1}
];

(*update for wall-collisions*)
Do[If[Abs[pos[[ball,dim]]-center[[dim]]]>halfdim[[dim]]-1,
		pos[[ball,dim]]=center[[dim]]+(halfdim[[dim]]-1) Sign[pos[[ball,dim]]-center[[dim]]];
		vel[[ball,dim]]= -vel[[ball,dim]]
	],
{ball,6+1},{dim,2}
];

(*general Newton's 1st law update*)
Do[pos[[ball]]+=vel[[ball]] timeinterval, {ball,6+1}];

(*print once if we got one!*)
If[goalAchievedQ[pos],Print["yay!"]];

{pos, vel}
]


(* ::Input:: *)
(*(*test with initial values*)*)
(*ListAnimate[drawTable[#,colors]&/@NestList[onestep,{pos, vel},599]]*)


(* ::Section::GrayLevel[1]:: *)
(*Problem:: find parameters which result in getting a ball in a pocket*)


(* ::Text::RGBColor[0., 0., 0.5407740858432025]:: *)
(*SectionOptions[*)
(*	Required \[RightArrow] True*)
(*]::*)


(* ::Section::GrayLevel[1]:: *)
(*Solution:: find parameters which result in getting a ball in a pocket*)


(* ::Text::RGBColor[0., 0.36015799728487574`, 0.]:: *)
(*SectionOptions[*)
(*	Storage \[RightArrow] Cloud*)
(*]::*)


(* ::Text:: *)
(*We can use "brute force" to test "all" the possible breaks we could do (well, at least some fine-grained mesh of possibilities).*)
(*We look for when the animation has "frozen", by checking the last two position sets for equality:*)


v=10;

	pos={
		{10 Sin[q]+b Cos[q],-10 Cos[q]+b Sin[q]},
		{0,0},
		{-1.1,1.1 Sqrt[3]},
		{1.1,1.1 Sqrt[3]},
		{-2.2,2.2 Sqrt[3]},
		{0,2.2 Sqrt[3]},
		{2.2,2.2 Sqrt[3]}
	};

	vel={
		{-v Sin[q],v Cos[q]},
		{0,0},{0,0},{0,0},
		{0,0},{0,0},{0,0}
	};

ArrayPlot[
	Table[
		pos[[1]] = {10 Sin[q]+b Cos[q],-10 Cos[q]+b Sin[q]};
		vel[[1]] = {-v Sin[q],v Cos[q]};
		Boole[If[#,Print[{q,b},{pos, vel}];#,#]&[Equal@@(NestList[onestep,{pos, vel},189][[-2;;]])]],
	{b,-1,1,.1},{q,0,1,.1}]
]



(* ::Text:: *)
(*	Seems like about 7 combinations of parameters worked! However, they are not all in one place. This is a sign of chaotic dynamics (idealized billiards are studied by chaos theorists)*)
(**)
(*lets take the biggest cluster of successes and take a look at the centroid:*)


(* ::Input:: *)
(*ClusteringComponents*)


(* ::Input:: *)
(*Graphics[Raster[{{1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1}, {1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1}, {0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, {1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1}, {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, {1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1}, {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, {1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1}, {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}}, {{0, 0}, {11, 21}}, {0, 1}], Frame -> Automatic, FrameLabel -> {None, None}, FrameTicks -> {{None, None}, {None, None}}, GridLinesStyle -> Directive[GrayLevel[0.5, 0.4]], Method -> {"DefaultBoundaryStyle" -> Automatic, "DefaultPlotStyle" -> Automatic}][[1,1]]*)


(* ::Input:: *)
(*Position[%,0]*)


(* ::Input:: *)
(*{{1,6},{2,5},{2,6},{3,1},{7,3},{16,9},{20,2}}*)


(* ::Input:: *)
(*Table[*)
(*		pos[[1]] = {10 Sin[q]+b Cos[q],-10 Cos[q]+b Sin[q]};*)
(*		vel[[1]] = {-v Sin[q],v Cos[q]};*)
(*		{pos, vel},*)
(*	{b,-1,1,.1},{q,0,1,.1}][[2,6]]*)


(* ::Input:: *)
(*testAnimate[pos_,t_]:=ListAnimate[drawTable[#,colors]&/@NestList[onestep,pos,t]]*)


(* ::Input:: *)
(*testAnimate[{{{-1.`,-10.`},{0,0},{-1.1`,1.905255888325765`},{1.1`,1.905255888325765`},{-2.2`,3.81051177665153`},{0,3.81051177665153`},{2.2`,3.81051177665153`}},{{0.`,10.`},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0}}},199]*)


(* ::Input:: *)
(*{{{4.004431080340694`,-9.20730860364751`},{0,0},{-1.1`,1.905255888325765`},{1.1`,1.905255888325765`},{-2.2`,3.81051177665153`},{0,3.81051177665153`},{2.2`,3.81051177665153`}},{{-7.191383079063045`,13.163738428355591`},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0}}}*)


(* ::Text:: *)
(*Wow! wouldn't it be cool if our robot can consistently do that!*)


(* ::Section::GrayLevel[1]:: *)
(*Problem:: Add friction to the simulation*)


(* ::Text::RGBColor[0., 0., 0.8060059560662053]:: *)
(*SectionOptions[*)
(*	Required \[RightArrow] True*)
(*]::*)


(* ::Section::GrayLevel[1]:: *)
(*Solution:: Add friction to the simulation*)


(* ::Text:: *)
(*neglecting weird effects (masse' is later in semester), there are two types of friction we could add: rolling friction, and energy loss from not-totally elastic collisions.*)


(* ::Text::RGBColor[0., 0.4524097405848069, 0.]:: *)
(*SectionOptions[*)
(*	Storage \[RightArrow] Cloud*)
(*]::*)


(* ::Section::GrayLevel[1]:: *)
(*Problem:: investigate the best course of action if the distances between the balls in the break are a random number from 0 to .1*)


(* ::Text::RGBColor[0., 0., 0.7901833052779211]:: *)
(*SectionOptions[*)
(*	Required \[RightArrow] False*)
(*]::*)
