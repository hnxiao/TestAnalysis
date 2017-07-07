(* ::Package:: *)

Clear[TestAnalysis];

TestAnalysis::usage="TestAnalysis[data] returns data for test analysis.";

TestAnalysis[data_List]:=Module[{DAT,DATT,TI,N,AI,ExcellentCriteria,ExcellentNumber,PI,OutstandingGroup,AverageGroup,GI,LI,DI,AverageP,AverageD},
	DAT=data[[1]];
	DATT=Transpose@DAT;

	(*\:5404\:9898\:6ee1\:5206*)
	TI=Flatten@data[[2]];

	(*\:5b66\:751f\:4eba\:6570*)
	N=Length@DAT;

	(*\:5404\:9898\:5e73\:5747\:5206*)
	AI=Divide[#,N]&/@Accumulate[DAT][[N]];
	Print["\:5e73\:5747\:5206(Ai): ",AI];

	(*\:7b54\:5bf9\:5206\:6570*)
	ExcellentCriteria=Times[#,.9]&/@TI;
	(*\:7b54\:5bf9\:4eba\:6570*)
	ExcellentNumber=MapThread[Count[#1,u_/;u>#2]&,{DATT,ExcellentCriteria}];
	Print["\:7b54\:5bf9\:4eba\:6570: ",ExcellentNumber];

	(*\:96be\:5ea6\:7cfb\:6570*)
	PI=MapThread[Divide,{AI,TI}];
	Print["\:96be\:5ea6\:7cfb\:6570(Pi): ",PI];

	(*\:9ad8\:5206\:7ec4\:6210\:7ee9*)
	OutstandingGroup=Take[Sort[DAT,Last[#1]>Last[#2]&],Ceiling[N*.27]];
	(*\:4f4e\:5206\:7ec4\:6210\:7ee9*)
	AverageGroup=Take[Sort[DAT,Last[#1]>Last[#2]&],-Floor[N*.27]];
	(*\:9ad8\:5206\:7ec4\:7b54\:5bf9\:4eba\:6570*)
	GI=MapThread[Count[#1,u_/;u>#2]&,{Transpose@OutstandingGroup,ExcellentCriteria}];
	(*\:4f4e\:5206\:7ec4\:7b54\:5bf9\:4eba\:6570*)
	LI=MapThread[Count[#1,u_/;u>#2]&,{Transpose@AverageGroup,ExcellentCriteria}];
	(*\:533a\:5206\:5ea6*)
	DI=(GI-LI)/(.27*N);
	Print["\:533a\:5206\:5ea6(Di): ",DI];

	(*\:5e73\:5747\:96be\:5ea6\:7cfb\:6570*)
	AverageP=Last[AI]/100;
	Print["\:5e73\:5747\:96be\:5ea6\:7cfb\:6570(PA): ",AverageP];

	(*\:5e73\:5747\:533a\:5206\:5ea6*)
	AverageD=0.01*Accumulate@MapThread[Times,{Drop[TI,-1],Drop[DI,-1]}]//Last;
	Print["\:5e73\:5747\:533a\:5206\:5ea6(DA): ",AverageD];
]

Print["TestAnalysis initialized."]

