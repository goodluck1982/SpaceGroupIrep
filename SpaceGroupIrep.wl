(* ::Package:: *)

(* A package for irreducible representations of space group using the conventions in the book of 
   C.J. Bradley and A.P. Cracknell, "The Mathematical Theory of Symmetry in Solids", 
   called "the BC book" afterwards. *)
   
(* Package Name: SpaceGroupIrep *)
(* Author: Gui-Bin Liu *)
(* Package verseion: See the following SpaceGroupIrep`Private`Version *)
(* Mathematica version: >=11.2 *)
(* License: GPLv3 http://www.gnu.org/licenses/gpl-3.0.txt *)

SpaceGroupIrep`Private`Version={1,2,7};  (*Specify version here.*)

With[{p=DirectoryName[$InputFileName]}, If[!MemberQ[$Path,p],AppendTo[$Path, p]]];

BeginPackage["SpaceGroupIrep`", {"AbstractGroupData`", "LittleGroupIrepData`"}]

Unprotect@@Names["SpaceGroupIrep`*"];
ClearAll@@Names["SpaceGroupIrep`*"];

Protect[u,v,w,a,b,c,\[Alpha],\[Beta],\[Gamma],t\:2081,t\:2082,t\:2083];


(* ::Section:: *)
(*Usages of most functions and variables. Another part is in the end of the file.*)


SGSymText::usage="A list of text-version international symbols of space groups.";
SGSymTextBC::usage="A list of text-version space-group international symbols which are consistent "<>
   "with the orientations used in the BC book.";
SGSymStd::usage="SGSymStd[sgno]  returns the international symbol of the space group with No. sgno. "<>
   "If option \"TeX\"->True is used, then the output is in LaTeX format.";
SGSymBC::usage="SGSymBC[sgno]  returns the international symbol of the space group with No. sgno "<>
   "which is consistent with the orientation in the BC book.";
BCOrientation::usage="Orientation[sgno]  returns the orientation used in the BC book for the space group No. sgno.";
SGSymScho::usage="SGSymScho[sgno]  returns the Schoenflies symbol of the space group with No. sgno. "<>
   "Option \"TeX\"->True will output LaTeX format, and option \"full\"->True will output the full symbol "<>
   "with the type of Bravais lattice, such as \!\(\*SubsuperscriptBox[\(\[CapitalGamma]\), \(o\), \(v\)]\)\!\(\*TemplateBox[{\"D\",\"2\",\"8\"},\n\"Subsuperscript\"]\) for SG 23.";
showSGSym::usage="showSGSym[listOrSpan]  returns a table of space group symbols and numbers specified "<>
   "by listOrSpan. If listOrSpan is omitted, all 230 space groups are output. An option \"ncol\" can be "<>
   "used to specify the number of columns in a row. For example: showSGSym[11;;34,\"ncol\"->8]";
PGinfo::usage="PGinfo stores the information of the 32 point groups.";
showPGinfo::usage="showPGinfo[]  shows the information of the 32 point groups. Default options are "<>
   "\"long\"->True, \"color\"->True, and \"double\"->False.";
JonesSymbol::usage="A list for Jones symbol, including rotation names, Jones symbols and rotation matrices.";
getJones::usage="Two usages, e.g:\n getJones[rotName,\"type\"->type] returns Jones symbol when type"<>
   "=\"xyz\" and rotation matrix when type=\"mat\".\n getJones[xyzOrRotMat, p2] return the rotation "<>
   "name of given Jones symbol or rotation matrix xyzOrRotMat and p2 is either \"c\" for cubic or "<>
   "\"h\" for hexagonal or the space group number.";
BravLatt::usage="An association from 1-14 to corresponding Bravais lattices.";
iBravLatt::usage="An association from Bravais lattices to their indices 1-14.";
BravLattSymb::usage="An association from 1-14 to corresponding symbols of Bravais lattices.";
BasicVectors::usage="An association from Bravais lattices to their basic vectors defined in Tab 3.1 of the BC book.";
JonesTM::usage="An association from each Bravais lattice to a transformation matirx which transforms "<>
   "the lattice basic vectors defining Jones' symbol to the ones defined in Tab 3.1 of the BC book.";
getSGLatt::usage="getSGLatt[sgno]  returns the Bravais lattice of space group No. sgno.";
checkBasVec::usage="checkBasVec[brav,basvec] checks whether basvec is the basic vectors of the form in "<>
   "BC Tab.3.1. It also calculates the BZ type. The return value is a list of the form {True,\"a\",{a->2,c->3}}";
RotMat::usage="RotMat[[1]] associates Bravais lattice to their available rotation names.\n"<>
   "RotMat[[2]] associates Bravais lattice to their available rotation matrices.";
checkRotMat::usage="checkRotMat[brav]  shows the result of RotMat applied on basic vectors (t1,t2,t3) "<>
   "in table form like BC Tab. 3.2.";
getRotMat::usage="getRotMat[brav,rotname]  returns the rotation matirx of rotname for Bravais lattice brav.";
getRotName::usage="getRotName[brav,rotmat]  returns the rotation name of the matrix rotmat for Bravais lattice brav.";
getRotMatOfK::usage="getRotMatOfK[brav,rotname]  returns the rotation matirx in reciprocal space "<>
   "(for wave vector k) of rotname for Bravais lattice brav.";
RotMatCart::usage="RotMatCart[rotname]  returns the rotation matrix of rotname in cartesian coordinate system.";
checkRotMatOfK::usage="checkRotMatOfK[brav]  shows the result of rotation matrix in reciprocal space "<>
   "applied on reciprocal basic vectors (g1,g2,g3) in table form like BC Tab. 3.4.";
TMspglibToBC::usage="TMspglibToBC[brav]  gives transformation matrix and rotation matrix "<>
   "{Q,S1} which transforms the idealized standard basic "<>
   "vectors (as',bs',cs') from spglib to the ones (t1_BC2', t2_BC2', t3_BC2') of the second line "<>
   "in BC Tab. 3.7. Note that the vectors here are COLUMN vectors. "<>
   "The relation is:\n  (t1_BC2', t2_BC2', t3_BC2')=S1.(as',bs',cs').Q";
getQandS::usage="getQandS[SGNo]  gives {Q,S} where Q is just TMspglibToBC[SGNo][[1]] and S=S2.S1. "<>
   "Here S2 is a rotation matrix which together with transformation matirx U converts the "<>
   "basic vectors of the second line to the ones of the first line in BC Tab 3.7 and U is "<>
   "given by SGGenElem. The relation is:\n(t1_BC1', t2_BC1', t3_BC1')=S2.(t1_BC2', t2_BC2', t3_BC2').U";
SGGenElem::usage="SGGenElem[SGNo]  gives information of the space group SGNo given in the BC Tab. 3.7 "<>
   "including generating elements of the two lines (if the second line exists) and the orgion shift "<>
   "and transformation matrix U between the two lines. Note that some informations of several space groups"<>
   " are revised which are different from BC Tab. 3.7.";
BCHighSymKpt::usage="BCHighSymKpt is an association. \nKeys are full BZ types such as \"OrthPrim\", \"OrthBase(a)\", .... "<>
   "\nAnd values are informations of the high-symmetry k points given in BC Tab. 3.6.";
showBCHSKpt::usage="showBCHSKpt[fullBZtype]  shows the informations of BCHighSymKpt[fullBZtype] in table form.";
checkBCHSKpt::usage="checkBCHSKpt[fullBZtype]  checks whether informations of BCHighSymKpt satisfy necessary condition.";
kBCcoord::usage="kBCcoord[bravORsgno, kname]  gives the k-point coordinates and corresponding BZs according to kname. "<>
   "bravORsgno can be either the Bravais lattice or the SG number.";
WSCell3DMesh::usage="WSCell3DMesh[recilatt]  returns a BoundaryMeshRegion object whichs is the Wigner-Seitz "<>
   "cell of the reciprocal lattice recilatt. ";
findURange::usage="findURange[fullBZtype, basVec, kname]  retruns the range of u of a high-symmetry k line named "<>
   "kname for fullBZtype with basic vectors basVec.";
showBZDemo::usage="showBZDemo[fullBZtype]  or  showBZDemo[fullBZtype, basVec]\nshows the Brillouin zone of fullBZtype. "<>
   "Basic vectors basVec is optional.";
keqmod::usage="keqmod[k1,k2,prec]  judges whether two wave vectors k1 and k2 are equivalent. The precision prec "<>
   "is optional, which is 1*^-5 by default. The two k points are of fractional coordinations.";
identifyBCHSKpt::usage="identifyBCHSKpt[fullBZtype, kOrKlist]  identifies the k-point name of fullBZtype from its "<>
   "coordinates according to informations of BCHighSymKpt. Note that kOrKlist is numerical and can be coordinates "<>
   "of either a k point or a list of k points. And for k points on some k lines, two entries with different knames "<>
   "may be retruned.";
doubleKuMax::usage="doubleKuMax[fullBZtype]  gives {umax, the lower limit of umax, the upper limit of umax} "<>
   "in which umax is the max u values which make the k points lie within or on the "<>
   "boundary of the BZ for the k points which can be identified as two knames.";
identifyBCHSKptBySG::usage="identifyBCHSKptBySG[sgno, BZtypeOrBasVec, kOrKlist]  identifies the kname of kOrKlist "<>
   "for certain space group sgno. BZtypeOrBasVec is either the type of BZ (\"a\",\"b\",\"c\",\"d\",or just \"\") "<>
   "or the basic vectors for determining the BZtype.  kOrKlist is numerical and can be coordinates of either a k "<>
   "point or a list of k points. Note that if only BZtype is given, the range of u, i.e. (0,umax), may "<>
   "be incorrect for some k lines because in these cases umax depends on lattice constants without which umax can "<>
   "not be determined and is assigned a makeshift value 1/4. But if basic vectors are given then the range of u can "<>
   "be determined correctly. With default option, this function returns one entry of information with only one kname. "<>
   "But if the option \"allowtwok\"->True is used, when only BZtype is given, this function will return two entries of "<>
   "information with different knames in the cases with umax depneding on lattice constants.";
SeitzTimes::usage="Usages:\nSeitzTimes[{R1,v1}, {R2,v2}]    OR\nSeitzTimes[{R1,v1}, ..., {Rn,vn}]    OR\n"<>
                           "SeitzTimes[brav][{Rname1,v1}, {Rname2,v2}]    OR\nSeitzTimes[brav][{Rname1,v1}, ..., {Rnamen,vn}]\n"<>
   "Multiplication of two or more space group operations. R1, R2, and Rn are rotation matrices, while Rname1, Rname2, and "<>
   "Rnamen are the names of the rotations. For the last two usages, the Bravais lattice brav has to be given.";
invSeitz::usage="Two usages:  invSeitz[{R,v}]  or  invSeitz[brav][{Rname,v}]  gives the inverse of the space "<>
   "group operation {R,v} or {Rname,v}.";
powerSeitz::usage="Two usages:  powerSeitz[{R,v},n]  or powerSeitz[brav][{Rname,v},n]  gives {R,v}^n or {Rname,v}^n";
RotTimes::usage="";  (*This usage definition is postponed. But the symbol RotTimes also have to be declared to make it public.*)
powerRot::usage="powerRot[Rname,n]  returns the n-th power of a rotation by its name string.";
invRot::usage="invRot[Rname]  returns the inverse of a rotation by its name string.";
DRotTimes::usage=""; (*Real usage definition is moved to the end.*)
powerDRot::usage="powerDRot[Rname,n]  returns the n-th power of a double-point-group rotation by its name string.";
invDRot::usage="invDRot[Rname]  returns the inverse of a double-point-group rotation by its name string.";
CentExtTimes::usage="CentExtTimes[brav,adict][{R1,alpha},{R2,beta}]  multiplies two elements {R1,alpha} and "<>
   "{R2,beta} of a central extension of little cogroup according to adict which is given by aCentExt. R1 and R2 "<>
   "are either rotation matrices or rotation names.";
CentExtPower::usage="CentExtPower[brav,adict][{R,alpha},n]  gives {R,alpha}^n for a central extension of "<>
   "little cogroup, where R is either a rotation matrix a rotation name.";
getSpinRotOp::usage="getSpinRotOp[Rname]  gives the rotation operation {srot,o3det} of Rname in double space group "<>
   "where srot is the SU(2) matrix and o3det is the determinant of corresponding O(3) matrix of Rname. In fact, "<>
   "getSpinRotOp is an association.";
getSpinRotName::usage="getSpinRotName[brav,{srot, o3det}]  gives the double space group rotation name of {srot, o3det} "<>
   "for Bravais lattice brav. In fact brav here is only used to distinguish hexagonal or trigonal lattices from other "<>
   "lattices. Here srot is SU(2) matrix and o3det is the determinant of corresponding O(3) matrix.";   
SpinRotTimes::usage="Two usages:\nSpinRotTimes[{srot1,o3det1},{srot2,o3det2}]  or  SpinRotTimes[brav][rotname1, rotname2]\n"<>
   "Multiplication between two double space group rotations."; 
DSGSeitzTimes::usage="DSGSeitzTimes[brav][{Rname1,v1}, {Rname2,v2}]    OR\n"<>
                     "DSGSeitzTimes[brav][{Rname1,v1}, ..., {Rnamen,vn}]\n"<>
   "calculates the multiplication of two or more elements of double space group.";
DSGpowerSeitz::usage="DSGpowerSeitz[brav][{Rname,v},n]  gives {Rname,v}^n for double space group.";
DSGinvSeitz::usage="DSGinvSeitz[brav][{Rname,v}]  gives the inversion of double space group operation {Rname,v}."; 
DSGCentExtTimes::usage="DSGCentExtTimes[brav,adict][{Rname1,alpha},{Rname2,beta}]  multiplies two elements {Rname1,alpha} and "<>
   "{Rname2,beta} of a central extension of little cogroup of double space group according to adict which is given by aCentExt "<>
   "with option \"DSG\"->True. ";
DSGCentExtPower::usage="DSGCentExtPower[brav,adict][{Rname,alpha},n]  gives {Rname,alpha}^n for a central extension of "<>
   "little cogroup for double space group.";
rotAxisAngle::usage="rotAxisAngle[O3RotMat]  gives the rotation axis and angle of an O(3) rotation matrix O3RotMat.";
getKStar::usage="getKStar[sgno,kin]  gives the star of kin for space group sgno. kin can be the coordinates or name of the "<>
   "k-point. Option \"cosets\"->True (default: False) will also gives the cosets.";
generateGroup::usage="generateGroup[gens,identityElement,multiply]  gives all the elements of a group according to its "<>
   "generators, identity element, and multiplication. If the option \"generationProcess\"->True is used, the process of "<>
   "generating each element is given in the form of multiplication sequence of generators.";
modone::usage="modone[x]  is my version of Mod[x,1] in which x can be anything. For non-numeric quantity nothing is done.";
seteq::usage="seteq[s1,s2]  judges whether sets s1 is equal to s2.";
getHLGElem::usage="Two usages:  getHLGElem[brav,{m,n},gens]  or  getHLGElem[sgno,kname]\nThe first one gives the Herring "<>
   "little group according to the abstract group G_m^n and generators given in BC Tab. 5.7 or 6.13. Bravais lattice is needed. "<>
   "The second one gives the Herring little group according to sgno and kname directly. "<>
   "For double space group the option \"DSG\"->True is needed.";
getLGElem::usage="getLGElem[sgno,k]  gives the little group of k for space gorup sgno. In fact this is only the coset "<>
   "representatives with respect to the translation group. k can be either its name or coordinates. For double space group the "<>
   "option \"DSG\"->True is needed."
getSGElem::usage="getSGElem[sgno]  gives the elements of space group sgno. This is equivalent to getLGElem[sgno,\"\[CapitalGamma]\"]. "<>
   "In fact, it gives the coset representatives with respect to the translation group. For double space group, the option "<>
   "\"DSG\"->True is needed.";
aCentExt::usage="Three usages:\naCentExt[sgno,kname,BZtype]  or  aCentExt[sgno,kname]  or  aCentExt[brav,Gk,k]\n"<>
   "This function gives the a(Hj,Hk) in the BC eq.(3.7.27) for central extension of little cogroup. kname is a string "<>
   "and k is coordinates. Gk is little group. If BZtype is not given, it is \"a\" by default. For double space group the "<>
   "option \"DSG\"->True is needed."
getCentExt::usage="getCentExt[sgno,kname]  returns the central extension of little cogroup of kname for space group sgno. "<>
   "For double space group the option \"DSG\"->True is needed.";
getSGElemAndSQ::usage="getSGElemAndSQ[sgno]  returns three sets {G,S,Q} in which G is all coset representatives of the "<>
   "space group G with respect to its translation subgroup, S consists of g elements which satisfy g.G.g^-1==G, and Q "<>
   "is used for debug.";
detPointGroup::usage="detPointGroup[rots]  determinates the name of a point group according to the set of rotations rots "<>
   "which can be either set of rotation matrices and rotation names.";
reduceRep::usage="reduceRep[{m,n},chars]  gives the number of times of each irreducible representation occuring in the "<>
   "characters chars of the abstract group G_m^n. chars is a list of characters for each class of G_m^n in the same order "<>
   "of the character table in the BC Tab. 5.1.";
calcRep::usage="calcRep[sgno,kinfo]  is used to get the irep infos for \"GP\" and \"UN\" kpoints which are not given in the "<>
   "BC book. For the kpoints whose irep infos are given in the BC book (i.e. BC Tab. 5.7 and 6.13), this function is only "<>
   "used for checking purpose.";
str2Mulliken::usage=""; (*Real usage definition is moved to the end.*)
Mulliken2str::usage=""; (*Real usage definition is moved to the end.*)
str2GammaLabel::usage=""; (*Real usage definition is moved to the end.*)
GammaLabel2str::usage=""; (*Real usage definition is moved to the end.*)
LGIrepLabel::usage="LGIrepLabel[{m,n}]  gives the informations of BC Tab. 5.8.";
DLGIrepLabel::usage="DLGIrepLabel[{m,n}]  gives the informations of BC Tab. 6.14.";
showRepLabel::usage="showRepLabel[{m,n},dsgtag]  gives the table like BC Tab. 5.8 (for dsgtag=\"s\" which is default) or Tab. 6.14 "<>
   "(for dsgtag=\"d\" or \"D\") for checking purpose.";
showLGIrepLabel::usage="showLGIrepLabel[{m,n}]  gives the table like BC Tab. 5.8, which is equivalent to showRepLabel[{m,n},\"s\"]";
showDLGIrepLabel::usage="showDLGIrepLabel[{m,n}]  gives the table like BC Tab. 6.14, which is equivalent to showRepLabel[{m,n},\"d\"]";
formatRepMat::usage="formatRepMat[mat]  is used to format the matrix elements of mat.";
mapLGIrepLabel::usage="mapLGIrepLabel[sgno,kname]  gives the correspondence/mapping between the abstract-group irep label, "<>
   "the extended Mulliken label, and the Gamma label for the LG ireps of kname. If kname is not designated, all k-points of the "<>
   "space group sgno are used. Option \"DSG\"->True can be used for double-valued ireps.";
getPGElem::usage="getPGElem[pg]  gives the elements of the point group pg. Default option is \"double\"->False.";
getPGCharTab::usage="getPGCharTab[pg]  gives the character table of point group pg. pg can be the sequence number or the name string "<>
   "of a point group, e.g. 14, or \"D2d\", or \"-42m\". Full list can be obtained by showPGinfo[], or by triggering a tip via a wrong "<>
   "input such as getPGCharTab[0]. When the option \"double\"->True is used, the character table of the corresponding double "<>
   "point group is given. For single-valued ireps, both the Mulliken labels and Gamma labels are consitent with the ones "<>
   "in the Table 2.2 in the BC book, while the double-valued irep labels are consistent with those in the table 6.5 in the BC book.";
showPGCharTab::usage="showPGCharTab[pg]  shows the character table of point group pg in a user-friendly table form. Default "<>
   "options are \"double\"->False, \"mode\"->4, \"class\"->Automatic, \"elem\"->All, \"irep\"->All, and \"linewidth\"->2. "<>
   "For available values of the input argument pg and the options \"mode\", \"class\", \"elem\", and \"irep\", tips will be "<>
   "triggered by a wrong input, e.g. \"class\"->0.";
getPGIrepTab::usage="getPGIrepTab[pg]  gives the table of irep matrices for point group pg. Default options "<>
   "are \"double\"->False and \"trace\"->False. For available values of the input argument pg and the option \"double\" "<>
   "tips will be triggered by a wrong input, e.g. \"double\"->0. In this function, \"double\" can be True, False, and Full.";
showPGIrepTab::usage="showPGIrepTab[pg]  shows the table of irep matrices for point group pg in a user-friendly table form. "<>
   "Default options are \"double\"->True, \"rotmat\"->True, \"elem\"->All, \"irep\"->All, \"trace\"->False, \"spin\"->"<>
   "\"downup\", \"cartesian\"->False, and \"linewidth\"->2. For available values of the input argument pg and the options "<>
   "\"double\", \"elem\", and \"irep\", tips will be triggered by a wrong input, e.g. \"double\"->0. In this function, "<>
   "\"double\" can be True, False, and Full.";
PGIrepDirectProduct::usage="PGIrepDirectProduct[pg, ireps1, ireps2]  gives the direct products between ireps1 and "<>
   "ireps2 for point group pg. ireps1 and ireps2 are both optional. If ireps2 is omitted, it takes the same value "<>
   "as ireps1, and if both of them are omitted, they both take the vaule All. The output styles 1-4 can be controlled "<>
   "by the option \"output\" with default value 1. For available values of the input arguments pg, ireps1, and ireps2, "<>
   "and the option \"output\", tips will be triggered by a wrong input, e.g. \"output\"->0. Note that both single-valued "<>
   "and double-valued ireps are calculated.";
showPGIrepDirectProduct::usage="showPGIrepDirectProduct[pg, ireps1, ireps2]  shows the direct products between ireps1 and "<>
   "ireps2 for point group pg in a user-friendly table form. ireps1 and ireps2 are both optional. If ireps2 is omitted, "<>
   "it takes the same value as ireps1, and if both of them are omitted, they both take the vaule All. Default options "<>
   "are \"label\"->1 (1 for Mulliken labels and 2 for Gamma labels), \"double\"->True, \"linewidth\"->2, and \"emph\"->None. "<>
   "For available values of the input arguments pg, ireps1, and ireps2, and the option \"emph\", tips will be triggered "<>
   "by a wrong input, e.g. \"emph\"->0. Note that both single-valued and double-valued ireps are shown by default, and "<>
   "only single-valued ireps are shown when \"double\"->False is used.";
getLGIrepTab::usage="getLGIrepTab[sgno, k]  gives the data for showing the irep table of the little group of k for "<>
   "space group sgno. k can be either its name or coordinates. Option \"abcOrBasVec\"->None is default, and if "<>
   "None is replaced by the basic vectors, specific BZ type is selected.";
getLGCharTab::usage="getLGCharTab[sgno, k]  gives the character table of the k little group of space group sgno, "<>
   "other infomation is the same as getLGIrepTab[sgno, k].";
LGIRtwokRelation::usage="LGIRtwokRelation[repinfos]  or  LGIRtwokRelation[sgno,k]   gives the correspondence of "<>
   "the LGIR labels between two knames if repinfos=getLGCharTab[sgno,k] has two items. The returned value is an "<>
   "association.";
checkLGIrep::usage="checkLGIrep[repinfo]  checks whether the representation matrices in the result of getLGIrepTab "<>
   "satisfy correct multiplications for LG IR. repinfo is the returned value of getLGIrepTab. Things are all right "<>
   "if all the returned numbers are zero."
getRepMat::usage="getRepMat[k,Gk,rep][RvOrRvList]  get the representation matrix(es) (or character(s)) of the "<>
   "element or list of elements RvOrRvList. k is the k-point coordinates. Gk is the list of LG elements. rep is "<>
   "the representation matrices (or characters) of Gk. rep can be for one representation or a list of representations.";
getLGIrepMat::usage="getLGIrepMat[repinfo,IRids][RvOrRvList]  get the representation matrix(es) (or character(s)) "<>
   "of the element or list of elements RvOrRvList "<>
   "according to repinfo which returned by getLGIrepTab (or getLGCharTab). repinfo can be the List of Association "<>
   "returned by getLGIrepTab (or getLGCharTab) (in this case the first Association is used) or one Association in "<>
   "the List. IRids indicates the index(es) of the requested representations, such as 2 or {2,3,4}. IRids is optional, "<>
   "and if it is omitted all representations are processed. An option \"uNumeric\" is available which is False by default.";
showRot::usage="shotRot[rotName]  shows the symbol of the rotName string. Similar to showSeitz but only for the rotation part.";
showSeitz::usage="showSeitz[{Rname,v}]  shows the Seitz symbol of {Rname,v}. Options: \"format\" can be \"std\""<>
   "(default), \"simple\", or \"TeX\"; \"fullbar\" is True by default.";
showLGIrepTab::usage="showLGIrepTab[sgno, k]  shows the table of ireps of k little group of space group sgno in "<>
   "table form. Default options of this function are \"uNumeric\"->False, \"irep\"->All, \"elem\"->All, "<>
   "\"rotmat\"->True, \"trace\"->False, \"spin\"->\"downup\", \"abcOrBasVec\"->None, and \"linewidth\"->2.";
showLGCharTab::usage="showLGCharTab[sgno, k]  shows the character table of k little group of space group sgno in "<>
   "table form. Default options of this function are \"uNumeric\"->False, \"irep\"->All, \"elem\"->All, "<>
   "\"rotmat\"->True, \"spin\"->\"downup\", \"abcOrBasVec\"->None, and \"linewidth\"->2.";
getSGIrepTab::usage="getSGIrepTab[sgno, k]  gives the space group ireps of k star for space group sgno. "<>
   "k can be either its name or coordinates. Option \"abcOrBasVec\"->None is default, and if "<>
   "None is replaced by the basic vectors, specific BZ type is selected.";
showSGIrepTab::usage="showSGIrepTab[sgno, k]  shows the space group ireps of k star for space group sgno in "<>
   "table form. Default options of this function are \"uNumeric\"->False, \"irep\"->All, \"elem\"->All, "<>
   "\"rotmat\"->True, \"maxDim\"->4, \"trace\"->False, \"spin\"->\"downup\", \"abcOrBasVec\"->None, and \"linewidth\"->2.";
getFullRepMat::usage="getFullRepMat[G,rep][RvOrRvList]  get the SG IR matrix(es) (or character(s), if "<>
   "option \"trace\"->True is used) of the element or list of elements RvOrRvList. G is the list of SG elements. rep is "<>
   "the representation matrices of G. rep can be for one representation or a list of representations. This function also "<>
   "works for magnetic space group and corepresentations.";
getSGIrepMat::usage="getSGIrepMat[repinfo,IRids][RvOrRvList]  get the representation matrix(es) (or character(s)) "<>
   "of the element or list of elements RvOrRvList according to repinfo which returned by getSGIrepTab. repinfo can be "<>
   "the List of Association returned by getSGIrepTab (in this case the first Association is used) or one Association in "<>
   "the List. IRids indicates the index(es) of the requested representations, such as 2 or {2,3,4}. IRids is optional, "<>
   "and if it is omitted all representations are processed. Options \"uNumeric\" and \"trace\" are available which "<>
   "are both False by default.";
checkSGIrep::usage="checkSGIrep[repinfo]  checks whether the representation matrices in the result of getSGIrepTab "<>
   "satisfy correct multiplications for SG IR. repinfo is the returned value of getSGIrepTab. Things are all right "<>
   "if all the returned numbers are zero.";
generateLibLGIrep::usage="generateLibLGIrep[filename]  is used to generate a MX file which stores LG IRs "<>
  "for all named k's. The argument filename is optional and is \"libLGIrep.mx\" (under Directory[]) by default.";
SGIrepDirectProduct::usage="SGIrepDirectProduct[sgno, kin1, kin2]  calculates the decomposition of the "<>
   "direct product of the SG ireps of kin1 star and SG ireps of kin2 star. The input k-point (kin1 or kin2) can "<>
   "be either its name (only for high-symmetry k-points not k-lines) or its numeric coordinates. Option \"abcOrBasVec\"->None "<>
   "is default, and if None is replaced by the basic vectors, specific BZ type is selected.";
showSGIrepDirectProduct::usage="showSGIrepDirectProduct[sgno, kin1, kin2]  shows the decomposition of the "<>
   "direct product of the SG ireps of kin1 star and SG ireps of kin2 star. The input k-point (kin1 or kin2) can "<>
   "be either its name (only for high-symmetry k-points not k-lines) or its numeric coordinates. Default options are "<>
   "\"label\"->1, \"abcOrBasVec\"->None, \"linewidth\"->2.";
readVasp2trace::usage="readVasp2trace[filename]  reads the trace.txt file generated by vasp2trace. filename is "<>
   "the path to trace.txt file.";
getBandRep::usage="Three usages:\ngetBandRep[sgno, BZtypeOrBasVec, traceData, ikOrListOrSpan, ibOrListOrSpan]\n"<>
   "getBandRep[sgno, BZtypeOrBasVec, traceData, ikOrListOrSpan]\ngetBandRep[sgno, BZtypeOrBasVec, traceData]\n"<>
   "This function gives the little-group ireps of the Bloch states for the k points specified by ikOrListOrSpan "<>
   "and bands specified by ibOrListOrSpan. If ikOrListOrSpan and ibOrListOrSpan are not specified, ireps of "<>
   "all k points and all bands are given. traceData is the result of readVasp2trace. Note that if the trace.txt "<>
   "file is generated using a BC standard cell the traceData can be directly used by getBandRep; but if the cell "<>
   "is not of BC standard then the traceData has to be converted to BC standard by convTraceToBC before being "<>
   "used by getBandRep. BZtypeOrBasVec can be BZtype (\"a\", \"b\", \"c\", \"d\", \"\") or basic vectors and if "<>
   "it is basic vectors then the names of some high-symmetry k lines can be identified correctly.";
showBandRep::usage="showBandRep[rep, ik, ibOrListOrSpan]  shows the small reps (LG IRs) at the ik-th k-point for the bands "<>
  "specified by ibOrListOrSpan. If ibOrListOrSpan is omitted, all bands at the k-point are shown. rep is the returned "<>
  "value of getBandRep[sgno, BZtypeOrBasVec, tr]. Default option is \"bottomUp\"->True which means that the band energy "<>
  "increases from the bottom up in the table shown."
convTraceToBC::usage="convTraceToBC[sgno,traceData,P,p0,stdR]  converts traceData from non-BC standard to BC "<>
   "standard for getBandRep to determinate little group ireps. P, p0, and stdR are respectively "<>
   "dataset['transformation_matrix'], dataset['origin_shift'] and dataset['std_rotation_matrix'] "<>
   "from spglib acting on the initial cell. For non-SOC case stdR is not needed but for SOC case "<>
   "stdR has to be given.";
readPOSCAR::usage="readPOSCAR[filename]  reads the VASP structure file POSCAR.";
spglibGetSym::usage="spglibGetSym[{basVec,pos,atnum}]  calls external python package spglib to "<>
   "give symmetry data for cell {basVec,pos,atnum}, including P, p0, and stdR used by convTraceToBC. "<>
   "basVec is basic vectors of the cell, pos is a list of fractional coordinates of all atoms "<>
   "in the cell, and atnum is a list of atomic numbers for all atoms.";
autoConvTraceToBC::usage="autoConvTraceToBC[poscarFile,traceData,prec]  converts trace data to "<>
   "BC standard automatically according to POSCAR file and corresponding traceData. prec is optional "<>
   "and is 1*^-5 by default. This function calls spglibGetSym and hence needs external python package spglib. "<>
   "Note that the output is different if the option \"cellData\"->True is used, and in this case it is output[\"trace\"] "<>
   "that has the same data structure with traceData and should be used in getBandRep function.";
allBCSkLGdat::usage="allBCSkLGdat contains all little-groups ireps data of all space groups under BCS convention.";
kptBCStoBC::usage="kptBCStoBC[sgno, BZtype]  gives correspondence between k points of BCS convention "<>
   "and BC convention. BZtype is optional and is \"a\" by default.";
showKptBCStoBC::usage="showKptBCStoBC[sgno, BZtype]  shows correspondence between k points of BCS convention "<>
   "and BC convention in talbe form. BZtype is optional and is \"a\" by default.";
buildTr4BCSrep::usage="buildTr4BCSrep[sgno, BZtype]  builds traceData from BCS ireps data and converts them "<>
   "to BC convention which can be used by getBandRep. For double space group the option \"DSG\"->True is needed.";
krepBCStoBC::usage="krepBCStoBC[sgno, BZtype]  gives correspondence between little-gorup ireps of BCS convention "<>
   "and BC convention. BZtype is optional and is \"a\" by default.  For double space group the option \"DSG\"->True is needed.";
showKrepBCStoBC::usage="showKrepBCStoBC[sgno, BZtype]  shows correspondence between little-gorup ireps of BCS convention "<>
   "and BC convention in talbe form. BZtype is optional and is \"a\" by default. For double space group the option \"DSG\"->True is needed."


Begin["`Private`"]


(* ::Section:: *)
(*Space group symbols*)


SGSymText={"P1", "P-1", "P2", "P21", "C2", "Pm", "Pc", "Cm", "Cc", "P2/m", 
   "P21/m", "C2/m", "P2/c", "P21/c", "C2/c", "P222", "P2221", "P21212", "P212121", "C2221",
   "C222", "F222", "I222", "I212121", "Pmm2", "Pmc21", "Pcc2", "Pma2", "Pca21", "Pnc2", 
   "Pmn21", "Pba2", "Pna21", "Pnn2", "Cmm2", "Cmc21", "Ccc2", "Amm2", "Aem2", "Ama2", 
   "Aea2", "Fmm2", "Fdd2", "Imm2", "Iba2", "Ima2", "Pmmm", "Pnnn", "Pccm", "Pban", 
   "Pmma", "Pnna", "Pmna", "Pcca", "Pbam", "Pccn", "Pbcm", "Pnnm", "Pmmn", "Pbcn", 
   "Pbca", "Pnma", "Cmcm", "Cmce", "Cmmm", "Cccm", "Cmme", "Ccce", "Fmmm", "Fddd", 
   "Immm", "Ibam", "Ibca", "Imma", "P4", "P41", "P42", "P43", "I4", "I41", 
   "P-4", "I-4", "P4/m", "P42/m", "P4/n", "P42/n", "I4/m", "I41/a", "P422", "P4212", 
   "P4122", "P41212", "P4222", "P42212", "P4322", "P43212", "I422", "I4122", "P4mm", "P4bm", 
   "P42cm", "P42nm", "P4cc", "P4nc", "P42mc", "P42bc", "I4mm", "I4cm", "I41md", "I41cd", 
   "P-42m", "P-42c", "P-421m", "P-421c", "P-4m2", "P-4c2", "P-4b2", "P-4n2", "I-4m2", "I-4c2", 
   "I-42m", "I-42d", "P4/mmm", "P4/mcc", "P4/nbm", "P4/nnc", "P4/mbm", "P4/mnc", "P4/nmm", "P4/ncc", 
   "P42/mmc", "P42/mcm", "P42/nbc", "P42/nnm", "P42/mbc", "P42/mnm", "P42/nmc", "P42/ncm", "I4/mmm", "I4/mcm", 
   "I41/amd", "I41/acd", "P3", "P31", "P32", "R3", "P-3", "R-3", "P312", "P321", 
   "P3112", "P3121", "P3212", "P3221", "R32", "P3m1", "P31m", "P3c1", "P31c", "R3m", 
   "R3c", "P-31m", "P-31c", "P-3m1", "P-3c1", "R-3m", "R-3c", "P6", "P61", "P65", 
   "P62", "P64", "P63", "P-6", "P6/m", "P63/m", "P622", "P6122", "P6522", "P6222", 
   "P6422", "P6322", "P6mm", "P6cc", "P63cm", "P63mc", "P-6m2", "P-6c2", "P-62m", "P-62c", 
   "P6/mmm", "P6/mcc", "P63/mcm", "P63/mmc", "P23", "F23", "I23", "P213", "I213", "Pm-3", 
   "Pn-3", "Fm-3", "Fd-3", "Im-3", "Pa-3", "Ia-3", "P432", "P4232", "F432", "F4132", 
   "I432", "P4332", "P4132", "I4132", "P-43m", "F-43m", "I-43m", "P-43n", "F-43c", "I-43d", 
   "Pm-3m", "Pn-3n", "Pm-3n", "Pn-3m", "Fm-3m", "Fm-3c", "Fd-3m", "Fd-3c", "Im-3m", "Ia-3d"};
 
SGSymTextBC=SGSymText;
SGSymTextBC[[{5,7,8,9,12,13,14,15}]]={"B2","Pb","Bm","Bb","B2/m","P2/b","P21/b","B2/b"};
SGSymTextBC[[{17,19,28,29,31,33,36,38,39,40,41,46,51,52,53,54,57,60,61,70,122}]]={
  "P2221", "P212121", "Pbm2", "Pbc21", "Pnm21", "Pbn21", "Ccm21", "Cm2m", "Cm2e", "Cc2m", 
  "Cc2e", "Ibm2", "Pcmm", "Pnan", "Pnmb", "Pcaa", "Pbma", "Pcnb", "Pcab", "Fddd", "I-42d"};
(* Note that for 17,19,70,122, the SG symbols do not change. In fact, these is no need to
   change the orientation and only change of origin is needed for these four SGs. *)
  
Options[SGSymStd]={"TeX"->False, "BC"->False};
SGSymStd[sgno_Integer, OptionsPattern[]]/;1<=sgno<=230:=
 Module[{symtext,chs,c,L,s1,s2,s3,j,tmp,bar,sub,tex},
  tex=(OptionValue["TeX"]===True);
  sub[s1_,s2_]:=If[tex, s1<>"_"<>s2, Subscript[s1,s2]];
  bar[s1_]:=If[tex, "\\bar{"<>s1<>"}", OverBar[s1]];
  symtext=If[OptionValue["BC"], SGSymTextBC[[sgno]], SGSymText[[sgno]]];
  chs=Characters[symtext];   L=chs[[1]];  chs=chs[[2;;]];
  c=If[chs[[1]]!="-",chs[[1]],chs[[2]]];
  tmp=Position[chs,"/"];  If[tmp!={},tmp=tmp[[1,1]]];
  If[tmp=!={},
    If[tmp==2,s1=StringJoin@@chs[[;;tmp+1]],
      s1=If[tex,StringJoin,Row][{sub[chs[[1]],chs[[2]]],"/",chs[[4]]}]
    ];  
    If[Length[chs]>tmp+1, {s2,s3}=chs[[tmp+2;;]], s2=s3=""],
    (*--------else, no / ---------*)
    If[chs[[1]]=="-", 
      s1=bar[chs[[2]]]; 
      Switch[Length[chs[[3;;]]],   0, s2=s3="",   1, s2=chs[[3]];s3="",
             2, s2=chs[[3]];s3=chs[[4]],  3, s2=sub[chs[[3]],chs[[4]]];s3=chs[[5]]],
      (*------else, chs[[1]]\[NotEqual]"-" --------*)
      Switch[Length[chs],
        1, s1=chs[[1]]; s2=s3="",
        2, If[L=="R"||chs=={"2","3"},  s1=chs[[1]];s2=chs[[2]];s3="",
              s1=sub[chs[[1]],chs[[2]]];s2=s3="" ],
        3, If[chs=={"2","1","3"}, s1=sub[chs[[1]],chs[[2]]]; s2=chs[[3]]; s3="",
            If[chs[[2]]!="-", {s1,s2,s3}=chs,
              s1=chs[[1]]; s2=bar[chs[[3]]]; s3=""]],
        4, If[chs[[1]]=="3"||chs[[1]]=="6", s1=sub[chs[[1]],chs[[2]]]; {s2,s3}=chs[[3;;]],
             If[chs[[2]]=="-", s1=chs[[1]]; s2=bar[chs[[3]]]; s3=chs[[4]],
               tmp=Position[chs,"1"];
               If[tmp=={}, s1=sub[chs[[1]],chs[[2]]]; {s2,s3}=chs[[3;;]],
                  Switch[tmp[[1,1]],
                    2, s1=sub[chs[[1]],chs[[2]]]; {s2,s3}=chs[[3;;]],
                    3, s1=chs[[1]]; s2=sub[chs[[2]],chs[[3]]]; s3=chs[[4]],
                    4, s1=chs[[1]]; s2=chs[[2]]; s3=sub[chs[[3]],chs[[4]]]
                  ]; 
               ]
             ]],
         5, s1=sub[chs[[1]],chs[[2]]]; s2=sub[chs[[3]],chs[[4]]]; s3=chs[[5]],   
         6, s1=sub[chs[[1]],chs[[2]]]; s2=sub[chs[[3]],chs[[4]]]; s3=sub[chs[[5]],chs[[6]]]   
       ]      
    ]
  ];
  If[tex, StringJoin[{"$",L,s1,s2,s3,"$"}], Row[{L,s1,s2,s3}]]
]
  
Options[SGSymBC]={"TeX"->False};
SGSymBC[sgno_Integer, OptionsPattern[]]:=SGSymStd[sgno,"TeX"->OptionValue["TeX"],"BC"->True]

(*Note that in the BC book, t1=-b and t2=a for MonoPrim and OrthPrim.
  But here the a,b,c represent the conventional basic vectors in ITA. *)
BCOrientation[sgno_Integer]/;1<=sgno<=230:=Which[
    MemberQ[{17,19,28,29,31,33,53,61,36,46,70,122},sgno], {"b\!\(\*OverscriptBox[\(a\), \(_\)]\)c",{b,-a,c}},
    MemberQ[{38,39,40,41,57},sgno], {"bca",{b,c,a}},
    MemberQ[{51,54},sgno], {"\!\(\*OverscriptBox[\(c\), \(_\)]\)ba",{-c,b,a}},
    MemberQ[{52,60},sgno]||3<=sgno<=15, {"a\!\(\*OverscriptBox[\(c\), \(_\)]\)b",{a,-c,b}},
    True, {"abc",{a,b,c}}
]

SGSymSchoData=<|1->{"C1",1},2->{"Ci",1},3->{"C2",1},4->{"C2",2},5->{"C2",3},6->{"Cs",1},7->{"Cs",2},
  8->{"Cs",3},9->{"Cs",4},10->{"C2h",1},11->{"C2h",2},12->{"C2h",3},13->{"C2h",4},14->{"C2h",5},15->{"C2h",6},
  16->{"D2",1},17->{"D2",2},18->{"D2",3},19->{"D2",4},20->{"D2",5},21->{"D2",6},22->{"D2",7},23->{"D2",8},
  24->{"D2",9},25->{"C2v",1},26->{"C2v",2},27->{"C2v",3},28->{"C2v",4},29->{"C2v",5},30->{"C2v",6},
  31->{"C2v",7},32->{"C2v",8},33->{"C2v",9},34->{"C2v",10},35->{"C2v",11},36->{"C2v",12},37->{"C2v",13},
  38->{"C2v",14},39->{"C2v",15},40->{"C2v",16},41->{"C2v",17},42->{"C2v",18},43->{"C2v",19},44->{"C2v",20},
  45->{"C2v",21},46->{"C2v",22},47->{"D2h",1},48->{"D2h",2},49->{"D2h",3},50->{"D2h",4},51->{"D2h",5},
  52->{"D2h",6},53->{"D2h",7},54->{"D2h",8},55->{"D2h",9},56->{"D2h",10},57->{"D2h",11},58->{"D2h",12},
  59->{"D2h",13},60->{"D2h",14},61->{"D2h",15},62->{"D2h",16},63->{"D2h",17},64->{"D2h",18},65->{"D2h",19},
  66->{"D2h",20},67->{"D2h",21},68->{"D2h",22},69->{"D2h",23},70->{"D2h",24},71->{"D2h",25},72->{"D2h",26},
  73->{"D2h",27},74->{"D2h",28},75->{"C4",1},76->{"C4",2},77->{"C4",3},78->{"C4",4},79->{"C4",5},80->{"C4",6},
  81->{"S4",1},82->{"S4",2},83->{"C4h",1},84->{"C4h",2},85->{"C4h",3},86->{"C4h",4},87->{"C4h",5},88->{"C4h",6},
  89->{"D4",1},90->{"D4",2},91->{"D4",3},92->{"D4",4},93->{"D4",5},94->{"D4",6},95->{"D4",7},96->{"D4",8},
  97->{"D4",9},98->{"D4",10},99->{"C4v",1},100->{"C4v",2},101->{"C4v",3},102->{"C4v",4},103->{"C4v",5},
  104->{"C4v",6},105->{"C4v",7},106->{"C4v",8},107->{"C4v",9},108->{"C4v",10},109->{"C4v",11},110->{"C4v",12},
  111->{"D2d",1},112->{"D2d",2},113->{"D2d",3},114->{"D2d",4},115->{"D2d",5},116->{"D2d",6},117->{"D2d",7},
  118->{"D2d",8},119->{"D2d",9},120->{"D2d",10},121->{"D2d",11},122->{"D2d",12},123->{"D4h",1},124->{"D4h",2},
  125->{"D4h",3},126->{"D4h",4},127->{"D4h",5},128->{"D4h",6},129->{"D4h",7},130->{"D4h",8},131->{"D4h",9},
  132->{"D4h",10},133->{"D4h",11},134->{"D4h",12},135->{"D4h",13},136->{"D4h",14},137->{"D4h",15},138->{"D4h",16},
  139->{"D4h",17},140->{"D4h",18},141->{"D4h",19},142->{"D4h",20},143->{"C3",1},144->{"C3",2},145->{"C3",3},
  146->{"C3",4},147->{"S6",1},148->{"S6",2},149->{"D3",1},150->{"D3",2},151->{"D3",3},152->{"D3",4},153->{"D3",5},
  154->{"D3",6},155->{"D3",7},156->{"C3v",1},157->{"C3v",2},158->{"C3v",3},159->{"C3v",4},160->{"C3v",5},
  161->{"C3v",6},162->{"D3d",1},163->{"D3d",2},164->{"D3d",3},165->{"D3d",4},166->{"D3d",5},167->{"D3d",6},
  168->{"C6",1},169->{"C6",2},170->{"C6",3},171->{"C6",4},172->{"C6",5},173->{"C6",6},174->{"C3h",1},175->{"C6h",1},
  176->{"C6h",2},177->{"D6",1},178->{"D6",2},179->{"D6",3},180->{"D6",4},181->{"D6",5},182->{"D6",6},183->{"C6v",1},
  184->{"C6v",2},185->{"C6v",3},186->{"C6v",4},187->{"D3h",1},188->{"D3h",2},189->{"D3h",3},190->{"D3h",4},
  191->{"D6h",1},192->{"D6h",2},193->{"D6h",3},194->{"D6h",4},195->{"T",1},196->{"T",2},197->{"T",3},198->{"T",4},
  199->{"T",5},200->{"Th",1},201->{"Th",2},202->{"Th",3},203->{"Th",4},204->{"Th",5},205->{"Th",6},206->{"Th",7},
  207->{"O",1},208->{"O",2},209->{"O",3},210->{"O",4},211->{"O",5},212->{"O",6},213->{"O",7},214->{"O",8},215->{"Td",1},
  216->{"Td",2},217->{"Td",3},218->{"Td",4},219->{"Td",5},220->{"Td",6},221->{"Oh",1},222->{"Oh",2},223->{"Oh",3},
  224->{"Oh",4},225->{"Oh",5},226->{"Oh",6},227->{"Oh",7},228->{"Oh",8},229->{"Oh",9},230->{"Oh",10}|>;

(* Give the Schoenflies symbol of the space group with number sgno *)
Options[SGSymScho]={"TeX"->False, "full"->False};
SGSymScho[sgno_Integer, OptionsPattern[]]:=Module[{dat,s1,s2,full,re,slat,brav},
  dat=SGSymSchoData[sgno];  full=OptionValue["full"]===True;
  s1=StringTake[dat[[1]],1];  s2=StringTake[dat[[1]],{2,-1}];
  If[OptionValue["TeX"]===True,
    re=s1<>If[s2!="","_{"<>s2<>"}",""]<>"^{"<>ToString[dat[[2]]]<>"}";
    If[full, brav=getSGLatt[sgno];
      slat=ToString@TeXForm@BravLattSymb@iBravLatt@brav;
      If[brav!="TrigPrim",slat=StringReplace[slat,{"_"->"_\\text{","^"->"}^\\text{"}]<>"}"];
      re=StringReplace[slat," "->""]<>re
      ];
    re="$"<>re<>"$",
    (*----else-----*)
    re=Subsuperscript[s1,s2,dat[[2]]];
    If[full, re=Row[{BravLattSymb@iBravLatt@getSGLatt[sgno],re}]]
    ];
  re
]

Options[showSGSym]={"ncol"->10};
showSGSym[OptionsPattern[]]:=showSGSym[All,"ncol"->OptionValue["ncol"]];
showSGSym[listOrSpan_, OptionsPattern[]]:=Module[{bgc,i,j,lt=0.96,s1,s2,tab,bgs,h,g,ncol,nrow,
  cls,sglist,nsg},
  sglist=If[IntegerQ[listOrSpan], {listOrSpan}, Range[230][[listOrSpan]]];   
  nsg=Length[sglist];  ncol=OptionValue["ncol"];  nrow=Ceiling[nsg/ncol];
  tab={#,s1=SGSymStd[#],s2=SGSymBC[#]; If[s2===s1,s2,Style[s2,Red]], SGSymScho[#]}&/@sglist//Partition[#,UpTo[ncol]]&;
  tab=Map[Column[#,ItemSize->{Full,1.2}]&,tab,{2}];
  bgc[n1_,n2_,color_]:=#->color&/@Range[n1,n2];
  cls=Join[bgc[1,2,Lighter[Red,lt]], bgc[3,15,Lighter[Blue,lt]], bgc[16,74,Lighter[Orange,lt]],
           bgc[75,142,Lighter[Cyan,lt]], bgc[143,167,Lighter[Yellow,lt]], 
           bgc[168,194,Lighter[Green,lt]],bgc[195,230,Lighter[Purple,lt]]]//Association;
  bgs=(#1->#2)&@@@Transpose@{Flatten[Table[{i,j},{i,nrow},{j,ncol}],1][[1;;nsg]],cls/@sglist};
  g=Grid[tab, Alignment->Left, Dividers->{{True,{},True},{True,{Thin},True}}, Spacings->{1, 1},
              Background->{None,None,bgs}, ItemSize->Full];
  h="Row 1: The SG number.\nRow 2: The standard SG international symbol.\n"<>
    "Row 3: The SG international symbol conforming to the BC orientation.\n"<>
    "Row 4: The Schoenflies symbol of SG.";
  Column[{h,g}]
]

PGinfo={ (*Point group information*)
  {1,"C1","1",{1, 1},1,{1, 1},{"E"},2,{2, 1},{"barE"}},
  {2,"Ci","-1",{2, 2},2,{2, 1},{"I"},4,{4, 2},{"I", "barE"}},
  {3,"C2","2",{3, 5},2,{2, 1},{"C2z"},4,{4, 1},{"C2z"}},
  {4,"Cs","m",{6, 9},2,{2, 1},{"\[Sigma]z"},4,{4, 1},{"\[Sigma]z"}},
  {5,"C2h","2/m",{10, 15},4,{4, 2},{"C2z", "I"},8,{8, 2},{"C2z", "I"}},
  {6,"D2","222",{16, 24},4,{4, 2},{"C2z", "C2y"},5,{8, 5},{"C2z", "C2y"}},
  {7,"C2v","mm2",{25, 46},4,{4, 2},{"C2z", "\[Sigma]y"},5,{8, 5},{"C2z", "\[Sigma]y"}},
  {8,"D2h","mmm",{47, 74},8,{8, 3},{"C2z", "C2y", "I"},10,{16, 11},{"C2z", "C2y", "I"}},
  {9,"C4","4",{75, 80},4,{4, 1},{"C4z+"},8,{8, 1},{"C4z+"}},
  {10,"S4","-4",{81, 82},4,{4, 1},{"S4z+"},8,{8, 1},{"S4z+"}},
  {11,"C4h","4/m",{83, 88},8,{8, 2},{"C4z+", "I"},16,{16, 2},{"C4z+", "I"}},
  {12,"D4","422",{89, 98},5,{8, 4},{"C4z+", "C2x"},7,{16, 14},{"C4z+", "C2x"}},
  {13,"C4v","4mm",{99, 110},5,{8, 4},{"C4z+", "\[Sigma]y"},7,{16, 14},{"C4z+", "\[Sigma]y"}},
  {14,"D2d","-42m",{111, 122},5,{8, 4},{"S4z+", "C2x"},7,{16, 14},{"S4z+", "C2x"}},
  {15,"D4h","4/mmm",{123, 142},10,{16, 9},{"C4z+", "C2x", "I"},14,{32, 9},{"C4z+", "C2x", "I"}},
  {16,"C3","3",{143, 146},3,{3, 1},{"C3+"},6,{6, 1},{"C3+"}},
  {17,"S6","-3",{147, 148},6,{6, 1},{"S6+"},12,{12, 6},{"S6+", "I"}},
  {18,"D3","32",{149, 155},3,{6, 2},{"C3+", "C21p"},6,{12, 4},{"C3+", "C21p"}},
  {19,"C3v","3m",{156, 161},3,{6, 2},{"C3+", "\[Sigma]v1"},6,{12, 4},{"C3+", "\[Sigma]v1"}},
  {20,"D3d","-3m",{162, 167},6,{12, 3},{"S6+", "C21p"},12,{24, 3},{"C3+", "C21p", "I"}},
  {21,"C6","6",{168, 173},6,{6, 1},{"C6+"},12,{12, 1},{"C6+"}},
  {22,"C3h","-6",{174, 174},6,{6, 1},{"S3+"},12,{12, 1},{"S3+"}},
  {23,"C6h","6/m",{175, 176},12,{12, 2},{"C3+", "C2", "I"},24,{24, 12},{"C6+", "I"}},
  {24,"D6","622",{177, 182},6,{12, 3},{"C6+", "C21p"},9,{24, 11},{"C6+", "C21p"}},
  {25,"C6v","6mm",{183, 186},6,{12, 3},{"C6+", "\[Sigma]v1"},9,{24, 11},{"C6+", "\[Sigma]d1"}},
  {26,"D3h","-6m2",{187, 190},6,{12, 3},{"S3+", "C21p"},9,{24, 11},{"S3+", "C21p"}},
  {27,"D6h","6/mmm",{191, 194},12,{24, 5},{"C3+", "C21p", "C2", "I"},18,{48, 15},{"C6+", "C21p", "I"}},
  {28,"T","23",{195, 199},4,{12, 5},{"C31+", "C2z", "C2y"},7,{24, 9},{"C31-", "C2x", "barC2y"}},
  {29,"Th","m-3",{200, 206},8,{24, 10},{"S61+", "C2z", "C2y"},14,{48, 4},{"C31-", "C2x", "barC2y", "I"}},
  {30,"O","432",{207, 214},5,{24, 7},{"C31-", "C2z", "C2x", "C2a"},8,{48, 10},{"C4x+", "barC31-", "C2b"}},
  {31,"Td","-43m",{215, 220},5,{24, 7},{"C31-", "C2z", "C2x", "\[Sigma]da"},8,{48, 10},{"S4x-", "barC31-", "\[Sigma]db"}},
  {32,"Oh","m-3m",{221, 230},10,{48, 7},{"S61-", "\[Sigma]x", "\[Sigma]z", "C2c"},16,{96, 8},{"C4x+", "barC31-", "C2b", "I"}}
};

showPGSch[s_String]:=If[StringLength[s]==1,s,Subscript[StringTake[s,1],StringTake[s,{2,-1}]]]
showPGInt[s_String]:=Module[{c=Characters[s], p}, p=Position[c, "-"]; 
                 If[p=={}, s, p = p[[1, 1]]; Row@Flatten@{c[[;;p-1]], OverBar[c[[p+1]]], c[[p+2;;]]}] ] 
Options[showPGinfo]={"long"->True, "color"->True, "double"->False}
showPGinfo[OptionsPattern[]]:=Module[{stab,lt=0.90,bgc,bgs,cls,ncol,ltab,note,dtag,dt},
  bgc[n1_,n2_,color_]:=#->color&/@Range[n1,n2];
  dtag=If[OptionValue["double"]===True, dt=3; " (D)", dt=0; ""];

  stab={#[[1]],showPGSch[#[[2]]],Row[{"(",showPGInt[#[[3]]],")"}]}&/@PGinfo;
  stab[[2,2]]=Row[{stab[[2,2]],Subscript["/S","2"]}];
  stab[[4,2]]=Row[{stab[[4,2]],Subscript["/C","1h"]}];
  stab=Grid[{#}, ItemSize->{{1.6,Full,Full}}, Alignment->Left, Spacings->0]&/@stab;
  cls=Join[bgc[1,2,Lighter[Red,lt]], bgc[3,5,Lighter[Blue,lt]], bgc[6,8,Lighter[Orange,lt]],
           bgc[9,15,Lighter[Cyan,lt]], bgc[16,20,Lighter[Yellow,lt]], 
           bgc[21,27,Lighter[Green,lt]],bgc[28,32,Lighter[Purple,lt]]]//Association;
  ncol=8;  
  bgs=(#1->#2)&@@@Transpose@{Flatten[Table[{i,j},{i,Ceiling[32/ncol]},{j,ncol}],1][[;;32]],cls/@Range[32]};
  stab=Partition[stab, UpTo[ncol]]//Grid[#,Alignment->Left, Spacings->{2,0.3}, 
                       Background->If[OptionValue["color"],{None,None,bgs},{}]]&;
  If[OptionValue["long"]==False, Return[stab]];

  ltab={#[[1]], showPGSch[#[[2]]], showPGInt[#[[3]]], Row[{#[[4,1]],"~",#[[4,2]]}],
        #[[dt+5]], Subsuperscript["G",#[[dt+6,1]],#[[dt+6,2]]], Row[showRot/@#[[dt+7]],","]}&/@PGinfo;
  ltab[[2,2]]=Row[{ltab[[2,2]],Subscript["/S","2"]}];
  ltab[[4,2]]=Row[{ltab[[4,2]],Subscript["/C","1h"]}];
  ltab=Prepend[ltab, {"No.", Column[{"Schoenflies","symbol"}], Column[{"International","symbol"}], 
                     Column[{"Space","groups     "}], Column[{"Number of","classes"<>dtag}], 
                     Column[{"Abstract","group"<>dtag}], "Generators"<>dtag}];
  cls=Join[bgc[1,1,Lighter[Gray,lt]], bgc[2,3,Lighter[Red,lt]], 
           bgc[4,6,Lighter[Blue,lt]], bgc[7,9,Lighter[Orange,lt]],
           bgc[10,16,Lighter[Cyan,lt]], bgc[17,21,Lighter[Yellow,lt]], 
           bgc[22,28,Lighter[Green,lt]],bgc[29,33,Lighter[Purple,lt]]];
  ltab=Grid[ltab, Alignment->Left, Spacings->{2,0.3}, Frame->True, Dividers->{{},{2->True}}, 
                  Background->If[OptionValue["color"],{{},cls},{}]];
  note="Note that the generators are not unique. The generators here are consistent with\n"<>
       "the rotation parts of the generators of the Herring little group at \[CapitalGamma] point of the\n"<>
       "first space group with this point group, NOT the same with those in BC-Tab. 5.2 or 6.4.";
  Column[{ltab,note}]
]


(* ::Section:: *)
(*Jones' faithful representation symbols (Tab. 1.4)*)


(* ::Text:: *)
(*Note that the Jones' symbols are based on certain basic vectors of the lattice.*)


(* JonesSymbol[[1]] for cubic crystal system, also usable for monoclinic, orthorhombic, and tetragonal systems
   JonesSymbol[[2]] for hexagonal and trigonal crystal systems. *)
JonesSymbol=Module[{xyzHex,xyzCub,nameHex,nameCub,matHex,matCub},Block[{x,y,z},
    nameCub={"E","C2x","C2y","C2z","C31+","C32+","C33+","C34+",
             "C31-","C32-","C33-","C34-","C4x+","C4y+","C4z+","C4x-",
             "C4y-","C4z-","C2a","C2b","C2c","C2d","C2e","C2f",
             "I","\[Sigma]x","\[Sigma]y","\[Sigma]z","S61-","S62-","S63-","S64-",
             "S61+","S62+","S63+","S64+","S4x-","S4y-","S4z-","S4x+",
             "S4y+","S4z+","\[Sigma]da","\[Sigma]db","\[Sigma]dc","\[Sigma]dd","\[Sigma]de","\[Sigma]df"};
    xyzCub={{x,y,z},{x,-y,-z},{-x,y,-z},{-x,-y,z},{z,x,y},{-z,x,-y},{-z,-x,y},{z,-x,-y},
            {y,z,x},{y,-z,-x},{-y,z,-x},{-y,-z,x},{x,-z,y},{z,y,-x},{-y,x,z},{x,z,-y},
            {-z,y,x},{y,-x,z},{y,x,-z},{-y,-x,-z},{z,-y,x},{-x,z,y},{-z,-y,-x},{-x,-z,-y},
            {-x,-y,-z},{-x,y,z},{x,-y,z},{x,y,-z},{-z,-x,-y},{z,-x,y},{z,x,-y},{-z,x,y},
            {-y,-z,-x},{-y,z,x},{y,-z,x},{y,z,-x},{-x,z,-y},{-z,-y,x},{y,-x,-z},{-x,-z,y},
            {z,-y,-x},{-y,x,-z},{-y,-x,z},{y,x,z},{-z,y,-x},{x,-z,-y},{z,y,x},{x,z,y}};
    nameHex={"E","C6+","C3+","C2","C3-","C6-","C21p","C22p",
             "C23p","C21pp","C22pp","C23pp","I","S3-","S6-","\[Sigma]h",
             "S6+","S3+","\[Sigma]d1","\[Sigma]d2","\[Sigma]d3","\[Sigma]v1","\[Sigma]v2","\[Sigma]v3"};
    xyzHex={{x,y,z},{x-y,x,z},{-y,x-y,z},{-x,-y,z},{-x+y,-x,z},{y,-x+y,z},{-x+y,y,-z},{x,x-y,-z},
            {-y,-x,-z},{x-y,-y,-z},{-x,-x+y,-z},{y,x,-z},{-x,-y,-z},{-x+y,-x,-z},{y,-x+y,-z},{x,y,-z},
            {x-y,x,-z},{-y,x-y,-z},{x-y,-y,z},{-x,-x+y,z},{y,x,z},{-x+y,y,z},{x,x-y,z},{-y,-x,z}};
    matCub=Normal@CoefficientArrays[#,{x,y,z}][[2]]&/@xyzCub;
    matHex=Normal@CoefficientArrays[#,{x,y,z}][[2]]&/@xyzHex;
    {{nameCub,xyzCub,matCub}\[Transpose],
     {nameHex,xyzHex,matHex}\[Transpose]}
]];

(* opName is a string for the name of operations defined in JonesSymbol, 
   by default return the xyz form of the Jones' symbol,
   if use option "type"\[Rule]"mat" return the corresponding rotation matrix.*)
Options[getJones]={"type"->"xyz"};
getJones[opName_String,OptionsPattern[]]:=Block[{x,y,z,hex,cub,re},
    {cub,hex}=JonesSymbol;
    If[OptionValue["type"]=="xyz",
       re=Association[Rule@@#&/@Join[cub,hex][[All,{1,2}]]],
       re=Association[Rule@@#&/@Join[cub,hex][[All,{1,3}]]]
    ];
    re[opName]
]
(* p1 is either the xyz form of the Jones' symbol or its matrix
   p2 can be "c" or "h" to designate cubic or hexagonal, p2 can also be the space group number *)
getJones[p1_,p2_]:=Block[{x,y,z,isCub,i,toname1,toname2},
   If[!ListQ[p1],Return[""]];
   If[StringQ[p2],
      Which[p2=="c",isCub=True,p2=="h",isCub=False],
      isCub=!(143<=p2<=194)   (* space groups with number in [143,194] are hexagonal or trigonal systems*)
     ];
   i=If[isCub,1,2]; 
   toname1=Association[Rule@@#[[{2,1}]]&/@JonesSymbol[[i]]];
   toname2=Association[Rule@@#[[{3,1}]]&/@JonesSymbol[[i]]];
   Switch[Dimensions[p1],
     {3},   toname1[p1],
     {3,3}, toname2[p1]
   ]
]


(* ::Section:: *)
(*The 14 Bravais lattices (Tab. 3.1)*)


Module[{latt,idx,lattS},
   latt={"TricPrim","MonoPrim","MonoBase","OrthPrim","OrthBase",
         "OrthBody","OrthFace","TetrPrim","TetrBody","TrigPrim",
         "HexaPrim","CubiPrim","CubiFace","CubiBody"};
   lattS={"\!\(\*SubscriptBox[\(\[CapitalGamma]\), \(t\)]\)",
          "\!\(\*SubscriptBox[\(\[CapitalGamma]\), \(m\)]\)",
          "\!\(\*SubsuperscriptBox[\(\[CapitalGamma]\), \(m\), \(b\)]\)",
          "\!\(\*SubscriptBox[\(\[CapitalGamma]\), \(o\)]\)",
          "\!\(\*SubsuperscriptBox[\(\[CapitalGamma]\), \(o\), \(b\)]\)",
          "\!\(\*SubsuperscriptBox[\(\[CapitalGamma]\), \(o\), \(v\)]\)",
          "\!\(\*SubsuperscriptBox[\(\[CapitalGamma]\), \(o\), \(f\)]\)",
          "\!\(\*SubscriptBox[\(\[CapitalGamma]\), \(q\)]\)",
          "\!\(\*SubsuperscriptBox[\(\[CapitalGamma]\), \(q\), \(v\)]\)",
          "\!\(\*SubscriptBox[\(\[CapitalGamma]\), \(rh\)]\)",
          "\!\(\*SubscriptBox[\(\[CapitalGamma]\), \(h\)]\)",
          "\!\(\*SubscriptBox[\(\[CapitalGamma]\), \(c\)]\)",
          "\!\(\*SubsuperscriptBox[\(\[CapitalGamma]\), \(c\), \(f\)]\)",
          "\!\(\*SubsuperscriptBox[\(\[CapitalGamma]\), \(c\), \(v\)]\)"};
   idx=Range[14];
   BravLatt=Association[Rule@@#&/@Transpose[{idx,latt}]];
   iBravLatt=Association[Rule@@#&/@Transpose[{latt,idx}]];
   BravLattSymb=Association[Rule@@#&/@Transpose[{idx,lattS}]];
]
	

BasicVectors=Block[{bvec,rules,a,b,c,\[Alpha],\[Beta],\[Gamma]},
   rules={
        "TricPrim"->{{a,0,0},{b Cos[\[Gamma]],b Sin[\[Gamma]],0},{c Cos[\[Beta]],c(Cos[\[Alpha]]Csc[\[Gamma]]-Cos[\[Beta]]Cot[\[Gamma]]),c Sqrt[Sin[\[Beta]]^2 -(Cos[\[Alpha]]Csc[\[Gamma]]-Cos[\[Beta]]Cot[\[Gamma]])^2 ]}},
        "MonoPrim"->{{0,-b,0},{a Sin[\[Gamma]],-a Cos[\[Gamma]],0},{0,0,c}},
        "MonoBase"->{{0,-b,0},{a Sin[\[Gamma]],-a Cos[\[Gamma]],-c}/2,{a Sin[\[Gamma]],-a Cos[\[Gamma]],c}/2},
        "OrthPrim"->{{0,-b,0},{a,0,0},{0,0,c}},
        "OrthBase"->{{a,-b,0}/2,{a,b,0}/2,{0,0,c}},
        "OrthBody"->{{a,b,c}/2,{-a,-b,c}/2,{a,-b,-c}/2},
        "OrthFace"->{{a,0,c}/2,{0,-b,c}/2,{a,-b,0}/2},
        "TetrPrim"->{{a,0,0},{0,a,0},{0,0,c}},
        "TetrBody"->{{-a,a,c}/2,{a,-a,c}/2,{a,a,-c}/2},
        "TrigPrim"->{{0,-a,c},{a Sqrt[3]/2,a/2,c},{-a Sqrt[3]/2,a/2,c}},
        "HexaPrim"->{{0,-a,0},{a Sqrt[3]/2,a/2,0},{0,0,c}},
        "CubiPrim"->{{a,0,0},{0,a,0},{0,0,a}},
        "CubiFace"->{{0,a,a},{a,0,a},{a,a,0}}/2,
        "CubiBody"->{{-a,a,a},{a,-a,a},{a,a,-a}}/2};
    bvec=Association[rules];
    bvec=Append[bvec,Rule@@#&/@Transpose[{Range[14],Values[rules]}]]
];

(* Transformation matrix from the lattice basic vectors defining Jones' symbol to the ones of each Bravais lattice 
   Let the Jone's basic vectors are a,b,c (all are column vectors), the Bravais lattice basic vectors
   are t1,t2,t3, and the transformation matrix is M, then the equation is:  (t1,t2,t3)=(a,b,c)M
   Because of column vectors, the code should be {t1,t2,t3}\[Transpose]={a,b,c}\[Transpose].M
   Note that the crystal has not been changed/rotated, and only the primitive cell definition changed.
*)
JonesTM=Module[{L0,M,brav}, M=<||>; Block[{a,b,c,\[Gamma]},
   M["TricPrim"]=M["MonoPrim"]=M["HexaPrim"]=IdentityMatrix[3];
   (* for MonoBase *)
   L0=BasicVectors["MonoPrim"];
   brav="MonoBase";  M[brav]=Inverse[L0\[Transpose]].BasicVectors[brav]\[Transpose]//Simplify;
   (* for OthPrim, OrthBase, OrthBody, OrthFace *)
   L0={{a,0,0},{0,b,0},{0,0,c}};  
   brav="OrthPrim";   M[brav]=Inverse[L0\[Transpose]].BasicVectors[brav]\[Transpose]//Simplify;
   brav="OrthBase";  M[brav]=Inverse[L0\[Transpose]].BasicVectors[brav]\[Transpose]//Simplify;
   brav="OrthBody";  M[brav]=Inverse[L0\[Transpose]].BasicVectors[brav]\[Transpose]//Simplify;
   brav="OrthFace";  M[brav]=Inverse[L0\[Transpose]].BasicVectors[brav]\[Transpose]//Simplify;
   (* for TrigPrim *)
   L0=BasicVectors["HexaPrim"]; 
   brav="TrigPrim";   M[brav]=Inverse[L0\[Transpose]].BasicVectors[brav]\[Transpose]//Simplify;
   (* for TetrPrim, TetrBody, CubiPrim, CubiBadyC, CubiFace *)  
   L0=BasicVectors["TetrPrim"];  
   brav="TetrPrim";   M[brav]=Inverse[L0\[Transpose]].BasicVectors[brav]\[Transpose]//Simplify;
   brav="TetrBody";  M[brav]=Inverse[L0\[Transpose]].BasicVectors[brav]\[Transpose]//Simplify;
   L0=BasicVectors["CubiPrim"];  
   brav="CubiPrim";   M[brav]=Inverse[L0\[Transpose]].BasicVectors[brav]\[Transpose]//Simplify;
   brav="CubiBody";  M[brav]=Inverse[L0\[Transpose]].BasicVectors[brav]\[Transpose]//Simplify;
   brav="CubiFace";  M[brav]=Inverse[L0\[Transpose]].BasicVectors[brav]\[Transpose]//Simplify;
]; M];

(* get the Bravais lattice of the space group with number SGNo *)
getSGLatt[SGNo_Integer]:=Module[{BLNo=<||>, brav,i},
   If[Or[SGNo>230,SGNo<1],Print["Warning::getSGLatt: SGNo ",SGNo," out of range."]; Return[]];
   BLNo["TricPrim"]={1,2}; (*Subscript[\[CapitalGamma], t]*)
   BLNo["MonoPrim"]={3,4,6,7,10,11,13,14}; (* Subscript[\[CapitalGamma], m]*)
   BLNo["MonoBase"]={5,8,9,12,15}; (*Subsuperscript[\[CapitalGamma], m, b]*)
   BLNo["OrthPrim"]={16,17,18,19,25,26,27,28,29,30,31,32,33,34,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62}; (*Subscript[\[CapitalGamma], o]*)
   BLNo["OrthBase"]={20,21,35,36,37,38,39,40,41,63,64,65,66,67,68}; (*Subsuperscript[\[CapitalGamma], o, b]*)
   BLNo["OrthBody"]={23,24,44,45,46,71,72,73,74}; (*Subsuperscript[\[CapitalGamma], o, v]*)
   BLNo["OrthFace"]={22,42,43,69,70}; (*Subsuperscript[\[CapitalGamma], o, f]*)
   BLNo["TetrPrim"]={75,76,77,78,81,83,84,85,86,89,90,91,92,93,94,95,96,99,100,101,102,103,104,105,106,111,112,113,114,115,116,117,118,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138}; (*Subscript[\[CapitalGamma], q]*)
   BLNo["TetrBody"]={79,80,82,87,88,97,98,107,108,109,110,119,120,121,122,139,140,141,142}; (*Subsuperscript[\[CapitalGamma], q, v]*)
   BLNo["TrigPrim"]={146,148,155,160,161,166,167}; (*Subscript[\[CapitalGamma], rh]*)
   BLNo["HexaPrim"]={143,144,145,147,149,150,151,152,153,154,156,157,158,159,162,163,164,165,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194}; (*Subscript[\[CapitalGamma], h]*)
   BLNo["CubiPrim"]={195,198,200,201,205,207,208,212,213,215,218,221,222,223,224}; (*Subscript[\[CapitalGamma], c]*)
   BLNo["CubiFace"]={196,202,203,209,210,216,219,225,226,227,228}; (*Subsuperscript[\[CapitalGamma], c, f]*)
   BLNo["CubiBody"]={197,199,204,206,211,214,217,220,229,230}; (*Subsuperscript[\[CapitalGamma], c, v]*)
   For[i=1,i<=14,i++, brav=BravLatt[i]; If[MemberQ[BLNo[brav],SGNo],Return[brav]]]
]

(* Check whether bvec is the basic vectors of the form in BC Tab.3.1. Also calculate the BZ type. *)
checkBasVec[brav_,bvec_]/;MatrixQ[bvec,NumericQ]&&Dimensions[bvec]=={3,3}:=
 Block[{a,b,c,\[Alpha],\[Beta],\[Gamma],a1,b1,c1,\[Alpha]1,\[Beta]1,\[Gamma]1,pbv,TM,prec=1*^-5,type},
   If[brav=="TricPrim", {a1,b1,c1}=Norm/@bvec;
     \[Alpha]1=ArcCos[bvec[[2]].bvec[[3]]/(b1*c1)];
     \[Beta]1=ArcCos[bvec[[1]].bvec[[3]]/(a1*c1)];
     \[Gamma]1=ArcCos[bvec[[1]].bvec[[2]]/(a1*b1)];
     Return[{True,"",{a->a1,b->b1,c->c1,\[Alpha]->\[Alpha]1,\[Beta]->\[Beta]1,\[Gamma]->\[Gamma]1}}]
     ];
     
   If[MemberQ[{"MonoPrim","MonoBase"},brav],
     TM=JonesTM[brav];
     pbv=Chop[Transpose[bvec\[Transpose].Inverse[TM]],prec]; 
     If[Flatten[pbv][[{1,3,6,7,8}]]!={0,0,0,0,0},Return[{False}]];
     b1=-pbv[[1,2]];   a1=Norm[pbv[[2]]];  c1=pbv[[3,3]];
     \[Gamma]1=ArcCos[-pbv[[2,2]]/a1];
     If[Abs[pbv[[2,1]]-a1*Sin[\[Gamma]1]]<prec&&b1>0&&c1>0, 
       Return[{True,"",{a->a1,b->b1,c->c1,\[Gamma]->\[Gamma]1}}], Return[{False}]
       ]
    ];
    
   If[MemberQ[{"OrthPrim","OrthBase","OrthBody","OrthFace"},brav],
     TM={{0,-1,0},{1,0,0},{0,0,1}}.JonesTM[brav];
     pbv=Chop[Transpose[bvec\[Transpose].Inverse[TM]],prec]; 
     If[Flatten[pbv][[{1,3,5,6,7,8}]]!={0,0,0,0,0,0},Return[{False}]];
     b1=-pbv[[1,2]];   a1=pbv[[2,1]];  c1=pbv[[3,3]];
     If[!(a1>0&&b1>0&&c1>0), Return[{False}]];
     type=Switch[brav,
       "OrthPrim", "",
       "OrthBase", If[a1>b1,"a","b"],
       "OrthBody", Switch[Max[a1,b1,c1],a1,"a",b1,"b",c1,"c"],
       "OrthFace", Which[1/a1^2<1/b1^2+1/c1^2&&1/b1^2<1/a1^2+1/c1^2&&1/c1^2<1/a1^2+1/b1^2,"a",
                          1/c1^2>1/a1^2+1/b1^2,"b",    1/b1^2>1/a1^2+1/c1^2,"c", 
                          1/a1^2>1/b1^2+1/c1^2,"d"]
      ];
     Return[{True,type,{a->a1,b->b1,c->c1}}]
    ];
    
   If[MemberQ[{"TetrPrim","TetrBody"},brav],
     TM=JonesTM[brav];
     pbv=Chop[Transpose[bvec\[Transpose].Inverse[TM]],prec]; 
     If[Flatten[pbv][[{2,3,4,6,7,8}]]!={0,0,0,0,0,0},Return[{False}]];
     a1=pbv[[1,1]];   b1=pbv[[2,2]];  c1=pbv[[3,3]];  
     If[!(Abs[a1-b1]<prec&&a1>0&&c1>0), Return[{False}]];
     a1=(a1+b1)/2;
     type=If[brav=="TetrBody",If[a1>c1,"a","b"],""];
     Return[{True,type,{a->a1,c->c1}}]
    ];
    
   If[brav=="TrigPrim",
     a1=Mean[Norm/@bvec[[All,{1,2}]]];   c1=Mean[bvec[[All,3]]];
     If[Max@Abs[(BasicVectors[brav]/.{a->a1,c->c1})-bvec]>prec, Return[{False}]];
     type=If[a1>Sqrt[2]c1,"a","b"];
     Return[{True,type,{a->a1,c->c1}}]
   ];
   
   If[brav=="HexaPrim",
     a1=Mean[Norm/@bvec[[{1,2}]]];   c1=bvec[[3,3]];
     If[Max@Abs[(BasicVectors[brav]/.{a->a1,c->c1})-bvec]>prec, Return[{False}]];
     Return[{True,"",{a->a1,c->c1}}]
   ];   
   
   If[MemberQ[{"CubiPrim","CubiBody","CubiFace"},brav],
     TM=JonesTM[brav];
     pbv=Chop[Transpose[bvec\[Transpose].Inverse[TM]],prec]; 
     If[Flatten[pbv][[{2,3,4,6,7,8}]]!={0,0,0,0,0,0},Return[{False}]];
     a1=pbv[[1,1]];   b1=pbv[[2,2]];  c1=pbv[[3,3]];  
     If[Abs[a1-b1]>prec||Abs[a1-c1]>prec||Abs[b1-c1]>prec, Return[{False}]];
     Return[{True,"",{a->(a1+b1+c1)/3}}]
   ];
 ]


(* ::Section:: *)
(*Rotation matrices for each Bravais lattice (Tab. 3.2 and Tab. 3.4)*)


(* RotMat[[1]][brav] contain the names of rotations for lattice type of brav.
   RotMat[[2]][brav] contain the rotation matrices for lattice type of brav.  *)
RotMat=Module[{opNames,M,tmp,i,brav,opMats},
   opNames=<||>;   opMats=<||>;
   opNames["TricPrim"]={"E","I"};
   opNames["MonoPrim"]=opNames["MonoBase"]={"E","C2z","I","\[Sigma]z"};
   opNames["OrthPrim"]=opNames["OrthBase"]=opNames["OrthBody"]=opNames["OrthFace"]={"E",
      "C2x","C2y","C2z","I","\[Sigma]x","\[Sigma]y","\[Sigma]z"};  
   tmp={"E","C4z+","C2z","C4z-","C2x","C2y","C2a","C2b"};
   tmp=Join[tmp,getJones[-getJones[#,"type"->"mat"],"c"]&/@tmp];
   opNames["TetrPrim"]=opNames["TetrBody"]=tmp;   
   tmp={"E","C3+","C3-","C21p","C22p","C23p"};
   opNames["TrigPrim"]=Join[tmp,getJones[-getJones[#,"type"->"mat"],"h"]&/@tmp];
   opNames["HexaPrim"]=JonesSymbol[[2,All,1]];
   opNames["CubiPrim"]=opNames["CubiBody"]=opNames["CubiFace"]=JonesSymbol[[1,All,1]];
   For[i=1,i<=14,i++,
      brav=BravLatt[i];   M=JonesTM[brav];  
      opMats[brav]=Inverse[M].getJones[#,"type"->"mat"].M&/@opNames[brav];
   ];
   {opNames,opMats}
];

(* It's faster that looking up the inverse of a rotation matrix than calculating it directly. *)
invRotMat=Association[#->Inverse[#]&/@(Join@@Values[RotMat[[2]]]//DeleteDuplicates)];

(* Show the result of RotMat applied on basic vectors (t1,t2,t3) in table form like Tab. 3.2 *)
checkRotMat[brav_]:=Module[{names,tt},
   names=RotMat[[1]][brav];
   tt={"t1","t2","t3"}.#&/@RotMat[[2]][brav]//Simplify;
   Column[{brav,TableForm[tt,TableHeadings->{names}]},Center,Dividers-> {{},{2->Black,3->Black}}]
]

(* Initialize three functions: getRotMat getRotMatOfK, getRotName*)
Module[{names,rots,dict1,dict2,brav,opname,mat},
 Do[
   names=RotMat[[1]][brav];    rots=RotMat[[2]][brav];
   dict1=Association@(Rule@@#&/@Transpose[{names,rots}]);
   dict2=Association@(Rule@@#&/@Transpose[{rots,names}]);
   Do[getRotMat[brav,opname]=dict1[opname],{opname,names}];
   Do[getRotMatOfK[brav,opname]=Inverse@Transpose[dict1[opname]],{opname,names}];
   Do[getRotName[brav,mat]=dict2[mat],{mat,rots}];
   ,{brav,Values[BravLatt]}]
];

(*Rotation matrices in cartesian coordinate system.*)
RotMatCart=<||>;
(RotMatCart[#]=getRotMat["CubiPrim",#])&/@RotMat[[1]]["CubiPrim"];
(RotMatCart[#]=With[{bv=Transpose@BasicVectors["HexaPrim"]},bv.getRotMat["HexaPrim",#].Inverse[bv]])&/@
  DeleteCases[RotMat[[1]]["HexaPrim"],"E"|"I"];

(* Note that the rotation matrix rotmat has to be basend on the t1,t2,t3 defined in Tab. 3.1.
   In fact, if the basic vectors are (t1',t2',t3')=(R.t1,R.t2,R.t3) in which R is a O(3) rotation
   matrix, then the rotmat remains unchanged.
   \:5728\:8fd9\:4e2a\:610f\:4e49\:4e0b\:ff0c\:6bd4\:5982C2x\:5e76\:975e\:4e00\:5b9a\:662f\:7ed5x\:8f74\:8f6c180\:5ea6\:ff0c\:800c\:662f\:7ed5t1\:8f74\:8f6c180\:5ea6(\:7b80\:5355\:56db\:65b9\:548c\:7b80\:5355\:7acb\:65b9\:6676\:683c)\:ff0c\:6216\:662f\:7ed5
   t2\:8f74\:8f6c180\:5ea6\:ff08\:7b80\:5355\:6b63\:4ea4\:6676\:683c\:ff09\:ff0c\:6216\:662f\:7ed5t1+t2\:65b9\:5411\:8f6c180\:5ea6\:ff08\:5e95\:5fc3\:6b63\:4ea4\:6676\:683c\:ff09\:ff0c\:6216\:662f\:7ed5t2+t3\:8f6c180\:5ea6\:ff08\:4f53\:5fc3\:7acb\:65b9\:ff09\:ff0c
   \:6216\:662f\:7ed5t2+t3-t1\:8f6c180\:5ea6\:ff08\:9762\:5fc3\:7acb\:65b9\:ff09\:7b49 *)

(* Show the result of RotMat in reciprocal space applied on reciprocal basic vectors (g1,g2,g3) 
   in table form like Tab. 3.4 *)
checkRotMatOfK[brav_]:=Module[{names,tg},
   names=RotMat[[1]][brav];
   tg={"g1","g2","g3"}.Inverse[#\[Transpose]]&/@RotMat[[2]][brav]//Simplify;
   Column[{brav,TableForm[tg,TableHeadings->{names}]},Center,Dividers-> {{},{2->Black,3->Black}}]
]


(*  The lattice basic vectors which can generate the operations defined in the BC (means Bradley 
    and Cracknell) Tab.3.7 are named (t1_BC1,t2_BC1,t3_BC1) and (t1_BC2,t2_BC2,t3_BC2) which 
    corresond to the first line and second line in Tab.3.7 respectively. These BC1 and BC2 basic vectors
    are compitable with the ones defined in BC Tab.3.1 but additional rotations may be needed to 
    give the same form in Tab.3.1. 
     
    Transformation matrix (Q) transforms the idealized standard lattice (as',bs',cs') from spglib (also the 
    same with ITA) to BC2 by (t1_BC2,t2_BC2,t3_BC2)=(as',bs',cs').Q. To result in the same form
    in Tab.3.1, say (t1'_BC2,t2'_BC2,t3'_BC2), a rotation S1 is needed, that is 
    (t1_BC2', t2_BC2', t3_BC2')=S1.(t1_BC2,t2_BC2,t3_BC2)=S1.(as',bs',cs').Q
    However, Tab.3.7 uses different orientations in the first line (i.e. BC1) for some spacegroups, 
    a transformation matrix U is needed to convert BC2 to BC1,
    (t1_BC1,t2_BC1,t3_BC1)=(t1_BC2,t2_BC2,t3_BC2).U.  
    The BC1 basic vectors also maybe need a rotation S to convert it to the Tab.3.1 form
    (t1_BC1', t2_BC1', t3_BC1')=S.(t1_BC1,t2_BC1,t3_BC1)=S.(as',bs',cs').Q.U.  
    If we start with BC2', then (t1_BC2', t2_BC2', t3_BC2').U also need a rotation to the BC1' form
    (t1_BC1', t2_BC1', t3_BC1')=S2.(t1_BC2', t2_BC2', t3_BC2').U=S2.S1.(as',bs',cs').Q.U
    Then we can combine the to rotations S1 and S2 to form S=S2.S1.
    
    Thus, the rotation matrix S has two origins, S1 and S2.  S1 originates from the different lattice 
    directions between the idealized standard lattice in spglib (the same as ITA) and the one in BC Tab.3.1,
    and S2 originates from the different orientions used in Tab.3.7 for some space groups. 
    In fact, it turns out that if S1!=I3 then S2==I3 and if S2!=I3 then S1==I3, which means either
    S1 or S2 takes effect.
    
    This function only gives Q and S1. U is given by SGGenElem.

    Note that all basic vectors in the above equations are COLUMN vectors.
    For as',bs',cs' see https://atztogo.github.io/spglib/definition.html#def-idealize-cell
*)
TMspglibToBC[brav_]:=Module[{spgbvec=<||>,BCbvec,rotBCbvec,Q,S1,BCrotMats=<||>,LS=<||>},
   LS=Association[#->StringTake[#,4]&/@{"TricPrim","MonoPrim","MonoBase","OrthPrim",
        "OrthBase","OrthBody","OrthFace","TetrPrim","TetrBody","TrigPrim","HexaPrim",
        "CubiPrim","CubiFace","CubiBody"}];
   Block[{a,b,c,\[Alpha],\[Beta],\[Gamma]},
     spgbvec["Tric"]=BasicVectors["TricPrim"];
   BCrotMats["Tric"]=IdentityMatrix[3];
     spgbvec["Mono"]={{a,0,0},{0,c,0},{b Cos[\[Gamma]],0,b Sin[\[Gamma]]}};
   BCrotMats["Mono"]=Inverse[RotationMatrix[4Pi/3,{1,1,1}].RotationMatrix[Pi-\[Gamma],{0,0,1}]]//Simplify;
     spgbvec["Orth"]={{a,0,0},{0,b,0},{0,0,c}};
   BCrotMats["Orth"]=IdentityMatrix[3];     
     spgbvec["Tetr"]={{a,0,0},{0,a,0},{0,0,c}};
   BCrotMats["Tetr"]=IdentityMatrix[3];
(* \:6ce8\:610f\:ff1a\:5728\:76ee\:524d spglib 1.14.1 \:7248\:672c\:65f6\:ff0c\:7f51\:9875 https://atztogo.github.io/spglib/definition.html#def-idealize-cell
   \:4e0a\:8bf4 Rhombohedral(R)\:683c\:5b50\:7406\:60f3\:5316\:7684\:6807\:51c6\:57fa\:77e2\:5e94\:8be5\:662f a,b,c \:7684z\:5206\:91cf\:76f8\:7b49\:ff0c\:5b83\:4eec\:5728 xy \:9762\:5185\:7684\:6295\:5f71\:5927\:5c0f\:4e5f\:76f8\:7b49\:ff0c\:9006\:65f6\:9488\:76f8\:9694
   120\:5ea6\:ff0c\:4e14a\:5728xy\:9762\:5185\:7684\:6295\:5f71a_xy\:6307\:5411+x\:65b9\:5411\:3002\:7136\:800c\:7a0b\:5e8f\:7684\:884c\:4e3a\:5e76\:975e\:5982\:6b64\:ff0cget_symmetry_dataset \:4e2d\:7ed9\:51fa\:7684 std_lattice\:ff0c
   \:4e5f\:5c31\:662f refine_cell \:7ed9\:51fa\:7684\:57fa\:77e2\:5e76\:975eR\:683c\:5b50\:ff0c\:800c\:662f\:4f20\:7edf\:516d\:89d2\:7684H\:683c\:5b50\:3002\:5373\:4f7f\:7528 find_primitive \:5f97\:5230\:7684R\:683c\:5b50\:57fa\:77e2\:5176\:53d6\:5411
   \:4e5f\:5e76\:975e\:5982\:7f51\:9875\:4e0a\:6240\:8bf4a_xy\:6307\:5411+x\:65b9\:5411\:ff0c\:800c\:662f\:6307\:5411\:4e0e+x\:65b9\:5411\:5939\:89d230\:5ea6\:7684\:65b9\:5411\:3002\:6545\:6700\:521d\:6309\:7f51\:9875\:4e0a\:8bf4\:7684\:53d6\:5411\:53bb\:5904\:7406\:5f97\:51fa\:7684\:7ed3\:679c\:4e0d\:5bf9\:3002
   \:5173\:4e8e\:6b64\:95ee\:9898\:5df2\:5411 spglib \:63d0\:4ea4 bug\:ff0c\:5f97\:5230\:4e86\:80af\:5b9a\:7684\:56de\:590d\:ff0c\:540e\:7eed\:7248\:672c\:4e2d\:4fee\:6539\:4e86\:7f51\:9875\:63cf\:8ff0\:3002
     spgbvec["Trig"]={{a,0,c},{-a/2,a Sqrt[3]/2,c},{-a/2,-a Sqrt[3]/2,c}};
   BCrotMats["Trig"]=RotationMatrix[Pi/2,{0,0,1}]//Transpose;  *)
   (* \:6ce8\:610f\:ff1a BC\:4e66\:4e2dTab.3.1\:4e2dR\:683c\:5b50\:91cc\:7684 a,c \:5e76\:975e\:5bf9\:5e94\:4f20\:7edfH\:6676\:80de\:7684a\:548cc\:ff0c\:540e\:8005\:5206\:522b\:662f\:524d\:8005\:7684Sqrt[3]\:548c3\:500d\:3002 *)
     spgbvec["Trig"]={{a,0,0},{-a/2,Sqrt[3]a/2,0},{0,0,c}}/.{a->Sqrt[3]a,c->3c};
   BCrotMats["Trig"]=RotationMatrix[2Pi/3,{0,0,1}]//Transpose;
     spgbvec["Hexa"]={{a,0,0},{-a/2,Sqrt[3]a/2,0},{0,0,c}};
   BCrotMats["Hexa"]=RotationMatrix[Pi/2,{0,0,1}]//Transpose;
     spgbvec["Cubi"]={{a,0,0},{0,a,0},{0,0,a}};
   BCrotMats["Cubi"]=IdentityMatrix[3];
   BCbvec=BasicVectors[brav];     (* (t1,t2,t3)^T *) 
   S1=BCrotMats[LS[brav]];
   rotBCbvec=BCbvec.S1//Simplify;  (* (t1',t2',t3')^T *)
   Q=Inverse[spgbvec[LS[brav]]\[Transpose]].(rotBCbvec\[Transpose])//Simplify;
   ];
   {Q,S1}
]
getQandS[SGNo_]:=Module[{Q,S,S1,S2,OBC2A},
   {Q,S1}=TMspglibToBC[getSGLatt[SGNo]];
   Block[{a,b,c,OBvecC,OBvecA},
      OBvecC=BasicVectors["OrthBase"];
      OBvecA={{0,-b,c}/2,{a,0,0},{0,b,c}/2};
      OBC2A=Inverse[OBvecC\[Transpose]].OBvecA\[Transpose];
   ];
   If[38<=SGNo<=41,Q=Q.OBC2A];   (*\:8fd9\:51e0\:4e2a\:7a7a\:95f4\:7fa4\:4f7f\:7528\:4e86A\:7684\:6b63\:4ea4\:5e95\:5fc3\:ff0c\:89c1Tab.3.1\:7684note(v)*)
   S2=IdentityMatrix[3];
   If[MemberQ[{17,19,28,29,31,33,36,46,53,61,70,122},SGNo],
      S2=RotationMatrix[-Pi/2,{0,0,1}]];  (* C4z-, for "b -a c" orientation *)
   If[MemberQ[{38,39,40,41,57},SGNo],
      S2=RotationMatrix[-2Pi/3,{1,1,1}]];  (* C3(111)-, for "b c a" orientation *)
   If[MemberQ[{51,54},SGNo],S2=RotationMatrix[-Pi/2,{0,1,0}]];  (* C4y-, for "-c b a" orientation *)
   If[MemberQ[{52,60},SGNo],S2=RotationMatrix[Pi/2,{1,0,0}]];  (* C4x+, for "a -c b" orientation *)
   S=S2.S1;
   {Q,S}
]


(* ::Section::Closed:: *)
(*Space group generating elements. (Tab. 3.7)*)


Module[{I3,o,t1,t2,t3,h1,h2,h3,h12,h13,h23,h123,q1,q3,r1,r2,s1,s5,
        OBvecA,OBvecC,OBA2C,UbAc0,Ubca0,UCba0,UaCb0,UbAcOrthP,UbAcOrthC,UbAcOrthB,
        UbAcOrthF,UbAcTetrB,UbcaOrthP,UbcaOrthC,UCbaOrthP,UaCbOrthP,U38to41},
t1={1,0,0};  t2={0,1,0};  t3={0,0,1};   h1=t1/2;  h2=t2/2;  h3=t3/2;
h23=h2+h3;   h13=h1+h3;   h12=h1+h2;    h123=h1+h2+h3;
q1=t3/4;     q3=3t3/4;
r1=t3/3;     r2=2t3/3;    s1=t3/6;      s5=5t3/6;
I3=IdentityMatrix[3];   o={0,0,0};  
UbAc0=Block[{a,b,c},Transpose@Normal@CoefficientArrays[{b,-a,c},{a,b,c}][[2]]];
Ubca0=Block[{a,b,c},Transpose@Normal@CoefficientArrays[{b,c,a},{a,b,c}][[2]]];
UCba0=Block[{a,b,c},Transpose@Normal@CoefficientArrays[{-c,b,a},{a,b,c}][[2]]];
UaCb0=Block[{a,b,c},Transpose@Normal@CoefficientArrays[{a,-c,b},{a,b,c}][[2]]];
UbAcOrthP=With[{X=TMspglibToBC["OrthPrim"][[1]]},Inverse[X].UbAc0.X];
UbAcOrthC=With[{X=TMspglibToBC["OrthBase"][[1]]},Inverse[X].UbAc0.X];
UbAcOrthB=With[{X=TMspglibToBC["OrthBody"][[1]]},Inverse[X].UbAc0.X];
UbAcOrthF=With[{X=TMspglibToBC["OrthFace"][[1]]},Inverse[X].UbAc0.X];
UbAcTetrB=With[{X=TMspglibToBC["TetrBody"][[1]]},Inverse[X].UbAc0.X];
UbcaOrthP=With[{X=TMspglibToBC["OrthPrim"][[1]]},Inverse[X].Ubca0.X];
UbcaOrthC=With[{X=TMspglibToBC["OrthBase"][[1]]},Inverse[X].Ubca0.X];
UCbaOrthP=With[{X=TMspglibToBC["OrthPrim"][[1]]},Inverse[X].UCba0.X];
UaCbOrthP=With[{X=TMspglibToBC["OrthPrim"][[1]]},Inverse[X].UaCb0.X];
Block[{a,b,c},
   OBvecC=BasicVectors["OrthBase"];
   OBvecA={{0,-b,c}/2,{a,0,0},{0,b,c}/2};
   OBA2C=Inverse[OBvecA\[Transpose]].OBvecC\[Transpose];
];
U38to41=OBA2C.UbcaOrthC;
SGGenElem=<||>;
SGGenElem[1]={{{"E",o}},o,I3,{}};
SGGenElem[2]={{{"I",o}},o,I3,{}};
SGGenElem[3]={{{"C2z",o}},o,I3,{}};
SGGenElem[4]={{{"C2z",h3}},o,I3,{}};
SGGenElem[5]={{{"C2z",o}},o,I3,{}};
SGGenElem[6]={{{"\[Sigma]z",o}},o,I3,{}};
SGGenElem[7]={{{"\[Sigma]z",h1}},o,I3,{}};
SGGenElem[8]={{{"\[Sigma]z",o}},o,I3,{}};
SGGenElem[9]={{{"\[Sigma]z",h1}},o,I3,{}};
SGGenElem[10]={{{"C2z",o},{"I",o}},o,I3,{}};
SGGenElem[11]={{{"C2z",h3},{"I",h3}},t3/4,I3,{{"C2z",h3},{"I",o}}};
SGGenElem[12]={{{"C2z",o},{"I",o}},o,I3,{}};
SGGenElem[13]={{{"C2z",o},{"I",h1}},t1/4,I3,{{"C2z",h1},{"I",o}}};
SGGenElem[14]={{{"C2z",h3},{"I",h13}},-t1/4+t3/4,I3,{{"C2z",h13},{"I",o}}};
SGGenElem[15]={{{"C2z",o},{"I",h1}},t1/4,I3,{{"C2z",h1},{"I",o}}};
SGGenElem[16]={{{"C2x",o},{"C2y",o}},o,I3,{}};
SGGenElem[17]={{{"C2x",h3},{"C2y",o}},o,UbAcOrthP,{{"C2x",o},{"C2y",h3}}};
SGGenElem[18]={{{"C2x",h12},{"C2y",h12}},o,I3,{}};
SGGenElem[19]={{{"C2x",h23},{"C2y",h12}},o,UbAcOrthP,{{"C2x",h12},{"C2y",h13}}};
SGGenElem[20]={{{"C2x",o},{"C2y",h3}},o,I3,{}};
SGGenElem[21]={{{"C2x",o},{"C2y",o}},o,I3,{}};
SGGenElem[22]={{{"C2x",o},{"C2y",o}},o,I3,{}};
SGGenElem[23]={{{"C2x",o},{"C2y",o}},o,I3,{}};
SGGenElem[24]={{{"C2x",h23},{"C2y",h12}},t1/2,I3,{{"C2x",h12},{"C2y",h13}}};
SGGenElem[25]={{{"\[Sigma]x",o},{"\[Sigma]y",o}},o,I3,{}};
SGGenElem[26]={{{"\[Sigma]x",o},{"\[Sigma]y",h3}},o,I3,{}};
SGGenElem[27]={{{"\[Sigma]x",h3},{"\[Sigma]y",h3}},o,I3,{}};
SGGenElem[28]={{{"\[Sigma]x",h1},{"\[Sigma]y",o}},-t1/4,UbAcOrthP,{{"\[Sigma]x",h2},{"\[Sigma]y",h2}}};
SGGenElem[29]={{{"\[Sigma]x",h1},{"\[Sigma]y",h3}},-t1/4,UbAcOrthP,{{"\[Sigma]x",h23},{"\[Sigma]y",h2}}};
SGGenElem[30]={{{"\[Sigma]x",h13},{"\[Sigma]y",h3}},-t1/4,I3,{{"\[Sigma]x",h13},{"\[Sigma]y",h13}}};
SGGenElem[31]={{{"\[Sigma]x",h13},{"\[Sigma]y",o}},o,UbAcOrthP,{{"\[Sigma]x",o},{"\[Sigma]y",h23}}};
SGGenElem[32]={{{"\[Sigma]x",h1},{"\[Sigma]y",h2}},-t1/4+t2/4,I3,{{"\[Sigma]x",h12},{"\[Sigma]y",h12}}};
SGGenElem[33]={{{"\[Sigma]x",h1},{"\[Sigma]y",h23}},-t1/4+t2/4,UbAcOrthP,{{"\[Sigma]x",h123},{"\[Sigma]y",h12}}};
SGGenElem[34]={{{"\[Sigma]x",h13},{"\[Sigma]y",h23}},-t1/4+t2/4,I3,{{"\[Sigma]x",h123},{"\[Sigma]y",h123}}};
SGGenElem[35]={{{"\[Sigma]x",o},{"\[Sigma]y",o}},o,I3,{}};
SGGenElem[36]={{{"\[Sigma]x",h3},{"\[Sigma]y",o}},o,UbAcOrthC,{{"\[Sigma]x",o},{"\[Sigma]y",h3}}};
SGGenElem[37]={{{"\[Sigma]x",h3},{"\[Sigma]y",h3}},o,I3,{}};
SGGenElem[38]={{{"\[Sigma]z",o},{"\[Sigma]x",o}},o,U38to41,{{"\[Sigma]x",o},{"\[Sigma]y",o}}};
SGGenElem[39]={{{"\[Sigma]z",h12},{"\[Sigma]x",h12}},o,U38to41,{{"\[Sigma]x",h13},{"\[Sigma]y",h13}}};
SGGenElem[40]={{{"\[Sigma]z",h3},{"\[Sigma]x",h3}},o,U38to41,{{"\[Sigma]x",h2},{"\[Sigma]y",h2}}};
SGGenElem[41]={{{"\[Sigma]z",h123},{"\[Sigma]x",h123}},o,U38to41,{{"\[Sigma]x",h123},{"\[Sigma]y",h123}}};
SGGenElem[42]={{{"\[Sigma]x",o},{"\[Sigma]y",o}},o,I3,{}};
SGGenElem[43]={{{"\[Sigma]x",h2},{"\[Sigma]y",h1}},t3/4,I3,{{"\[Sigma]x",{3,3,3}/4},{"\[Sigma]y",{3,3,3}/4}}};
SGGenElem[44]={{{"\[Sigma]x",o},{"\[Sigma]y",o}},o,I3,{}};
SGGenElem[45]={{{"\[Sigma]x",h12},{"\[Sigma]y",h12}},o,I3,{}};
SGGenElem[46]={{{"\[Sigma]x",h23},{"\[Sigma]y",h23}},o,UbAcOrthB,{{"\[Sigma]x",h13},{"\[Sigma]y",h13}}};
SGGenElem[47]={{{"C2x",o},{"C2y",o},{"I",o}},o,I3,{}};
SGGenElem[48]={{{"C2x",o},{"C2y",o},{"I",h123}},o,I3,{}};
SGGenElem[49]={{{"C2x",o},{"C2y",o},{"I",h3}},t3/4,I3,{{"C2x",h3},{"C2y",h3},{"I",o}}};
SGGenElem[50]={{{"C2x",o},{"C2y",o},{"I",h12}},o,I3,{}};
SGGenElem[51]={{{"C2x",h3},{"C2y",o},{"I",o}},o,UCbaOrthP,{{"C2x",h2},{"C2y",o},{"I",o}}};
SGGenElem[52]={{{"C2x",h3},{"C2y",o},{"I",h12}},t1/4+t2/4,UaCbOrthP,{{"C2x",h13},{"C2y",h123},{"I",o}}};
SGGenElem[53]={{{"C2x",h3},{"C2y",o},{"I",h1}},-t1/4,UbAcOrthP,{{"C2x",o},{"C2y",h23},{"I",o}}};
SGGenElem[54]={{{"C2x",h3},{"C2y",o},{"I",h2}},t2/4,UCbaOrthP,{{"C2x",h23},{"C2y",h3},{"I",o}}};
SGGenElem[55]={{{"C2x",h12},{"C2y",h12},{"I",o}},o,I3,{}};
SGGenElem[56]={{{"C2x",h12},{"C2y",h12},{"I",h123}},-(t1+t2+t3)/4,I3,{{"C2x",h23},{"C2y",h13},{"I",o}}};
SGGenElem[57]={{{"C2x",h12},{"C2y",h12},{"I",h2}},t2/4,UbcaOrthP,{{"C2x",h1},{"C2y",h13},{"I",o}}};
SGGenElem[58]={{{"C2x",h12},{"C2y",h12},{"I",h3}},-t3/4,I3,{{"C2x",h123},{"C2y",h123},{"I",o}}};
SGGenElem[59]={{{"C2x",h12},{"C2y",h12},{"I",h12}},o,I3,{}};
SGGenElem[60]={{{"C2x",h12},{"C2y",h12},{"I",h13}},-t1/4+t3/4,UaCbOrthP,{{"C2x",h12},{"C2y",h3},{"I",o}}};
SGGenElem[61]={{{"C2x",h23},{"C2y",h12},{"I",o}},o,UbAcOrthP,{{"C2x",h12},{"C2y",h13},{"I",o}}};
SGGenElem[62]={{{"C2x",h23},{"C2y",h12},{"I",h12}},t1/4+t2/4,I3,{{"C2x",h123},{"C2y",h1},{"I",o}}};
SGGenElem[63]={{{"C2x",o},{"C2y",h3},{"I",o}},o,I3,{}};
SGGenElem[64]={{{"C2x",o},{"C2y",h3},{"I",h12}},t1/4+t2/4,I3,{{"C2x",o},{"C2y",h123},{"I",o}}};
SGGenElem[65]={{{"C2x",o},{"C2y",o},{"I",o}},o,I3,{}};
SGGenElem[66]={{{"C2x",o},{"C2y",o},{"I",h3}},t3/4,I3,{{"C2x",h3},{"C2y",h3},{"I",o}}};
SGGenElem[67]={{{"C2x",o},{"C2y",o},{"I",h12}},t1/4+t2/4,I3,{{"C2x",o},{"C2y",h12},{"I",o}}};
(* SGGenElem[68]={{{"C2x",o},{"C2y",o},{"I",h123}},(-t1+t2+t3)/4,I3,{{"C2x",h123},{"C2y",h3},{"I",o}}}; *)
SGGenElem[68]={{{"C2x",o},{"C2y",o},{"I",h123}},o,I3,{},"Revised"};
SGGenElem[69]={{{"C2x",o},{"C2y",o},{"I",o}},o,I3,{}};
SGGenElem[70]={{{"C2x",o},{"C2y",o},{"I",{1,1,1}/4}},o,UbAcOrthF,{{"C2x",o},{"C2y",o},{"I",{3,3,3}/4}}};
SGGenElem[71]={{{"C2x",o},{"C2y",o},{"I",o}},o,I3,{}};
SGGenElem[72]={{{"C2x",o},{"C2y",o},{"I",h12}},t1/4+t2/4,I3,{{"C2x",h12},{"C2y",h12},{"I",o}}};
SGGenElem[73]={{{"C2x",h23},{"C2y",h12},{"I",o}},t1/2,I3,{{"C2x",h12},{"C2y",h13},{"I",o}}};
SGGenElem[74]={{{"C2x",h23},{"C2y",h12},{"I",h12}},3t1/4+t2/4,I3,{{"C2x",o},{"C2y",h23},{"I",o}}};
SGGenElem[75]={{{"C4z+",o}},o,I3,{}};
SGGenElem[76]={{{"C4z+",q1}},o,I3,{}};
SGGenElem[77]={{{"C4z+",h3}},o,I3,{}};
SGGenElem[78]={{{"C4z+",q3}},o,I3,{}};
SGGenElem[79]={{{"C4z+",o}},o,I3,{}};
SGGenElem[80]={{{"C4z+",{3,1,2}/4}},o,I3,{}};
SGGenElem[81]={{{"S4z+",o}},o,I3,{}};
SGGenElem[82]={{{"S4z+",o}},o,I3,{}};
SGGenElem[83]={{{"C4z+",o},{"I",o}},o,I3,{}};
SGGenElem[84]={{{"C4z+",h3},{"I",h3}},t3/4,I3,{{"C4z+",h3},{"I",o}}};
SGGenElem[85]={{{"C4z+",h12},{"I",h12}},o,I3,{}};
SGGenElem[86]={{{"C4z+",h123},{"I",h123}},o,I3,{}};
SGGenElem[87]={{{"C4z+",o},{"I",o}},o,I3,{}};
SGGenElem[88]={{{"C4z+",{3,1,2}/4},{"I",{3,1,2}/4}},o,I3,{}};
SGGenElem[89]={{{"C4z+",o},{"C2x",o}},o,I3,{}};
SGGenElem[90]={{{"C4z+",o},{"C2x",h12}},t1/2,I3,{{"C4z+",h12},{"C2x",h12}}};
SGGenElem[91]={{{"C4z+",q1},{"C2x",o}},t3/4,I3,{{"C4z+",q1},{"C2x",h3}}};
SGGenElem[92]={{{"C4z+",q1},{"C2x",h12}},t1/2-3t3/8,I3,{{"C4z+",{2,2,1}/4},{"C2x",{2,2,3}/4}}};
SGGenElem[93]={{{"C4z+",h3},{"C2x",o}},o,I3,{}};
SGGenElem[94]={{{"C4z+",h3},{"C2x",h12}},t1/2+t3/4,I3,{{"C4z+",h123},{"C2x",h123}}};
SGGenElem[95]={{{"C4z+",q3},{"C2x",o}},t3/4,I3,{{"C4z+",q3},{"C2x",h3}}};
SGGenElem[96]={{{"C4z+",q3},{"C2x",h12}},t1/2-t3/8,I3,{{"C4z+",{2,2,3}/4},{"C2x",{2,2,1}/4}}};
SGGenElem[97]={{{"C4z+",o},{"C2x",o}},o,I3,{}};
SGGenElem[98]={{{"C4z+",{3,1,2}/4},{"C2x",h23}},t1/8+t2/8,I3,{{"C4z+",{3,1,2}/4},{"C2x",{3,1,2}/4}}};
SGGenElem[99]={{{"C4z+",o},{"\[Sigma]x",o}},o,I3,{}};
SGGenElem[100]={{{"C4z+",o},{"\[Sigma]x",h12}},o,I3,{}};
SGGenElem[101]={{{"C4z+",h3},{"\[Sigma]x",h3}},o,I3,{}};
SGGenElem[102]={{{"C4z+",h3},{"\[Sigma]x",h123}},t1/2,I3,{{"C4z+",h123},{"\[Sigma]x",h123}}};
SGGenElem[103]={{{"C4z+",o},{"\[Sigma]x",h3}},o,I3,{}};
SGGenElem[104]={{{"C4z+",o},{"\[Sigma]x",h123}},o,I3,{}};
SGGenElem[105]={{{"C4z+",h3},{"\[Sigma]x",o}},o,I3,{}};
SGGenElem[106]={{{"C4z+",h3},{"\[Sigma]x",h12}},o,I3,{}};
SGGenElem[107]={{{"C4z+",o},{"\[Sigma]x",o}},o,I3,{}};
SGGenElem[108]={{{"C4z+",o},{"\[Sigma]x",h12}},o,I3,{}};
SGGenElem[109]={{{"C4z+",{3,1,2}/4},{"\[Sigma]x",o}},o,I3,{}};
SGGenElem[110]={{{"C4z+",{3,1,2}/4},{"\[Sigma]x",h12}},o,I3,{}};
SGGenElem[111]={{{"S4z+",o},{"C2x",o}},o,I3,{}};
SGGenElem[112]={{{"S4z+",o},{"C2x",h3}},o,I3,{}};
SGGenElem[113]={{{"S4z+",o},{"C2x",h12}},o,I3,{}};
SGGenElem[114]={{{"S4z+",o},{"C2x",h123}},o,I3,{}};
SGGenElem[115]={{{"S4z+",o},{"C2a",o}},o,I3,{}};
SGGenElem[116]={{{"S4z+",o},{"C2a",h3}},o,I3,{}};
SGGenElem[117]={{{"S4z+",o},{"C2a",h12}},o,I3,{}};
SGGenElem[118]={{{"S4z+",o},{"C2a",h123}},o,I3,{}};
SGGenElem[119]={{{"S4z+",o},{"C2a",o}},o,I3,{}};
SGGenElem[120]={{{"S4z+",o},{"C2a",h12}},o,I3,{}};
SGGenElem[121]={{{"S4z+",o},{"C2x",o}},o,I3,{}};
SGGenElem[122]={{{"S4z+",o},{"C2x",{1,3,2}/4}},o,UbAcTetrB,{{"S4z+",o},{"C2x",{3,1,2}/4}}};
SGGenElem[123]={{{"C4z+",o},{"C2x",o},{"I",o}},o,I3,{}};
SGGenElem[124]={{{"C4z+",o},{"C2x",o},{"I",h3}},t3/4,I3,{{"C4z+",o},{"C2x",h3},{"I",o}}};
(* SGGenElem[125]={{{"C4z+",h12},{"C2x",o},{"I",h12}},t1/4-t2/4,I3,{{"C4z+",h1},{"C2x",h2},{"I",o}}}; *)
SGGenElem[125]={{{"C4z+",h12},{"C2x",o},{"I",h12}},t1/2,I3,{{"C4z+",o},{"C2x",o},{"I",h12}},"Revised"};
SGGenElem[126]={{{"C4z+",h12},{"C2x",o},{"I",h123}},t1/2,I3,{{"C4z+",o},{"C2x",o},{"I",h123}}};
SGGenElem[127]={{{"C4z+",h12},{"C2x",h12},{"I",o}},t1/2,I3,{{"C4z+",o},{"C2x",h12},{"I",o}}};
SGGenElem[128]={{{"C4z+",h12},{"C2x",h12},{"I",h3}},t1/2+t3/4,I3,{{"C4z+",o},{"C2x",h123},{"I",o}}};
SGGenElem[129]={{{"C4z+",o},{"C2x",h12},{"I",h12}},t1/2,I3,{{"C4z+",h12},{"C2x",h12},{"I",h12}}};
SGGenElem[130]={{{"C4z+",o},{"C2x",h12},{"I",h123}},t1/2+t3/4,I3,{{"C4z+",h12},{"C2x",h123},{"I",h12}}};
SGGenElem[131]={{{"C4z+",h3},{"C2x",o},{"I",o}},o,I3,{}};
SGGenElem[132]={{{"C4z+",h3},{"C2x",o},{"I",h3}},t3/4,I3,{{"C4z+",h3},{"C2x",h3},{"I",o}}};
SGGenElem[133]={{{"C4z+",h123},{"C2x",o},{"I",h12}},t3/4,I3,{{"C4z+",h123},{"C2x",h3},{"I",h123}}};
SGGenElem[134]={{{"C4z+",h123},{"C2x",o},{"I",h123}},o,I3,{}};
SGGenElem[135]={{{"C4z+",h123},{"C2x",h12},{"I",o}},t1/2,I3,{{"C4z+",h3},{"C2x",h12},{"I",o}}};
SGGenElem[136]={{{"C4z+",h123},{"C2x",h12},{"I",h3}},t3/4,I3,{{"C4z+",h123},{"C2x",h123},{"I",o}}};
SGGenElem[137]={{{"C4z+",h3},{"C2x",h12},{"I",h12}},t1/2+t3/4,I3,{{"C4z+",h123},{"C2x",h123},{"I",h123}}};
SGGenElem[138]={{{"C4z+",h3},{"C2x",h12},{"I",h123}},t1/2,I3,{{"C4z+",h123},{"C2x",h12},{"I",h123}}};
SGGenElem[139]={{{"C4z+",o},{"C2x",o},{"I",o}},o,I3,{}};
SGGenElem[140]={{{"C4z+",h12},{"C2x",o},{"I",h12}},3t1/4+t2/4+t3/2,I3,{{"C4z+",o},{"C2x",h12},{"I",o}}};
(* SGGenElem[141]={{{"C4z+",h2},{"C2x",h12},{"I",h12}},t1/4+t2/4,I3,{{"C4z+",h2},{"C2x",o},{"I",o}}}; *)
SGGenElem[141]={{{"C4z+",h2},{"C2x",h12},{"I",h12}},3t1/8+t2/8+t3/4,I3,{{"C4z+",{3,1,2}/4},{"C2x",{3,1,2}/4},{"I",{3,1,2}/4}},"Revised"};
(* SGGenElem[142]={{{"C4z+",h1},{"C2x",h12},{"I",o}},o,tbd,{{"C4z+",h2},{"C2x",h12},{"I",o}}}; *)
SGGenElem[142]={{{"C4z+",h1},{"C2x",h12},{"I",o}},t1/8+3t2/8-t3/4,I3,{{"C4z+",{3,1,2}/4},{"C2x",{1,3,2}/4},{"I",{3,1,2}/4}},"Revised"};
SGGenElem[143]={{{"C3+",o}},o,I3,{}};
SGGenElem[144]={{{"C3+",r1}},o,I3,{}};
SGGenElem[145]={{{"C3+",r2}},o,I3,{}};
SGGenElem[146]={{{"C3+",o}},o,I3,{}};
SGGenElem[147]={{{"S6+",o}},o,I3,{}};
SGGenElem[148]={{{"S6+",o}},o,I3,{}};
SGGenElem[149]={{{"C3+",o},{"C21p",o}},o,I3,{}};
SGGenElem[150]={{{"C3+",o},{"C21pp",o}},o,I3,{}};
SGGenElem[151]={{{"C3+",r1},{"C21p",r2}},t3/6,I3,{{"C3+",r1},{"C21p",r1}}};
SGGenElem[152]={{{"C3+",r1},{"C21pp",r2}},o,I3,{}};
SGGenElem[153]={{{"C3+",r2},{"C21p",r1}},-t3/6,I3,{{"C3+",r2},{"C21p",r2}}};
SGGenElem[154]={{{"C3+",r2},{"C21pp",r1}},o,I3,{}};
(* SGGenElem[155]={{{"C3+",o},{"C21p",o}},o,tbd,{{"C3+",o},{"C21pp",o}}}; *)
SGGenElem[155]={{{"C3+",o},{"C21p",o}},o,I3,{},"Revised"};
SGGenElem[156]={{{"C3+",o},{"\[Sigma]v1",o}},o,I3,{}};
SGGenElem[157]={{{"C3+",o},{"\[Sigma]d1",o}},o,I3,{}};
SGGenElem[158]={{{"C3+",o},{"\[Sigma]v1",h3}},o,I3,{}};
SGGenElem[159]={{{"C3+",o},{"\[Sigma]d1",h3}},o,I3,{}};
(* SGGenElem[160]={{{"C3+",o},{"\[Sigma]d1",o}},o,tbd,{{"C3+",o},{"\[Sigma]v1",o}}}; *)
SGGenElem[160]={{{"C3+",o},{"\[Sigma]d1",o}},o,I3,{},"Revised"};
(* SGGenElem[161]={{{"C3+",o},{"\[Sigma]d1",h123}},o,tbd,{{"C3+",o},{"\[Sigma]v1",h123}}}; *)
SGGenElem[161]={{{"C3+",o},{"\[Sigma]d1",h123}},o,I3,{},"Revised"};
SGGenElem[162]={{{"S6+",o},{"\[Sigma]d1",o}},o,I3,{}};
SGGenElem[163]={{{"S6+",o},{"\[Sigma]d1",h3}},o,I3,{}};
SGGenElem[164]={{{"S6+",o},{"\[Sigma]v1",o}},o,I3,{}};
SGGenElem[165]={{{"S6+",o},{"\[Sigma]v1",h3}},o,I3,{}};
(* SGGenElem[166]={{{"S6+",o},{"\[Sigma]d1",o}},o,tbd,{{"S6+",o},{"\[Sigma]v1",o}}}; *)
SGGenElem[166]={{{"S6+",o},{"\[Sigma]d1",o}},o,I3,{},"Revised"};
(* SGGenElem[167]={{{"S6+",o},{"\[Sigma]d1",h123}},o,tbd,{{"S6+",o},{"\[Sigma]v1",h123}}}; *)
SGGenElem[167]={{{"S6+",o},{"\[Sigma]d1",h123}},o,I3,{},"Revised"};
SGGenElem[168]={{{"C6+",o}},o,I3,{}};
SGGenElem[169]={{{"C6+",s1}},o,I3,{}};
SGGenElem[170]={{{"C6+",s5}},o,I3,{}};
SGGenElem[171]={{{"C6+",r1}},o,I3,{}};
SGGenElem[172]={{{"C6+",r2}},o,I3,{}};
SGGenElem[173]={{{"C6+",h3}},o,I3,{}};
SGGenElem[174]={{{"S3+",o}},o,I3,{}};
SGGenElem[175]={{{"C6+",o},{"\[Sigma]h",o}},o,I3,{}};
SGGenElem[176]={{{"C6+",h3},{"\[Sigma]h",o}},t3/4,I3,{{"C6+",h3},{"\[Sigma]h",h3}}};
SGGenElem[177]={{{"C6+",o},{"C21p",o}},o,I3,{}};
(* SGGenElem[178]={{{"C6+",s1},{"C21p",o}},o,tbd,{{"C6+",s1},{"C21pp",o}}}; *)
SGGenElem[178]={{{"C6+",s1},{"C21p",o}},t3/4,I3,{{"C6+",s1},{"C21pp",o}}, "Revised"};
(* SGGenElem[179]={{{"C6+",s5},{"C21p",o}},o,tbd,{{"C6+",s5},{"C21pp",o}}}; *)
SGGenElem[179]={{{"C6+",s5},{"C21p",o}},t3/4,I3,{{"C6+",s5},{"C21pp",o}}, "Revised"};
SGGenElem[180]={{{"C6+",r1},{"C21p",o}},o,I3,{}};
SGGenElem[181]={{{"C6+",r2},{"C21p",o}},o,I3,{}};
(* SGGenElem[182]={{{"C6+",h3},{"C21p",o}},o,tbd,{{"C6+",h3},{"C21pp",o}}}; *)
SGGenElem[182]={{{"C6+",h3},{"C21p",o}},t3/4,I3,{{"C6+",h3},{"C21pp",o}}, "Revised"};
SGGenElem[183]={{{"C6+",o},{"\[Sigma]v1",o}},o,I3,{}};
SGGenElem[184]={{{"C6+",o},{"\[Sigma]v1",h3}},o,I3,{}};
SGGenElem[185]={{{"C6+",h3},{"\[Sigma]v1",h3}},o,I3,{}};
SGGenElem[186]={{{"C6+",h3},{"\[Sigma]v1",o}},o,I3,{}};
SGGenElem[187]={{{"S3+",o},{"\[Sigma]v1",o}},o,I3,{}};
SGGenElem[188]={{{"S3+",o},{"\[Sigma]v1",h3}},t3/4,I3,{{"S3+",h3},{"\[Sigma]v1",h3}}};
SGGenElem[189]={{{"S3+",o},{"\[Sigma]d1",o}},o,I3,{}};
SGGenElem[190]={{{"S3+",o},{"\[Sigma]d1",h3}},t3/4,I3,{{"S3+",h3},{"\[Sigma]d1",h3}}};
SGGenElem[191]={{{"C6+",o},{"C21p",o},{"I",o}},o,I3,{}};
SGGenElem[192]={{{"C6+",o},{"C21p",h3},{"I",o}},o,I3,{}};
SGGenElem[193]={{{"C6+",h3},{"C21p",o},{"I",o}},o,I3,{}};
SGGenElem[194]={{{"C6+",h3},{"C21p",h3},{"I",o}},o,I3,{}};
SGGenElem[195]={{{"C2z",o},{"C2x",o},{"C31+",o}},o,I3,{}};
SGGenElem[196]={{{"C2z",o},{"C2x",o},{"C31+",o}},o,I3,{}};
SGGenElem[197]={{{"C2z",o},{"C2x",o},{"C31+",o}},o,I3,{}};
SGGenElem[198]={{{"C2z",h13},{"C2x",h12},{"C31+",o}},o,I3,{}};
SGGenElem[199]={{{"C2z",h13},{"C2x",h12},{"C31+",o}},o,I3,{}};
SGGenElem[200]={{{"C2z",o},{"C2x",o},{"C31+",o},{"I",o}},o,I3,{}};
SGGenElem[201]={{{"C2z",o},{"C2x",o},{"C31+",o},{"I",h123}},o,I3,{}};
SGGenElem[202]={{{"C2z",o},{"C2x",o},{"C31+",o},{"I",o}},o,I3,{}};
SGGenElem[203]={{{"C2z",o},{"C2x",o},{"C31+",o},{"I",h123/2}},o,I3,{}};
SGGenElem[204]={{{"C2z",o},{"C2x",o},{"C31+",o},{"I",o}},o,I3,{}};
SGGenElem[205]={{{"C2z",h13},{"C2x",h12},{"C31+",o},{"I",o}},o,I3,{}};
SGGenElem[206]={{{"C2z",h13},{"C2x",h12},{"C31+",o},{"I",o}},o,I3,{}};
SGGenElem[207]={{{"C2z",o},{"C2x",o},{"C2a",o},{"C31+",o}},o,I3,{}};
SGGenElem[208]={{{"C2z",o},{"C2x",o},{"C2a",h123},{"C31+",o}},o,I3,{}};
SGGenElem[209]={{{"C2z",o},{"C2x",o},{"C2a",o},{"C31+",o}},o,I3,{}};
SGGenElem[210]={{{"C2z",o},{"C2x",o},{"C2a",h123/2},{"C31+",o}},o,I3,{}};
SGGenElem[211]={{{"C2z",o},{"C2x",o},{"C2a",o},{"C31+",o}},o,I3,{}};
SGGenElem[212]={{{"C2z",h13},{"C2x",h12},{"C2a",{1,3,3}/4},{"C31+",o}},o,I3,{}};
SGGenElem[213]={{{"C2z",h13},{"C2x",h12},{"C2a",{3,1,1}/4},{"C31+",o}},o,I3,{}};
SGGenElem[214]={{{"C2z",h13},{"C2x",h12},{"C2a",h1},{"C31+",o}},o,I3,{}};
SGGenElem[215]={{{"C2z",o},{"C2x",o},{"\[Sigma]da",o},{"C31+",o}},o,I3,{}};
SGGenElem[216]={{{"C2z",o},{"C2x",o},{"\[Sigma]da",o},{"C31+",o}},o,I3,{}};
SGGenElem[217]={{{"C2z",o},{"C2x",o},{"\[Sigma]da",o},{"C31+",o}},o,I3,{}};
SGGenElem[218]={{{"C2z",o},{"C2x",o},{"\[Sigma]da",h123},{"C31+",o}},o,I3,{}};
SGGenElem[219]={{{"C2z",o},{"C2x",o},{"\[Sigma]da",h123},{"C31+",o}},o,I3,{}};
SGGenElem[220]={{{"C2z",h13},{"C2x",h12},{"\[Sigma]da",h1},{"C31+",o}},o,I3,{}};
SGGenElem[221]={{{"C2z",o},{"C2x",o},{"C2a",o},{"C31+",o},{"I",o}},o,I3,{}};
SGGenElem[222]={{{"C2z",o},{"C2x",o},{"C2a",o},{"C31+",o},{"I",h123}},o,I3,{}};
SGGenElem[223]={{{"C2z",o},{"C2x",o},{"C2a",h123},{"C31+",o},{"I",o}},o,I3,{}};
SGGenElem[224]={{{"C2z",o},{"C2x",o},{"C2a",h123},{"C31+",o},{"I",h123}},o,I3,{}};
SGGenElem[225]={{{"C2z",o},{"C2x",o},{"C2a",o},{"C31+",o},{"I",o}},o,I3,{}};
SGGenElem[226]={{{"C2z",o},{"C2x",o},{"C2a",o},{"C31+",o},{"I",h123}},(t1+t2+t3)/4,I3,{{"C2z",o},{"C2x",o},{"C2a",h123},{"C31+",o},{"I",o}}};
SGGenElem[227]={{{"C2z",o},{"C2x",o},{"C2a",h123/2},{"C31+",o},{"I",h123/2}},o,I3,{}};
SGGenElem[228]={{{"C2z",o},{"C2x",o},{"C2a",h123/2},{"C31+",o},{"I",{3,3,3}/4}},o,I3,{}};
SGGenElem[229]={{{"C2z",o},{"C2x",o},{"C2a",o},{"C31+",o},{"I",o}},o,I3,{}};
SGGenElem[230]={{{"C2z",h13},{"C2x",h12},{"C2a",h1},{"C31+",o},{"I",o}},o,I3,{}}; 
]


(* ::Section:: *)
(*High-symmetry k points (Tab. 3.6)*)


BCHighSymKpt=Block[{u,hskpt,i,idx,D2hxyz,C2vx,C2vy,C2vz,C2vazb,C2vzab,C2vbza,D4hz,D4hy,C4vz,C4vy,D2hzab,
                    D3d,D6h,D3h,C6v,Oh,vera,verb,verc,verd},
  hskpt=<||>;
  hskpt["TricPrim"]={
    {"\[CapitalGamma]","",{0,0,0},"Ci",{"E","I"}},
    {"B","",{1/2,0,0},"Ci",{"E","I"}},
    {"F","",{0,1/2,0},"Ci",{"E","I"}},
    {"G","",{0,0,1/2},"Ci",{"E","I"}}
  };
  hskpt["MonoPrim"]={
    {"\[CapitalGamma]","",{0,0,0},"C2h",{"E","C2z","I","\[Sigma]z"}},
    {"B","",{-1/2,0,0},"C2h",{"E","C2z","I","\[Sigma]z"}},
    {"Y","",{0,1/2,0},"C2h",{"E","C2z","I","\[Sigma]z"}},
    {"Z","",{0,0,1/2},"C2h",{"E","C2z","I","\[Sigma]z"}},
    {"C","",{0,1/2,1/2},"C2h",{"E","C2z","I","\[Sigma]z"}},
    {"D","",{-1/2,0,1/2},"C2h",{"E","C2z","I","\[Sigma]z"}},
    {"A","",{1/2,1/2,0},"C2h",{"E","C2z","I","\[Sigma]z"}},
    {"E","",{1/2,1/2,1/2},"C2h",{"E","C2z","I","\[Sigma]z"}},
    {"\[CapitalLambda]","\[CapitalGamma]Z",{0,0,u},"C2",{"E","C2z"}},
    {"V","BD",{-1/2,0,u},"C2",{"E","C2z"}},
    {"W","YC",{0,1/2,u},"C2",{"E","C2z"}},
    {"U","AE",{1/2,1/2,u},"C2",{"E","C2z"}}
  };
  hskpt["MonoBase"]={
    {"\[CapitalGamma]","",{0,0,0},"C2h",{"E","C2z","I","\[Sigma]z"}},
    {"A","",{-1/2,0,0},"C2h",{"E","C2z","I","\[Sigma]z"}},
    {"Z","",{0,-1/2,1/2},"C2h",{"E","C2z","I","\[Sigma]z"}},
    {"M","",{-1/2,-1/2,1/2},"C2h",{"E","C2z","I","\[Sigma]z"}},
    {"L","",{-1/2,0,1/2},"Ci",{"E","I"}},
    {"V","",{0,0,1/2},"Ci",{"E","I"}},
    {"\[CapitalLambda]","\[CapitalGamma]Z",{0,-u,u},"C2",{"E","C2z"}},
    {"U","AM",{-1/2,-u,u},"C2",{"E","C2z"}}
  };
  D2hxyz={"E","C2x","C2y","C2z","I","\[Sigma]x","\[Sigma]y","\[Sigma]z"};
  C2vx={"E","C2x","\[Sigma]y","\[Sigma]z"};
  C2vy={"E","C2y","\[Sigma]z","\[Sigma]x"};
  C2vz={"E","C2z","\[Sigma]x","\[Sigma]y"};
  hskpt["OrthPrim"]={
    {"\[CapitalGamma]","",{0,0,0},"D2h",D2hxyz},
    {"Y","",{-1/2,0,0},"D2h",D2hxyz},
    {"X","",{0,1/2,0},"D2h",D2hxyz},
    {"Z","",{0,0,1/2},"D2h",D2hxyz},
    {"U","",{0,1/2,1/2},"D2h",D2hxyz},
    {"T","",{-1/2,0,1/2},"D2h",D2hxyz},
    {"S","",{-1/2,1/2,0},"D2h",D2hxyz},
    {"R","",{-1/2,1/2,1/2},"D2h",D2hxyz},
    {"\[CapitalDelta]","\[CapitalGamma]Y",{-u,0,0},"C2v",C2vy},
    {"D","XS",{-u,1/2,0},"C2v",C2vy},
    {"P","UR",{-u,1/2,1/2},"C2v",C2vy},
    {"B","ZT",{-u,0,1/2},"C2v",C2vy},
    {"\[CapitalSigma]","\[CapitalGamma]X",{0,u,0},"C2v",C2vx},
    {"C","YS",{-1/2,u,0},"C2v",C2vx},
    {"E","TR",{-1/2,u,1/2},"C2v",C2vx},
    {"A","ZU",{0,u,1/2},"C2v",C2vx},
    {"\[CapitalLambda]","\[CapitalGamma]Z",{0,0,u},"C2v",C2vz},
    {"H","YT",{-1/2,0,u},"C2v",C2vz},
    {"Q","SR",{-1/2,1/2,u},"C2v",C2vz},
    {"G","XU",{0,1/2,u},"C2v",C2vz}
  };
  hskpt["OrthBase(a)"]={ (*for a>b*)
    {"\[CapitalGamma]","",{0,0,0},"D2h",D2hxyz},
    {"Y","",{1/2,1/2,0},"D2h",D2hxyz},
    {"Z","",{0,0,1/2},"D2h",D2hxyz},
    {"T","",{1/2,1/2,1/2},"D2h",D2hxyz},
    {"S","",{0,1/2,0},"C2h",{"E","C2z","I","\[Sigma]z"}},
    {"R","",{0,1/2,1/2},"C2h",{"E","C2z","I","\[Sigma]z"}},
    {"\[CapitalLambda]","\[CapitalGamma]Z",{0,0,u},"C2v",C2vz},
    {"H","YT",{1/2,1/2,u},"C2v",C2vz},
    {"D","SR",{0,1/2,u},"C2",{"E","C2z"}},
    {"A","ZT",{u,u,1/2},"C2v",C2vx},
    {"\[CapitalSigma]","\[CapitalGamma]Y",{u,u,0},"C2v",C2vx},
    {"\[CapitalDelta]","\[CapitalGamma]\[CapitalDelta]",{-u,u,0},"C2v",C2vy},
    {"B","ZB",{-u,u,1/2},"C2v",C2vy},
    {"G","TG",{1/2-u,1/2+u,1/2},"C2v",C2vy},
    {"F","YF",{1/2-u,1/2+u,0},"C2v",C2vy}
  };
  verb=hskpt["OrthBase(a)"];  (* for a<b *)
  idx[str_]:=Position[hskpt["OrthBase(a)"],str][[1,1]];
  verb[[idx["Y"],3]]={-1/2,1/2,0};
  verb[[idx["T"],3]]={-1/2,1/2,1/2};
  verb[[idx["H"],3]]={-1/2,1/2,u};
  verb[[idx["A"],2]]="\[CapitalGamma]A";    (* The BC Tab.3.6 has error for this item *)
  verb[[idx["\[CapitalSigma]"],2]]="\[CapitalGamma]\[CapitalSigma]";    (* The BC Tab.3.6 has error for this item *)
  verb[[idx["\[CapitalDelta]"],2]]="\[CapitalGamma]Y";    (* The BC Tab.3.6 has error for this item *)
  verb[[idx["B"],2]]="ZT";    (* The BC Tab.3.6 has error for this item *)
  verb[[idx["G"]]]={"E","TE",{-1/2+u,1/2+u,1/2},"C2v",C2vx};
  verb[[idx["F"]]]={"C","YC",{-1/2+u,1/2+u,0},"C2v",C2vx};
  hskpt["OrthBase(b)"]=verb;

  hskpt["OrthBody(a)"]={ (* for a longest *)
    {"\[CapitalGamma]","",{0,0,0},"D2h",D2hxyz},
    {"X","",{1/2,-1/2,1/2},"D2h",D2hxyz},
    {"R","",{1/2,0,0},"C2h",{"E","C2y","I","\[Sigma]y"}},
    {"S","",{1/2,0,-1/2},"C2h",{"E","C2x","I","\[Sigma]x"}},
    {"T","",{1/2,-1/2,0},"C2h",{"E","C2z","I","\[Sigma]z"}},
    {"W","",{3/4,-1/4,-1/4},"D2",{"E","C2x","C2y","C2z"}},
    {"\[CapitalLambda]","\[CapitalGamma]\[CapitalLambda]",{u,u,-u},"C2v",C2vz},
    {"G","XG",{1/2+u,-1/2+u,1/2-u},"C2v",C2vz},
    {"P","TW",{1/2+u,-1/2+u,-u},"C2",{"E","C2z"}},
    {"\[CapitalSigma]","\[CapitalGamma]X",{u,-u,u},"C2v",C2vx},
    {"D","SW",{1/2+u,-u,-1/2+u},"C2",{"E","C2x"}},
    {"\[CapitalDelta]","\[CapitalGamma]\[CapitalDelta]",{u,-u,-u},"C2v",C2vy},
    {"U","XU",{1/2+u,-1/2-u,1/2-u},"C2v",C2vy},
    {"Q","RW",{1/2+u,-u,-u},"C2",{"E","C2y"}}
  };
  verb=hskpt["OrthBody(a)"]; (* for b longest *)
  verc=hskpt["OrthBody(a)"]; (* for c longest *)
  idx[str_]:=Position[hskpt["OrthBody(a)"],str][[1,1]];
  verb[[idx["X"],3]]={1/2,-1/2,-1/2};
  verc[[idx["X"],3]]={1/2,1/2,-1/2};
  verc[[idx["\[CapitalLambda]"],2]]="\[CapitalGamma]X";
  verb[[idx["G"],3]]={1/2+u,-1/2+u,-1/2-u};
  verb[[idx["\[CapitalSigma]"],2]]= "\[CapitalGamma]\[CapitalSigma]";
  verc[[idx["\[CapitalSigma]"],2]]= "\[CapitalGamma]\[CapitalSigma]";
  verb[[idx["U"]]]={"F","XF",{1/2+u,-1/2-u,-1/2+u},"C2v",C2vx};
  verc[[idx["G"]]]={"F","XF",{1/2+u,1/2-u,-1/2+u},"C2v",C2vx};
  verb[[idx["\[CapitalDelta]"],2]]= "\[CapitalGamma]X";
  verc[[idx["U"],3]]={1/2+u,1/2-u,-1/2-u};
  hskpt["OrthBody(b)"]=verb;
  hskpt["OrthBody(c)"]=verc; 

  hskpt["OrthFace(a)"]={ (* for 1/a^2 < 1/b^2 + 1/c^2, 1/b^2 < 1/a^2 + 1/c^2 and 1/c^2 < 1/a^2 + 1/b^2  *)
    {"\[CapitalGamma]","",{0,0,0},"D2h",D2hxyz},
    {"Y","",{0,-1/2,-1/2},"D2h",D2hxyz},
    {"X","",{1/2,0,1/2},"D2h",D2hxyz},
    {"Z","",{1/2,1/2,0},"D2h",D2hxyz},
    {"L","",{1/2,0,0},"Ci",{"E","I"}},
    {"\[CapitalLambda]","\[CapitalGamma]Z",{u,u,0},"C2v",C2vz},
    {"G","XG",{1/2+u,u,1/2},"C2v",C2vz},
    {"H","YH",{u,-1/2+u,-1/2},"C2v",C2vz},
    {"\[CapitalSigma]","\[CapitalGamma]X",{u,0,u},"C2v",C2vx},
    {"C","YC",{u,-1/2,-1/2+u},"C2v",C2vx},
    {"A","ZA",{1/2+u,1/2,u},"C2v",C2vx},
    {"\[CapitalDelta]","\[CapitalGamma]Y",{0,-u,-u},"C2v",C2vy},
    {"D","XD",{1/2,-u,1/2-u},"C2v",C2vy},
    {"B","ZB",{1/2,1/2-u,-u},"C2v",C2vy}
  };
  verb=hskpt["OrthFace(a)"]; (* for 1/c^2 > 1/b^2 + 1/a^2 *)
  verc=hskpt["OrthFace(a)"]; (* for 1/b^2 > 1/c^2 + 1/a^2 *)
  verd=hskpt["OrthFace(a)"]; (* for 1/a^2 > 1/b^2 + 1/c^2 *)
  idx[str_]:=Position[hskpt["OrthFace(a)"],str][[1,1]];
  verc[[idx["Y"],3]]={1,1/2,1/2};
  verd[[idx["X"],3]]={1/2,0,-1/2};
  verb[[idx["Z"],3]]={1/2,-1/2,0};
  verb[[idx["\[CapitalLambda]"],2]]="\[CapitalGamma]\[CapitalLambda]";
  verc[[idx["G"],2]]="XY";
  verd[[idx["H"],2]]="YX";
  verd[[idx["\[CapitalSigma]"],2]]="\[CapitalGamma]\[CapitalSigma]";
  verb[[idx["C"],2]]="YZ";
  verc[[idx["A"],2]]="ZY";
  verc[[idx["\[CapitalDelta]"],2]]="\[CapitalGamma]\[CapitalDelta]";
  verb[[idx["D"],2]]="XZ";
  verd[[idx["B"],2]]="ZX";
  verb=Delete[verb,{{idx["A"]},{idx["B"]}}];
  verc=Delete[verc,{{idx["H"]},{idx["C"]}}];
  verd=Delete[verd,{{idx["G"]},{idx["D"]}}];
  i=Position[verb,"\[CapitalSigma]"][[1,1]];
  verb=Insert[verb,{"Q","ZQ",{1/2+u,-1/2+u,0},"C2v",C2vz},i];
  verc=Insert[verc,{"R","YR",{1,1/2-u,1/2-u},"C2v",C2vy},-1];
  i=Position[verd,"\[CapitalDelta]"][[1,1]];
  verd=Insert[verd,{"U","XU",{1/2+u,0,-1/2+u},"C2v",C2vx},i];
  hskpt["OrthFace(b)"]=verb;
  hskpt["OrthFace(c)"]=verc;
  hskpt["OrthFace(d)"]=verd;

  D4hz={"E","C4z+","C4z-","C2z","C2x","C2y","C2a","C2b","I","S4z-","S4z+","\[Sigma]z","\[Sigma]x","\[Sigma]y","\[Sigma]da","\[Sigma]db"};
  C4vz={"E","C4z+","C4z-","C2z","\[Sigma]x","\[Sigma]y","\[Sigma]da","\[Sigma]db"};
  C2vazb={"E","C2a","\[Sigma]z","\[Sigma]db"};
  hskpt["TetrPrim"]={ 
    {"\[CapitalGamma]","",{0,0,0},"D4h",D4hz},
    {"M","",{1/2,1/2,0},"D4h",D4hz},
    {"Z","",{0,0,1/2},"D4h",D4hz},
    {"A","",{1/2,1/2,1/2},"D4h",D4hz},
    {"R","",{0,1/2,1/2},"D2h",D2hxyz},
    {"X","",{0,1/2,0},"D2h",D2hxyz},
    {"\[CapitalDelta]","\[CapitalGamma]X",{0,u,0},"C2v",C2vy},
    {"U","ZR",{0,u,1/2},"C2v",C2vy},
    {"\[CapitalLambda]","\[CapitalGamma]Z",{0,0,u},"C4v",C4vz},
    {"V","MA",{1/2,1/2,u},"C4v",C4vz},
    {"\[CapitalSigma]","\[CapitalGamma]M",{u,u,0},"C2v",C2vazb},
    {"S","ZA",{u,u,1/2},"C2v",C2vazb},
    {"Y","XM",{u,1/2,0},"C2v",C2vx},
    {"T","RA",{u,1/2,1/2},"C2v",C2vx},
    {"W","XR",{0,1/2,u},"C2v",C2vz}
  };

  D2hzab={"E","C2z","C2a","C2b","I","\[Sigma]z","\[Sigma]da","\[Sigma]db"};
  C2vzab={"E","C2z","\[Sigma]da","\[Sigma]db"};
  C2vbza={"E","C2b","\[Sigma]z","\[Sigma]da"};
  hskpt["TetrBody(a)"]={ (* for a>c *)
    {"\[CapitalGamma]","",{0,0,0},"D4h",D4hz},
    {"N","",{0,1/2,0},"C2h",{"E","C2y","I","\[Sigma]y"}},
    {"X","",{0,0,1/2},"C2h",D2hzab},
    {"Z","",{-1/2,1/2,1/2},"D4h",D4hz},
    {"P","",{1/4,1/4,1/4},"D2d",{"E","C2x","C2y","C2z","\[Sigma]da","\[Sigma]db","S4z-","S4z+"}},
    {"\[CapitalLambda]","\[CapitalGamma]\[CapitalLambda]",{u,u,-u},"C4v",C4vz},
    {"V","ZV",{-1/2+u,1/2+u,1/2-u},"C4v",C4vz},
    {"W","XP",{u,u,1/2-u},"C2v",C2vzab},
    {"\[CapitalSigma]","\[CapitalGamma]Z",{-u,u,u},"C2v",C2vx},
    {"F","ZF",{1/2-u,1/2+u,-1/2+u},"C2v",C2vx},  (* for (b) *)
    {"Q","NP",{u,1/2-u,u},"C2",{"E","C2y"}},
    {"\[CapitalDelta]","\[CapitalGamma]X",{0,0,u},"C2v",C2vazb},
    {"U","ZU",{1/2,1/2,-1/2+u},"C2v",C2vazb}, (* for (b) *)
    {"Y","XZ",{-u,u,1/2},"C2v",C2vbza}
  };
  verb=hskpt["TetrBody(a)"]; (* for a<c *)
  idx[str_]:=Position[verb,str][[1,1]];
  hskpt["TetrBody(a)"]=Delete[hskpt["TetrBody(a)"],{{idx["F"]},{idx["U"]}}];
  verb=Delete[verb,idx["V"]];
  verb[[idx["Z"],3]]={1/2,1/2,-1/2};
  verb[[idx["\[CapitalLambda]"],2]]="\[CapitalGamma]Z";
  verb[[idx["\[CapitalSigma]"],2]]="\[CapitalGamma]\[CapitalSigma]";
  verb[[idx["Y"],2]]="XY";
  hskpt["TetrBody(b)"]=verb;

  D3d={"E","C3+","C3-","C21p","C22p","C23p","I","S6-","S6+","\[Sigma]d1","\[Sigma]d2","\[Sigma]d3"};
  hskpt["TrigPrim(a)"]={ (* for a>sqrt(2)*c *)
    {"\[CapitalGamma]","",{0,0,0},"D3d",D3d},
    {"Z","",{1/2,1/2,-1/2},"D3d",D3d},
    {"L","",{0,1/2,0},"C2h",{"E","C22p","I","\[Sigma]d2"}},
    {"F","",{0,1/2,-1/2},"C2h",{"E","C21p","I","\[Sigma]d1"}},
    {"\[CapitalLambda]","\[CapitalGamma]\[CapitalLambda]",{u,u,u},"C3v",{"E","C3+","C3-","\[Sigma]d1","\[Sigma]d2","\[Sigma]d3"}},
    {"P","ZP",{1/2-u,1/2-u,-1/2-u},"C3v",{"E","C3+","C3-","\[Sigma]d1","\[Sigma]d2","\[Sigma]d3"}},
    {"B","ZB",{1/2,1/2+u,1/2-u},"C2",{"E","C21p"}}, (* for (b) *)
    {"\[CapitalSigma]","\[CapitalGamma]F",{0,u,-u},"C2",{"E","C21p"}},
    {"Q","FQ",{1/2-u,1/2+u,0},"C2",{"E","C23p"}}, (* for (b) *)
    {"Y","LZ",{u,1/2,-u},"C2",{"E","C22p"}}
  };
  verb=hskpt["TrigPrim(a)"]; (* for a<sqrt(2)*c *)
  idx[str_]:=Position[verb,str][[1,1]];
  hskpt["TrigPrim(a)"]=Delete[hskpt["TrigPrim(a)"],{{idx["B"]},{idx["Q"]}}];
  verb=Delete[verb,idx["P"]];
  verb[[idx["Z"],3]]={1/2,1/2,1/2};
  verb[[idx["F"],{3,5}]]={{1/2,1/2,0},{"E","C23p","I","\[Sigma]d3"}};
  verb[[idx["\[CapitalLambda]"],2]]="\[CapitalGamma]Z";
  verb[[idx["\[CapitalSigma]"],2]]="\[CapitalGamma]\[CapitalSigma]";
  verb[[idx["Y"],2]]="LY";
  hskpt["TrigPrim(b)"]=verb;

  D6h=JonesSymbol[[2,All,1]];
  D3h={"E","C3+","C3-","C21pp","C22pp","c23pp","\[Sigma]h","S3+","S3-","\[Sigma]d1","\[Sigma]d2","\[Sigma]d3"};
  C6v={"E","C6+","C6-","C3+","C3-","C2","\[Sigma]v1","\[Sigma]v2","\[Sigma]v3","\[Sigma]d1","\[Sigma]d2","\[Sigma]d3"};
  hskpt["HexaPrim"]={ 
    {"\[CapitalGamma]","",{0,0,0},"D6h",D6h},
    {"M","",{0,1/2,0},"D2h",{"E","C2","C21p","C21pp","I","\[Sigma]h","\[Sigma]d1","\[Sigma]v1"}},
    {"A","",{0,0,1/2},"D6h",D6h},
    {"L","",{0,1/2,1/2},"D2h",{"E","C2","C21p","C21pp","I","\[Sigma]h","\[Sigma]d1","\[Sigma]v1"}},
    {"K","",{-1/3,2/3,0},"D3h",D3h},
    {"H","",{-1/3,2/3,1/2},"D3h",D3h},
    {"\[CapitalDelta]","\[CapitalGamma]A",{0,0,u},"C6v",C6v},
    {"U","ML",{0,1/2,u},"C2v",{"E","C2","\[Sigma]v1","\[Sigma]d1"}},
    {"P","KH",{-1/3,2/3,u},"C3v",{"E","C3+","C3-","\[Sigma]d1","\[Sigma]d2","\[Sigma]d3"}},
    {"T","\[CapitalGamma]K",{-u,2*u,0},"C2v",{"E","C22pp","\[Sigma]h","\[Sigma]d2"}},
    {"S","AH",{-u,2*u,1/2},"C2v",{"E","C22pp","\[Sigma]h","\[Sigma]d2"}},
    {"T'","MK",{-2*u,1/2+u,0},"C2v",{"E","C21pp","\[Sigma]h","\[Sigma]d1"}},
    {"S'","LH",{-2*u,1/2+u,1/2},"C2v",{"E","C21pp","\[Sigma]h","\[Sigma]d1"}},
    {"\[CapitalSigma]","\[CapitalGamma]M",{0,u,0},"C2v",{"E","C21p","\[Sigma]h","\[Sigma]v1"}},
    {"R","AL",{0,u,1/2},"C2v",{"E","C21p","\[Sigma]h","\[Sigma]v1"}}
  };

  Oh=JonesSymbol[[1,All,1]];
  D4hy={"E","C4y+","C4y-","C2y","C2x","C2z","C2c","C2e","I","S4y-","S4y+","\[Sigma]y","\[Sigma]x","\[Sigma]z","\[Sigma]dc","\[Sigma]de"};
  C4vy={"E","C4y+","C4y-","C2y","\[Sigma]x","\[Sigma]z","\[Sigma]dc","\[Sigma]de"};
  hskpt["CubiPrim"]={ 
    {"\[CapitalGamma]","",{0,0,0},"Oh",Oh},
    {"X","",{0,1/2,0},"D4h",D4hy},
    {"M","",{1/2,1/2,0},"D4h",D4hz},
    {"R","",{1/2,1/2,1/2},"Oh",Oh},
    {"\[CapitalDelta]","\[CapitalGamma]X",{0,u,0},"C4v",C4vy},
    {"\[CapitalSigma]","\[CapitalGamma]M",{u,u,0},"C2v",C2vazb},
    {"\[CapitalLambda]","\[CapitalGamma]R",{u,u,u},"C3v",{"E","C31+","C31-","\[Sigma]db","\[Sigma]de","\[Sigma]df"}},
    {"S","XR",{u,1/2,u},"C2v",{"E","C2c","\[Sigma]de","\[Sigma]y"}},
    {"Z","XM",{u,1/2,0},"C2v",C2vx},
    {"T","MR",{1/2,1/2,u},"C4v",C4vz}
  };

  hskpt["CubiFace"]={ 
    {"\[CapitalGamma]","",{0,0,0},"Oh",Oh},
    {"X","",{1/2,0,1/2},"D4h",D4hy},
    {"L","",{1/2,1/2,1/2},"D3d",{"E","C31+","C31-","C2b","C2e","C2f","I","S6-","S6+","\[Sigma]db","\[Sigma]de","\[Sigma]df"}},
    {"W","",{1/2,1/4,3/4},"D2d",{"E","C2x","C2d","C2f","\[Sigma]y","\[Sigma]z","S4x+","S4x-"}},
    {"\[CapitalDelta]","\[CapitalGamma]X",{u,0,u},"C4v",C4vy},
    {"\[CapitalLambda]","\[CapitalGamma]L",{u,u,u},"C3v",{"E","C31+","C31-","\[Sigma]db","\[Sigma]de","\[Sigma]df"}},
    {"\[CapitalSigma]","\[CapitalGamma]\[CapitalSigma]",{u,u,2*u},"C2v",C2vazb},
    {"S","XS",{1/2+u,2*u,1/2+u},"C2v",{"E","C2c","\[Sigma]de","\[Sigma]y"}},
    {"Z","XW",{1/2,u,1/2+u},"C2v",C2vx},
    {"Q","LW",{1/2,1/2-u,1/2+u},"C2",{"E","C2f"}}
  };
   
  hskpt["CubiBody"]={ 
    {"\[CapitalGamma]","",{0,0,0},"Oh",Oh},
    {"H","",{1/2,-1/2,1/2},"Oh",Oh},
    {"P","",{1/4,1/4,1/4},"Td",{"E","C31+","C31-","C32+","C32-","C33+","C33-","C34+","C34-","C2x","C2y","C2z","S4x+","S4x-","S4y+","S4y-","S4z+","S4z-","\[Sigma]da","\[Sigma]db","\[Sigma]dc","\[Sigma]dd","\[Sigma]de","\[Sigma]df"}},
    {"N","",{0,0,1/2},"D2h",{"E","C2a","C2z","C2b","I","\[Sigma]da","\[Sigma]z","\[Sigma]db"}},
    {"\[CapitalSigma]","\[CapitalGamma]N",{0,0,u},"C2v",C2vazb},
    {"\[CapitalDelta]","\[CapitalGamma]H",{u,-u,u},"C4v",C4vy},
    {"\[CapitalLambda]","\[CapitalGamma]P",{u,u,u},"C3v",{"E","C31+","C31-","\[Sigma]db","\[Sigma]de","\[Sigma]df"}},
    {"D","NP",{u,u,1/2-u},"C2v",C2vzab},
    {"G","HN",{u,-u,1/2},"C2v",C2vbza},
    {"F","PH",{1/4+u,1/4-3*u,1/4+u},"C3v",{"E","C34+","C34-","\[Sigma]da","\[Sigma]dd","\[Sigma]de"}}
  };

  hskpt
];

showBCHSKpt[fullBZtype_]:=Block[{u,keys, hsk, hsk2},
  keys = Keys[BCHighSymKpt];
  If[! MemberQ[keys, fullBZtype], 
   Print["The parameter fullBZtype shoud be one of ", InputForm[keys]]; 
   Return[]];
  hsk=BCHighSymKpt[fullBZtype];
  hsk2={If[#2 != "", #1<>"("<>#2<>")", #1], Row[{"(",Row[#3,","],")"}], #4,
        Column[Row[#,","]&/@Partition[#5, UpTo[8]]]} & @@@ hsk;
  Column[{Style[fullBZtype,{Blue, Bold}], TableForm[hsk2, TableAlignments -> {Left, Top}]}, 
         Dividers->{{},{1->Black,2->Black,3->Black}}, Spacings->1]
 ]

checkBCHSKpt[fullBZtype_]:=Block[{u,keys, hsk, lat, i, j, opnames, op, k, ok=True},
  keys=Keys[BCHighSymKpt];
  If[!MemberQ[keys,fullBZtype], 
     Print["The parameter fullBZtype shoud be one of ", InputForm[keys]]; Return[]];
  hsk=BCHighSymKpt[fullBZtype];
  lat=If[StringTake[fullBZtype,-1]==")", StringTake[fullBZtype,;;-4],fullBZtype];
  For[i= 1, i<=Length[hsk], i++,
   k=hsk[[i,3]];  opnames=hsk[[i, 5]];
   For[j=1, j<=Length[opnames], j++, 
    op=getRotMatOfK[lat, opnames[[j]]];
    If[Mod[op.k-k,1]!={0,0,0}, 
     Print[fullBZtype, ": ", hsk[[i,1]], " is not invariant under ", opnames[[j]]]; o =False]; ]
   ];
  If[ok, "OK", "NotOK"]
 ]

kBCcoord[bravORsgno_, kname_String]:=Module[{ks,brav,sgno,tmp,kco,BZs,hsk,hskdict},
  If[StringQ[bravORsgno], 
    brav=bravORsgno; tmp=Values[BravLatt];
    sgno=Association[Rule@@@Transpose[{tmp,{1,3,5,16,20,23,22,75,79,146,143,205,196,197}}]][brav];
    If[!MemberQ[tmp,brav], Print["Bravais lattice should be one of ",InputForm/@tmp]; Abort[]],
    (*-------else-------*)
    sgno=bravORsgno; 
    If[!IntegerQ[sgno]||sgno<=0||sgno>230, Print["Space group number should be an integer in [1,230]."]; Abort[]];
    brav=getSGLatt[sgno];
  ]; 
  If[sgno==205&&kname=="Z'", Return[{{{1/2,u,0},{"CubiPrim"}}}]];
  If[brav=="TrigPrim"&&kname=="aF", Return[{{{0,1/2,-1/2},{"TrigPrim(a)"}}}]];
  If[brav=="TrigPrim"&&kname=="bF", Return[{{{1/2,1/2,0},{"TrigPrim(b)"}}}]];
  ks=LGIrep[sgno]//Keys;
  tmp=If[StringQ[bravORsgno]&&brav=="CubiPrim", " (Z' only for SG 205)",""];
  (* If[brav\[Equal]"TrigPrim",ks=Insert[ks,"F",4]]; *)
  If[!MemberQ[ks,kname],Print["kBCcoord: kname ",kname," is not in ",ks,tmp]; Abort[]];
  BZs=Switch[brav,
    "OrthBase", {"OrthBase(a)","OrthBase(b)"},
    "OrthBody", {"OrthBody(a)","OrthBody(b)","OrthBody(c)"},
    "OrthFace", {"OrthFace(a)","OrthFace(b)","OrthFace(c)","OrthFace(d)"},
    "TetrBody", {"TetrBody(a)","TetrBody(b)"},
    "TrigPrim", {"TrigPrim(a)","TrigPrim(b)"},
    _, {brav} ];
  hskdict=Association[Rule@@@BCHighSymKpt[#][[All,{1,3}]]]&;
  hsk=hskdict/@BZs;
  hsk=Select[{#[kname]&/@hsk,BZs}\[Transpose],!MissingQ[#[[1]]]&];
  hsk=GatherBy[hsk,First];
  hsk={#[[1,1]],#[[All,2]]}&/@hsk;
  hsk
]

(* For certain parameters, this function, in fact the last sentence RegionIntersection,
   will not work correctly in mathematica 11.2, but can work well in mathematica 12.0,
   12.1, and 13.0. For example the following input will cause problem.
      Inverse@Transpose[BasicVectors["TrigPrim"]/.{a\[Rule]15,b\[Rule]3.7,c\[Rule]4.3}] 
   For versions 12.2 and 12.3, this function can not work in any case due to a bug in
   the function DelaunayMesh. Other versions work all right.
*)
WSCell3DMesh[reciLatt_]/;MatrixQ[reciLatt,NumericQ]&&Dimensions[reciLatt]=={3,3} := 
 Module[{pad,bds,pts,pp,dmin,bmax,num,dm,conn,adj,lc,pc,cpts,hpts,hns,hp,vcells,oidx,out},
  (*\:5bf9\:4e8e\:6bd4\:8f83\:7279\:6b8a\:7684\:5012\:539f\:80de\:ff0c\:6bd4\:5982\:5f88\:6241\:7684\:ff0c\:4e3a\:4e86\:80fd\:591f\:4ea7\:751f\:6b63\:786e\:7684 Wigner-Seitz \:539f\:80de\:ff0c\:9700\:8981\:6240\:53d6\:5012
  \:683c\:70b9\:7684\:7a7a\:95f4\:5206\:5e03\:533a\:57df\:5c3d\:91cf\:5728\:5404\:4e2a\:65b9\:5411\:4e0a\:5c3a\:5ea6\:76f8\:5f53\:3002\:4e3a\:6b64\:5148\:627e\:51fa\:5012\:539f\:80de\:4e2d\:5fc3\:5230\:5012\:539f\:80de\:5404\:8868\:9762\:7684\:6700\:5c0f\:8ddd\:79bb
  dmin\:ff0c\:7136\:540e\:6c42\:51fa\:6700\:957f\:5012\:683c\:57fa\:77e2 bmax \:4e0e dmin \:7684\:6bd4\:503c num\:ff0c\:518d\:6839\:636e num \:6765\:4ea7\:751f\:5012\:683c\:70b9\:5e76\:9009\:53d6\:5230\:4e2d\:5fc3\:4e00\:5b9a
  \:8ddd\:79bb\:ff0c\:6bd4\:5982 2.0*bmax\:ff0c\:7684\:5012\:683c\:70b9\:6765\:4f7f\:7528\:3002*)
  pp=BoundaryDiscretizeGraphics[Parallelepiped[-Total[reciLatt]/2,reciLatt]];
  dmin=Abs[SignedRegionDistance[pp, {0, 0, 0}]];
  bmax=Max[Norm /@ reciLatt];
  num=Ceiling[bmax/dmin];
  pts=Flatten[Table[{i,j,k}.reciLatt, {i,-num,num-1}, {j,-num,num-1}, {k,-num,num-1}], 2];
  pts=Select[pts, Norm[#]<=2.0bmax&];
  (*------------------------------------*)
  oidx=Position[N[pts], N[{0,0,0}]][[1,1]];
  pad[\[Delta]_][{min_, max_}]:={min,max} + \[Delta](max-min){-1,1};
  bds=pad[.1]/@ MinMax/@Transpose[pts]; (* \:753b\:56fe\:8303\:56f4 *)
  dm=DelaunayMesh[pts]; 
  conn=dm["ConnectivityMatrix"[0, 1]];  (* \:5f97\:5230\:6240\:6709\:70b9\:4e0e\:7ebf\:4e4b\:95f4\:7684\:8fde\:63a5\:77e9\:9635 *)
  adj=conn.Transpose[conn]; (* \:5f97\:5230\:6240\:6709\:70b9\:4e0e\:70b9\:4e4b\:95f4\:7684\:8fde\:63a5\:77e9\:9635 *)
  lc=conn["MatrixColumns"]; (* \:5f97\:5230\:4e0e\:6240\:6709\:70b9\:76f8\:8fde\:7684\:7ebf\:7684\:7f16\:53f7 *)
  pc=adj["MatrixColumns"];  (* \:5f97\:5230\:4e0e\:6240\:6709\:70b9\:76f8\:8fde\:7684\:70b9\:7684\:7f16\:53f7 *)
  cpts= MeshCoordinates[dm]; (* \:6240\:6709\:70b9\:7684\:5750\:6807 *)
  hpts=PropertyValue[{dm,{1,lc[[oidx]]}}, MeshCellCentroid]; (* \:4e0e\:7b2coidx\:4e2a\:70b9\:76f8\:8fde\:7684\:7ebf\:7684\:4e2d\:70b9\:5750\:6807 *)
  hns=(#-cpts[[oidx]])&/@cpts[[DeleteCases[pc[[oidx]], oidx]]]; (* \:4ece\:7b2coidx\:4e2a\:70b9\:5230\:6240\:6709\:4e0e\:5176\:76f8\:8fde\:7684\:70b9\:7684\:77e2\:91cf *)
  hp=MapThread[HalfSpace, {hns, hpts}];
  vcells=BoundaryDiscretizeGraphics[#, PlotRange->bds]&/@hp;
  out=Check[RegionIntersection@@vcells, Print["WSCell3DMesh: Error occured! This error ",
            "is caused by a bug in versions 12.2 and 12.3 which cannot run the function ",
            "DelaunayMesh correctly. Versions <=12.1 and >=13.0 are all right."]; Abort[]];
  If[Head[out]===BooleanRegion, 
     Print["WSCell3DMesh: Warning! The function RegionIntersection cannot work correctly for certain ",
           "parameters in mathematica v11.2. The known versions which do not have this problem ",
           "are 12.0, 12.1, and 13.0."]
  ];
  out
 ]
 
(* Find the range of u for the kpoint in a high-symmetry line. Taking k={u,u,0} for example, 
   this function find the max u that makes k lie within or on the boundary of BZ. The parameter
   kOrList can be either one k or a list of k's. Each k must have u as variable. *) 
findURange0[basVec_, kOrList_]/;MatrixQ[basVec,NumericQ]&&Dimensions[basVec]=={3,3}:=
  findURange0[basVec, kOrList, WSCell3DMesh[Inverse@Transpose[basVec]]]
findURange0[basVec_, kOrList_, bz_]/;MatrixQ[basVec,NumericQ]&&Dimensions[basVec]=={3,3}:=
 Block[{u,chkbv,kcart,reciLatt,ds,du,eps,d,d1,d2,t,su1,forOneK},
  reciLatt=Inverse@Transpose[basVec];
  ds=1.*^-8;  eps=0.1ds;
  forOneK[k_]:=Module[{n=0, umin=0, umax=0.7},
    t=(umin+umax)/2;    kcart=k.reciLatt//Simplify;
    d1=Check[SignedRegionDistance[bz,kcart/.u->umin],
         Print["Warning: findURange0 has run into problem and always returns 0.5 in this case. This ",
               "is because RegionIntersection cannot work correctly for certain parameters in ",
               "mathematica v11.2."];  Return[0.5], 
         SignedRegionDistance::rnimpl];
    d2=SignedRegionDistance[bz,kcart/.u->umax];
    If[d1>eps&&d2>eps, Return[0]];
    d=SignedRegionDistance[bz,kcart/.u->t];
    While[Abs[d-ds]>eps, 
      (* Print[n," t=",t,", d=",d,", umin=",umin,", umax=",umax];  (*for debug*) *)
      If[d<ds,umin=t, umax=t];
      t=(umin+umax)/2;
      d=SignedRegionDistance[bz,kcart/.u->t];
      If[n++>50,Break[]]; 
    ];
    su1=Norm[(kcart/.u->1)-(kcart/.u->0)];
    du=ds/su1;
    t-du
  ];
  If[VectorQ[kOrList[[1]]], forOneK/@kOrList, forOneK[kOrList]]
 ];
 
findURange[fullBZtype_String, basVec_, kname_String]:=Block[{a,b,c,kumax,ks,brav,ckbv},
   kumax=doubleKuMax[fullBZtype];
   ks=Keys[kumax];
   If[!MemberQ[ks,kname], Return@If[MemberQ[uMaxQuarter[fullBZtype],kname], 1/4, 1/2]];
   brav=StringTake[fullBZtype,8];
   ckbv=checkBasVec[brav,basVec];
   If[!(ckbv[[1]]&&brav<>If[ckbv[[2]]!="","("<>ckbv[[2]]<>")",""]==fullBZtype), 
     Print["findURange: basic vectors ",basVec," is not of the BZ type ",fullBZtype,"."];
     Abort[]
   ];
   kumax[kname][[1]]/.ckbv[[3]]//N
 ]

(* Note that the parameter basVec is the real space basic vectors, not the reciprocal ones. *)
showBZDemo[fullBZtype_, basVec_]/;StringQ[fullBZtype]&&MatrixQ[basVec,NumericQ]&&Dimensions[basVec]=={3,3}:=
 Block[{u,keys,reciLatt,hsk,kname1,kname2,kname2p,kn1,kn2,kp1,kp2,kpos1,kpos2,t1,t2,
         i,lat,bz,g,o,mm,rs,x,y,z,cs,uMax,ckbv, realBZtype},
  keys = Keys[BCHighSymKpt];
  If[!MemberQ[keys,fullBZtype],Print["The parameter fullBZtype shoud be one of ", InputForm[keys]]; Return[]];
  lat=If[StringTake[fullBZtype,-1]==")", StringTake[fullBZtype, ;;-4], fullBZtype];
  ckbv=checkBasVec[lat,basVec]; 
  If[!First@ckbv, 
    Print["The given basic vectors ",basVec," are not of the BC type for ",lat,": ", BasicVectors[lat]];
    Abort[],
    realBZtype=lat<>If[ckbv[[2]]!="", "("<>ckbv[[2]]<>")", ""]; 
    If[fullBZtype==realBZtype, Print[fullBZtype],
      Print["The real fullBZtype of the basic vectors is ",realBZtype];
    ]
  ]; 
  reciLatt=Inverse@Transpose[basVec];
  If[MemberQ[{"TricPrim","MonoPrim","MonoBase"},realBZtype], 
     bz=BoundaryDiscretizeGraphics[Parallelepiped[-Total[reciLatt]/2,reciLatt]],
     bz=WSCell3DMesh[reciLatt]
    ];
    
  mm=Check[(#2-#1)&@@@MinMax/@(MeshCoordinates[bz]\[Transpose]),
           Print["showBZDemo: This error is inherited from the RegionIntersection problem in WSCell3DMesh."];
           Abort[] ];
  rs=Max[mm];   {x,y,z}=IdentityMatrix[3]*0.75mm;
  o={0,0,0};   cs={Red, Green, Blue};
  hsk=BCHighSymKpt[realBZtype];
  {kname1,kpos1}=Transpose[Select[hsk,#[[2]]==""&][[All,{1,3}]]]; 
  If[fullBZtype!="TricPrim",
    {kname2,kname2p,kpos2}=Transpose[Select[hsk,#[[2]]!=""&][[All,{1,2,3}]]],
    kname2=kname2p=kpos2={}];
  Print[Transpose[{kname1,kpos1}]];
  Print[{#1<>"("<>#2<>")",#3}&@@@Transpose[{kname2,kname2p,kpos2}]];
  kpos1=#.reciLatt&/@kpos1;

  For[i=1, i<=Length[kname2],i ++, 
    kn1=StringTake[kname2p[[i]],1];    kn2=StringTake[kname2p[[i]],-1];
    t1=Position[kname1,kn1];            t2=Position[kname1,kn2];  
    If[t1!={}&&t2!={}, 
      kp1=kpos1[[t1[[1,1]]]];   kp2=kpos1[[t2[[1,1]]]],
      (* else *)
      (* uMax= findURange0[basVec,kpos2[[i]],bz]; *)
      uMax=findURange[fullBZtype,basVec,kname2[[i]]];
      Print[kname2[[i]],"(",kname2p[[i]],"): ",kpos2[[i]],", u range is [0,",uMax,"]"];
      kp1=kpos2[[i]].reciLatt/.u->0;    kp2=kpos2[[i]].reciLatt/.u->uMax;
    ];    
    kpos2[[i]]={kp1,kp2,(kp1+kp2)/2}
  ];
  
  bz=BoundaryMeshRegion[bz, MeshCellStyle->{2->{Opacity[0.7]}}, PlotRegion->0.6*rs];
  g=Graphics3D[{Arrowheads[0.025 rs], {GrayLevel[0.7],Arrow[Tube[{-#,#},0.002rs]]&/@{x,y,z}},
     Text[Style[#[[1]],Italic,Bold,FontSize->20,FontFamily->"Times"], #[[2]]*1.07]&/@({{"x","y","z"},{x,y,z}}\[Transpose]),
     {#[[2]],Arrow[Tube[{o,#[[1]]},0.002rs]]}&/@({reciLatt,cs}\[Transpose]),
     Text[Style[#[[1]],#[[3]],Italic,Bold,FontSize->20,FontFamily->"Times"], #[[2]]*1.07]&/@({
          "\!\(\*SubscriptBox[\(g\),SubscriptBox[\(\"\"\),\("<>#<>"\)]]\)"&/@{"1","2","3"}, reciLatt, cs}\[Transpose]),
     PointSize[0.008rs], {#[[2]], Point[#[[1]]]}&/@({reciLatt/2, cs}\[Transpose]),   
     {PointSize[Large],Point[#2],Text[Style[#1,Italic,FontSize->18], #2*1.15]}&@@@({kname1,kpos1}\[Transpose]),
     If[kname2=={}, {}, {Thick,Purple,Line[#2[[;;2]]],
        Text[Style[#1,Italic,FontSize->18], #2[[3]]*1.15]}&@@@({kname2,kpos2}\[Transpose])]
     }, Boxed -> False];
  Show[bz, g, ImageSize->600, ViewProjection -> "Orthographic"]
 ]
 
showBZDemo[fullBZtype_]/;StringQ[fullBZtype]:=
 Block[{lat,bvec},
  lat=If[StringTake[fullBZtype,-1]==")", StringTake[fullBZtype, ;;-4], fullBZtype];
  bvec=Block[{a=2,b,c,\[Alpha],\[Beta],\[Gamma]},
    If[fullBZtype=="TricPrim", a=2; b=3; c=4; \[Alpha]=70*Pi/180.; \[Beta]=100*Pi/180.; \[Gamma]=110*Pi/180.];
    If[MemberQ[{"MonoPrim","MonoBase"},fullBZtype], a=2; b=3; c=4; \[Gamma]=110*Pi/180.];
    If[fullBZtype=="OrthPrim", a=2; b=3; c=4];
    If[fullBZtype=="OrthBase(a)", a=3; b=2; c=4];
    If[fullBZtype=="OrthBase(b)", a=2; b=3; c=4];
    If[fullBZtype=="OrthBody(a)", a=4; b=2; c=3];
    If[fullBZtype=="OrthBody(b)", a=3; b=4; c=2];
    If[fullBZtype=="OrthBody(c)", a=2; b=3; c=4];
    If[fullBZtype=="OrthFace(a)", a=3; b=3.5; c=4];
    If[fullBZtype=="OrthFace(b)", a=3; b=4; c=2];
    If[fullBZtype=="OrthFace(c)", a=4; b=2; c=3];
    If[fullBZtype=="OrthFace(d)", a=2; b=3; c=4];
    If[fullBZtype=="TetrPrim"   , a=b=2; c=3];
    If[fullBZtype=="TetrBody(a)", a=b=3; c=2];
    If[fullBZtype=="TetrBody(b)", a=b=2; c=4];
    If[fullBZtype=="TrigPrim(a)", a=5; c=2];
    If[fullBZtype=="TrigPrim(b)", a=2; c=3];
    If[fullBZtype=="HexaPrim",    a=2; c=3];
    BasicVectors[lat]  
   ];
   showBZDemo[fullBZtype,bvec]
 ];


(* ::Section:: *)
(*Identify BC high - symmetry k points*)


(* Note: the result of Mod[Round[-3.0,1.0*^-5],1] is 1, not 0! 
   For safe, had better use Rationalize rather than Round. *)
keqmod[k1_,k2_,prec_]:=Module[{dk}, 
  dk = Mod[Rationalize[k2 - k1, prec], 1]; 
  (*dk = Mod[Map[If[NumericQ[#], Print[#];Round[#,prec],#]&, k2 - k1, -1], 1]//Chop; *) 
  If[!VectorQ[dk,NumberQ], Return[False]]; dk == 0*dk 
]
keqmod[k1_,k2_]:=keqmod[k1,k2,1.0*^-5]

(* Identify k-points according to Tab. 3.6. The parameter fullBZtype is a certain type of BZ
   such as "OrthPrim", "OrthBase(a)", "OrthBase(b)", ... *) 
   
(* The output of identifyBCHSKpt may be of the folowing form:

 {{{0.15,0.15,0.},"\[CapitalSigma]","\[CapitalGamma]\[CapitalSigma]","C2v",{u,u,0},"E",{u,u,0},{0,0,0},u\[Rule]0.15},
  {{0.15,0.15,0.},"C","YC","C2v",{-1/2+u,1/2+u,0},"C2y",{-1/2-u,1/2-u,0},{1,0,0},u\[Rule]0.35}}
  
 {{{0.3,0.3,0},"\[CapitalSigma]","\[CapitalGamma]\[CapitalSigma]","C2v",{u,u,0},"E",{u,u,0},{0,0,0},u\[Rule]0.3},
  {{0.3,0.3,0},"C","YC","C2v",{-1/2+u,1/2+u,0},"C2y",{-1/2-u,1/2-u,0},{1,0,0},u\[Rule]0.2}}
  
 {{{0.5,0.5,0.5},"T","","D2h",{-1/2,1/2,1/2},"E",{-1/2,1/2,1/2},{1,0,0}}}
 
 {{{0.5,0.4,0.5},"UN","",{"E","\[Sigma]z"}}}
 
 {{{0.5,0.4,0.3},"GP","","C1"}}
*)
(* 
  When version<=1.0.4, the behavior is "selectNoTranslation"\[Rule]True, that is for high-symmetry line
  the items with no rotation are first selected and then the items with no translations are selected.
  However, when "items with no rotation" do not exist this may lead to items with different rotations
  are selected for wave vectors differing by a reciprocal lattice vector (RLV). For example, 
  identifyBCHSKpt["OrthFace(a)",{-0.1,-0.6,-0.5}][[2]]  gives
  {{-0.1,-0.6,-0.5},"H","YH","C2v",{u,-(1/2)+u,-(1/2)},"C2y",{-u,-(1/2)-u,-(1/2)},{0,0,0},u\[Rule]0.1}
  identifyBCHSKpt["OrthFace(a)",{ 0.9,-0.6,-0.5}][[2]]  gives
  {{0.9,-0.6,-0.5},"H","YH","C2v",{u,-(1/2)+u,-(1/2)},"C2x",{-u,1/2-u,1/2},{1,-1,-1},u\[Rule]0.1}
  The first selects "C2y" and the second selects "C2x". This will leads to different {S|w}.
  And different {S|w} may leads to differnt labels for LGIR.
  In fact, {-0.1,-0.6,-0.5} is equvilent to {0.9,-0.6,-0.5} and they should have the same LGIR labels.
  To achieve this, we change the default behavior to "selectNoTranslation"\[Rule]False, that is only items 
  with no rotation are selected first and whether translations are zero is not cared. 
  This makes sure that k-points differing by RLV all have the same {S|w}.
  Fortunately, after systematic check it's found that this change will not affect the result of krepBCStoBC.
  Note that:
  SetOptions[identifyBCHSKpt,"selectNoTranslation"\[Rule]True] will change the behavior to the former (\[LessEqual]v1.0.4). 
  And this option should be used only for debug purpose.
*)
Options[identifyBCHSKpt]={"selectNoTranslation"->False};
identifyBCHSKpt[fullBZtype_, klist_, OptionsPattern[]]/;MatrixQ[klist,NumericQ]&&Dimensions[klist][[2]]==3 :=
 Block[{u, keys, hsk, lat, stars0, stars, nstars, kstar, kstar0, Gk, G0, i, j, k1, iu,
         keq, sub, identifyOneK, hskp, prec = 1*^-5},
  keys = Keys[BCHighSymKpt];
  If[!MemberQ[keys,fullBZtype],
    Print["identifyBCHSkpt: The parameter fullBZtype, ",InputForm[fullBZtype],
          ", shoud be one of ", InputForm[keys]];
    Abort[] ];
  hsk = BCHighSymKpt[fullBZtype];
  lat = If[StringTake[fullBZtype,-1] == ")", StringTake[fullBZtype, ;;-4], fullBZtype];
  G0 = hsk[[1, 5]];
  keq[k1_,k2_] := Module[{dk}, dk = Rationalize[k2 - k1, prec];
                         If[!VectorQ[dk,NumberQ], Return[False]]; dk == 0*dk ];
  stars0=Table[{i, getRotMatOfK[lat,i].hsk[[j,3]]}, {j, Length[hsk]}, {i,G0}];
  stars0=DeleteDuplicates[#, keq[#1[[2]],#2[[2]]]&]& /@ stars0;
  stars=DeleteDuplicates[#, keqmod[#1[[2]],#2[[2]]]&]& /@ stars0;  
  nstars=Length/@stars;
  identifyOneK[k_] := Block[{re = {}},
    kstar0={#, getRotMatOfK[lat, #].k} & /@ G0;
    Gk=Select[kstar0, keqmod[k, #[[2]]] &][[All, 1]];
    If[Gk=={"E"}, re = {{"GP", "", "C1"}}; Goto["endfor"]];    (* GP means general point *)
    kstar=DeleteDuplicates[kstar0, keqmod[#1[[2]],#2[[2]]]&];
    For[i=1, i<= Length[hsk], i++,
     If[nstars[[i]] != Length[kstar], Continue[]];
     If[Position[stars[[i, 1]], u] == {}, (*for High-symmetry k point *)
      hskp=True; 
      (* The 1st round: for direct equal *)
      For[j = 1, j <= Length[stars0[[i]]], j++, k1=stars0[[i,j,2]];
       If[keq[k, k1],
        re=Append[re, Join@@{hsk[[i,{1,2,4,3}]], stars0[[i,j]], {Rationalize[k-k1,prec]}}];
        ]];  
      (* The 2nd round: equal after mod *)
      For[j = 1, j <= Length[stars[[i]]], j++, k1=stars[[i,j,2]];
       If[keqmod[k, k1],
        re=Append[re, Join@@{hsk[[i,{1,2,4,3}]], stars[[i,j]], {Rationalize[k-k1,prec]}}]; 
        ]];
      re=DeleteDuplicates[re];
      If[re!={},Break[]],
      (*-------- Else:  for High-symmetry k line --------*) 
      hskp=False;
      For[j = 1, j <= Length[stars0[[i]]], j++, k1=stars0[[i,j,2]];
      (* identifyBCHSKpt["HexaPrim",{4/5,1/10,1/2}] should return two items "S" and "S'", but if
         iu takes only Position[k1,u][[1,1]] the output only has the "S'" term. *)
       Do[
        sub=Solve[{k[[iu]]==k1[[iu]]}, u];
        If[sub == {}, Continue[]];
        sub=sub[[1, 1]];
        sub=sub[[1]]->Mod[sub[[2]],1];
        If[keqmod[k, k1/.sub],
         re=Append[re, Join@@{hsk[[i,{1,2,4,3}]], stars0[[i,j]], {Rationalize[k-k1/.sub,prec], sub}}]; Break[]
        ];
       ,{iu, Position[k1,u][[All,1]]}]; (* end do*)
       ]; (* end for j *)
      ];  (* end if hskp *)
     ]; (* end for i *)
    Label["endfor"];    
    (* There may be more than one high-symmetry-line point matching a given k. 
       In this case we choose for each kname the one with smallest u. *)
    If[Length[re]>1,  
      If[hskp,
        (*For hskp\[Equal]True, we prefer to select the one with no rotation. *)
        re=With[{s=Select[re,#[[5]]=="E"&]}, If[s!={},s,{re[[1]]}]],
        (*------else: hskp\[Equal]False------*)
        re=Gather[re,First[#1]==First[#2]&];  
        re=MinimalBy[#, Round[Last[#][[2]],1.*^-14] &]&/@re; 
        (*For hskp\[Equal]False, we prefer to first select the one with no rotation, then whether to select the one
                           with no translation depends on the option "selectNoTranslation". *)
        re=With[{s=Select[#,#[[5]]=="E"&]}, 
             If[s!={},{First[s]},
               If[OptionValue["selectNoTranslation"]===True,
                 With[{s2=Select[#,#[[-2]]=={0,0,0}&]}, If[s2!={},{First[s2]},{First[#]}]], (*the behavior of version\[LessEqual]v1.0.4, for debug purpose*)
                 {First[#]} (*default behavior of version \[GreaterEqual] v1.0.5*)
               ]
           ]]&/@re;
        re=Flatten[re,1];      
        ];
      ];
    If[re=={}, re={{"UN", "", Gk}}];  (* for unnamed k points*)
    Prepend[#, k]&/@re
    ];
  identifyOneK /@ klist
 ]

identifyBCHSKpt[fullBZtype_, k_, OptionsPattern[]]/;VectorQ[k,NumericQ]&&Dimensions[k]=={3} :=
  With[{re = identifyBCHSKpt[fullBZtype, {k}, "selectNoTranslation"->OptionValue["selectNoTranslation"]]}, 
       If[re === Null, re, re[[1]]]]


(* These kpoints are all in high-symmetry lines and each pair are in the same k-star.
   One k in this case can be identified as either one in the pair but with different u.
   But only one (except the critical case) has u in its range. In each pair, the two max 
   values of u add to 0.5. Note that k points with u<umax lie within BZ or on the boundary. *)
(*
doubleKnames[fullBZtype_]:=Switch[fullBZtype,
  "OrthBase(a)", {{"\[CapitalDelta]","F"},{"B","G"}},
  "OrthBase(b)", {{"A","E"},{"\[CapitalSigma]","C"}},
  "OrthBody(a)", {{"\[CapitalLambda]","G"},{"\[CapitalDelta]","U"}},
  "OrthBody(b)", {{"\[CapitalLambda]","G"},{"\[CapitalSigma]","F"}},
  "OrthBody(c)", {{"\[CapitalDelta]","U"},{"\[CapitalSigma]","F"}},
  "OrthFace(a)", {{"G","H"},{"C","A"},{"D","B"}},
  "OrthFace(b)", {{"\[CapitalLambda]","Q"},{"G","H"}},
  "OrthFace(c)", {{"\[CapitalDelta]","R"},{"D","B"}},
  "OrthFace(d)", {{"\[CapitalSigma]","U"},{"C","A"}},
  "TetrBody(a)", {{"\[CapitalLambda]","V"}},
  "TetrBody(b)", {{"\[CapitalSigma]","F"},{"U","Y"}},
  "TrigPrim(a)", {{"\[CapitalLambda]","P"}},
  "TrigPrim(b)", {{"B","Y"},{"\[CapitalSigma]","Q"}},
  "HexaPrim",    {{"T","T'"},{"S","S'"}},
  "CubiBody",    {{"\[CapitalLambda]","F"}},
  "CubiFace",    {{"\[CapitalSigma]","S"}},
  _,             {}
]
*)

doubleKuMax[fullBZtype_]:=Block[{a,b,c,tri={1,1,1}},
 Switch[fullBZtype,
  "OrthBase(a)", <|"\[CapitalDelta]"->{1/4+b^2/(4a^2), 1/4, 1/2}, "F"->{1/4-b^2/(4a^2), 0 ,1/4},
                   "B"->{1/4+b^2/(4a^2), 1/4, 1/2}, "G"->{1/4-b^2/(4a^2), 0, 1/4}|>,
  "OrthBase(b)", <|"A"->{1/4+a^2/(4b^2), 1/4, 1/2}, "E"->{1/4-a^2/(4b^2), 0, 1/4},
                   "\[CapitalSigma]"->{1/4+a^2/(4b^2), 1/4, 1/2}, "C"->{1/4-a^2/(4b^2), 0, 1/4}|>,
  "OrthBody(a)", <|"\[CapitalLambda]"->{1/4+c^2/(4a^2), 1/4, 1/2}, "G"->{1/4-c^2/(4a^2), 0, 1/4},
                   "\[CapitalDelta]"->{1/4+b^2/(4a^2), 1/4, 1/2}, "U"->{1/4-b^2/(4a^2), 0, 1/4}|>,
  "OrthBody(b)", <|"\[CapitalLambda]"->{1/4+c^2/(4b^2), 1/4, 1/2}, "G"->{1/4-c^2/(4b^2), 0, 1/4},
                   "\[CapitalSigma]"->{1/4+a^2/(4b^2), 1/4, 1/2}, "F"->{1/4-a^2/(4b^2), 0, 1/4}|>,
  "OrthBody(c)", <|"\[CapitalDelta]"->{1/4+b^2/(4c^2), 1/4, 1/2}, "U"->{1/4-b^2/(4c^2), 0, 1/4},
                   "\[CapitalSigma]"->{1/4+a^2/(4c^2), 1/4, 1/2}, "F"->{1/4-a^2/(4c^2), 0, 1/4}|>,
  "OrthFace(a)", <|"G"->{1/4-c^2/(4a^2)+c^2/(4b^2), 0, 1/2}, "H"->{1/4+c^2/(4a^2)-c^2/(4b^2), 0, 1/2},
                   "C"->{1/4-a^2/(4b^2)+a^2/(4c^2), 0, 1/2}, "A"->{1/4+a^2/(4b^2)-a^2/(4c^2), 0, 1/2},
                   "D"->{1/4-b^2/(4a^2)+b^2/(4c^2), 0, 1/2}, "B"->{1/4+b^2/(4a^2)-b^2/(4c^2), 0, 1/2}|>,
  "OrthFace(b)", <|"\[CapitalLambda]"->{1/4+c^2/(4a^2)+c^2/(4b^2), 1/4, 1/2}, "Q"->{1/4-c^2/(4a^2)-c^2/(4b^2), 0, 1/4},
                   "G"->{1/4-c^2/(4a^2)+c^2/(4b^2), 0, 1/2}, "H"->{1/4+c^2/(4a^2)-c^2/(4b^2), 0, 1/2}|>,
  "OrthFace(c)", <|"\[CapitalDelta]"->{1/4+b^2/(4a^2)+b^2/(4c^2), 1/4, 1/2}, "R"->{1/4-b^2/(4a^2)-b^2/(4c^2), 0, 1/4},
                   "D"->{1/4-b^2/(4a^2)+b^2/(4c^2), 0, 1/2}, "B"->{1/4+b^2/(4a^2)-b^2/(4c^2), 0, 1/2}|>,
  "OrthFace(d)", <|"\[CapitalSigma]"->{1/4+a^2/(4b^2)+a^2/(4c^2), 1/4, 1/2}, "U"->{1/4-a^2/(4b^2)-a^2/(4c^2), 0, 1/4},
                   "C"->{1/4-a^2/(4b^2)+a^2/(4c^2), 0, 1/2}, "A"->{1/4+a^2/(4b^2)-a^2/(4c^2), 0, 1/2}|>,
  "TetrBody(a)", <|"\[CapitalLambda]"->{1/4+c^2/(4a^2), 1/4, 1/2}, "V"->{1/4-c^2/(4a^2), 0, 1/4}|>,
  "TetrBody(b)", <|"\[CapitalSigma]"->{1/4+a^2/(4c^2), 1/4, 1/2}, "F"->{1/4-a^2/(4c^2), 0, 1/4},
                   "U"->{1/2-a^2/(2c^2), 0, 1/2}, "Y"->{a^2/(2c^2), 0, 1/2}|>,
  "TrigPrim(a)", <|"\[CapitalLambda]"->{1/6+(2c^2)/(3a^2), 1/6, 1/2}, "P"->{1/3-(2c^2)/(3a^2), 0, 1/3}|>,
  "TrigPrim(b)", <|"B"->{1/3-a^2/(6c^2), 0, 1/3}, "Y"->{1/6+a^2/(6c^2), 1/6, 1/2},
                   "\[CapitalSigma]"->{1/3+a^2/(12c^2), 1/3, 1/2}, "Q"->{1/6-a^2/(12c^2), 0, 1/6}|>,
  "HexaPrim",    <|"T"->1/3tri, "T'"->1/6tri, "S"->1/3tri, "S'"->1/6tri|>,
  "CubiBody",    <|"\[CapitalLambda]"->1/4tri, "F"->1/4tri|>,
  "CubiFace",    <|"\[CapitalSigma]"->3/8tri, "S"->1/8tri|>,
  _,             <||>
 ]
]

(*List of klines which have umax=1/4 but are not have double knames.*)
uMaxQuarter[bravOrFullBZtype_String]:=Switch[StringTake[bravOrFullBZtype,8],
  "OrthBody", {"P","D","Q"},    "TetrBody", {"W","Q"},
  "CubiFace", {"Z","Q"},        "CubiBody", {"D"},
  _, {}
]


(* Identify k-points according to a certain space group. The 2nd parameter BZtypeOrBasVec may be 
   either "a","b","c","d" or the basic vectors of the lattice. 
   Note that basic vectors MUST be in the forms given in BC Tab.3.1. *)  
Options[identifyBCHSKptBySG]={"allowtwok"->False};
identifyBCHSKptBySG[sgno_, BZtypeOrBasVec_, klist_, OptionsPattern[]]/;
 MatrixQ[klist,NumericQ]&&Dimensions[klist][[2]]==3 :=
 Block[{u, a,b,c, hsk, lat, fullBZtype, outklist, GSQ, P, i, j, k1, iu, uRange, dbkn, dbk, dbkur,
        keq, sub, identifyOneK, prec = 1*^-5,chkbv, kk, hslk, tmp},
  lat=getSGLatt[sgno];     fullBZtype=lat;
  If[MatrixQ[BZtypeOrBasVec], 
    If[!MatrixQ[BZtypeOrBasVec,NumericQ], 
      Print["identifyBCHSKptBySG: The basic vectors ",BZtypeOrBasVec," are not numeric."]; Abort[]];
    chkbv=checkBasVec[lat,BZtypeOrBasVec];
    If[chkbv[[1]], fullBZtype=lat<>If[chkbv[[2]]=="","","("<>chkbv[[2]]<>")"],
      Print["identifyBCHSKptBySG: the basic vectors ",BZtypeOrBasVec," are not of the BC form for ", 
             lat, ": ",BasicVectors[lat]];  Return[{{}}];
      ],
    If[MemberQ[{"OrthBase","OrthBody","OrthFace","TetrBody","TrigPrim"},lat],
      fullBZtype=lat<>"("<>BZtypeOrBasVec<>")"]
   ];
  hsk = BCHighSymKpt[fullBZtype];    P=hsk[[1,5]];
  keq[k1_,k2_] := Module[{dk}, dk = Rationalize[k2 - k1, prec];
                         If[!VectorQ[dk,NumberQ], Return[False]]; dk == 0*dk ];
  outklist=identifyBCHSKpt[fullBZtype,klist];
  GSQ=getSGElemAndSQ[sgno];
  
  (* Initialize all uRange=0.5 first, and then set them to the right value. *)
  hslk=hsk[[Select[Range[Length[hsk]],hsk[[#,2]]!=""&],1]];
  (uRange[#]=0.5)&/@hslk; 
  (uRange[#]=0.25)&/@uMaxQuarter[fullBZtype];
  dbkn=Keys@doubleKuMax[fullBZtype];
  If[dbkn!={},
    If[MatrixQ[BZtypeOrBasVec,NumericQ],
      (*dbk=hsk[[Position[hsk[[All,1]],#][[1,1]],3]]&/@dbkn;  
      dbkur=findURange0[BZtypeOrBasVec,dbk];*)
      dbkur=findURange[fullBZtype,BZtypeOrBasVec,#]&/@dbkn;
      For[i=1,i<=Length[dbkn],i++,uRange[dbkn[[i]]]=dbkur[[i]]],
      (* When no basic vectors are given, if two k names are not allowed then we just set uRange 
         as 1/4 even if this may be incorrect, else uRange uses the upper limit of umax. *)
      For[i=1,i<=Length[dbkn],i++,
        uRange[dbkn[[i]]]=If[!OptionValue["allowtwok"], 1/4, doubleKuMax[fullBZtype][dbkn[[i]]][[3]]]]; 
      (* The following cases have definite uRange independent of basic vectors. *)
      If[fullBZtype=="CubiFace",uRange["\[CapitalSigma]"]=3/8; uRange["S"]=1/8];
      If[fullBZtype=="CubiBody",uRange["\[CapitalLambda]"]=1/4; uRange["F"]=1/4];
      If[fullBZtype=="HexaPrim",uRange["T"]=uRange["S"]=1/3; uRange["T'"]=uRange["S'"]=1/6]
    ];
  ];
  (* Process outklist. Select one k for each item of outklist according to uRange. *)
  For[i=1,i<=Length[outklist],i++,
    kk=outklist[[i,All,2]]; 
    If[SubsetQ[dbkn,kk], 
      tmp=Select[Range[Length[kk]], Round[outklist[[i,#,-1,2]],1.*^-5]<=Round[uRange[kk[[#]]],1.*^-5]&];
      tmp=outklist[[i,tmp]];   
      If[(!OptionValue["allowtwok"])||MatrixQ[BZtypeOrBasVec],
        (*If "allowtwok"\[Rule]Flase, tmp has two entries only when u\[Equal]umax, i.e. the critical case, 
          otherwise only one entry of kinfo is selected from tmp. For the critical case, here we choose one k to output.*)
        (*To keep the similar behavior with identifyBCHSKpt, change "select no translation" to "select no rotation"*)
        tmp=With[{s=Select[tmp,#[[6]]=="E"&]}, If[s!={},First[s],First[tmp]]];
      ];
      If[Length[Dimensions[tmp]]==1,
        outklist[[i]]=Append[tmp,uRange[tmp[[2]]]],
        outklist[[i]]=Append[#,uRange[#[[2]]]]&/@tmp
      ],
      (*-------else--------*)
      If[SubsetQ[hslk,kk],
        outklist[[i]]=Append[outklist[[i,1]],uRange[kk[[1]]]],
        outklist[[i]]=outklist[[i,1]]
      ]
    ]
  ];   
        
  identifyOneK[outk_]:=Module[{k,newk,op,G0,Gk0,Gk0name,star0,starS,starP,kstar,R,kbd,Rkbd,sec,tag,Zpstar,re},
    If[GSQ[[2]]=={}&&GSQ[[3]]=={}, (* For case in which point gorup is holosymmetric. *)
      If[Length[outk]==4, Return[outk]]; (* UN or GP *)
      op=GSQ[[Sequence@@(Position[GSQ,outk[[6]]][[1,;;-2]])]];
      newk=Append[outk,"in G"];   newk[[6]]=op;  
      Return[newk]
     ];
     
    If[outk[[2]]=="GP",Return[outk]];
    G0=GSQ[[1,All,1]];   
    k=outk[[1]];    
    Gk0=Select[{#, getRotMatOfK[lat, #].k}&/@G0, keqmod[k, #[[2]]] &][[All, 1]];
    If[outk[[2]]=="UN", 
      If[Sort[Gk0]==Sort[outk[[4]]], Return[outk]];
      If[Gk0=={"E"},Return[{k,"GP", "", "C1"}]];
      Return[{k,"UN","",Gk0}];
     ];
    Gk0name=detPointGroup[Gk0]; If[Gk0name=="undetermined",Print[Gk0]];

    kbd=outk[[5]];  (* The k in basic domain *)
    If[Position[outk//Last,u]!={},kbd=kbd/.Last[outk]];
    starP={#, getRotMatOfK[lat, #].kbd}&/@P;     
    star0={#, getRotMatOfK[lat, #].kbd}&/@G0; 
    R=outk[[6]];
    op=GSQ[[Sequence@@(Position[GSQ,R][[1,;;2]])]];
    Rkbd=Association[Rule@@@starP][R];        
    newk=outk;
    (* Format of outk is like: {k,"S","XR","C2v",{u,1/2,u},"C34+",{u,-u,-(1/2)},{0,1,1},u\[Rule]0.4, 0.5} *)
    (* R\:628akbd\:53d8\:5230k\:ff0c\:4e0b\:9762\:5904\:7406R\:4e0d\:5728\:7a7a\:95f4\:7fa4\:7684\:70b9\:7fa4G0\:91cc\:7684\:60c5\:51b5\:ff0c\:5982\:679c\:6b64\:65f6\:6709\:4e00\:4e2a\:5728G0\:91cc\:7684\:65cb\:8f6c\:540c\:6837
       \:80fd\:628akbd\:53d8\:5230k\:7684\:8bdd\:ff0c\:5219\:7528\:5b83\:6240\:5bf9\:5e94\:7684\:7a7a\:95f4\:7fa4\:64cd\:4f5cop\:66ff\:4ee3R\:5bf9\:5e94\:7684\:7a7a\:95f4\:7fa4\:64cd\:4f5c *)
    If[MemberQ[G0,R], tag="in G",
      tag="not in G";
      sec=Select[star0,keqmod[Rkbd,#[[2]]]&][[All,1]];
      If[sec!={},
        op=GSQ[[Sequence@@(Position[GSQ,First[sec]][[1,;;2]])]];
        newk[[7]]=getRotMatOfK[lat,op[[1]]].newk[[5]];
        (*Note that Rationalize[-0.1, 0.1] returns -1/9 *)
        tmp=If[Position[newk[[7]],u]!={}, newk[[-2]], {}];
        newk[[8]]=Rationalize[(newk[[1]]-newk[[7]])/.tmp,0.1];
        (* Print["k=",k,", S0=",R,", S in G0:",sec]; (* for debug *) *)
        tag="in G";
       ]
     ];
    (* \:82e5R\:5728Q\:4e2d\:ff0c\:5219\:5c1d\:8bd5\:4ece GSQ[[2]] \:4e2d\:627e\:5230\:64cd\:4f5c\:4f7f\:5176\:540c\:6837\:628ak\:8f6c\:5230 R.k\:3002
       \:5426\:5219 getLGIRtab[144,{0.2,-0.6,-0.5}] \:4f1a\:51fa\:9519\:ff0c\:5176\:4e2dk\:70b9\:4e3a -S' \:70b9 *)
    starS={#, getRotMatOfK[lat, #].kbd}&/@GSQ[[2,All,1]]; 
    If[MemberQ[GSQ[[3]],R],
      sec=Select[starS,keqmod[Rkbd,#[[2]]]&][[All,1]];
      If[sec!={},
        op=GSQ[[Sequence@@(Position[GSQ,First[sec]][[1,;;2]])]];
        newk[[7]]=getRotMatOfK[lat,op[[1]]].newk[[5]];
        tmp=If[Position[newk[[7]],u]!={}, newk[[-2]], {}];
        newk[[8]]=Rationalize[(newk[[1]]-newk[[7]])/.tmp,0.1];
       ]
     ];
     
    If[sgno==205&&outk[[2]]=="Z"&&!MemberQ[G0,R],      
      Zpstar=Table[{i, getRotMatOfK[lat,i].{1/2,u,0}}, {i,G0}];
      Zpstar=DeleteDuplicates[Zpstar, keqmod[#1[[2]],#2[[2]]]&];
      re={};
      For[j = 1, j <= Length[Zpstar], j++, k1=Zpstar[[j,2]];
        iu=Position[k1,u][[1,1]];
        sub=Solve[{k[[iu]]==k1[[iu]]}, u];
        If[sub == {}, Continue[]];
        sub=sub[[1, 1]];
        sub=sub[[1]]->Mod[sub[[2]],1];
        If[keqmod[k, k1/.sub],
          op=GSQ[[Sequence@@(Position[GSQ,Zpstar[[j,1]]][[1,;;2]])]];
          Rkbd=Zpstar[[j,2]];
          re=Append[re, Join@@{{"Z'","X'M",Gk0name,{1/2,u,0}},{op,Rkbd}, {Rationalize[k-k1/.sub,prec], sub, 0.5}}]
         ]
       ]; 
      If[Length[re]>1, re=First@MinimalBy[re, #[[-2,2]] &]];
      newk=Append[Prepend[re,k],"in G"];
      Return[newk];
    ];
    
    If[StringQ[op],  Print["k=",k,", S in Q: ", R]]; (* This case should not appear. *)
    newk=Append[newk,tag];   newk[[6]]=op;   newk[[4]]=Gk0name;
    Return[newk]
  ];

  If[Length[Dimensions[#]]==1, identifyOneK[#], identifyOneK/@#]&/@outklist
]

identifyBCHSKptBySG[sgno_, BZtypeOrBasVec_, k_, OptionsPattern[]]/;VectorQ[k,NumericQ]&&Dimensions[k]=={3} :=
  identifyBCHSKptBySG[sgno,BZtypeOrBasVec,{k}, "allowtwok"->OptionValue["allowtwok"]][[1]] 


(* ::Section:: *)
(*Group theory utilities*)


(* ::Subsection:: *)
(*Multiplication of Seitz symbols and elems of central extension*)


(* Definition of the multiplication and inverse of space group Seitz symbols *)
SeitzTimes[{R1_,v1_},{R2_,v2_}]:={R1.R2,R1.v2+v1}
SeitzTimes[Rv1_,Rv2_,more__]:=Fold[SeitzTimes,Rv1,{Rv2,more}]
(* g1_\[CircleDot]g2__:=Fold[SeitzTimes,g1,{g2}] *)
invSeitz[{R_,v_}]:={Inverse[R],-Inverse[R].v}
powerSeitz[{R_,v_},n_Integer]/;n>=0:=If[n==0,{IdentityMatrix[3],{0,0,0}},
                                             Fold[SeitzTimes,{R,v},Table[{R,v},{i,n-1}]]]
SeitzTimes[brav_][{Rname1_,v1_},{Rname2_,v2_}]:=Module[{R1,R2},
  R1=getRotMat[brav,Rname1];  R2=getRotMat[brav,Rname2];
  {getRotName[brav,R1.R2],R1.v2+v1}
]
SeitzTimes[brav_][Rv1_,Rv2_,more__]:=Fold[SeitzTimes[brav],Rv1,{Rv2,more}]
powerSeitz[brav_][{Rname_,v_},n_Integer]/;n>=0:=If[n==0,{"E",{0,0,0}},
                                        Fold[SeitzTimes[brav],{Rname,v},Table[{Rname,v},{i,n-1}]]]
invSeitz[brav_][{Rname_,v_}]:=Block[{R=getRotMat[brav,Rname],iR}, iR=invRotMat[R]; {getRotName[brav,iR],-iR.v}]

CentExtTimes[brav_,adict_][{R1_,alpha_},{R2_,beta_}]:=Module[{g,R1mat,R1name,R2mat,R2name},
  g=adict["g"];  
  (* Note: Cannot use g=Max@Values[adict]+1. e.g. No.91 kname="S", a={{1,0},{0,0}} but g=4 not g=2! *)
  If[MatrixQ[R1,NumberQ],
    R1mat=R1;  R2mat=R2;  R1name=getRotName[brav,R1];  R2name=getRotName[brav,R2],
    R1name=R1;  R2name=R2;  R1mat=getRotMat[brav,R1];  R2mat=getRotMat[brav,R2]
  ];
  {getRotName[brav,R1mat.R2mat],Mod[alpha+beta+adict[{R1name,R2name}],g]}
]
CentExtPower[brav_,adict_][{R_,alpha_},n_Integer]/;n>=0:=If[n==0,{"E",0},
                           Fold[CentExtTimes[brav,adict],{R,alpha},Table[{R,alpha},{i,n-1}]]]



(* ::Subsection:: *)
(*Multiplication of rotation names*)


RotTimes[Rname1_String,Rname2_String]:=Module[{crots,hrots,brav,EI={"E","I"},tmp},
  crots=RotMat[[1]]["CubiPrim"];
  hrots=RotMat[[1]]["HexaPrim"];
  If[MemberQ[EI, Rname1], 
    brav=If[MemberQ[crots, Rname2], "CubiPrim", "HexaPrim"],
    brav=If[MemberQ[crots, Rname1], "CubiPrim", "HexaPrim"];
    If[!MemberQ[EI, Rname2], 
      tmp=If[MemberQ[crots, Rname2], "CubiPrim", "HexaPrim"];
      If[brav!=tmp, Print["RotTimes: Error! Rname1 and Rname2 should be in the same list of either\n",crots,"\nor\n",hrots]; Abort[]];
    ] 
  ];
  getRotName[brav, getRotMat[brav,Rname1].getRotMat[brav,Rname2]]
]
RotTimes[R1_, R2_, more__]:=Fold[RotTimes, R1, {R2,more}]

powerRot[Rname_String, n_Integer]/;n>=0:=If[n==0, "E", Fold[RotTimes,Rname,Table[Rname,n-1]]]

invRot[Rname_String]:=Module[{brav},
  brav=If[MemberQ[RotMat[[1]]["CubiPrim"], Rname], "CubiPrim", "HexaPrim"];
  getRotName[brav,invRotMat@getRotMat[brav,Rname]]
]



(* ::Subsection:: *)
(*Spin Rotations (Tab. 6.1 and Tab. 6.7) and multiplications for double space group (DSG)*)


(* Note that the matrix of C32- in BC book tab 6.1 and 6.7 are wrong. C34+ in the tab 6.1 of my BC
   book is also wrong, but the one in the pdf version is right. *)
SpinRot0=<|"E"->{{1,0},{0,1}}, "C2x"->{{0,-I},{-I,0}},
  "C2y"->{{0,-1},{1,0}}, "C2z"->{{-I,0},{0,I}},
  "C31+"->{{1+I,1-I},{-1-I,1-I}}/2, "C32+"->{{1+I,-1+I},{1+I,1-I}}/2,
  "C33+"->{{1-I,-1-I},{1-I,1+I}}/2, "C34+"->{{1-I,1+I},{-1+I,1+I}}/2,
  "C31-"->{{1-I,-1+I},{1+I,1+I}}/2, "C32-"->{{1-I,1-I},{-1-I,1+I}}/2,
  "C33-"->{{1+I,1+I},{-1+I,1-I}}/2, "C34-"->{{1+I,-1-I},{1-I,1-I}}/2,
  "C4x+"->{{1,-I},{-I,1}}/Sqrt[2], "C4y+"->{{1,1},{-1,1}}/Sqrt[2],
  "C4z+"->{{1+I,0},{0,1-I}}/Sqrt[2], "C4x-"->{{1,I},{I,1}}/Sqrt[2],
  "C4y-"->{{1,-1},{1,1}}/Sqrt[2], "C4z-"->{{1-I,0},{0,1+I}}/Sqrt[2],
  "C2a"->{{0,-1+I},{1+I,0}}/Sqrt[2], "C2b"->{{0,-1-I},{1-I,0}}/Sqrt[2],
  "C2c"->{{-I,I},{I,I}}/Sqrt[2], "C2d"->{{-I,-1},{1,I}}/Sqrt[2],
  "C2e"->{{-I,-I},{-I,I}}/Sqrt[2], "C2f"->{{I,-1},{1,-I}}/Sqrt[2],
  "C6+"->{{I+Sqrt[3],0},{0,-I+Sqrt[3]}}/2, "C3+"->{{1+I Sqrt[3],0},{0,1-I Sqrt[3]}}/2,
  "C2"->{{-I,0},{0,I}}, "C3-"->{{1-I Sqrt[3],0},{0,1+I Sqrt[3]}}/2,
  "C6-"->{{-I+Sqrt[3],0},{0,I+Sqrt[3]}}/2, "C21p"->{{0,-I},{-I,0}},
  "C22p"->{{0,I+Sqrt[3]},{I-Sqrt[3],0}}/2, "C23p"->{{0,I-Sqrt[3]},{I+Sqrt[3],0}}/2,
  "C21pp"->{{0,-1},{1,0}}, "C22pp"->{{0,-1+I Sqrt[3]},{1+I Sqrt[3],0}}/2,
  "C23pp"->{{0,-1-I Sqrt[3]},{1-I Sqrt[3],0}}/2|>;
    
(* Note that each spin rotation operation is a list {su2, o3det}, in which su2 is the
   SU(2) spin rotation matrix and o3det is the determinant of the O(3) rotation, i.e. 1 or -1. *)
getSpinRotOp=Module[{name0,name1,mat0,op1,op2,op3,op4},
   name0={"E","C2x","C2y","C2z","C31+","C32+","C33+","C34+","C31-","C32-",
          "C33-","C34-","C4x+","C4y+","C4z+","C4x-","C4y-","C4z-","C2a","C2b",
          "C2c","C2d","C2e","C2f","C6+","C3+","C2","C3-","C6-","C21p",
          "C22p","C23p","C21pp","C22pp","C23pp"};
   name1={"I","\[Sigma]x","\[Sigma]y","\[Sigma]z","S61-","S62-","S63-","S64-","S61+","S62+",
          "S63+","S64+","S4x-","S4y-","S4z-","S4x+","S4y+","S4z+","\[Sigma]da","\[Sigma]db",
          "\[Sigma]dc","\[Sigma]dd","\[Sigma]de","\[Sigma]df","S3-","S6-","\[Sigma]h","S6+","S3+","\[Sigma]d1",
          "\[Sigma]d2","\[Sigma]d3","\[Sigma]v1","\[Sigma]v2","\[Sigma]v3"};
   mat0=SpinRot0/@name0;
   op1=Rule@@@Transpose[{name0,{#,1}&/@mat0}];
   op2=Rule@@@Transpose[{name1,{#,-1}&/@mat0}];
   op3=Rule@@@Transpose[{"bar"<>#&/@name0,{-#,1}&/@mat0}];
   op4=Rule@@@Transpose[{"bar"<>#&/@name1,{-#,-1}&/@mat0}];
   Association@Join[op1,op2,op3,op4]
];

SpinRotName=Module[{namec,nameh,nm1,nm2,p=0.001},
  namec={"E","C2x","C2y","C2z","C31+","C32+","C33+","C34+","C31-","C32-","C33-","C34-",
          "C4x+","C4y+","C4z+","C4x-","C4y-","C4z-","C2a","C2b","C2c","C2d","C2e","C2f", 
          "I","\[Sigma]x","\[Sigma]y","\[Sigma]z","S61-","S62-","S63-","S64-","S61+","S62+","S63+","S64+",
          "S4x-","S4y-","S4z-","S4x+","S4y+","S4z+","\[Sigma]da","\[Sigma]db","\[Sigma]dc","\[Sigma]dd","\[Sigma]de","\[Sigma]df"};
  nameh={"E","C6+","C3+","C2","C3-","C6-","C21p","C22p","C23p","C21pp","C22pp","C23pp",
         "I","S3-","S6-","\[Sigma]h","S6+","S3+","\[Sigma]d1","\[Sigma]d2","\[Sigma]d3","\[Sigma]v1","\[Sigma]v2","\[Sigma]v3"};
  namec=Join[namec,"bar"<>#&/@namec];
  nameh=Join[nameh,"bar"<>#&/@nameh];  
  nm1=Rule@@@Transpose[{Round[getSpinRotOp/@namec,p], namec}];
  nm2=Rule@@@Transpose[{Round[getSpinRotOp/@nameh,p], nameh}];
  Association/@{nm1,nm2}
];
 
getSpinRotName[brav_String,{srot_, o3det_Integer}]/;MatrixQ[srot,NumericQ]&&Dimensions[srot]=={2,2} :=
 Module[{toname, p=0.001},
   toname=If[MemberQ[{"HexaPrim","TrigPrim"},brav], SpinRotName[[2]], SpinRotName[[1]]];
   toname[Round[{srot,o3det},p]]
 ]
 
(* \:7528\:540d\:79f0\:76f8\:4e58\:7684\:7248\:672c\:66f4\:4fdd\:9669\:3002\:7528\:77e9\:9635\:76f8\:4e58\:7684\:7248\:672c\:5728\:7528 C3+ \:4e3a\:751f\:6210\:5143\:8c03\:7528 generateGroup \:65f6\:4f1a\:8ba1\:7b97\:51fa\:9519\:ff0c
   \:539f\:56e0\:5728\:4e8e\:4e58\:51fa\:7684\:7ed3\:679c\:6ca1\:6709\:5316\:7b80\:5bfc\:81f4\:6bd4\:8f83\:662f\:5426\:76f8\:7b49\:65f6\:672c\:6765\:76f8\:7b49\:5374\:8ba4\:4e3a\:4e0d\:76f8\:7b49\:3002\:867d\:7136\:7ed9 srot1.sort2 \:8fdb\:884c 
   FullSimplify \:6216 ComplexExpand \:64cd\:4f5c\:80fd\:89e3\:51b3\:95ee\:9898\:ff0c\:4f46\:4f1a\:663e\:8457\:589e\:52a0\:8ba1\:7b97\:91cf\:ff0c\:4e0d\:53ef\:53d6\:3002\:7528 Re + I*Im \:65b9\:5f0f
   \:867d\:7136\:76f8\:5bf9\:597d\:4e9b\:ff0c\:4f46\:4e5f\:589e\:52a0\:4e0d\:5c11\:8ba1\:7b97\:91cf\:3002\:800c\:540d\:79f0\:76f8\:4e58\:7684\:7248\:672c\:7531\:4e8e\:8c03\:7528\:4e86 Round \:8fdb\:884c\:4e86\:6570\:503c\:5316\:5904\:7406\:ff0c\:65e2\:4fdd\:8bc1
   \:6b63\:786e\:53c8\:4fdd\:8bc1\:4e86\:8ba1\:7b97\:901f\:5ea6\:3002*)
SpinRotTimes[{srot1_,o3det1_Integer},{srot2_,o3det2_Integer}]:={srot1.srot2,o3det1*o3det2}
SpinRotTimes[brav_String][opname1_String,opname2_String]:=
  getSpinRotName[brav,SpinRotTimes[getSpinRotOp[opname1],getSpinRotOp[opname2]]]
SpinRotTimes[opname1_String,opname2_String]:=DRotTimes[opname1,opname2]
DSGSeitzTimes[brav_][{Rname1_,v1_},{Rname2_,v2_}]:=
  {SpinRotTimes[brav][Rname1,Rname2], getRotMat[brav,StringReplace[Rname1,"bar"->""]].v2+v1}
DSGSeitzTimes[brav_][Rv1_, Rv2_, more__]:=Fold[DSGSeitzTimes[brav], Rv1, {Rv2,more}]
DSGpowerSeitz[brav_][{Rname_,v_},n_Integer]/;n>=0:=If[n==0,{"E",{0,0,0}},
                                      Fold[DSGSeitzTimes[brav],{Rname,v},Table[{Rname,v},{i,n-1}]]]
DSGinvSeitz[brav_][{Rname_,v_}]:=With[{R=getRotMat[brav,StringReplace[Rname,"bar"->""]]},
          {getSpinRotName[brav,{ConjugateTranspose[#1],#2}]&@@getSpinRotOp[Rname],-invRotMat[R].v}]

DSGCentExtTimes[brav_,adict_][{opname1_,alpha_},{opname2_,beta_}]:=Module[{g},
  g=adict["g"];  
  {SpinRotTimes[brav][opname1,opname2],Mod[alpha+beta+adict[{opname1,opname2}],g]}
]
DSGCentExtPower[brav_,adict_][{opname_,alpha_},n_Integer]/;n>=0:=If[n==0,{"E",0},
                    Fold[DSGCentExtTimes[brav,adict],{opname,alpha},Table[{opname,alpha},{i,n-1}]]]

(* Operations on rotation names*)
DRotTimes[Rname1_String,Rname2_String]:=Module[{crots,hrots,brav,barEI={"E","I","barE","barI"},tmp},
  crots=SpinRotName[[1]]//Values;
  hrots=SpinRotName[[2]]//Values;
  If[MemberQ[barEI, Rname1], 
    brav=If[MemberQ[crots, Rname2], "CubiPrim", "HexaPrim"],
    brav=If[MemberQ[crots, Rname1], "CubiPrim", "HexaPrim"];
    If[!MemberQ[barEI, Rname2], 
      tmp=If[MemberQ[crots, Rname2], "CubiPrim", "HexaPrim"];
      If[brav!=tmp, Print["DRotTimes: Error! Rname1 and Rname2 should be in the same list of either\n",crots,"\nor\n",hrots]; Abort[]];
    ] 
  ];
  getSpinRotName[brav,SpinRotTimes[getSpinRotOp[Rname1],getSpinRotOp[Rname2]]]
]
DRotTimes[R1_, R2_, more__]:=Fold[DRotTimes, R1, {R2,more}]
powerDRot[Rname_String, n_Integer]/;n>=0:=If[n==0, "E", Fold[DRotTimes,Rname,Table[Rname,n-1]]]
invDRot[Rname_String]:=Module[{brav},
  brav=If[MemberQ[SpinRotName[[1]]//Values, Rname], "CubiPrim", "HexaPrim"];
  getSpinRotName[brav,{ConjugateTranspose[#1],#2}]&@@getSpinRotOp[Rname]
]


(* Give the determinant, axis, and angle of an O(3) rotation matrix. Using the axis angle we can
   construct the SU(2) spin rotation matrix. *)
rotAxisAngle[O3RotMat_]/;MatrixQ[O3RotMat,NumericQ]:=
 Module[{eps=1.*^-5,mateq,a,sol1,ax,det,so3,eqs,x,y,z,I3,pos,vars,idx,ap1,ap2,X1,X2,X3,tmp,depend},
   I3=IdentityMatrix[3];
   mateq[m1_,m2_]:=Max@Abs@Flatten[N[m1]-m2]<eps;
   If[!mateq[O3RotMat\[Transpose].O3RotMat,I3],
       Print["The matrix is not an O(3) rotation matrix."]; Abort[]
   ];
   If[mateq[O3RotMat,I3], Return[{1,{0,0,1},0}]];
   If[mateq[O3RotMat,-I3], Return[{-1,{0,0,1},0}]];
   det=Round@FullSimplify@Det[O3RotMat];
   so3=det*O3RotMat;  
   (*(*-------------find the axis, myown algorithm-----(A)-------*)
   eqs=so3.{x,y,z}-{x,y,z}//Simplify;
   depend[eq1_,eq2_]:=Module[{sol,u1,u2},   sol=Solve[eq1\[Equal]u*eq2,u];  
     If[sol\[Equal]{}, Return[False], sol=sol[[1]]];     If[(u/.sol)\[Equal]0, Return[False]];
     u1=0; While[u1\[Equal]0,u1=(u/.sol/.({x\[Rule]#1,y\[Rule]#2,z\[Rule]#3}&@@RandomReal[1,3]))];
     u2=0; While[u2\[Equal]0,u2=(u/.sol/.({x\[Rule]#1,y\[Rule]#2,z\[Rule]#3}&@@RandomReal[1,3]))];
     Abs[u1-u2]<1*^-14
   ];
   tmp=depend[eqs[[#1]],eqs[[#2]]]&@@@{{1,2},{2,3},{1,3}};
   eqs=If[tmp[[#]],0,eqs[[#]]]&/@{1,2,3};   (* remove dependent equation *)
   eqs=Thread[eqs\[Equal]{0,0,0}];
   pos=Position[eqs,True]//Flatten;
   If[pos!={}, idx=DeleteCases[{1,2,3},pos[[1]]],
      tmp=Length[Position[eqs,#]]&/@{x,y,z};
      If[AllTrue[tmp,#>1&], idx={1,2},
        pos=Position[tmp,1]//Flatten;
        idx=If[Length[pos]==2, pos, {DeleteCases[{1,2,3},pos[[1]]][[1]],pos[[1]]} ]
      ]
   ];
   vars={x,y,z}[[idx]];
   sol1=Solve[eqs[[idx]],vars][[1]];
   (*----------------------(B)-------------------------*)
   (* (* Another algorithm: codes between (A) and (B) can be replaced by codes between (B) and (C). *)
   eqs=RowReduce[so3-I3,ZeroTest\[Rule](Abs[#]<1*^-12&)].{x,y,z}; (*ZeroTest is essential*)
   sol1=Quiet@Solve[Thread[eqs\[Equal]{0,0,0}],{x,y,z}][[1]];   *)
   (*----------------------(C)--------------------------*)
   ap1=({x,y,z}/.sol1)/.{x->1,y->1,z->1};
   ap2=({x,y,z}/.sol1)/.{x->2,y->2,z->2};
   ax=Normalize[ap2-ap1]; 
   (*---------------------------------------------------*) *)
   ax=Normalize[NullSpace[so3-I3][[1]]]; (*NullSpace gives the axis directly!*)
   a=FullSimplify@With[{b=Tr[so3]},2ArcTan[Sqrt[1+b]/2,Sqrt[3-b]/2]]; 
   (*\:4e0a\:9762\:662f Sin[3a/2]/Sin[a/2]\[Equal]b \:7684\:4e00\:4e2a\:89e3\:ff0c\:5b9e\:9645\:8f6c\:89d2\:53ef\:80fd\:4e3a\:5176\:8d1f\:503c\:ff0c\:9700\:8981\:8fdb\:4e00\:6b65\:5224\:65ad*)
   X1={{0,0,0},{0,0,-1},{0,1,0}};
   X2={{0,0,1},{0,0,0},{-1,0,0}};
   X3={{0,-1,0},{1,0,0},{0,0,0}};
   tmp=MatrixExp[a*ax.{X1,X2,X3}];
   If[mateq[tmp\[Transpose],so3], ax=-ax, If[!mateq[tmp,so3],
      Print["rotAxisAngle: Error occurs when finding the axis and angle of ",O3RotMat]
   ]];
   {det,ax,a}//Re//Chop
]


(* ::Subsection:: *)
(*Generate a group using generators*)


(* Generate all group elements from its generators gens for any group, provided that
   the group multiplication and identity element are given. If the option
   "generationProcess"->True is used, the process of generating each element is given
   in the form of multiplication sequence of generators. *)
Options[generateGroup]={"generationProcess"->False};
generateGroup[gens_,identityElement_,multiply_,OptionsPattern[]]:=
 Module[{i,j,ng,MAXORDER=200,orders,subs,mlist,g1,g2,g3,powers,g2tmp,mseq,gp},
   gp=OptionValue["generationProcess"]===True;
   ng=Length[gens];    orders=subs=Table[0,ng];
   powers=<||>;
   powers[identityElement]={1,0};
   For[i=1,i<=ng,i++,
     If[gens[[i]]==identityElement, orders[[i]]=1; subs[[i]]={identityElement}; Continue[]];
     mlist=Range[MAXORDER];  mlist[[1]]=gens[[i]];
     For[j=2,j<=MAXORDER,j++, mlist[[j]]=multiply[mlist[[j-1]],gens[[i]]];
       If[mlist[[j]]==identityElement, orders[[i]]=j; Break[]]
     ];
     If[j>MAXORDER,
       Print["Error: The order of ",gens[[i]]," is larger than MAXORDER(=",MAXORDER,")."];
       Print["       The list of powers is: ",mlist];
       Abort[]
     ];
     subs[[i]]=mlist[[;;orders[[i]]]];
     If[gp, For[j=1,j<orders[[i]],j++, powers[subs[[i,j]]]={i,j}]]
   ];
   g1=Union@@subs; 
   If[gp,
     g2tmp=Table[multiply[g1[[i]],g1[[j]]]->{g1[[i]],g1[[j]]},{i,Length[g1]},{j,Length[g1]}];
     g2tmp=Union[Sequence@@g2tmp,SameTest->(#1[[1]]==#2[[1]]&)];
     g2=g2tmp[[All,1]];
     g3=Complement[g2,g1];  g1=g2;
     mseq=<||>;  (mseq[#]={#})&/@Keys[powers];  (*mseq records the multiplication sequence of elements*)
     g2tmp=Association[g2tmp]; (mseq[#]=g2tmp[#])&/@g3,   
     (*--------else---------*)
     g2=Union@@Table[multiply[g1[[i]],g1[[j]]],{i,Length[g1]},{j,Length[g1]}];
     g3=Complement[g2,g1];  g1=g2;
   ];
   While[g3!={},
     If[gp,
       g2tmp=Table[multiply[g3[[i]],g1[[j]]]->{g3[[i]],g1[[j]]},{i,Length[g3]},{j,Length[g1]}];
       g2tmp=Union[Sequence@@g2tmp,SameTest->(#1[[1]]==#2[[1]]&)];
       g2=g2tmp[[All,1]];
       g3=Complement[g2,g1];  g1=g2;
       g2tmp=Association[g2tmp];  (mseq[#]=g2tmp[#])&/@g3,
       (*-------else------*)
       g2=Union@@Table[multiply[g3[[i]],g1[[j]]],{i,Length[g3]},{j,Length[g1]}];
       g3=Complement[g2,g1]; g1=g2;
     ]
   ]; 
   If[gp,
     mseq=NestWhile[Table[Sequence@@mseq[i],{i,#}]&,#,Or@@(MissingQ[powers[#]]&/@#)&]&/@mseq;
     powers=mseq/.Normal[powers],
     g2
   ]
 ]


(* ::Subsection:: *)
(*Get the elements of little group (LG) and Herring little group (HLG)*)


modone[x_] := If[ListQ[x], modone /@ x, If[NumericQ[x], Mod[x, 1], x]];
(*seteq[s1_,s2_]:=SubsetQ[s1,s2]&&SubsetQ[s2,s1];*)Op
Options[seteq]={SameTest->Automatic};
seteq[s1_,s2_,OptionsPattern[]]:=Complement[s1,s2,SameTest->OptionValue[SameTest]]=={}&&
                 Complement[s2,s1,SameTest->OptionValue[SameTest]]=={};

(* Get the elements of a Herring little group (HLG) or a space group (the HLG of \[CapitalGamma]) 
   according to the abstract group G_m^n and the generators for the HLG given in Tab.5.7. 
   In fact this function gives the coset representatives of the Herring little group,
   therefore it is only for the high symmetry points, not for high symmetry lines.
   The rotation part of gens may be either its name or its matrix. 
   Note that if a HLG has the form G_m^n*T_q in Tab.5.7, this function only give G_m^n.
   Although G_m^n is not the HLG from the original definition, G_m^n is enough to 
   determine the ireps of little group and is more concise. *)

(* Option "DSG" means double space group with value True or False. *)
Options[getHLGElem]={"DSG"->False};
getHLGElem[brav_String,{m_Integer,n_Integer},gens_,OptionsPattern[]]:=
 Module[{pows, ngen, namegens, times, power},
  pows=Flatten[AGClasses[m,n],1];   ngen=Length[pows[[1]]];
  If[ngen!=Length[gens], Print["getHLEGlem: Wrong number of generators!"]; Abort[]];
  namegens=gens;
  If[OptionValue["DSG"]==False,
    If[!StringQ[gens[[1,1]]], namegens={getRotName[brav,#1],#2}&@@@gens];  
    times=SeitzTimes[brav];  power=powerSeitz[brav],
    (*----else: "DSG"\[Rule]True for double space group, only rotation name supported ----*)
    times=DSGSeitzTimes[brav];  power=DSGpowerSeitz[brav]
  ];
  Table[Fold[times,#[[1]],#[[2;;]]]&@ 
               (power[#1,#2]&@@@Transpose[{namegens,pows[[i]]}]),{i,Length[pows]}]
]

getHLGElem[sgno_Integer, kname_String, OptionsPattern[]]:=Module[{AGNo,gens,brav,ks,
  Irep},
  brav=getSGLatt[sgno];
  ks=Select[LGIrep[sgno]//Keys,VectorQ[LGIrep[sgno,#][[2,1,2]]]&];   
  If[!MemberQ[ks,kname], 
    Print["getLGElem: kname ",kname," is not one of the high-symmetry k points ",ks]; 
    Abort[]];
  Irep=If[OptionValue["DSG"], DLGIrep, LGIrep][sgno];
  {AGNo,gens}=Irep[kname][[{1,2}]];
  getHLGElem[brav,AGNo,gens,"DSG"->OptionValue["DSG"]]
]

(* This function return the coset representatives of a little group with respect to the 
   translation group, available for both high-symmetry points and lines in Tab.5.7. *)

(* For sorting the rotation names of getLGElem. *)
Module[{rots,idx},
  rots=JonesSymbol[[All,All,1]]//Flatten//DeleteDuplicates;
  allRotNames=Join[rots,"bar"<>#&/@rots];
  idx=Range[Length[allRotNames]];
  RotNameIndex=Association[Rule@@@Transpose[{allRotNames,idx}]];
];

Options[getLGElem]={"DSG"->False};
getLGElem[sgno_Integer,kname_,OptionsPattern[]]/;1<=sgno<=230&&StringQ[kname]:=
 Module[{ks,AGno,gens,brav,GMAGno,GMgens,G,mats,Gk0,Gk,dsg,Irep,tmp},
  ks=LGIrep[sgno]//Keys;
  If[!MemberQ[ks,kname],Print["getLGElem: kname ",kname," is not in ",ks]; Abort[]];
  brav=getSGLatt[sgno];  dsg=OptionValue["DSG"];
  Irep=If[dsg, DLGIrep, LGIrep];
  {AGno,gens}=Irep[sgno][kname][[{1,2}]];
  If[ListQ[gens[[1,2]]],
    tmp=DeleteDuplicates[modone[getHLGElem[brav,AGno,gens,"DSG"->dsg]]];
    Return[SortBy[tmp,RotNameIndex[#[[1]]]&]]
  ];
  {GMAGno,GMgens}=Irep[sgno]["\[CapitalGamma]"][[{1,2}]];
  G=modone[getHLGElem[brav,GMAGno,GMgens,"DSG"->dsg]];
  If[dsg==False,
    mats=getRotMat[brav,#]&/@gens[[All,1]];
    Gk0=getRotName[brav,#]&/@generateGroup[mats,IdentityMatrix[3],Dot],
    (*--------else--------*)
    Gk0=generateGroup[gens[[All,1]],"E",SpinRotTimes[brav]]
  ];
  Gk=G[[Position[G,#][[1,1]]&/@SortBy[Gk0,RotNameIndex]]]
 ]
 
getLGElem[sgno_Integer, k_, OptionsPattern[]]/;1<=sgno<=230&&VectorQ[k,NumericQ]:=
  Module[{G,Gk,brav},   brav=getSGLatt[sgno];
    G=getLGElem[sgno,"\[CapitalGamma]"];
    Gk=Select[G,keqmod[getRotMatOfK[brav,#[[1]]].k,k]&];
    If[OptionValue["DSG"], Gk=Join[Gk,{"bar"<>#1,#2}&@@@Gk]];
    Gk
  ]
  
Options[getSGElem]={"DSG"->False};
getSGElem[sgno_Integer, OptionsPattern[]]/;1<=sgno<=230:=
  getLGElem[sgno,"\[CapitalGamma]", "DSG"->OptionValue["DSG"]]


(* ::Subsection:: *)
(*Get the elements of the central extension of the little cogroup*)


(* This function gives a(Ri,Rj) for the central extension of the little cogroup of k. 
   Note that in genral a(Ri,Rj) may be different for k and k+g in which g is a reciprocal vector.
   For lattices with more than one BZ types, e.g. OrthBase(a) and OrthBase(b), a 
   high-symmetry-line k point with a certain name, e.g. H, may has different coordinates differing
   by a reciprocal vector, say k and k+g. In this case, will different BZ types give different
   a(Ri,Rj)? Fortunately the answer is no. In fact only H of OrthBase and G,F,U of OrthBody have
   different corrdinates for different BZ types. However in all these cases a(Ri,Rj) happens
   to be the same for different BZ types. *) 
Options[aCentExt]={"DSG"->False}; 
aCentExt[sgno_Integer,kname_,BZtype_String,OptionsPattern[]]/;1<=sgno<=230&&StringQ[kname]:=
 Block[{u,brav,k,ks,Gk,fullBZtype,hsk,kn},
   Gk=getLGElem[sgno,kname,"DSG"->OptionValue["DSG"]];   
   brav=getSGLatt[sgno];
   If[MemberQ[{"OrthBase","OrthBody","OrthFace","TetrBody","TrigPrim"},brav],
     fullBZtype=brav<>"("<>BZtype<>")", fullBZtype=brav];
   hsk=BCHighSymKpt[fullBZtype]; 
   ks=hsk[[All,1]];
   If[sgno==205&&kname=="Z'",  k={1/2,u,0},
     kn=kname/.{"aF"->"F","bF"->"F"};
     If[!MemberQ[ks,kn],Print["aCentExt: kname ",kn," is not in ",ks," for ",fullBZtype]; Abort[]];
     k=hsk[[Position[hsk[[All,1]],kn][[1,1]],3]]; 
   ];
   aCentExt[brav,Gk,k,"DSG"->OptionValue["DSG"]]
 ]
aCentExt[sgno_Integer,kname_,OptionsPattern[]]/;1<=sgno<=230&&StringQ[kname]:=
   aCentExt[sgno,kname,"a","DSG"->OptionValue["DSG"]]
 
aCentExt[brav_,Gk_,k_,OptionsPattern[]]/;VectorQ[k]:=
 Module[{Rmatsk,gi,vs,fullBZtype,hsk,N0,aog,g,a,adict},
   vs=Gk[[All,2]];
   Rmatsk=getRotMatOfK[brav,StringReplace[#,"bar"->""]]&/@Gk[[All,1]];
   N0=Length[Rmatsk];
   gi=Inverse[#//N].k-k&/@Rmatsk//Simplify//Chop;
   aog=Table[-gi[[i]].vs[[j]],{i,N0},{j,N0}]//Rationalize;
   g=Max[aog//Denominator];
   a=Mod[aog*g,g];   adict=<||>;     adict["g"]=g;
   Table[adict[{Gk[[i,1]],Gk[[j,1]]}]=a[[i,j]],{i,N0},{j,N0}];
   adict
 ]

 
(* Get the central extension of the little cogroup of k, with its elements in the same order 
   as in the corresponding abstract group. *)
Options[getCentExt]={"DSG"->False};  
getCentExt[sgno_Integer,kname_,OptionsPattern[]]/;1<=sgno<=230&&StringQ[kname]:=
 Module[{adict,ks,AGno,gens,brav,pows,ngen,BZtype="a",ks2,set,Irep,dsg,times,power,ce},
   dsg=OptionValue["DSG"];
   Irep=If[dsg, DLGIrep, LGIrep];
   ks=Irep[sgno]//Keys;
   ks=Select[ks,!VectorQ[Irep[sgno][#][[2,1,2]]]&];
   If[!MemberQ[ks,kname],Print["getCentExt: kname ",kname," is not in ",ks];Return[]];
   {AGno,gens}=Irep[sgno][kname][[{1,2}]];
   (* If[VectorQ[gens[[1,2]]],
     Print["Note: The k point ",kname," does not belong to a high-symmetry line."];
     Return[]]; *)
   brav=getSGLatt[sgno];   
   set={"OrthBase","OrthBody","OrthFace","TetrBody","TrigPrim"};
   If[MemberQ[set,brav]&&!MemberQ[BCHighSymKpt[brav<>"(a)"][[All,1]],kname],
     Switch[brav,
       "OrthBase",BZtype="b",
       "TetrBody",BZtype="b",
       "TrigPrim",BZtype="b",
       "OrthBody",BZtype=If[MemberQ[BCHighSymKpt["OrthBody(b)"][[All,1]],kname],"b","c"],
       "OrthFace",BZtype=If[MemberQ[BCHighSymKpt["OrthFace(b)"][[All,1]],kname],"b",
                           If[MemberQ[BCHighSymKpt["OrthFace(c)"][[All,1]],kname],"c","d"]]
     ];
    ]; 
   adict=aCentExt[sgno,kname,BZtype,"DSG"->dsg]; 
   pows=Flatten[AGClasses@@AGno,1];   ngen=Length[pows[[1]]];
   If[dsg==False, 
     times=CentExtTimes[brav,adict];     power=CentExtPower[brav,adict],
     times=DSGCentExtTimes[brav,adict];  power=DSGCentExtPower[brav,adict]
   ];
   ce=Table[Fold[times,#[[1]],#[[2;;]]]&@(power@@@Transpose[{gens,pows[[i]]}]),{i,Length[pows]}];
   (* for debug
   If[Length@DeleteDuplicates[ce]\[NotEqual]Length[ce],
      Print["getCentExt: sgno=",sgno,", k=",kname,", dsg=",dsg,", duplicate elements appear."]
   ]; *)
   ce
 ] 



(* ::Subsection:: *)
(*getSGElemAndSQ*)


(* \:8bf4\:660e\:89c1 getSGElemAndSQ \:51fd\:6570 *)
getSGElemAndSQ0[sgno_Integer]/;1<=sgno<=230:=Block[{times,inv,G,SGiS,Q,P,gens,AGno,brav,u,v,w, 
  i,S,selectedS={},found,try1,try2,tryuvw,tmp1,tmp2,tmp3},
  {AGno,gens}=LGIrep[sgno]["\[CapitalGamma]"][[{1,2}]];
  brav=getSGLatt[sgno];
  G=SortBy[modone[getHLGElem[brav,AGno,gens]],RotNameIndex[#[[1]]]&];  
  If[brav=="TrigPrim",
    P=BCHighSymKpt["TrigPrim(a)"][[1,5]],
    P=BCHighSymKpt[StringTake[brav,4]<>"Prim"][[1,5]]
  ];
  Q=Complement[P,G[[All,1]]];
  If[Q=={}, Return[{G,{},{}}]];   (* Point group is holosymmetric *)
  If[Union@Flatten[G[[All,2]]]=={0}, Return[{G,{#,{0,0,0}}&/@Q,{}}]];  (* symmorphic space group *)
  (* No supergroup with the same lattice of which the space group is its invariant subgroup. *)
  If[MemberQ[{91,95,92,96,178,179,180,181,212,213,205},sgno], Return[{G,{},Q}]];  
  
  tmp1=Flatten[Table[{u,v,w},{u,0,0.9,1/2},{v,0,0.9,1/2},{w,0,0.9,1/2}],2];
  tmp2=Flatten[Table[{u,v,w},{u,0,0.9,1/4},{v,0,0.9,1/4},{w,0,0.9,1/4}],2];
  try1=Join[DeleteCases[tmp1,{0,0,0}],Complement[tmp2,tmp1]];
  tmp2=Flatten[Table[{u,v,w},{u,0,0.9,1/3},{v,0,0.9,1/3},{w,0,0.9,1/3}],2];
  tmp3=Flatten[Table[{u,v,w},{u,0,0.9,1/6},{v,0,0.9,1/6},{w,0,0.9,1/6}],2];
  try2=Join[DeleteCases[tmp1,{0,0,0}],DeleteCases[tmp2,{0,0,0}],Complement[tmp3,tmp2,tmp1]];
  tryuvw=If[143<=sgno<=194,try2,try1];
  
  (* \:5148\:5c1d\:8bd5\:7ed9\:6bcf\:4e2aQ\:4e2d\:5143\:7d20\:914d\:4e2a {0,0,0} \:5e73\:79fb\:770b\:662f\:5426\:6ee1\:8db3\:8981\:6c42\:ff0c\:8fd9\:80fd\:89e3\:51b3\:6389\:5f88\:5927\:4e00\:90e8\:5206\:7a7a\:95f4\:7fa4 *)
  times=SeitzTimes[brav];     inv=invSeitz[brav];
  For[i=1,i<=Length[Q],i++,  S={Q[[i]],{0,0,0}};   
    SGiS=modone[times[times[S,#],inv[S]]&/@G];
    (* Print["i=",i,"  S=",S]; *)
    If[Complement[SGiS,G]=={},  Print[S];
       selectedS=Join[selectedS,modone[times[S,#]&/@G]]; 
       Q=Complement[Q,selectedS[[All,1]]];
       Break[];  
      ];
  ];
  
  (* \:7136\:540e\:518d\:901a\:8fc7\:7a77\:4e3e\:6cd5\:ff0c\:5bf9\:6bcf\:4e00\:4e2aQ\:4e2d\:5143\:7d20\:ff0c\:5c1d\:8bd5\:6240\:6709\:53ef\:80fd\:7684\:5e73\:79fb\:64cd\:4f5c\:ff0c\:5bf9\:4e8e\:975e\:4e09\:65b9\:516d\:65b9\:6676\:7cfb\:ff0c
     \:5176\:5e73\:79fb\:90e8\:5206\:7684\:5206\:91cf\:53ea\:80fd\:662f 0,1/4,1/2,3/4\:ff1b\:800c\:5bf9\:4e8e\:4e09\:65b9\:516d\:65b9\:6676\:7cfb\:ff0c\:5176\:5e73\:79fb\:90e8\:5206\:7684\:5206\:91cf\:53ea\:80fd\:4e3a
     0,1/6,1/3,1/2,2/3,5/6\:3002 *)        
  For[i=1,i<=Length[Q],i++, found=False;
    (* Print["i=",i,"  ",Q[[i]]]; *)
    Do[ S={Q[[i]],uvw};
      SGiS=modone[times[times[S,#],inv[S]]&/@G];
      If[Complement[SGiS,G]=={}, found=True;  Break[]]
    ,{uvw,tryuvw}];  (* End Do *)
    If[found, 
      selectedS=Join[selectedS,modone[times[S,#]&/@G]]; 
      Q=Complement[Q,selectedS[[All,1]]]; 
      i=0;
    ];
  ];

  {G,selectedS,Q}
]

(* \:8fd4\:56de\:503c\:4e3a {G,selectedS,Q}\:ff0c\:5176\:4e2d G \:662f sgno \:53f7\:7a7a\:95f4\:7fa4\:5173\:4e8e\:5176\:5e73\:79fb\:7fa4\:7684\:966a\:96c6\:4ee3\:8868\:5143\:7684\:96c6\:5408\:ff0c\:7528\:4e8e\:63cf\:8ff0\:8be5\:7a7a\:95f4\:7fa4\:3002
   \:8bbe\:8be5\:7a7a\:95f4\:7fa4\:6240\:5bf9\:5e94\:683c\:7cfb\:7684\:5168\:5bf9\:79f0\:70b9\:7fa4\:4e3aP\:ff0c\:90a3\:4e48\:6700\:521d\:7684Q\:662fP\:4e2d\:9664\:53bbG\:91cc\:6240\:6709\:65cb\:8f6c\:90e8\:5206\:540e\:7684\:65cb\:8f6c\:64cd\:4f5c\:7684\:96c6\:5408\:ff0c
   \:4e4b\:540e\:5c1d\:8bd5\:7ed9Q\:4e2d\:6bcf\:4e00\:4e2a\:65cb\:8f6c\:914d\:4e00\:4e2a\:5e73\:79fb\:90e8\:5206\:ff0c\:5f97\:5230\:4e00\:4e2a\:7a7a\:95f4\:7fa4\:64cd\:4f5cS\:ff0c\:5982\:679c\:7528\:8fd9\:4e2aS\:5bf9\:539f\:7a7a\:95f4\:7fa4\:505a\:5171\:8f6d\:64cd\:4f5c\:540e
   \:5f97\:5230\:7684\:7a7a\:95f4\:7fa4\:8fd8\:662f\:539f\:7a7a\:95f4\:7fa4\:7684\:8bdd\:ff0c\:90a3\:4e48\:8fd9\:6837\:7684S\:5c31\:9009\:51fa\:6765\:ff0c\:76f8\:5e94\:65cb\:8f6c\:90e8\:5206\:4eceQ\:4e2d\:53bb\:9664\:3002\:90a3\:4e48\:6700\:540e\:6240\:6709\:9009\:51fa\:6765\:6ee1\:8db3
   \:6761\:4ef6\:7684S\:5c31\:6784\:6210\:4e86\:6700\:540e\:8fd4\:56de\:7684 selectedS\:3002\:800cQ\:5219\:662f\:6700\:540e\:5269\:4e0b\:6765\:90a3\:4e9b\:65cb\:8f6c\:64cd\:4f5c\:ff0c\:5b83\:4eec\:914d\:4ec0\:4e48\:5e73\:79fb\:4e5f\:65e0\:6cd5\:4f7f\:5f97\:539f\:6765
   \:7684\:7a7a\:95f4\:7fa4\:4e0d\:53d8\:3002selectedS\:4e2d\:5143\:7d20\:5bf9\:5224\:65ad\:5728\:8868\:793a\:57df\:5185\:4f46\:4e0d\:5728\:57fa\:672c\:57df\:5185\:7684k\:70b9\:7684\:5c0f\:8868\:793a\:6709\:7528\:3002
       \:8be5\:51fd\:6570\:662f\:5728 getSGElemAndSQ0 \:51fd\:6570\:8ba1\:7b97\:7ed3\:679c\:7684\:57fa\:7840\:4e0a\:4fee\:6539\:800c\:6765\:3002getSGElemAndSQ0 \:662f\:7a77\:4e3e\:6240\:6709\:53ef\:80fd\:7684\:5e73\:79fb\:6765\:5224\:65ad S\:ff0c
   \:5bf9\:4e8e\:6709\:7684\:7a7a\:95f4\:7fa4\:ff08\:5373BC\:4e66\:4e2d414\:9875list(b)\:4e2d\:90a3\:4e9b\:ff09\:5f88\:8017\:65f6\:3002\:4e8e\:662f\:5206\:6790\:8ba1\:7b97\:7ed3\:679c\:53d1\:73b0\:6240\:6709230\:4e2a\:7a7a\:95f4\:7fa4\:7528\:4e8e\:5c1d\:8bd5\:7684
   \:4e0d\:540cS\:5e76\:4e0d\:591a\:ff0c\:4e8e\:662f\:5e72\:8106\:53ea\:7528\:5b83\:4eec\:6765\:5224\:65ad\:ff0c\:8fd9\:6837\:5927\:5927\:52a0\:901f\:4e86\:8ba1\:7b97\:3002\:4e0d\:8fc7\:6ce8\:610f\:ff0c\:6ee1\:8db3\:8981\:6c42\:7684S\:5176\:5e73\:79fb\:90e8\:5206\:5e76\:4e0d\:552f\:4e00\:ff0c
   \:7531\:4e8e\:5c1d\:8bd5\:987a\:5e8f\:7684\:4e0d\:540c\:ff0cgetSGElemAndSQ0 \:4e0e getSGElemAndSQ \:7684\:8fd4\:56de\:7ed3\:679c\:53ef\:80fd\:4e0d\:540c\:ff0c\:4e0d\:540c\:5728\:4e8e\:5c1d\:8bd5\:7684S\:5148\:540e\:987a\:5e8f\:7684\:4e0d\:540c\:5bfc\:81f4
   \:4e86 selectedS \:7684\:4e0d\:540c\:3002\:4f46\:8fd9\:79cd\:4e0d\:540c\:5e76\:4e0d\:5f71\:54cd\:4f7f\:7528\:3002 *)
getSGElemAndSQ[sgno_Integer]/;1<=sgno<=230:=Block[{times,inv,G,SGiS,Q,P,gens,AGno,brav,S,selectedS={},tryS},
  {AGno,gens}=LGIrep[sgno]["\[CapitalGamma]"][[{1,2}]];
  brav=getSGLatt[sgno];
  G=SortBy[modone[getHLGElem[brav,AGno,gens]],RotNameIndex[#[[1]]]&];  
  If[brav=="TrigPrim",
    P=BCHighSymKpt["TrigPrim(a)"][[1,5]],
    P=BCHighSymKpt[StringTake[brav,4]<>"Prim"][[1,5]]
  ];
  Q=Complement[P,G[[All,1]]];
  If[Q=={}, Return[{G,{},{}}]];   (* Point group is holosymmetric *)
  If[Union@Flatten[G[[All,2]]]=={0}, Return[{G,{#,{0,0,0}}&/@Q,{}}]];  (* symmorphic space group *)
  (* No supergroup with the same lattice of which the space group is its invariant subgroup. *)
  If[MemberQ[{91,95,92,96,178,179,180,181,212,213,205},sgno], Return[{G,{},Q}]];  
  
  tryS={{"I",{0,0,0}},{"C2z",{0,0,0}},{"C2x",{0,0,0}},{"C2a",{0,0,0}},{"I",{0,0,1/2}},
    {"\[Sigma]da",{0,0,1/2}},{"I",{0,1/2,1/2}},{"\[Sigma]da",{0,1/2,1/2}},{"\[Sigma]x",{0,0,0}},{"C2",{0,0,0}},
    {"C21p",{0,0,1/2}},{"C21pp",{0,0,1/2}},{"C21p",{0,0,0}},{"\[Sigma]d1",{0,0,1/2}},{"C2a",{1/4,3/4,3/4}},
    {"S4x-",{1/4,1/4,3/4}},{"C2a",{1/2,0,0}},{"S4x-",{0,0,1/2}},{"S4x-",{0,0,0}}};
  
  (* \:5148\:5c1d\:8bd5\:7ed9\:6bcf\:4e2aQ\:4e2d\:5143\:7d20\:914d\:4e2a {0,0,0} \:5e73\:79fb\:770b\:662f\:5426\:6ee1\:8db3\:8981\:6c42\:ff0c\:8fd9\:80fd\:89e3\:51b3\:6389\:5f88\:5927\:4e00\:90e8\:5206\:7a7a\:95f4\:7fa4 *)
  times=SeitzTimes[brav];     inv=invSeitz[brav];
  Do[ 
    If[!MemberQ[Q,S[[1]]],Continue[]];
    SGiS=modone[times[times[S,#],inv[S]]&/@G];
    If[Complement[SGiS,G]=={},  
       selectedS=Join[selectedS,modone[times[S,#]&/@G]]; 
       Q=Complement[Q,selectedS[[All,1]]];
      ];
    If[Q=={},Break[]]
  ,{S,tryS}];

  {G,selectedS,Q}
]


(* ::Subsection:: *)
(*Get the star of a k-point*)


Options[getKStar]={"cosets"->False};
getKStar[sgno_Integer, kin_, OptionsPattern[]]:=Module[{k,SG,brav,kall,star0,star,cosets},
  k=If[!StringQ[kin], kin, kBCcoord[sgno,kin][[1,1]]];
  SG=getSGElem[sgno];   brav=getSGLatt[sgno];
  kall={getRotMatOfK[brav,#[[1]]].k, #}&/@SG;
  star0=Gather[kall,keqmod[#1[[1]],#2[[1]]]&];
  star=star0[[All,1,1]];   cosets=star0[[All,All,2]];
  If[OptionValue["cosets"]===True, {star,cosets}, star]
]


(* ::Subsection:: *)
(*Determine the name of a point group*)


 (* Determine the point group name according to rots which can be either a list of
    rotation matrixes or a list of rotation names. *) 
detPointGroup[rots_]:=Module[{type, pg, type2, counts},
   type=<|{-2,-1}->-6,{-1,-1}->-4,{0,-1}->-3,{1,-1}->-2,{-3,-1}->-1,
          {3,1}->1,{-1,1}->2,{0,1}->3,{1,1}->4,{2,1}->6|>;
   pg=<|{0,0,0,0,0,1,0,0,0,0}->"C1",    {0,0,0,0,1,1,0,0,0,0}->"Ci",    
        {0,0,0,0,0,1,1,0,0,0}->"C2",    {0,0,0,1,0,1,0,0,0,0}->"Cs",    
        {0,0,0,1,1,1,1,0,0,0}->"C2h",   {0,0,0,0,0,1,3,0,0,0}->"D2",  
        {0,0,0,2,0,1,1,0,0,0}->"C2v",   {0,0,0,3,1,1,3,0,0,0}->"D2h",   
        {0,0,0,0,0,1,1,0,2,0}->"C4",    {0,2,0,0,0,1,1,0,0,0}->"S4",  
        {0,2,0,1,1,1,1,0,2,0}->"C4h",   {0,0,0,0,0,1,5,0,2,0}->"D4",  
        {0,0,0,4,0,1,1,0,2,0}->"C4v",   {0,2,0,2,0,1,3,0,0,0}->"D2d",   
        {0,2,0,5,1,1,5,0,2,0}->"D4h",   {0,0,0,0,0,1,0,2,0,0}->"C3",    
        {0,0,2,0,1,1,0,2,0,0}->"S6",    {0,0,0,0,0,1,3,2,0,0}->"D3",    
        {0,0,0,3,0,1,0,2,0,0}->"C3v",   {0,0,2,3,1,1,3,2,0,0}->"D3d",   
        {0,0,0,0,0,1,1,2,0,2}->"C6",    {2,0,0,1,0,1,0,2,0,0}->"C3h",   
        {2,0,2,1,1,1,1,2,0,2}->"C6h",   {0,0,0,0,0,1,7,2,0,2}->"D6",    
        {0,0,0,6,0,1,1,2,0,2}->"C6v",   {2,0,0,4,0,1,3,2,0,0}->"D3h",   
        {2,0,2,7,1,1,7,2,0,2}->"D6h",   {0,0,0,0,0,1,3,8,0,0}->"T",     
        {0,0,8,3,1,1,3,8,0,0}->"Th",    {0,0,0,0,0,1,9,8,6,0}->"O",     
        {0,6,0,6,0,1,3,8,0,0}->"Td",    {0,6,8,9,1,1,9,8,6,0}->"Oh"|>;
   type2[str_]:=Module[{}, If[str=="E",Return[1]]; If[str=="I",Return[-1]];
     If[StringTake[str,1]=="\[Sigma]",Return[-2]];
     Switch[StringTake[str,2],"C2",2,"C3",3,"C4",4,"C6",6,"S3",-6,"S6",-3,"S4",-4]];
   counts=If[StringQ[First[rots]], type2/@rots, type[Round[{Tr[#],Det[#]}]]&/@rots];
   counts=Delete[BinCounts[counts,{-6,7}],{{2},{7},{12}}];
   If[MemberQ[Keys[pg],counts],pg[counts],"undetermined"]
 ]
 


(* ::Subsection:: *)
(*Calculate the ireps of little group*)


 (* Reduce the representation whose characters are chars according to the character table of the
   abstract group Gm^n. Note that chars is a list of characters for each class, not for each 
   group element, and the sequence of the classes should be the same as in BC Tab.5.1. *)
reduceRep[{m_Integer,n_Integer},chars_]:=Module[{cls,ct,nc,ap},
  cls=AGClasses[m,n];    nc=Length/@cls;
  ct=AGCharTab[m,n];
  ap=Total[nc*Conjugate[#]*chars]/m&/@ct;
  ap
]

(* Format of kinfo is like:
  {{-0.2,0.6,0.1},"GP","","C1"}
  {{-0.2,0.6,0.1},"UN","",{"E","\[Sigma]z"}}
  {{0,0.1,0.5},"Z","XM","C2v",{u,0.5,0},{"C31+",{0,0,0}},{0,u,0.5},{0,0,0},u\[Rule]0.1`,0.5,"in G"}  *)
(* Calculate the allowed representations and realities. In fact, if the k point is "GP", the
   rep is trivial and only the realities need to be calculated.  If the k point is "UN", it's a
   point in a high-symmetry plane except No.2 space group. All the central extensions of the
   little co-group for "UN" k-point are either G_2^1 or G_4^1 and their allowed reps are
   {1,2} and {2,4} respectively. Also, the realities need to be calculated. If the k point is
   a high-symmetry point or line, this function only calculate the allowed reps and the realities
   and compared with the data in the BC book to check the correctness.
   Note that the output for "GP" and "UN" has more information than other cases. *)
(* In fact, this function is used to get the irep infos for "GP" and "UN" kpoints which are not
   given in the BC book. For the kpoints whose irep infos are given in the BC book, this function
   is only used for checking purpose. *)

Options[calcRep]={"DSG"->False};
calcRep[sgno_Integer,kinfo_,OptionsPattern[]]/;1<=sgno<=230:=
 Block[{k,kname,kname2,ktip,kBD,brav,prec=1*^-5,G0,G02,idx,re,AGno,gens,avRep,ct,LG,HLG,
  CE,idx2,idx3,dt,factor,sG02char,nc,clsidx,abc,reptype,Rs,Gmnchar,i,allow,n1,tmp,g,gE,Gk,a,
  dsg,G0nobar,posbarE,allow0,barEfac},
  dsg=OptionValue["DSG"];  
  {k,kname,ktip}=kinfo[[1;;3]];
  brav=getSGLatt[sgno];
  G0=getLGElem[sgno,"\[CapitalGamma]","DSG"->dsg];
  G0nobar=G0;
  If[dsg, G0nobar={StringReplace[#1,"bar"->""],#2}&@@@G0];
  G02=If[dsg,DSGSeitzTimes[brav],SeitzTimes[brav]][#,#]&/@G0; 
  (* Print["G0=",G0,"\nG0nobar=",G0nobar,"\nG02=",G02];   (*for debug*) *)
  If[kname=="GP",
     idx=Select[Range[Length[G0]],keqmod[getRotMatOfK[brav,G0nobar[[#,1]]].k,-k]&]; 
     If[idx=={}&&!dsg, Return[{"GP",k,{{1,"x"}},"a",{1,1},{{"E",0}}}]];
     If[idx=={}&&dsg,  Return[{"GP",k,{{2,"x"}},"a",{2,1},{{"E",0},{"barE",0}}}]];
     re=Exp[-I*k.G02[[idx[[1]],2]]*2Pi]//Chop//Rationalize;
     tmp=<|"E"->1,"barE"->-1|>;
     re=Total[tmp[#1]*Exp[-I*k.#2*2Pi]&@@@G02[[idx]]]/Length[idx]//Chop//Rationalize;
     re=re/.{-1->2,0->3}; 
     If[!dsg,
       Return[{"GP",k,{{1,re}},"a",{1,1},{{"E",0}}}],
       Return[{"GP",k,{{2,re}},"a",{2,1},{{"E",0},{"barE",0}}}]
     ]
  ];

  (* For single-valued reps, all "UN" has Length[Gk]\[Equal]2, and the point group of Gk is Ci for
     No. 2 space group and Cs for all other space groups. The central extension (CE) may be
     either G_2^1(g=1) or G_4^1(g=2). And when CE\[Equal]G_4^1, the allowed reps are R2 and R4. *)
  (* For double-valued reps, all "UN" has Length[Gk]\[Equal]4, and the point group fo Gk is Ci^D for
     No. 2 space group and Cs^D for all other space groups. The CE may be G_4^1 (g=1 and Cs^D),
     G_4^2 (g=1 and Ci^D), or G_8^2 (g=2). And when CE\[Equal]G_8^2, the allowed reps are R5 and R7. *)
  If[kname=="UN",
     tmp=kinfo[[4]];
     If[dsg, tmp=Join[tmp,"bar"<>#&/@tmp]];
     Gk=G0[[Position[G0,#][[1,1]]&/@tmp]];
     a=aCentExt[brav,Gk,k,"DSG"->dsg];
     gens={{Gk[[2,1]],0}};   
     If[!dsg, If[a["g"]==1,
       (* --- for G_2^1 --- *)
       AGno={2,1}; allow={1,2}; clsidx={1,2}; CE={{"E",0},gens[[1]]}; 
       reptype=If[sgno==2,"a","c"],
       (* --- for G_4^1 --- *)
       AGno={4,1}; allow={2,4}; clsidx={1,2,3,4}; CE=CentExtPower[brav,a][gens[[1]],#]&/@{0,1,2,3};
       reptype="a";
     ]];
     If[dsg, 
       If[a["g"]==1,
         If[sgno==2,
           (* --- for G_4^2 --- *)
           AGno={4,2}; allow={2,4}; clsidx={1,2,3,4};  reptype="a"; 
           gens={{Gk[[2,1]],0}, {"barE",0}},
           (* --- for G_4^1 --- *)
           AGno={4,1}; allow={2,4}; clsidx={1,2,3,4};  reptype="b"
          ];
         CE={#1,0}&@@@Gk,
         (* ---a["g"]\[Equal]2: for G_8^2 --- *)
         AGno={8,2}; allow={5,7}; clsidx=Range[8];  reptype="a";  
         gens={{Gk[[2,1]],0}, {"barE",0}};
         CE=Join[{#1,0}&@@@Gk[[{1,2}]], {#1,1}&@@@Gk[[{3,4}]], 
                 {#1,0}&@@@Gk[[{3,4}]], {#1,1}&@@@Gk[[{1,2}]]];
       ];
     ];
     idx=Select[Range[Length[G0]],keqmod[getRotMatOfK[brav,G0nobar[[#,1]]].k,-k]&];
     If[idx=={},Return[{"UN",k,{#,"x"}&/@allow,reptype,AGno,CE}]];
     idx2=Position[CE,{#,0}][[1,1]]&/@G02[[idx,1]];
     idx3=clsidx[[idx2]];
     factor=Exp[-I k.#*2.*Pi]&/@G02[[idx,2]]//Simplify//Chop; 
     ct=AGCharTab@@AGno;
     sG02char=#[[idx3]]*factor&/@ct//Chop; 
     re=Total[#]/Length[Gk]&/@sG02char[[allow]]//Chop//Rationalize;
     re=re/.{-1->2,0->3}; 
     Return[{"UN",k,Transpose[{allow,re}],reptype,AGno,CE}]
  ];
  
  kBD=kinfo[[5]];    kname2=kname;
  If[brav=="TrigPrim"&&kname=="F",kname2=If[kBD=={0,1/2,-1/2},"aF","bF"]];
  idx=Select[Range[Length[G0]],keqmod[getRotMatOfK[brav,G0nobar[[#,1]]].kBD,-kBD]&];
  {AGno,gens,avRep,reptype}=If[!dsg, LGIrep[sgno][kname2], DLGIrep[sgno][kname2]];
   
  (*===============\[Equal] begin: calculate the allowed reps ======================*)
  abc=Association[Rule@@@Transpose[{{"a","b","c","d","e","f"},Range[2,7]}]];
  tmp=If[!dsg, LGIrepLabel[AGno], DLGIrepLabel[AGno]];
  Rs=Select[Transpose[{tmp[[1,2]],tmp[[abc[reptype],2]]}],#[[2]]!=""&][[All,1]];
  If[Rs=!=avRep[[All,1]],
    Print["calcRep: sgno=",sgno,", DSG=",dsg,", kname=",kname," AGno=",AGno," avRep",avRep[[All,1]],
          "!=Rs",Rs," in Tab.5.8/Tab.6.14, maybe wrong AGno or reptype ",reptype,"."];
  ];

  (*clsidx is the class indices for each element of the abstract group G_m^n*)
  nc=Length/@(AGClasses@@AGno);
  clsidx=Table[#,nc[[#]]]&/@Range[Length[nc]]//Flatten; 
  (* Print["kBD=",kBD,"\nclsidx=",clsidx,"\nidx=",idx,"\nAGno=",AGno,
          "\ngens=",gens,"\navRep=",avRep]; (*for debug*) *)
  ct=AGCharTab@@AGno;
  LG=getLGElem[sgno,kname2,"DSG"->dsg];
  If[VectorQ[gens[[1,2]]],  (* for high-symmetry point *)
    HLG=getHLGElem[brav,AGno,gens,"DSG"->dsg]; 
    idx2=Position[HLG,#][[1,1]]&/@G02[[idx,1]];
    idx3=clsidx[[idx2]];
    dt=G02[[idx[[#]],2]]-HLG[[idx2[[#]],2]]&/@Range[Length[idx]]; 
    factor=Exp[-I kBD.#*2.*Pi]&/@dt//Chop;
    sG02char=#[[idx3]]*factor&/@ct//Chop;
    (* Print["HLG=",HLG,"\nidx2=",idx2,"\nidx3=",idx3,"\nfactor=",factor,
             "\ndt=",dt,"\nsG02char=",sG02char]; (*for debug*)  *)
    (* ----------- calculate the allowed small reps and compare with avRep -------------*)
    If[!dsg, allow0=Range[Length[ct]],
       (*\:4e0d\:80fd\:76f4\:63a5\:627e {"barE",{0,0,0}}, \:56e0\:4e3a\:5bf9\:4e8eHerring\:5c0f\:7fa4\:5199\:4e3a\:4e0e\:67d0\:4e2a\:5e73\:79fb\:7fa4\:76f4\:79ef\:5f62\:5f0f\:7684\:60c5\:51b5\:ff0c
         \:53ef\:80fd\:6ca1\:6709 {"barE",{0,0,0}}\:ff0c\:6b64\:65f6 "barE" \:5bf9\:5e94\:7684\:662f\:975e\:96f6\:7684\:5e73\:79fb *)
       posbarE=Position[HLG,"barE"][[1,1]];
       barEfac=Exp[-I*2Pi*kBD.HLG[[posbarE,2]]];  
       allow0=Select[Range[Length[ct]], ct[[#,clsidx[[posbarE]]]]==-ct[[#,1]]*barEfac&];
    ];
    If[Length[HLG]==Length[LG], allow=allow0,
      gE=Select[HLG,First[#]=="E"&];
      n1=Length[gE];
      allow={};
      Do[
        Gmnchar=Association[Rule@@@Transpose[{HLG,ct[[i]][[clsidx]]}]];
        tmp=Table[Exp[-I*kBD.gE[[j,2]]*2Pi]Gmnchar[gE[[1]]]==Gmnchar[gE[[j]]],
                 {j,2,n1}]//Simplify//Flatten//DeleteDuplicates;
        If[tmp==={True},allow=Append[allow,i]];
       ,{i,allow0}];
     ];
    (*----------------------------------------------------------------------------*)
   ];
  
  If[IntegerQ[gens[[1,2]]],  (* for high-symmetry line *)
    CE=getCentExt[sgno,kname, "DSG"->dsg]; 
    idx2=Position[CE,{#,0}][[1,1]]&/@G02[[idx,1]];
    idx3=clsidx[[idx2]];
    factor=Exp[-I kBD.#*2.*Pi]&/@G02[[idx,2]]//Simplify//Chop; 
    sG02char=#[[idx3]]*factor&/@ct//Chop; 
    (*Print["CE=",CE,"\nidx2=",idx2,"\nidx3=",idx3,"\nfactor=",factor,"\nsG02char=",sG02char]; (*for debug*) *)
    (* ----------- calculate the allowed small reps and compare with avRep -------------*)
    If[!dsg, allow0=Range[Length[ct]],
       posbarE=Position[CE,{"barE",0}][[1,1]];
       allow0=Select[Range[Length[ct]], ct[[#,clsidx[[posbarE]]]]==-ct[[#,1]]&];
    ];
    If[Length[CE]==Length[LG], allow=allow0,
      gE=Select[CE,First[#]=="E"&];
      g=Max[CE[[All,2]]]+1;
      n1=Length[gE];
      allow={};
      Do[
        Gmnchar=Association[Rule@@@Transpose[{CE,ct[[i]][[clsidx]]}]];
        tmp=Table[Exp[I*gE[[j,2]]/g*2Pi]Gmnchar[gE[[1]]]==Gmnchar[gE[[j]]],
                  {j,2,n1}]//Simplify//Flatten//DeleteDuplicates;
        If[tmp==={True},allow=Append[allow,i]];
       ,{i,allow0}];
     ]; 
    (*----------------------------------------------------------------------------*)
  ];
  
  If[allow=!=avRep[[All,1]],     
    Print["calcRep: sgno=",sgno,", DSG=",dsg,", kname=",kname," AGno=",AGno," avRep ",avRep[[All,1]],
          " != the allowed reps calculated ",allow,"."];
    ];  
  (*===============\[Equal]   end: calculate the allowed reps ======================*)
 
  (*---------------- calculate the reality ----------------*)
  If[idx=={}, re={#,"x"}&/@allow, 
    re=Total[#]/Length[LG]&/@sG02char[[allow]]//Chop//Rationalize;
    re=re/.{-1->2,0->3}; 
    re=Transpose[{allow,re}]
  ];
  If[re=!=avRep, Print["calcRep: sgno=",sgno,", DSG=",dsg,", kname=",kname," AGno=",AGno," ",re,
                        "(calculated) != ",avRep,"(in the book)"]];
                        
  {kname,kBD,re}
];


(* ::Section:: *)
(*Labels of little group representations (Tab. 5.8 and Tab. 6.14)*)


strPreSup="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\), \("<>#<>"\)]\)"&;
strSub="\!\(\*SubscriptBox[\("<>#1<>"\), \("<>#2<>"\)]\)"&;
strSup="\!\(\*SuperscriptBox[\("<>#1<>"\), \("<>#2<>"\)]\)"&;
strSubsup="\!\(\*SubsuperscriptBox[\("<>#1<>"\), \("<>#2<>"\), \("<>#3<>"\)]\)"&;
strBar="\!\(\*OverscriptBox[\("<>#<>"\), \(_\)]\)"&;

str2Mulliken[str_String] := Module[{cs, out="", L0, sub, sup, bar},
  bar=StringLength[str]>=3 && StringTake[str,3] =="bar";
  cs=Characters[str];   If[bar, cs=cs[[4;;]]];
  If[cs[[1]]=="1"||cs[[1]]=="2", out=strPreSup[cs[[1]]]; cs=cs[[2;;]]];
  L0=If[bar, strBar[cs[[1]]], cs[[1]]]; 
  cs=cs[[2;;]];
  If[cs=={}, out=out<>L0; Goto["end"]];
  If[cs[[-1]]!="'", out=out<>strSub[L0,StringJoin[cs]]; Goto["end"]];
  If[Length[cs]>=2&&cs[[-2]]=="'", 
    out=out<>If[cs[[1;;-3]]!={},
      strSubsup[L0, StringJoin[cs[[1;;-3]]], "\[DoublePrime]"],
      strSup[L0, "\[DoublePrime]"] ];
    Goto["end"] 
  ];
  If[cs[[-1]]=="'", 
    out=out<>If[cs[[1;;-2]]!={},
      strSubsup[L0, StringJoin[cs[[1;;-2]]], "\[Prime]"],
      strSup[L0, "\[Prime]"] ]
  ];
  Label["end"];
  out=StringReplace[out," "->""];  
  If[bar, StringReplace[out, {"\(\!\(\*Over"->"Over", "_\)]\)\)"->"_\)]"}], out]
]

Mulliken2str[MulStr_String]:=Module[{rmlist, sub, bar, str},
  sub={"\[DoublePrime]"->"''", "\[Prime]"->"'", "\[InvisiblePrefixScriptBase]"->""};
  str=StringReplace[MulStr,sub];
  rmlist= {"SuperscriptBox", "SubscriptBox", "SubsuperscriptBox", "OverscriptBox",
           "\*", "\!", "\(", "\)", ",", "[", "]"," "};
  str=StringReplace[str, #->""&/@rmlist];
  bar=StringPosition[str,"_"]!={};
  If[bar, "bar"<>StringReplace[str,"_"->""], str]
]

str2GammaLabel[str_String]:=Module[{knsub,pm,s,n,ds,bar},
  knsub={"DT"->"\[CapitalDelta]","GM"->"\[CapitalGamma]","LD"->"\[CapitalLambda]","SM"->"\[CapitalSigma]"};
  s=str;   pm=StringTake[s,-1];
  If[MemberQ[{"+","-"},pm], s=StringTake[s,{1,-2}], pm=""];
  If[StringLength[s]>3&&StringTake[s,3]=="bar", bar=True; s=StringTake[s,{4,-1}], bar=False];
  ds=ToString/@Range[0,9];
  If[MemberQ[ds,StringTake[s,{-2,-2}]], 
    n=StringTake[s,-2]; s=StringTake[s,{1,-3}],
    n=StringTake[s,-1];
    If[MemberQ[ds,n], s=StringTake[s,{1,-2}], n=""]
  ];
  s=StringReplace[s,knsub];
  If[bar, s="\!\(\*OverscriptBox[\("<>s<>"\), \(_\)]\)"];
  If[pm=="", RepGammaLabel[s,n], RepGammaLabel[s,{n,pm}]]
]

GammaLabel2str[GMLstr_String]:=Module[{sub,rmlist,str},
  sub={"\[CapitalDelta]"->"DT","\[CapitalGamma]"->"GM","\[CapitalLambda]"->"LD",
       "\[CapitalSigma]"->"SM","\[InvisiblePrefixScriptBase]"->""};
  str=StringReplace[GMLstr,sub];
  rmlist= {"SuperscriptBox", "SubscriptBox", "SubsuperscriptBox", "OverscriptBox",
           "\*", "\!", "\(", "\)", ",", "[", "]"," "};
  str=StringReplace[str, #->""&/@rmlist];
  bar=StringPosition[str,"_"]!={};
  If[bar, "bar"<>StringReplace[str,"_"->""], str]
]


Module[{na,Ag,Au,Ap,App,A1,A2,A3,B1,B2,B3,Bg,Bu,A1g,A1u,A2g,A2u,B1g,B1u,B2g,B2u,B3g,B3u,A1p,A1pp,A2p,A2pp,
        Ep,Epp,Eg,Eu,fE,sE,E1,E2,fE1,sE1,fE2,sE2,fEg,sEg,fEu,sEu,fE1g,sE1g,fE1p,fE2p,fE1pp,fE2pp,
        fE1u,sE1u,fE2g,sE2g,fE2u,sE2u,fEp,sEp,fEpp,sEpp,E1g,E1u,E2g,E2u,E3,E4,fF,sF,T1,T2,Tg,Tu,
        fF1,fF2,sF1,sF2,fEgp,sEgp,fEup,sEup,fEgpp,sEgpp,fEupp,sEupp,fE3,fE4,fF3,fFg,fFu,sFg,sFu,F2,
        fJ,T1g,T2g,T1u,T2u,fH,sH},
na="";
Ag="\!\(\*SubscriptBox[\(A\),\(g\)]\)";  Au="\!\(\*SubscriptBox[\(A\),\(u\)]\)";
Ap="\!\(\*SuperscriptBox[\(A\),\(\[Prime]\)]\)";  App="\!\(\*SuperscriptBox[\(A\),\(\[DoublePrime]\)]\)";
A1="\!\(\*SubscriptBox[\(A\),\(1\)]\)";  A2="\!\(\*SubscriptBox[\(A\),\(2\)]\)";  A3="\!\(\*SubscriptBox[\(A\),\(3\)]\)";
B1="\!\(\*SubscriptBox[\(B\),\(1\)]\)";  B2="\!\(\*SubscriptBox[\(B\),\(2\)]\)";  B3="\!\(\*SubscriptBox[\(B\),\(3\)]\)";
Bg="\!\(\*SubscriptBox[\(B\),\(g\)]\)";  Bu="\!\(\*SubscriptBox[\(B\),\(u\)]\)";
Ep="\!\(\*SuperscriptBox[\(E\),\(\[Prime]\)]\)";  Epp="\!\(\*SuperscriptBox[\(E\),\(\[DoublePrime]\)]\)";
Eg="\!\(\*SubscriptBox[\(E\),\(g\)]\)";  Eu="\!\(\*SubscriptBox[\(E\),\(u\)]\)";
fE="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)E";  sE="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)E";
E1="\!\(\*SubscriptBox[\(E\),\(1\)]\)";  E2="\!\(\*SubscriptBox[\(E\),\(2\)]\)";
A1g="\!\(\*SubscriptBox[\(A\),\(1g\)]\)";  A1u="\!\(\*SubscriptBox[\(A\),\(1u\)]\)";
A2g="\!\(\*SubscriptBox[\(A\),\(2g\)]\)";  A2u="\!\(\*SubscriptBox[\(A\),\(2u\)]\)";
B1g="\!\(\*SubscriptBox[\(B\),\(1g\)]\)";  B1u="\!\(\*SubscriptBox[\(B\),\(1u\)]\)";
B2g="\!\(\*SubscriptBox[\(B\),\(2g\)]\)";  B2u="\!\(\*SubscriptBox[\(B\),\(2u\)]\)";
B3g="\!\(\*SubscriptBox[\(B\),\(3g\)]\)";  B3u="\!\(\*SubscriptBox[\(B\),\(3u\)]\)";
A1p="\!\(\*SubsuperscriptBox[\(A\),\(1\),\(\[Prime]\)]\)";  A1pp="\!\(\*SubsuperscriptBox[\(A\),\(1\),\(\[DoublePrime]\)]\)";
A2p="\!\(\*SubsuperscriptBox[\(A\),\(2\),\(\[Prime]\)]\)";  A2pp="\!\(\*SubsuperscriptBox[\(A\),\(2\),\(\[DoublePrime]\)]\)";
fE1="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SubscriptBox[\(E\),\(1\)]\)";  sE1="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)\!\(\*SubscriptBox[\(E\),\(1\)]\)";
fE2="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SubscriptBox[\(E\),\(2\)]\)";  sE2="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)\!\(\*SubscriptBox[\(E\),\(2\)]\)";
fEg="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SubscriptBox[\(E\),\(g\)]\)";  sEg="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)\!\(\*SubscriptBox[\(E\),\(g\)]\)";
fEu="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SubscriptBox[\(E\),\(u\)]\)";  sEu="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)\!\(\*SubscriptBox[\(E\),\(u\)]\)";
fE1g="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SubscriptBox[\(E\),\(1g\)]\)";  sE1g="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)\!\(\*SubscriptBox[\(E\),\(1g\)]\)";
fE1u="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SubscriptBox[\(E\),\(1u\)]\)";  sE1u="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)\!\(\*SubscriptBox[\(E\),\(1u\)]\)";
fE2g="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SubscriptBox[\(E\),\(2g\)]\)";  sE2g="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)\!\(\*SubscriptBox[\(E\),\(2g\)]\)";
fE2u="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SubscriptBox[\(E\),\(2u\)]\)";  sE2u="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)\!\(\*SubscriptBox[\(E\),\(2u\)]\)";
fEp="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SuperscriptBox[\(E\),\(\[Prime]\)]\)";  sEp="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)\!\(\*SuperscriptBox[\(E\),\(\[Prime]\)]\)";
fEpp="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SuperscriptBox[\(E\),\(\[DoublePrime]\)]\)";  sEpp="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)\!\(\*SuperscriptBox[\(E\),\(\[DoublePrime]\)]\)";
fE1p="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SubsuperscriptBox[\(E\),\(1\),\(\[Prime]\)]\)";  fE1pp="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SubsuperscriptBox[\(E\),\(1\),\(\[DoublePrime]\)]\)";
fE2p="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SubsuperscriptBox[\(E\),\(2\),\(\[Prime]\)]\)";  fE2pp="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SubsuperscriptBox[\(E\),\(2\),\(\[DoublePrime]\)]\)";
E1g="\!\(\*SubscriptBox[\(E\),\(1g\)]\)";  E1u="\!\(\*SubscriptBox[\(E\),\(1u\)]\)";
E2g="\!\(\*SubscriptBox[\(E\),\(2g\)]\)";  E2u="\!\(\*SubscriptBox[\(E\),\(2u\)]\)";
E3="\!\(\*SubscriptBox[\(E\),\(3\)]\)";  E4="\!\(\*SubscriptBox[\(E\),\(4\)]\)";
fF="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)F";  sF="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)F";
T1="\!\(\*SubscriptBox[\(T\),\(1\)]\)";  T2="\!\(\*SubscriptBox[\(T\),\(2\)]\)";
Tg="\!\(\*SubscriptBox[\(T\),\(g\)]\)";  Tu="\!\(\*SubscriptBox[\(T\),\(u\)]\)";
fF1="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SubscriptBox[\(F\),\(1\)]\)";  sF1="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)\!\(\*SubscriptBox[\(F\),\(1\)]\)";
fF2="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SubscriptBox[\(F\),\(2\)]\)";  sF2="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)\!\(\*SubscriptBox[\(F\),\(2\)]\)";
fEgp="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SubsuperscriptBox[\(E\),\(g\),\(\[Prime]\)]\)";  fEgpp="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SubsuperscriptBox[\(E\),\(g\),\(\[DoublePrime]\)]\)";
fEup="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SubsuperscriptBox[\(E\),\(u\),\(\[Prime]\)]\)";  fEupp="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SubsuperscriptBox[\(E\),\(u\),\(\[DoublePrime]\)]\)";
sEgp="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)\!\(\*SubsuperscriptBox[\(E\),\(g\),\(\[Prime]\)]\)";  sEgpp="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)\!\(\*SubsuperscriptBox[\(E\),\(g\),\(\[DoublePrime]\)]\)";
sEup="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)\!\(\*SubsuperscriptBox[\(E\),\(u\),\(\[Prime]\)]\)";  sEupp="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)\!\(\*SubsuperscriptBox[\(E\),\(u\),\(\[DoublePrime]\)]\)";
fE3="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SubscriptBox[\(E\),\(3\)]\)";  fE4="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SubscriptBox[\(E\),\(4\)]\)";
fF3="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SubscriptBox[\(F\),\(3\)]\)";  F2="\!\(\*SubscriptBox[\(F\),\(2\)]\)";
fFg="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SubscriptBox[\(F\),\(g\)]\)";  sFg="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)\!\(\*SubscriptBox[\(F\),\(g\)]\)";
fFu="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SubscriptBox[\(F\),\(u\)]\)";  sFu="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)\!\(\*SubscriptBox[\(F\),\(u\)]\)";
fJ="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)J";  fH="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)H";  sH="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)H";
T1g="\!\(\*SubscriptBox[\(T\),\(1g\)]\)";  T1u="\!\(\*SubscriptBox[\(T\),\(1u\)]\)";
T2g="\!\(\*SubscriptBox[\(T\),\(2g\)]\)";  T2u="\!\(\*SubscriptBox[\(T\),\(2u\)]\)";

LGIrepLabel=<||>;
LGIrepLabel[{1,1}]={{"R",{1},{1}}, {"a",{"A"},{1}}};
LGIrepLabel[{2,1}]={{"R",{1,2},{1,1}},
  {"a",{Ag,Au},{{1,"+"},{1,"-"}}},
  {"b",{"A","B"},{1,2}},{"c",{Ap,App},{1,2}}};
LGIrepLabel[{3,1}]={{"R",{1,2,3},{1,1,1}},
  {"a",{"A",sE,fE},{1,2,3}},
  {"b",{"A",fE,sE},{1,2,3}}};
LGIrepLabel[{4,1}]={{"R",{1,2,3,4},{1,1,1,1}},
  {"a",{na,sE,na,fE},{na,1,na,2}},
  {"b",{"A",sE,"B",fE},{1,2,3,4}}};
LGIrepLabel[{4,2}]={{"R",{1,2,3,4},{1,1,1,1}},
  {"a",{Ag,Au,Bg,Bu},{{1,"+"},{1,"-"},{2,"+"},{2,"-"}}},
  {"b",{"A",B1,B2,B3},{1,3,2,4}},
  {"c",{A1,A2,B1,B2},{1,3,2,4}}};
LGIrepLabel[{6,1}]={{"R",{1,2,3,4,5,6},{1,1,1,1,1,1}},
  {"a",{Ag,fEu,sEg,Au,fEg,sEu},{{1,"+"},{3,"-"},{2,"+"},{1,"-"},{3,"+"},{2,"-"}}},
  {"b",{na,na,fE1,na,na,fE2},{na,na,1,na,na,2}},
  {"c",{na,fE1,na,na,fE2,na},{na,1,na,na,2,na}},
  {"d",{"A",sE2,fE1,"B",sE1,fE2},{1,3,5,4,6,2}},
  {"e",{Ap,fEpp,sEp,App,fEp,sEpp},{1,5,3,4,2,6}},
  {"f",{na,sE,na,"A",na,fE},{na,3,na,1,na,2}}};
LGIrepLabel[{6,2}]={{"R",{1,2,3},{1,1,2}},{"a",{A1,A2,"E"},{1,2,3}}};
LGIrepLabel[{8,1}]={{"R",{2,4,6,8},{1,1,1,1}},{"a",{sE1,fE2,sE2,fE1},{1,2,3,4}},
  {"b",{fE1,na,fE2,na},{1,na,2,na}},{"c",{na,fE1,na,fE2},{na,1,na,2}}};
LGIrepLabel[{8,2}]={{"R",{1,2,3,4,5,6,7,8},{1,1,1,1,1,1,1,1}},
  {"a",{na,sEg,na,fEg,na,sEu,na,fEu},{na,{1,"+"},na,{2,"+"},na,{1,"-"},na,{2,"-"}}},
  {"b",{na,na,na,na,"A",sE,"B",fE},{na,na,na,na,1,4,2,3}},
  {"c",{na,sEp,na,fEp,na,sEpp,na,fEpp},{na,1,na,2,na,3,na,4}},
  {"d",{na,sE1,na,fE1,na,sE2,na,fE2},{na,1,na,2,na,3,na,4}},
  {"e",{Ag,sEg,Bg,fEg,Au,sEu,Bu,fEu},{{1,"+"},{4,"+"},{2,"+"},{3,"+"},{1,"-"},{4,"-"},{2,"-"},{3,"-"}}}};
LGIrepLabel[{8,3}]={{"R",{1,2,3,4,5,6,7,8},{1,1,1,1,1,1,1,1}},
  {"a",{na,na,na,na,A1,B1,A2,B2},{na,na,na,na,1,2,3,4}},
  {"b",{Ag,B2g,B1g,B3g,Au,B2u,B1u,B3u},{{1,"+"},{2,"+"},{3,"+"},{4,"+"},{1,"-"},{2,"-"},{3,"-"},{4,"-"}}}};
LGIrepLabel[{8,4}]={{"R",{1,2,3,4,5},{1,1,1,1,2}},
  {"a",{na,na,na,na,"E"},{na,na,na,na,1}},
  {"b",{A1,A2,B1,B2,"E"},{1,2,3,4,5}}};
LGIrepLabel[{8,5}]={{"R",{5},{2}},{"a",{"E"},{1}}};
LGIrepLabel[{12,1}]={{"R",{2,4,6,8,10,12},{1,1,1,1,1,1}},
  {"a",{sE2,A1,fE2,sE1,A2,fE1},{6,1,5,4,2,3}},
  {"b",{na,na,fE1,na,na,fE2},{na,na,1,na,na,2}},
  {"c",{fE1,na,na,fE2,na,na},{1,na,na,2,na,na}}};
LGIrepLabel[{12,2}]={{"R",Range[12],Table[1,12]},
  {"a",{Ag,sE1g,fE1g,Bg,sE2g,fE2g,Au,sE1u,fE1u,Bu,sE2u,fE2u},
       {{1,"+"},{6,"+"},{5,"+"},{4,"+"},{3,"+"},{2,"+"},
        {1,"-"},{6,"-"},{5,"-"},{4,"-"},{3,"-"},{2,"-"}}}}; (* the last two Gamma reps are exchanged compared with the book*)
LGIrepLabel[{12,3}]={{"R",{1,2,3,4,5,6},{1,1,1,1,2,2}},
  {"a",{A1g,A2g,A1u,A2u,Eg,Eu},{{1,"+"},{2,"+"},{1,"-"},{2,"-"},{3,"+"},{3,"-"}}},
  {"b",{na,na,A1,A2,na,"E"},{na,na,1,2,na,3}},
  {"c",{A1,A2,B1,B2,E2,E1},{1,2,3,4,5,6}},
  {"d",{A1,A2,B2,B1,E2,E1},{1,2,3,4,5,6}},
  {"e",{A1p,A2p,A1pp,A2pp,Ep,Epp},{1,2,3,4,5,6}},
  {"f",{na,na,Ap,App,na,"E"},{na,na,1,2,na,3}}};
LGIrepLabel[{12,4}]={{"R",{3,4,6},{1,1,2}},{"a",{fE1,sE1,E2},{1,2,3}}};
LGIrepLabel[{12,5}]={{"R",{1,2,3,4},{1,1,1,3}},{"a",{"A",fE,sE,"T"},{1,3,2,4}}};
LGIrepLabel[{16,3}]={{"R",{5,6,7,8},{1,1,1,1}},{"a",{"A",sE,"B",fE},{1,2,3,4}}};
LGIrepLabel[{16,4}]={{"R",{2,4,6,8,10,12,14,16},{1,1,1,1,1,1,1,1}},
  {"a",{sE1g,fE1g,sE2g,fE2g,sE1u,fE1u,sE2u,fE2u},{{1,"+"},{2,"+"},{3,"+"},{4,"+"},{1,"-"},{2,"-"},{3,"-"},{4,"-"}}},
  (*Note that I changed the following line. 
    I think the origional labels in the book miss a superscript 1 at the left top corner. *)
  {"b",{na,fE1p,na,fE1pp,na,fE2p,na,fE2pp},{na,1,na,2,na,3,na,4}}};
LGIrepLabel[{16,6}]={{"R",{9,10},{2,2}},{"a",{na,"E"},{na,1}},{"b",{"E",na},{1,na}}};
LGIrepLabel[{16,7}]={{"R",{9,10},{2,2}},{"a",{Ep,Epp},{1,2}},{"b",{E1,E2},{1,2}},
  {"c",{na,"E"},{na,1}},{"d",{sF,fF},{1,2}},{"e",{na,fF},{na,1}},{"f",{fF,na},{1,na}}};
LGIrepLabel[{16,8}]={{"R",{5,6,7,8,10},{1,1,1,1,2}},{"a",{sE2,fE2,fE1,sE1,"E"},{1,2,3,4,5}}};
LGIrepLabel[{16,9}]={{"R",Range[10],{1,1,1,1,2,1,1,1,1,2}},
  {"a",{na,na,na,na,E1,na,na,na,na,E2},{na,na,na,na,1,na,na,na,na,2}},
  {"b",{na,na,na,na,Ep,na,na,na,na,Epp},{na,na,na,na,1,na,na,na,na,2}},
  {"c",{na,na,na,na,Eg,na,na,na,na,Eu},{na,na,na,na,{1,"+"},na,na,na,na,{1,"-"}}},
  {"d",{na,na,na,na,na,A1,A2,B1,B2,"E"},{na,na,na,na,na,1,2,3,4,5}},
  {"e",{A1g,A2g,B1g,B2g,Eg,A1u,A2u,B1u,B2u,Eu},
       {{1,"+"},{2,"+"},{3,"+"},{4,"+"},{5,"+"},{1,"-"},{2,"-"},{3,"-"},{4,"-"},{5,"-"}}},
  {"f",{na,na,na,na,"E",A1,A2,B1,B2,na},{na,na,na,na,5,1,2,3,4,na}}};
LGIrepLabel[{16,10}]={{"R",{5,6,7,8,9,10},{1,1,1,1,2,2}},
  {"a",{sE1,fE1,fE2,sE2,"E",na},{1,2,3,4,5,na}},
  {"b",{sEp,fEp,fEpp,sEpp,"E",na},{1,2,3,4,5,na}},
  {"c",{sEp,fEp,fEpp,sEpp,na,"E"},{1,2,3,4,na,5}},
  {"d",{na,na,na,na,E2,E1},{na,na,na,na,1,2}}};
LGIrepLabel[{16,11}]={{"R",{5,10},{2,2}},{"a",{Eg,Eu},{{1,"+"},{1,"-"}}}};
LGIrepLabel[{16,12}]={{"R",{6,7},{2,2}},{"a",{E1,E2},{1,2}}};
LGIrepLabel[{16,13}]={{"R",{6,7},{2,2}},{"a",{fF,sF},{1,2}}};
LGIrepLabel[{24,1}]={{"R",{7,8,9},{2,2,2}},{"a",{fF,sF,"E"},{1,2,3}}};
LGIrepLabel[{24,2}]={{"R",{7,8,9},{2,2,2}},{"a",{fF,sF,"E"},{1,2,3}}};
LGIrepLabel[{24,3}]={{"R",{3,4,6,9,10,12},{1,1,2,1,1,2}},
  {"a",{fE1,sE1,E3,fE2,sE2,E4},{1,2,3,4,5,6}}};
LGIrepLabel[{24,4}]={{"R",{4,5,6,10,11,12},{1,1,2,1,1,2}},
  {"a",{fE1,fE2,fF,sE1,sE2,sF},{1,2,3,4,5,6}}};
LGIrepLabel[{24,5}]={{"R",Range[12],Table[1,12]},
  {"a",{A1g,A2g,E2g,B1g,B2g,E1g,A1u,A2u,E2u,B1u,B2u,E1u},
       {{1,"+"},{2,"+"},{6,"+"},{3,"+"},{4,"+"},{5,"+"},{1,"-"},{2,"-"},{6,"-"},{3,"-"},{4,"-"},{5,"-"}}}};
LGIrepLabel[{24,6}]={{"R",{13,14,15},{2,2,2}},{"a",{sF,"E",fF},{1,2,3}}};
LGIrepLabel[{24,7}]={{"R",{1,2,3,4,5},{1,1,2,3,3}},{"a",{A1,A2,"E",T1,T2},{1,2,3,4,5}}};
LGIrepLabel[{24,8}]={{"R",{4,5,6,8},{1,1,1,3}},{"a",{"A",fE,sE,"T"},{1,2,3,4}}};
LGIrepLabel[{24,9}]={{"R",{4,5,6},{2,2,2}},{"a",{"E",sF,fF},{1,2,3}}};
LGIrepLabel[{24,10}]={{"R",Range[8],{1,1,1,3,1,1,1,3}},
  {"a",{Ag,fEg,sEg,Tg,Au,fEu,sEu,Tu},{{1,"+"},{2,"+"},{3,"+"},{4,"+"},{1,"-"},{2,"-"},{3,"-"},{4,"-"}}}};
LGIrepLabel[{32,1}]={{"R",{10,11,12,13},{2,2,2,2}},{"a",{E1,E2,fF,sF},{1,2,3,4}}};
LGIrepLabel[{32,2}]={{"R",{9,10,11,12,13,14},{2,2,2,2,2,2}},
  {"a",{E1,na,Ep,E2,na,Epp},{1,na,2,3,na,4}},
  {"b",{Ep,na,E1,Epp,na,E2},{1,na,2,3,na,4}},
  {"c",{na,E1,E2,na,E3,E4},{na,1,2,na,3,4}},
  {"d",{E1,na,E2,E3,E4,na},{1,na,2,3,4,na}}};
LGIrepLabel[{32,4}]={{"R",{11,12,13,14},{2,2,2,2}},
  {"a",{na,na,fF,sF},{na,na,1,2}},{"b",{fF1,fF2,na,na},{1,2,na,na}}};
LGIrepLabel[{32,5}]={{"R",{5,6,7,8,10,15,16,17,18,20},{1,1,1,1,2,1,1,1,1,2}},
  {"a",{sEgp,fEgp,fEgpp,sEgpp,Eg,sEup,fEup,fEupp,sEupp,Eu},
       {{1,"+"},{2,"+"},{3,"+"},{4,"+"},{5,"+"},{1,"-"},{2,"-"},{3,"-"},{4,"-"},{5,"-"}}},
  {"b",{sE1g,fE1g,fE2g,sE2g,Eg,sE1u,fE1u,fE2u,sE2u,Eu},
       {{1,"+"},{2,"+"},{3,"+"},{4,"+"},{5,"+"},{1,"-"},{2,"-"},{3,"-"},{4,"-"},{5,"-"}}}};
LGIrepLabel[{32,6}]={{"R",{9,10,13,14},{2,2,2,2}},{"a",{sF1,fF1,sF2,fF2},{1,2,3,4}}};
LGIrepLabel[{32,13}]={{"R",{13,14,15,16,20},{1,1,1,1,2}},
  {"a",{fE1,fE2,fE3,fE4,fF1},{1,2,3,4,5}}};
LGIrepLabel[{48,1}]={{"R",{13,14,15},{2,2,4}},{"a",{Ep,Epp,"F"},{1,2,3}}};
LGIrepLabel[{48,2}]={{"R",{7,8,9,16,17,18},{2,2,2,2,2,2}},
  {"a",{fF1,sF1,E1,fF2,sF2,E2},{1,2,3,4,5,6}}};
LGIrepLabel[{48,3}]={{"R",{7,8,9},{2,2,2}},{"a",{fF1,fF2,fF3},{1,2,3}}};
LGIrepLabel[{48,4}]={{"R",{4,5,6,11,12,13},{2,2,2,2,2,2}},
  {"a",{Eg,sFg,fFg,Eu,sFu,fFu},{{1,"+"},{2,"+"},{3,"+"},{1,"-"},{2,"-"},{3,"-"}}}};
LGIrepLabel[{48,5}]={{"R",{4,5,6,8,12,13,14,16},{1,1,1,3,1,1,1,3}},
  {"a",{Ag,fEg,sEg,Tg,Au,fEu,sEu,Tu},
       {{1,"+"},{2,"+"},{3,"+"},{4,"+"},{1,"-"},{2,"-"},{3,"-"},{4,"-"}}}};
LGIrepLabel[{48,6}]={{"R",{4,5,8},{2,2,4}},{"a",{fF1,sF1,F2},{1,2,3}}};
LGIrepLabel[{48,7}]={{"R",Range[10],{1,1,1,1,2,2,3,3,3,3}},
  {"a",{A1g,A2g,A2u,A1u,Eg,Eu,T1g,T2g,T1u,T2u},
       {{1,"+"},{2,"+"},{2,"-"},{1,"-"},{3,"+"},{3,"-"},{4,"+"},{5,"+"},{4,"-"},{5,"-"}}},
  {"b",{na,na,A1,A2,na,"E",na,na,T1,T2},{na,na,1,2,na,3,na,na,4,5}}}; (* The book is wrong for this line.*)
LGIrepLabel[{48,8}]={{"R",{3,4,6,9,10},{1,1,2,3,3}},{"a",{sE1,fE1,E2,sH,fH},{1,2,3,4,5}}};
LGIrepLabel[{96,1}]={{"R",{9,10,16},{2,2,4}},{"a",{fF1,fF2,fJ},{1,2,3}}};
LGIrepLabel[{96,2}]={{"R",{7,8,9,14},{2,2,2,6}},{"a",{"E",fF,sF,"H"},{1,2,3,4}}};
LGIrepLabel[{96,4}]={{"R",{7,8,9,14},{2,2,2,6}},{"a",{"E",fF,sF,"H"},{1,2,3,4}}};
];


Module[{na,Ab,Bb,Eb,Tb,Fb,Hb,Jb,Abg,Abu,Bbg,Bbu,Bb1,Bb2,Bb3,fEb,sEb,fEb1,sEb1,fEb2,sEb2,
        fEb3,sEb3,fEb4,sEb4,fEbg,sEbg,fEbu,sEbu,Eb1,Eb2,Ab1,Ab2,fEb1g,sEb1g,fEb1u,sEb1u,
        fEb2g,sEb2g,fEb2u,sEb2u,Eb1g,Eb1u,Eb2g,Eb2u,fEbp,sEbp,fEbpp,sEbpp,fFb,sFb,Ebg,Ebu,
        Abp,Abpp,Bbp,Bbpp,Eb3,Eb4,fEb3g,sEb3g,fEb3u,sEb3u,fEbgp,sEbgp,fEbgpp,sEbgpp,fEbup,
        sEbup,fEbupp,sEbupp,Abgp,Abup,Abgpp,Abupp,Bbgp,Bbup,Bbgpp,Bbupp,Tbg,Tbu,fFbg,sFbg,
        fFbu,sFbu,fFb1,sFb1,fFb2,sFb2,fHb,sHb,Eb3g,Eb3u,Fbg,Fbu,fFb3,sFb3,Fb2,fHb1,fHb2,fJb,sJb},
na=""; Ab="\!\(\*OverscriptBox[\(A\),\(_\)]\)"; Bb="\!\(\*OverscriptBox[\(B\),\(_\)]\)"; Eb="\!\(\*OverscriptBox[\(E\),\(_\)]\)";
Tb="\!\(\*OverscriptBox[\(T\),\(_\)]\)"; Fb="\!\(\*OverscriptBox[\(F\),\(_\)]\)"; Hb="\!\(\*OverscriptBox[\(H\),\(_\)]\)"; Jb="\!\(\*OverscriptBox[\(J\),\(_\)]\)";
Abg="\!\(\*SubscriptBox[OverscriptBox[\(A\),\(_\)],\(g\)]\)";  Abu="\!\(\*SubscriptBox[OverscriptBox[\(A\),\(_\)],\(u\)]\)";
Bbg="\!\(\*SubscriptBox[OverscriptBox[\(B\),\(_\)],\(g\)]\)";  Bbu="\!\(\*SubscriptBox[OverscriptBox[\(B\),\(_\)],\(u\)]\)";
Bb1="\!\(\*SubscriptBox[OverscriptBox[\(B\),\(_\)],\(1\)]\)";  Bb2="\!\(\*SubscriptBox[OverscriptBox[\(B\),\(_\)],\(2\)]\)";  Bb3="\!\(\*SubscriptBox[OverscriptBox[\(B\),\(_\)],\(3\)]\)";
fEb="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*OverscriptBox[\(E\),\(_\)]\)";  sEb="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)\!\(\*OverscriptBox[\(E\),\(_\)]\)";
fEb1="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SubscriptBox[OverscriptBox[\(E\),\(_\)],\(1\)]\)";  sEb1="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)\!\(\*SubscriptBox[OverscriptBox[\(E\),\(_\)],\(1\)]\)";
fEb2="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SubscriptBox[OverscriptBox[\(E\),\(_\)],\(2\)]\)";  sEb2="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)\!\(\*SubscriptBox[OverscriptBox[\(E\),\(_\)],\(2\)]\)";
fEb3="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SubscriptBox[OverscriptBox[\(E\),\(_\)],\(3\)]\)";  sEb3="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)\!\(\*SubscriptBox[OverscriptBox[\(E\),\(_\)],\(3\)]\)";
fEb4="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SubscriptBox[OverscriptBox[\(E\),\(_\)],\(4\)]\)";  sEb4="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)\!\(\*SubscriptBox[OverscriptBox[\(E\),\(_\)],\(4\)]\)";
fEbg="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SubscriptBox[OverscriptBox[\(E\),\(_\)],\(g\)]\)";  sEbg="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)\!\(\*SubscriptBox[OverscriptBox[\(E\),\(_\)],\(g\)]\)";
fEbu="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SubscriptBox[OverscriptBox[\(E\),\(_\)],\(u\)]\)";  sEbu="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)\!\(\*SubscriptBox[OverscriptBox[\(E\),\(_\)],\(u\)]\)";
Eb1="\!\(\*SubscriptBox[OverscriptBox[\(E\),\(_\)],\(1\)]\)";  Eb2="\!\(\*SubscriptBox[OverscriptBox[\(E\),\(_\)],\(2\)]\)";
Eb3="\!\(\*SubscriptBox[OverscriptBox[\(E\),\(_\)],\(3\)]\)";  Eb4="\!\(\*SubscriptBox[OverscriptBox[\(E\),\(_\)],\(4\)]\)";
Ab1="\!\(\*SubscriptBox[OverscriptBox[\(A\),\(_\)],\(1\)]\)";  Ab2="\!\(\*SubscriptBox[OverscriptBox[\(A\),\(_\)],\(2\)]\)";
fEb1g="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SubscriptBox[OverscriptBox[\(E\),\(_\)],\(1g\)]\)";  sEb1g="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)\!\(\*SubscriptBox[OverscriptBox[\(E\),\(_\)],\(1g\)]\)";
fEb1u="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SubscriptBox[OverscriptBox[\(E\),\(_\)],\(1u\)]\)";  sEb1u="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)\!\(\*SubscriptBox[OverscriptBox[\(E\),\(_\)],\(1u\)]\)";
fEb2g="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SubscriptBox[OverscriptBox[\(E\),\(_\)],\(2g\)]\)";  sEb2g="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)\!\(\*SubscriptBox[OverscriptBox[\(E\),\(_\)],\(2g\)]\)";
fEb2u="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SubscriptBox[OverscriptBox[\(E\),\(_\)],\(2u\)]\)";  sEb2u="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)\!\(\*SubscriptBox[OverscriptBox[\(E\),\(_\)],\(2u\)]\)";
fEb3g="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SubscriptBox[OverscriptBox[\(E\),\(_\)],\(3g\)]\)";  sEb3g="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)\!\(\*SubscriptBox[OverscriptBox[\(E\),\(_\)],\(3g\)]\)";
fEb3u="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SubscriptBox[OverscriptBox[\(E\),\(_\)],\(3u\)]\)";  sEb3u="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)\!\(\*SubscriptBox[OverscriptBox[\(E\),\(_\)],\(3u\)]\)";
Eb1g="\!\(\*SubscriptBox[OverscriptBox[\(E\),\(_\)],\(1g\)]\)";  Eb1u="\!\(\*SubscriptBox[OverscriptBox[\(E\),\(_\)],\(1u\)]\)";
Eb2g="\!\(\*SubscriptBox[OverscriptBox[\(E\),\(_\)],\(2g\)]\)";  Eb2u="\!\(\*SubscriptBox[OverscriptBox[\(E\),\(_\)],\(2u\)]\)";
Eb3g="\!\(\*SubscriptBox[OverscriptBox[\(E\),\(_\)],\(3g\)]\)";  Eb3u="\!\(\*SubscriptBox[OverscriptBox[\(E\),\(_\)],\(3u\)]\)";
fEbp="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SuperscriptBox[OverscriptBox[\(E\),\(_\)],\(\[Prime]\)]\)";  sEbp="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)\!\(\*SuperscriptBox[OverscriptBox[\(E\),\(_\)],\(\[Prime]\)]\)";
fEbpp="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SuperscriptBox[OverscriptBox[\(E\),\(_\)],\(\[DoublePrime]\)]\)";  sEbpp="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)\!\(\*SuperscriptBox[OverscriptBox[\(E\),\(_\)],\(\[DoublePrime]\)]\)";
fFb="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*OverscriptBox[\(F\),\(_\)]\)";  sFb="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)\!\(\*OverscriptBox[\(F\),\(_\)]\)";
Ebg="\!\(\*SubscriptBox[OverscriptBox[\(E\),\(_\)],\(g\)]\)";  Ebu="\!\(\*SubscriptBox[OverscriptBox[\(E\),\(_\)],\(u\)]\)";
Abp="\!\(\*SuperscriptBox[OverscriptBox[\(A\),\(_\)],\(\[Prime]\)]\)";  Abpp="\!\(\*SuperscriptBox[OverscriptBox[\(A\),\(_\)],\(\[DoublePrime]\)]\)";
Bbp="\!\(\*SuperscriptBox[OverscriptBox[\(B\),\(_\)],\(\[Prime]\)]\)";  Bbpp="\!\(\*SuperscriptBox[OverscriptBox[\(B\),\(_\)],\(\[DoublePrime]\)]\)";
fEbgp="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SubsuperscriptBox[OverscriptBox[\(E\),\(_\)],\(g\),\(\[Prime]\)]\)";  sEbgp="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)\!\(\*SubsuperscriptBox[OverscriptBox[\(E\),\(_\)],\(g\),\(\[Prime]\)]\)";
fEbup="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SubsuperscriptBox[OverscriptBox[\(E\),\(_\)],\(u\),\(\[Prime]\)]\)";  sEbup="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)\!\(\*SubsuperscriptBox[OverscriptBox[\(E\),\(_\)],\(u\),\(\[Prime]\)]\)";
fEbgpp="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SubsuperscriptBox[OverscriptBox[\(E\),\(_\)],\(g\),\(\[DoublePrime]\)]\)";  sEbgpp="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)\!\(\*SubsuperscriptBox[OverscriptBox[\(E\),\(_\)],\(g\),\(\[DoublePrime]\)]\)";
fEbupp="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SubsuperscriptBox[OverscriptBox[\(E\),\(_\)],\(u\),\(\[DoublePrime]\)]\)";  sEbupp="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)\!\(\*SubsuperscriptBox[OverscriptBox[\(E\),\(_\)],\(u\),\(\[DoublePrime]\)]\)";
Abgp="\!\(\*SubsuperscriptBox[OverscriptBox[\(A\),\(_\)],\(g\),\(\[Prime]\)]\)";  Abup="\!\(\*SubsuperscriptBox[OverscriptBox[\(A\),\(_\)],\(u\),\(\[Prime]\)]\)";
Bbgp="\!\(\*SubsuperscriptBox[OverscriptBox[\(B\),\(_\)],\(g\),\(\[Prime]\)]\)";  Bbup="\!\(\*SubsuperscriptBox[OverscriptBox[\(B\),\(_\)],\(u\),\(\[Prime]\)]\)";
Abgpp="\!\(\*SubsuperscriptBox[OverscriptBox[\(A\),\(_\)],\(g\),\(\[DoublePrime]\)]\)";  Abupp="\!\(\*SubsuperscriptBox[OverscriptBox[\(A\),\(_\)],\(u\),\(\[DoublePrime]\)]\)";
Bbgpp="\!\(\*SubsuperscriptBox[OverscriptBox[\(B\),\(_\)],\(g\),\(\[DoublePrime]\)]\)";  Bbupp="\!\(\*SubsuperscriptBox[OverscriptBox[\(B\),\(_\)],\(u\),\(\[DoublePrime]\)]\)";
Tbg="\!\(\*SubscriptBox[OverscriptBox[\(T\),\(_\)],\(g\)]\)";  Tbu="\!\(\*SubscriptBox[OverscriptBox[\(T\),\(_\)],\(u\)]\)";
Fbg="\!\(\*SubscriptBox[OverscriptBox[\(F\),\(_\)],\(g\)]\)";  Fbu="\!\(\*SubscriptBox[OverscriptBox[\(F\),\(_\)],\(u\)]\)";
fFbg="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SubscriptBox[OverscriptBox[\(F\),\(_\)],\(g\)]\)";  sFbg="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)\!\(\*SubscriptBox[OverscriptBox[\(F\),\(_\)],\(g\)]\)";
fFbu="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SubscriptBox[OverscriptBox[\(F\),\(_\)],\(u\)]\)";  sFbu="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)\!\(\*SubscriptBox[OverscriptBox[\(F\),\(_\)],\(u\)]\)";
fFb1="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SubscriptBox[OverscriptBox[\(F\),\(_\)],\(1\)]\)";  sFb1="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)\!\(\*SubscriptBox[OverscriptBox[\(F\),\(_\)],\(1\)]\)";
fFb2="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SubscriptBox[OverscriptBox[\(F\),\(_\)],\(2\)]\)";  sFb2="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)\!\(\*SubscriptBox[OverscriptBox[\(F\),\(_\)],\(2\)]\)";
fFb3="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SubscriptBox[OverscriptBox[\(F\),\(_\)],\(3\)]\)";  sFb3="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)\!\(\*SubscriptBox[OverscriptBox[\(F\),\(_\)],\(3\)]\)";
fHb="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*OverscriptBox[\(H\),\(_\)]\)";  sHb="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)\!\(\*OverscriptBox[\(H\),\(_\)]\)";
Fb2="\!\(\*SubscriptBox[OverscriptBox[\(F\),\(_\)],\(2\)]\)";
fHb1="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SubscriptBox[OverscriptBox[\(H\),\(_\)],\(1\)]\)";  fHb2="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*SubscriptBox[OverscriptBox[\(H\),\(_\)],\(2\)]\)";
fJb="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(1\)]\)\!\(\*OverscriptBox[\(J\),\(_\)]\)";  sJb="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\),\(2\)]\)\!\(\*OverscriptBox[\(J\),\(_\)]\)";

DLGIrepLabel=<||>;
DLGIrepLabel[{2,1}]={{"R",{2},{1}},{"a",{Ab},{2}}};
DLGIrepLabel[{4,1}]={{"R",{1,2,3,4},{1,1,1,1}},{"a",{na,Abg,na,Abu},{na,3,na,4}},
  {"b",{na,sEb,na,fEb},{na,3,na,4}},{"c",{Ab,na,Bb,na},{3,na,4,na}}};
DLGIrepLabel[{4,2}]={{"R",{2,4},{1,1}},{"a",{Abg,Abu},{{2,"+"},{2,"-"}}}};
DLGIrepLabel[{6,1}]={{"R",{1,2,3,4,5,6},{1,1,1,1,1,1}},
  {"a",{na,fEb,na,Ab,na,sEb},{na,4,na,6,na,5}},
  {"b",{Ab,na,fEb,na,sEb,na},{6,na,4,na,5,na}}};
DLGIrepLabel[{8,1}]={{"R",Range[8],Table[1,8]},
  {"a",{na,fEb1,na,fEb2,na,sEb2,na,sEb1},{na,5,na,6,na,7,na,8}},
  {"b",{Ab,na,fEb,na,Bb,na,sEb,na},{5,na,6,na,7,na,8,na}}};
DLGIrepLabel[{8,2}]={{"R",Range[8],Table[1,8]},
  {"a",{na,na,na,na,Ab,na,Bb,na},{na,na,na,na,3,na,4,na}},
  {"b",{na,sEbg,na,fEbg,na,sEbu,na,fEbu},{na,{3,"+"},na,{4,"+"},na,{3,"-"},na,{4,"-"}}},
  {"c",{Abg,na,Bbg,na,Abu,na,Bbu,na},{{3,"+"},na,{4,"+"},na,{3,"-"},na,{4,"-"},na}}};
DLGIrepLabel[{8,5}]={{"R",{1,2,3,4,5},{1,1,1,1,2}},
  {"a",{na,na,na,na,Eb},{na,na,na,na,5}},{"b",{Ab,Bb1,Bb2,Bb3,na},{2,3,4,5,na}}};
DLGIrepLabel[{12,1}]={{"R",Range[12],Table[1,12]},
  {"a",{na,na,na,na,na,fEb3,na,na,na,na,na,sEb1},{na,na,na,na,na,3,na,na,na,na,na,4}},
  {"b",{na,fEb1,na,na,na,na,na,sEb3,na,na,na,na},{na,3,na,na,na,na,na,4,na,na,na,na}},
  {"c",{na,fEb1,na,fEb2,na,fEb3,na,sEb3,na,sEb2,na,sEb1},{na,11,na,10,na,7,na,8,na,9,na,12}},
  {"d",{Ab,na,fEb2,na,fEb1,na,Bb,na,sEb1,na,sEb2,na},{7,na,8,na,9,na,10,na,11,na,12,na}}};
DLGIrepLabel[{12,4}]={{"R",{1,2,3,4,5,6},{1,1,1,1,2,2}},
  {"a",{na,na,fEb,sEb,na,Eb1},{na,na,5,6,na,4}},
  {"b",{Ab1,Ab2,na,na,Eb,na},{4,5,na,na,6,na}}};
DLGIrepLabel[{12,6}]={{"R",{2,4,6,7,8,9,10,11,12},{1,1,1,1,1,1,1,1,1}},
  {"a",{na,na,na,na,fEb,na,Ab,na,sEb},{na,na,na,na,4,na,5,na,6}},
  {"b",{na,na,na,Abu,fEbg,sEbu,Abg,fEbu,sEbg},{na,na,na,{4,"-"},{5,"+"},{6,"-"},{4,"+"},{5,"-"},{6,"+"}}},
  {"c",{fEbg,Abg,sEbg,Abu,na,sEbu,na,fEbu,na},{{4,"+"},{6,"+"},{5,"+"},{6,"-"},na,{5,"-"},na,{4,"-"},na}},
  {"d",{na,na,na,Ab,na,fEb,na,sEb,na},{na,na,na,4,na,5,na,6,na}}};
DLGIrepLabel[{16,2}]={{"R",{2,4,6,8,9,10,11,12,13,14,15,16},Table[1,12]},
  {"a",{na,na,na,na,na,na,na,fEb1,na,na,na,fEb2},{na,na,na,na,na,na,na,3,na,na,na,4}},
  {"b",{na,na,na,na,na,fEb1,na,fEb2,na,sEb2,na,sEb1},{na,na,na,na,na,5,na,6,na,7,na,8}},
  {"c",{na,na,na,na,Ab,na,fEb,na,Bb,na,sEb,na},{na,na,na,na,5,na,6,na,7,na,8,na}},
  {"d",{fEb1g,fEb2g,sEb2g,sEb1g,na,fEb1u,na,fEb2u,na,sEb2u,na,sEb1u},
       {{6,"+"},{8,"+"},{7,"+"},{5,"+"},na,{6,"-"},na,{8,"-"},na,{7,"-"},na,{5,"-"}}},
  {"e",{na,na,na,na,na,fEb1,na,na,na,fEb2,na,na},{na,na,na,na,na,3,na,na,na,4,na,na}},
  {"f",{fEb1,fEb2,sEb2,sEb1,na,na,na,na,na,na,na,na},{5,6,7,8,na,na,na,na,na,na,na,na}}};
DLGIrepLabel[{16,8}]={{"R",{5,6,7,8,9},{1,1,1,1,2}},
  {"a",{sEbpp,fEbpp,fEbp,sEbp,na},{2,3,4,5,na}},{"b",{na,na,na,na,Eb},{na,na,na,na,5}}};
DLGIrepLabel[{16,10}]={{"R",{9,10},{2,2}},{"a",{Eb,na},{2,na}},{"b",{na,Eb},{na,2}}};
DLGIrepLabel[{16,11}]={{"R",{5,6,7,8,9,10},{2,1,1,1,1,2}},
  {"a",{na,Ab1,Bb1,Bb2,Bb3,na},{na,2,3,4,5,na}},
  {"b",{na,Abp,Abpp,Bbp,Bbpp,na},{na,2,3,4,5,na}},
  {"c",{Ebg,na,na,na,na,Ebu},{{5,"+"},na,na,na,na,{5,"-"}}},
  {"d",{na,na,na,na,na,Eb},{na,na,na,na,na,5}},
  {"e",{Eb,na,na,na,na,na},{5,na,na,na,na,na}}};
DLGIrepLabel[{16,14}]={{"R",{6,7},{2,2}},{"a",{Eb1,Eb2},{6,7}}};
DLGIrepLabel[{24,3}]={{"R",{3,4,6,7,8,9,10,11,12},{1,1,2,1,1,1,1,2,2}},
  {"a",{na,na,na,Ab1,Ab2,na,na,Eb,na},{na,na,na,4,5,na,na,6,na}},
  {"b",{fEbg,sEbg,Eb1g,na,na,fEbu,sEbu,na,Eb1u},{{5,"+"},{6,"+"},{4,"+"},na,na,{5,"-"},{6,"-"},na,{4,"-"}}},
  {"c",{na,na,na,na,na,Ab1,Ab2,na,Eb},{na,na,na,na,na,4,5,na,6}}};
DLGIrepLabel[{24,9}]={{"R",{1,2,3,4,5,6,7},{1,1,1,2,2,2,3}},
  {"a",{na,na,na,Eb,fFb,sFb,na},{na,na,na,5,6,7,na}},
  {"b",{Ab,fEb,sEb,na,na,na,Tb},{4,5,6,na,na,na,7}}};
DLGIrepLabel[{24,11}]={{"R",{7,8,9},{2,2,2}},{"a",{Eb1,Eb2,Eb3},{7,8,9}}};
DLGIrepLabel[{24,12}]={{"R",{2,4,6,8,10,12,14,15,16,17,18,20,21,22,23,24},Table[1,16]}, 
  {"a",{fEb1g,fEb2g,fEb3g,sEb3g,sEb2g,sEb1g,fEb1u,na,fEb2u,na,fEb3u,sEb3u,na,sEb2u,na,sEb1u},
       {{11,"+"},{10,"+"},{7,"+"},{8,"+"},{9,"+"},{12,"+"},
        {11,"-"},na,{10,"-"},na,{7,"-"},{8,"-"},na,{9,"-"},na,{12,"-"}}},
  {"b",{na,na,na,na,na,na,na,na,na,fEb1,na,na,na,na,fEb2,na},{na,na,na,na,na,na,na,na,na,3,na,na,na,na,4,na}},
  {"c",{na,na,na,na,na,na,na,fEb1,na,na,na,na,fEb2,na,na,na},{na,na,na,na,na,na,na,3,na,na,na,na,4,na,na,na}}};
DLGIrepLabel[{32,7}]={{"R",{9,10,13,14},{2,2,2,2}},
  {"a",{na,na,fFb,sFb},{na,na,3,4}},{"b",{Eb1,Eb2,na,na},{3,4,na,na}}};
DLGIrepLabel[{32,8}]={{"R",{13,14,17,18},{1,1,1,1}},{"a",{fEb1,fEb2,fEb3,fEb4},{2,3,4,5}}};
DLGIrepLabel[{32,9}]={{"R",{6,7,13,14},{2,2,2,2}},
  {"a",{Eb1g,Eb2g,Eb1u,Eb2u},{{6,"+"},{7,"+"},{6,"-"},{7,"-"}}},
  {"b",{na,na,Eb1,Eb2},{na,na,6,7}}};
DLGIrepLabel[{32,10}]={{"R",{6,7,8,9,10,11,14},{2,2,1,1,1,1,2}},
  {"a",{na,na,fEb1,fEb2,sEb1,sEb2,Eb3},{na,na,3,4,5,6,7}},
  {"b",{Eb1,Eb2,na,na,na,na,na},{6,7,na,na,na,na,na}}};
DLGIrepLabel[{32,11}]={{"R",{6,7,8,9,10,11,12},{2,2,1,1,1,1,2}},
  {"a",{fFb,sFb,na,na,na,na,na},{6,7,na,na,na,na,na}},
  {"b",{na,na,fEb1,fEb2,sEb2,sEb1,Eb3},{na,na,3,4,5,6,7}}};
DLGIrepLabel[{32,12}]={{"R",{19,20},{2,2}},{"a",{fFb,sFb},{3,4}}};
DLGIrepLabel[{32,14}]={{"R",{5,6,7,8,9,15,16,17,18,19},{1,1,1,1,2,1,1,1,1,2}},
  {"a",{sEbgpp,fEbgpp,fEbgp,sEbgp,na,sEbupp,fEbupp,fEbup,sEbup,na},
       {{2,"+"},{3,"+"},{4,"+"},{5,"+"},na,{2,"-"},{3,"-"},{4,"-"},{5,"-"},na}},
  {"b",{na,na,na,na,Ebg,na,na,na,na,Ebu},{na,na,na,na,{5,"+"},na,na,na,na,{5,"-"}}},
  {"c",{sEbupp,fEbupp,fEbup,sEbup,na,sEbgp,fEbgp,fEbgpp,sEbgpp,na},
       {{2,"-"},{3,"-"},{5,"-"},{4,"-"},na,{2,"+"},{3,"+"},{5,"+"},{4,"+"},na}},
  {"d",{sEbgpp,fEbgpp,fEbgp,sEbgp,na,sEbupp,fEbupp,fEbup,sEbup,na},
       {{5,"+"},{6,"+"},{7,"+"},{8,"+"},na,{5,"-"},{6,"-"},{7,"-"},{8,"-"},na}}};
DLGIrepLabel[{32,15}]={{"R",{6,7,8,9,16,17,18,19},{1,1,1,1,1,1,1,1}},
  {"a",{Abgp,Abgpp,Bbgp,Bbgpp,Abup,Abupp,Bbup,Bbupp},
       {{2,"+"},{3,"+"},{4,"+"},{5,"+"},{2,"-"},{3,"-"},{4,"-"},{5,"-"}}}};
DLGIrepLabel[{32,17}]={{"R",{10,12,14,16},{1,1,1,1}},{"a",{fEb1,fEb2,fEb3,fEb4},{5,6,7,8}}};
DLGIrepLabel[{48,4}]={{"R",Range[14],{1,1,1,2,2,2,3,1,1,1,2,2,2,3}},
  {"a",{na,na,na,na,na,na,na,Ab,fEb,sEb,na,na,na,Tb},{na,na,na,na,na,na,na,4,5,6,na,na,na,7}},
  {"b",{na,na,na,na,na,na,na,na,na,na,Eb,fFb,sFb,na},{na,na,na,na,na,na,na,na,na,na,5,6,7,na}},
  {"c",{na,na,na,Ebg,fFbg,sFbg,na,na,na,na,Ebu,fFbu,sFbu,na},
       {na,na,na,{5,"+"},{6,"+"},{7,"+"},na,na,na,na,{5,"-"},{6,"-"},{7,"-"},na}},
  {"d",{Abg,fEbg,sEbg,na,na,na,Tbg,Abu,fEbu,sEbu,na,na,na,Tbu},
       {{4,"+"},{5,"+"},{6,"+"},na,na,na,{7,"+"},{4,"-"},{5,"-"},{6,"-"},na,na,na,{7,"-"}}}};
DLGIrepLabel[{48,9}]={{"R",{10,17,18},{2,2,2}},{"a",{Eb,fFb,sFb},{4,5,6}}};
DLGIrepLabel[{48,10}]={{"R",{4,5,8},{2,2,4}},{"a",{Eb1,Eb2,Fb},{6,7,8}}};
DLGIrepLabel[{48,11}]={{"R",{28,29,30},{2,2,2}},{"a",{fFb,Eb,sFb},{4,5,6}}};
DLGIrepLabel[{48,12}]={{"R",{7,8,9,10,11,12,13,14,15},{2,2,2,1,1,1,1,2,2}},
  {"a",{na,na,na,fEb1,sEb1,fEb2,sEb2,Eb3,Eb4},{na,na,na,4,5,6,7,8,9}},
  {"b",{Eb1,Eb2,Eb3,na,na,na,na,na,na},{7,8,9,na,na,na,na,na,na}}};
DLGIrepLabel[{48,13}]={{"R",{13,14,15},{2,2,2}},{"a",{Eb,fFb,sFb},{7,8,9}}};
DLGIrepLabel[{48,14}]={{"R",{5,6,7,8,11,12},{1,1,1,1,2,2}},
  {"a",{fEb1,fEb2,sEb1,sEb2,fFb,sFb},{4,5,6,7,8,9}}};
DLGIrepLabel[{48,15}]={{"R",{7,8,9,16,17,18},{2,2,2,2,2,2}},
  {"a",{Eb1g,Eb2g,Eb3g,Eb1u,Eb2u,Eb3u},{{7,"+"},{8,"+"},{9,"+"},{7,"-"},{8,"-"},{9,"-"}}}};
DLGIrepLabel[{64,1}]={{"R",{19,20,21,22},{2,2,2,2}},{"a",{fFb1,sFb1,fFb2,sFb2},{5,6,7,8}}};
DLGIrepLabel[{64,2}]={{"R",{19},{4}},{"a",{Fb},{5}}};
DLGIrepLabel[{64,3}]={{"R",{9,10,11,12,13,14,15,16,19,20},{1,1,1,1,1,1,1,1,2,2}},
  {"a",{na,na,na,na,fEb1,fEb2,fEb3,fEb4,na,fFb},{na,na,na,na,3,4,5,6,na,7}},
  {"b",{fEb1,fEb2,fEb3,fEb4,na,na,na,na,fFb,na},{3,4,5,6,na,na,na,na,7,na}}};
DLGIrepLabel[{64,4}]={{"R",{6,7,20,21},{2,2,2,2}},
  {"a",{fFbg,sFbg,fFbu,sFbu},{{6,"+"},{7,"+"},{6,"-"},{7,"-"}}}};
DLGIrepLabel[{64,5}]={{"R",{19},{4}},{"a",{Fb},{5}}};
DLGIrepLabel[{96,5}]={{"R",{22,23,24,28},{1,2,2,3}},{"a",{fEb,fFb1,fFb2,fHb},{4,5,6,7}}};
DLGIrepLabel[{96,7}]={{"R",{3,4,6,7,8,13,14,15},{1,1,1,1,2,3,3,4}},
  {"a",{fEb1,sEb1,na,na,Eb2,fHb,sHb,na},{4,5,na,na,6,7,8,na}},
  {"b",{na,na,fEb,sEb,na,na,na,Fb},{na,na,6,7,na,na,na,8}}};
DLGIrepLabel[{96,8}]={{"R",{4,5,8,12,13,16},{2,2,4,2,2,4}},
  {"a",{na,na,na,Eb1,Eb2,Fb},{na,na,na,6,7,8}},
  {"b",{Eb1g,Eb2g,Fbg,Eb1u,Eb2u,Fbu},{{6,"+"},{7,"+"},{8,"+"},{6,"-"},{7,"-"},{8,"-"}}}};
DLGIrepLabel[{96,9}]={{"R",{19,20,21,22,23,24},{2,2,2,2,2,2}},
  {"a",{fFb1,sFb1,fFb2,fFb3,sFb2,sFb3},{7,8,9,10,11,12}}};
DLGIrepLabel[{96,10}]={{"R",{15,16,24},{2,2,4}},{"a",{fFb1,sFb1,Fb2},{4,5,6}}};
DLGIrepLabel[{192,1}]={{"R",{17,18,21,27,28},{1,1,2,3,3}},{"a",{fEb1,fEb2,fFb,fHb1,fHb2},{4,5,6,7,8}}};
DLGIrepLabel[{192,2}]={{"R",{18,21,22},{4,4,4}},{"a",{Fb,fJb,sJb},{5,6,7}}};
(* Note that the last two labels for {192,2} is Jbar, not Mbar in the book. *)

];


RepGammaLabel[kname_,index_]:=Module[{},
  If[index=="",Return[""]];
  If[ListQ[index],
    strSubsup[kname,ToString@index[[1]],index[[2]]],
    strSub[kname,ToString@index]]
];
RepGammaLabel[index_]:=RepGammaLabel["\[CapitalGamma]",index]

showRepLabel[{m_,n_},dsgtag_]:=Module[{allrep, keys, rep, rep2,i,tag,ML,GL},
  allrep=If[dsgtag=="d"||dsgtag=="D",DLGIrepLabel,LGIrepLabel];
  keys=Keys[allrep];
  If[!MemberQ[keys,{m,n}], Print["showRepLabel: {m,n}=",{m,n}," is not in ",keys]; Return[]];
  rep=allrep[{m,n}];
  rep2=Table["",{i,2Length[rep]},{j,Length[rep[[1,2]]]+1}];
  rep2[[1,1]]=Style[Subsuperscript["G",m,n],Bold,Red];
  rep2[[1,2;;]]=Subscript["R",#]&/@rep[[1,2]];
  rep2[[2,2;;]]=rep[[1,3]];  rep2[[2,1]]="dim";
  For[i=2,i<=Length[rep],i++,
    {tag,ML,GL}=rep[[i]];
    rep2[[2i-1,1]]=tag;
    rep2[[2i-1,2;;]]=Style[#, FontFamily->"Times"]&/@(ML/.""->"-");
    rep2[[2i,2;;]]=Style[#,FontFamily->"Times"]&/@((RepGammaLabel/@GL)/.""->"-");
  ];
  Grid[rep2, Spacings->{2, {{0.8,0.2}}}, Dividers->{{False,Thin},{{Thin,False}}},
             Background->{None,{Lighter[Yellow,.9],Lighter[Yellow,.9]}}]
]
showRepLabel[{m_,n_}]:=showRepLabel[{m,n},"s"]
showLGIrepLabel[{m_,n_}]:=showRepLabel[{m,n}, "s"]
showDLGIrepLabel[{m_,n_}]:=showRepLabel[{m,n}, "d"]


Options[mapLGIrepLabel]={"DSG"->False};
mapLGIrepLabel[sgno_Integer, kname_String, OptionsPattern[]]/;1<=sgno<=230:=Module[
  {dsg,ir,kir,AGno, abc, lbtab, Rs, GMs,Mlks, dim, re},
  dsg=OptionValue["DSG"]===True;
  ir=If[dsg,DLGIrep[sgno],  LGIrep[sgno]];   kir=ir[kname];
  If[MissingQ[kir], Print["mapLGIrepLabel:  ",kname," should be in ",Keys[ir],"."]; Abort[]];
  AGno=kir[[1]];   abc=kir[[-1]];
  lbtab=If[dsg, DLGIrepLabel[AGno], LGIrepLabel[AGno]];
  Rs=StringTemplate["\!\(\*SubscriptBox[\(R\), \(``\)]\)"]/@lbtab[[1,2]];
  dim=lbtab[[1,3]];
  GMs=Select[lbtab,#[[1]]==abc&]//First;
  Mlks=GMs[[2]];
  GMs=SpaceGroupIrep`Private`RepGammaLabel[kname,#]&/@GMs[[3]];
  re=Select[{Rs,Mlks,GMs,dim}\[Transpose],#[[2]]!=""&];
  Prepend[re,{StringTemplate["\!\(\*SubsuperscriptBox[\(G\), \(`1`\), \(`2`\)]\)"]@@AGno,"Mulliken","\[CapitalGamma] label","dim"}]
]
mapLGIrepLabel[sgno_Integer, OptionsPattern[]]/;1<=sgno<=230:=Module[{keys},
  keys=Keys[LGIrep[sgno]];
  mapLGIrepLabel[sgno,#,"DSG"->OptionValue["DSG"]]&/@keys
]


(* ::Section:: *)
(*Ireps of point group*)


(* ::Subsection:: *)
(*PG Irep labels*)


(*This is the irep labels of the point group generated by the abstract groups
using the information in PGinfo. The order of ireps follows the abstract group
character table and the labels are consistent with those in BC-tab. 2.2.*)
BCtab2d2Label=<|
  1->{{"A","GM1"}},
  2->{{"Ag","GM1+"},{"Au","GM1-"}},
  3->{{"A","GM1"},{"B","GM2"}},
  4->{{"A'","GM1"},{"A''","GM2"}},
  5->{{"Ag","GM1+"},{"Au","GM1-"},{"Bg","GM2+"},{"Bu","GM2-"}},
  6->{{"A","GM1"},{"B1","GM3"},{"B2","GM2"},{"B3","GM4"}},
  7->{{"A1","GM1"},{"A2","GM3"},{"B1","GM2"},{"B2","GM4"}},
  8->{{"Ag","GM1+"},{"B2g","GM2+"},{"B1g","GM3+"},{"B3g","GM4+"},{"Au","GM1-"},
      {"B2u","GM2-"},{"B1u","GM3-"},{"B3u","GM4-"}},
  9->{{"A","GM1"},{"2E","GM3"},{"B","GM2"},{"1E","GM4"}},
  10->{{"A","GM1"},{"1E","GM4"},{"B","GM2"},{"2E","GM3"}},
  11->{{"Ag","GM1+"},{"2Eg","GM3+"},{"Bg","GM2+"},{"1Eg","GM4+"},{"Au","GM1-"},
       {"2Eu","GM3-"},{"Bu","GM2-"},{"1Eu","GM4-"}},
  12->{{"A1","GM1"},{"A2","GM2"},{"B1","GM3"},{"B2","GM4"},{"E","GM5"}},
  13->{{"A1","GM1"},{"A2","GM2"},{"B1","GM3"},{"B2","GM4"},{"E","GM5"}},
  14->{{"A1","GM1"},{"A2","GM2"},{"B1","GM3"},{"B2","GM4"},{"E","GM5"}},
  15->{{"A1g","GM1+"},{"A2g","GM2+"},{"B1g","GM3+"},{"B2g","GM4+"},{"Eg","GM5+"},
       {"A1u","GM1-"},{"A2u","GM2-"},{"B1u","GM3-"},{"B2u","GM4-"},{"Eu","GM5-"}},
  16->{{"A","GM1"},{"2E","GM2"},{"1E","GM3"}},
  17->{{"Ag","GM1+"},{"2Eu","GM2-"},{"1Eg","GM3+"},{"Au","GM1-"},{"2Eg","GM2+"},{"1Eu","GM3-"}},
  18->{{"A1","GM1"},{"A2","GM2"},{"E","GM3"}},
  19->{{"A1","GM1"},{"A2","GM2"},{"E","GM3"}},
  20->{{"A1g","GM1+"},{"A2g","GM2+"},{"A1u","GM1-"},{"A2u","GM2-"},{"Eg","GM3+"},{"Eu","GM3-"}},
  21->{{"A","GM1"},{"2E2","GM2"},{"1E1","GM6"},{"B","GM4"},{"2E1","GM5"},{"1E2","GM3"}},
  22->{{"A'","GM1"},{"1E''","GM6"},{"2E'","GM2"},{"A''","GM4"},{"1E'","GM3"},{"2E''","GM5"}},
  23->{{"Ag","GM1+"},{"2E1g","GM5+"},{"1E1g","GM6+"},{"Bg","GM4+"},{"2E2g","GM2+"},{"1E2g","GM3+"},
       {"Au","GM1-"},{"2E1u","GM5-"},{"1E1u","GM6-"},{"Bu","GM4-"},{"2E2u","GM2-"},{"1E2u","GM3-"}},
  24->{{"A1","GM1"},{"A2","GM2"},{"B1","GM3"},{"B2","GM4"},{"E2","GM6"},{"E1","GM5"}},
  25->{{"A1","GM1"},{"A2","GM2"},{"B1","GM4"},{"B2","GM3"},{"E2","GM6"},{"E1","GM5"}},
  26->{{"A1'","GM1"},{"A2'","GM2"},{"A1''","GM3"},{"A2''","GM4"},{"E'","GM6"},{"E''","GM5"}},
  27->{{"A1g","GM1+"},{"A2g","GM2+"},{"E2g","GM6+"},{"B1g","GM3+"},{"B2g","GM4+"},{"E1g","GM5+"},
       {"A1u","GM1-"},{"A2u","GM2-"},{"E2u","GM6-"},{"B1u","GM3-"},{"B2u","GM4-"},{"E1u","GM5-"}},
  28->{{"A","GM1"},{"1E","GM2"},{"2E","GM3"},{"T","GM4"}},
  29->{{"Ag","GM1+"},{"1Eg","GM2+"},{"2Eg","GM3+"},{"Tg","GM4+"},
       {"Au","GM1-"},{"1Eu","GM2-"},{"2Eu","GM3-"},{"Tu","GM4-"}},
  30->{{"A1","GM1"},{"A2","GM2"},{"E","GM3"},{"T1","GM4"},{"T2","GM5"}},
  31->{{"A1","GM1"},{"A2","GM2"},{"E","GM3"},{"T1","GM4"},{"T2","GM5"}},
  32->{{"A1g","GM1+"},{"A2g","GM2+"},{"A2u","GM2-"},{"A1u","GM1-"},{"Eg","GM3+"},
       {"Eu","GM3-"},{"T1g","GM4+"},{"T2g","GM5+"},{"T1u","GM4-"},{"T2u","GM5-"}}
|>;
BCtab2d2Label=MapAt[str2GammaLabel,MapAt[str2Mulliken,BCtab2d2Label,{All,All,1}],{All,All,2}];

(*(*-------------------------------------------------------------------
 This is my original assignment of the labels of double point group. 
 For single-valued ireps, both the Mulliken labels and Gamma labels are consitent with the ones 
 in the Table 2.2 in the BC book, while the double-valued irep labels are determined as follows:
 (1). For the 21 point groups without inversion "I", the double-valued irep labels for the little 
 group ireps at \[CapitalGamma] of the first corresponding space group are adopted. For examle, 
 C2v corresponds to SG 25-46 (see showPGinfo[]) and the double-valued irep labels of the \[CapitalGamma] 
 little group of the SG 25 are adopted for those of C2v.
 (2) For the rest 11 point groups with inversion, they all can be written as H\[CircleTimes]Ci, and 
 their double-valued irep labels are constructed from those of H. For example, S6=C3\[CircleTimes]Ci, the 
 double-valued irep barA (GM6) of C3 splits into barAg (GM6+) and barAu (GM6-) in S6 for inversion-even 
 and inversion-odd ireps respectively. 
** HOWEVER, these double-valued labels are not consistent with those in BC-Tab 6.5. **
*)
(*This is the irep labels of the double point group generated by the abstract groups
using the information in PGinfo. The ireps from AGCharTab are first divided into two 
parts, namely single-valued and double-valued parts. The integers give the position
of single-valued irep in BCtab2d2Label, and the strings give the irep labels of the
second part, i.e. the double-valued ones. *)
doublePGIrepLabel=<|
  1->{{1},{{"barA","GM2"}}},
  2->{{1,2},{{"barAg","GM2+"},{"barAu","GM2-"}}},
  3->{{1,2},{{"bar2E","GM3"},{"bar1E","GM4"}}},
  4->{{1,2},{{"bar2E","GM3"},{"bar1E","GM4"}}},
  5->{{1,3,2,4},{{"bar2Eg","GM3+"},{"bar1Eg","GM4+"},{"bar2Eu","GM3-"},{"bar1Eu","GM4-"}}},
  6->{{1,2,3,4},{{"barE","GM5"}}},
  7->{{1,2,3,4},{{"barE","GM5"}}},
  8->{{1,3,2,4,5,7,6,8},{{"barEg","GM5+"},{"barEu","GM5-"}}},
  9->{{1,2,3,4},{{"bar1E1","GM5"},{"bar1E2","GM6"},{"bar2E2","GM7"},{"bar2E1","GM8"}}},
  10->{{1,2,3,4},{{"bar1E1","GM5"},{"bar1E2","GM6"},{"bar2E2","GM7"},{"bar2E1","GM8"}}},
  11->{{1,2,3,4,5,6,7,8},{{"bar1E1g","GM5+"},{"bar1E2g","GM6+"},{"bar2E2g","GM7+"},{"bar2E1g","GM8+"},
       {"bar1E1u","GM5-"},{"bar1E2u","GM6-"},{"bar2E2u","GM7-"},{"bar2E1u","GM8-"}}},
  12->{{1,2,3,4,5},{{"barE1","GM6"},{"barE2","GM7"}}},
  13->{{1,2,3,4,5},{{"barE1","GM6"},{"barE2","GM7"}}},
  14->{{1,2,3,4,5},{{"barE1","GM6"},{"barE2","GM7"}}},
  15->{{1,2,3,4,5,6,7,8,9,10},{{"barE1g","GM6+"},{"barE2g","GM7+"},{"barE1u","GM6-"},{"barE2u","GM7-"}}},
  16->{{1,2,3},{{"bar1E","GM4"},{"barA","GM6"},{"bar2E","GM5"}}},
  17->{{1,3,5,2,4,6},{{"bar2Eg","GM5+"},{"barAg","GM6+"},{"bar1Eg","GM4+"},{"barAu","GM6-"},
       {"bar1Eu","GM4-"},{"bar2Eu","GM5-"}}},
  18->{{1,2,3},{{"bar1E","GM5"},{"bar2E","GM6"},{"barE1","GM4"}}},
  19->{{1,2,3},{{"bar1E","GM5"},{"bar2E","GM6"},{"barE1","GM4"}}},
  20->{{1,2,5,3,4,6},{{"bar1Eg","GM5+"},{"bar2Eg","GM6+"},{"barE1g","GM4+"},{"bar1Eu","GM5-"},
       {"bar2Eu","GM6-"},{"barE1u","GM4-"}}},
  21->{{1,2,3,4,5,6},{{"bar1E1","GM11"},{"bar1E2","GM10"},{"bar1E3","GM7"},{"bar2E3","GM8"},
       {"bar2E2","GM9"},{"bar2E1","GM12"}}},
  22->{{1,2,3,4,5,6},{{"bar1E1","GM11"},{"bar1E2","GM10"},{"bar1E3","GM7"},{"bar2E3","GM8"},
       {"bar2E2","GM9"},{"bar2E1","GM12"}}},
  23->{{1,5,3,4,2,6,7,11,9,10,8,12},{{"bar1E1g","GM11+"},{"bar1E2g","GM10+"},{"bar1E3g","GM7+"},
       {"bar2E3g","GM8+"},{"bar2E2g","GM9+"},{"bar2E1g","GM12+"},{"bar1E1u","GM11-"},{"bar1E2u","GM10-"},
       {"bar1E3u","GM7-"},{"bar2E3u","GM8-"},{"bar2E2u","GM9-"},{"bar2E1u","GM12-"}}},
  24->{{1,2,3,4,5,6},{{"barE1","GM7"},{"barE2","GM8"},{"barE3","GM9"}}},
  25->{{1,2,4,3,5,6},{{"barE1","GM7"},{"barE2","GM8"},{"barE3","GM9"}}},
  26->{{1,2,3,4,5,6},{{"barE1","GM7"},{"barE2","GM8"},{"barE3","GM9"}}},
  27->{{1,2,4,5,3,6,7,8,10,11,9,12},{{"barE1g","GM7+"},{"barE2g","GM8+"},{"barE3g","GM9+"},
       {"barE1u","GM7-"},{"barE2u","GM8-"},{"barE3u","GM9-"}}},
  28->{{1,3,2,4},{{"barE","GM5"},{"bar1F","GM6"},{"bar2F","GM7"}}},
  29->{{1,3,2,4,5,7,6,8},{{"barEg","GM5+"},{"bar1Fg","GM6+"},{"bar2Fg","GM7+"},{"barEu","GM5-"},
       {"bar1Fu","GM6-"},{"bar2Fu","GM7-"}}},
  30->{{1,2,3,4,5},{{"barE1","GM6"},{"barE2","GM7"},{"barF","GM8"}}},
  31->{{1,2,3,4,5},{{"barE1","GM6"},{"barE2","GM7"},{"barF","GM8"}}},
  32->{{1,2,5,7,8,4,3,6,9,10},{{"barE1g","GM6+"},{"barE2g","GM7+"},{"barFg","GM8+"},{"barE1u","GM6-"},
       {"barE2u","GM7-"},{"barFu","GM8-"}}}
|>;
(*---------------------------------------------------*) *)

(* These ireps labels are consistent with those in BC-Tab 6.5.*)
doublePGIrepLabel=<|
  1->{{1},{{"barA","GM2"}}},
  2->{{1,2},{{"barAg","GM2+"},{"barAu","GM2-"}}},
  3->{{1,2},{{"bar1E","GM3"},{"bar2E","GM4"}}},
  4->{{1,2},{{"bar1E","GM3"},{"bar2E","GM4"}}},
  5->{{1,3,2,4},{{"bar1Eg","GM3+"},{"bar2Eg","GM4+"},{"bar1Eu","GM3-"},{"bar2Eu","GM4-"}}},
  6->{{1,2,3,4},{{"barE","GM5"}}},
  7->{{1,2,3,4},{{"barE","GM5"}}},
  8->{{1,3,2,4,5,7,6,8},{{"barEg","GM5+"},{"barEu","GM5-"}}},
  9->{{1,2,3,4},{{"bar1E1","GM6"},{"bar2E2","GM7"},{"bar1E2","GM8"},{"bar2E1","GM5"}}},
  10->{{1,2,3,4},{{"bar2E1","GM5"},{"bar1E2","GM8"},{"bar2E2","GM7"},{"bar1E1","GM6"}}},
  11->{{1,2,3,4,5,6,7,8},{{"bar1E1g","GM6+"},{"bar2E2g","GM7+"},{"bar1E2g","GM8+"},{"bar2E1g","GM5+"},
       {"bar1E1u","GM6-"},{"bar2E2u","GM7-"},{"bar1E2u","GM8-"},{"bar2E1u","GM5-"}}},
  12->{{1,2,3,4,5},{{"barE1","GM6"},{"barE2","GM7"}}},
  13->{{1,2,3,4,5},{{"barE1","GM6"},{"barE2","GM7"}}},
  14->{{1,2,3,4,5},{{"barE1","GM6"},{"barE2","GM7"}}},
  15->{{1,2,3,4,5,6,7,8,9,10},{{"barE1g","GM6+"},{"barE2g","GM7+"},{"barE1u","GM6-"},{"barE2u","GM7-"}}},
  16->{{1,2,3},{{"bar2E","GM5"},{"barA","GM6"},{"bar1E","GM4"}}},
  17->{{1,3,5,2,4,6},{{"bar1Eg","GM4+"},{"barAg","GM6+"},{"bar2Eg","GM5+"},{"barAu","GM6-"},
       {"bar2Eu","GM5-"},{"bar1Eu","GM4-"}}},
  18->{{1,2,3},{{"bar1E","GM5"},{"bar2E","GM6"},{"barE1","GM4"}}},
  19->{{1,2,3},{{"bar1E","GM5"},{"bar2E","GM6"},{"barE1","GM4"}}},
  20->{{1,2,5,3,4,6},{{"bar1Eg","GM5+"},{"bar2Eg","GM6+"},{"barE1g","GM4+"},{"bar1Eu","GM5-"},
       {"bar2Eu","GM6-"},{"barE1u","GM4-"}}},
  21->{{1,2,3,4,5,6},{{"bar2E3","GM8"},{"bar1E1","GM11"},{"bar2E2","GM9"},{"bar1E2","GM10"},
       {"bar2E1","GM12"},{"bar1E3","GM7"}}},
  22->{{1,2,3,4,5,6},{{"bar1E3","GM7"},{"bar2E1","GM12"},{"bar1E2","GM10"},{"bar2E2","GM9"},
       {"bar1E1","GM11"},{"bar2E3","GM8"}}},
  23->{{1,5,3,4,2,6,7,11,9,10,8,12},{{"bar2E3g","GM8+"},{"bar1E1g","GM11+"},{"bar2E2g","GM9+"},
       {"bar1E2g","GM10+"},{"bar2E1g","GM12+"},{"bar1E3g","GM7+"},{"bar2E3u","GM8-"},{"bar1E1u","GM11-"},
       {"bar2E2u","GM9-"},{"bar1E2u","GM10-"},{"bar2E1u","GM12-"},{"bar1E3u","GM7-"}}},
  24->{{1,2,3,4,5,6},{{"barE3","GM9"},{"barE1","GM7"},{"barE2","GM8"}}},
  25->{{1,2,4,3,5,6},{{"barE3","GM9"},{"barE1","GM7"},{"barE2","GM8"}}},
  26->{{1,2,3,4,5,6},{{"barE3","GM9"},{"barE1","GM7"},{"barE2","GM8"}}},
  27->{{1,2,4,5,3,6,7,8,10,11,9,12},{{"barE3g","GM9+"},{"barE1g","GM7+"},{"barE2g","GM8+"},
       {"barE3u","GM9-"},{"barE1u","GM7-"},{"barE2u","GM8-"}}},
  28->{{1,3,2,4},{{"barE","GM5"},{"bar1F","GM6"},{"bar2F","GM7"}}},
  29->{{1,3,2,4,5,7,6,8},{{"barEg","GM5+"},{"bar1Fg","GM6+"},{"bar2Fg","GM7+"},{"barEu","GM5-"},
       {"bar1Fu","GM6-"},{"bar2Fu","GM7-"}}},
  30->{{1,2,3,4,5},{{"barE1","GM6"},{"barE2","GM7"},{"barF","GM8"}}},
  31->{{1,2,3,4,5},{{"barE1","GM6"},{"barE2","GM7"},{"barF","GM8"}}},
  32->{{1,2,5,7,8,4,3,6,9,10},{{"barE1g","GM6+"},{"barE2g","GM7+"},{"barFg","GM8+"},{"barE1u","GM6-"},
       {"barE2u","GM7-"},{"barFu","GM8-"}}}
|>;
doublePGIrepLabel=MapAt[str2GammaLabel,MapAt[str2Mulliken,doublePGIrepLabel,{All,2,All,1}],{All,2,All,2}];


MullikenIndex={
  "A", "A'", "A''", "Ag", "Au", "A1", "A1'", "A1''", "A1g", "A1u",
  "A2", "A2'", "A2''", "A2g", "A2u", "B", "Bg", "Bu", "B1", "B1g",
  "B1u", "B2", "B2g", "B2u", "B3", "B3g", "B3u", "E", "1E", "2E",
  "E'", "1E'", "2E'", "E''", "1E''", "2E''", "Eg", "1Eg", "2Eg", "Eu",
  "1Eu", "2Eu", "E1", "1E1", "2E1", "E1g", "1E1g", "2E1g", "E1u", "1E1u",
  "2E1u", "E2", "1E2", "2E2", "E2g", "1E2g", "2E2g", "E2u", "1E2u", "2E2u",
  "T", "Tg", "Tu", "T1", "T1g", "T1u", "T2", "T2g", "T2u", "barA",
  "barAg", "barAu", "barE", "bar1E", "bar2E", "barEg", "bar1Eg", "bar2Eg", "barEu", "bar1Eu",
  "bar2Eu", "barE1", "bar1E1", "bar2E1", "barE1g", "bar1E1g", "bar2E1g", "barE1u", "bar1E1u",
  "bar2E1u", "barE2", "bar1E2", "bar2E2", "barE2g", "bar1E2g", "bar2E2g", "barE2u", "bar1E2u", "bar2E2u",
  "barE3", "bar1E3", "bar2E3", "barE3g", "bar1E3g", "bar2E3g", "barE3u", "bar1E3u", "bar2E3u", "barF",
  "bar1F", "bar2F", "barFg", "bar1Fg", "bar2Fg", "barFu", "bar1Fu", "bar2Fu"};
MullikenIndex=str2Mulliken/@MullikenIndex;
MullikenIndex=Association@Table[MullikenIndex[[i]]->i,{i,Length[MullikenIndex]}];


(* ::Subsection:: *)
(*getPGCharTab*)


checkPGinput[numOrName_, fun_String]:=Module[{pgno,pgs,err=False,adjust},
  If[IntegerQ[numOrName], 
    pgno=numOrName; If[pgno<1||pgno>32, err=True], (*next line else*)
    If[StringQ[numOrName],
      pgno=Position[PGinfo[[All,{2,3}]], numOrName];
      If[pgno=={}, err=True, pgno=pgno[[1,1]]],      (*next line else*)
      err=True
    ]
  ];
  If[err, 
    adjust=Map[InputForm,PGinfo[[All,1;;3]],{2}];
    adjust=MapAt[Grid[{{#}},ItemSize->1,Alignment->Left]&, adjust, {All,1}];
    adjust=MapAt[Grid[{{#}},ItemSize->3,Alignment->Left]&, adjust, {All,2}];
    adjust=MapAt[Grid[{{#}},ItemSize->5,Alignment->Left]&, adjust, {All,3}];
    pgs=TableForm@Partition[Row[#,"|"]&/@adjust,4];
    Print[fun<>": The input is either the number or the name string of a point group:\n",pgs]; 
    Abort[]
  ];
  pgno
]

Options[getPGElem]={"double"->False};
getPGElem[numOrName_, OptionsPattern[]]:=Module[{pgno,cs,elems,agno,gens},
  pgno=checkPGinput[numOrName, "getPGElem"];
  {agno,gens}=PGinfo[[pgno,{6,7}]];
  cs=SortBy[#,RotNameIndex]&/@getAGClassesByGen[Sequence@@agno,gens,RotTimes];
  elems=Flatten[Values@cs];
  If[OptionValue["double"]=!=True, elems, Join[elems,"bar"<>#&/@elems]]
]

Options[getPGCharTab]={"double"->False};
getPGCharTab[numOrName_, OptionsPattern[]]:=Module[{pgno,agno,cs,gens,nc,name1,name2,sg,
  dagno,dgens,dnc,dcs,ct,dct,slabel,iridx,cidx,dct1,dct2,ibarE,tmp,diridx,dlabel,IRidx,
  d1label,d2label,dcidx,sre,dre},
  pgno=checkPGinput[numOrName, "getPGCharTab"];
  {name1,name2,sg,nc,agno,gens,dnc,dagno,dgens}=PGinfo[[pgno,2;;]];
  cs=SortBy[#,RotNameIndex]&/@getAGClassesByGen[Sequence@@agno,gens,RotTimes];
  ct=AGCharTab[Sequence@@agno];
  slabel=BCtab2d2Label[pgno];
  iridx=Table[slabel[[i,1]]->i,{i,nc}]//Association;
  slabel=SortBy[slabel, MullikenIndex[#[[1]]]&];
  cidx=All;    
  (*Adjust the class order for C6h and Oh*)
  If[name1=="C6h", cidx={1, 6, 2, 4, 3, 5, 7, 12, 8, 10, 9, 11}];
  If[name1=="Oh", cidx={1, 4, 7, 9, 5, 2, 3, 8, 10, 6}];
  IRidx=iridx/@slabel[[All,1]];
  ct=ct[[IRidx,cidx]];  
  cs=Thread[Keys[cs]->Values[cs][[cidx]]]//Association;
  (*TableForm[ct,TableHeadings->{slabel,Column/@cs//Values}]*)
  (*For 32 point groups, no pseudoreal irep exists, irep with complex characters must be complex, otherwise real*)
  sre=If[Total[#]==0,1,3]&/@(Abs@Im[ct]);
  If[OptionValue["double"]==False,
    Return[<|"number"->pgno, "symbol"->{name1,name2}, "double"->False, 
             "class"->cs, "label"->slabel, "reality"->sre, "charTab"->ct, "iridx"->IRidx|>]
  ];
  (*--------process double point group---------*)
  dcs=SortBy[#,RotNameIndex]&/@getAGClassesByGen[Sequence@@dagno,dgens,DRotTimes];
  dct=AGCharTab[Sequence@@dagno];
  ibarE=Position[Values[dcs],"barE"][[1,1]];
  dct1=Select[dct, #[[ibarE]]>0&]; (*single-valued irep*)
  dct2=Select[dct, #[[ibarE]]<0&]; (*double-valued irep*)
  tmp=doublePGIrepLabel[pgno];
  d1label=BCtab2d2Label[pgno][[First@tmp]];
  d2label=tmp[[2]];
  dlabel=Join[d1label,d2label];
  (*Adjust the class order for dnc==2nc*)
  If[dnc!=2*nc, dcidx=All, (*else*)
    dcidx=Join[Position[Values[dcs],#][[1,1]]&/@Values[cs][[All,1]],
               Position[Values[dcs],DRotTimes[#,"barE"]][[1,1]]&/@Values[cs][[All,1]]]
  ]; 
  diridx=Table[dlabel[[i,1]]->i,{i,dnc}]//Association;
  dlabel=SortBy[dlabel, MullikenIndex[#[[1]]]&];
  dcs=Thread[Keys[dcs]->Values[dcs][[dcidx]]]//Association;
  tmp=dct[[All,dcidx]];
  dct=Join[dct1,dct2][[diridx/@dlabel[[All,1]],dcidx]];
  IRidx=Position[Round[tmp,0.01],#][[1,1]]&/@Round[dct,0.01];
  (*For double-valued irep, irep with complex characters must be complex, otherwise 1D irep is real and >=2D irep is pseudoreal*)
  dre=If[Total[Abs@Im[#]]==0,If[#[[1]]==1,1,2],3]&/@dct[[Length[dct1]+1;;]];
  dre=Join[sre,dre];
  (*TableForm[dct[[All,dcidx]],TableHeadings->{dlabel,Column/@Values[dcs]}]//Print;*)
  Return[<|"number"->pgno, "symbol"->{name1,name2}, "double"->True, 
           "class"->dcs, "label"->dlabel, "reality"->dre, "charTab"->dct, "iridx"->IRidx|>]
]


(* ::Subsection:: *)
(*showPGCharTab*)


Options[showPGCharTab]={"double"->False,"mode"->4,"class"->Automatic,"elem"->All,"irep"->All, "linewidth"->2};
showPGCharTab[numOrName_, OptionsPattern[]]:=Module[{pgct,cs,cskey,label,ct,mode,clsopt,elmopt,tmp,elmidx,
  elems,elmerr,pper,nc,snc,txtirl,iropt,irerr,iridx,headC,headR,showclass,tab,grid,sty1,sty2,sidx,didx,
  bg0,bg1,bg2,bg3,bg4,bg5,idxE},
  (*-------check option "mode"---------*)
  mode=OptionValue["mode"];
  If[!MemberQ[{1,2,3,4},mode],
    Print["showPGCharTab: \"mode\" should be one of 1, 2, 3, or 4 (default):\n",
          "1. A class label as a column head, \"class\"->On as default.\n",
          "2. All elements in a class as a column head, \"class\"->Off as default.\n",
          "3. One element in a class as a column head, \"class\"->Automatic as default.\n",
          "4. One element (for Abel group) or a class label (otherwise) as a column head, \"class\"->Automatic as default." ];   
    Abort[]
  ];

  (*-------check option "class"---------*)
  clsopt=OptionValue["class"];
  If[!MemberQ[{On,Off,Automatic},clsopt],
    Print["showPGCharTab: \"class\" should be one of On, Off, or Automatic (default):\n",
          "On: Show elements in each class.\n", "Off: Do not show elements in each class.\n",
          "Automatic: Show elements in each class only for non-Abel group (for mode 3 and 4).\n"];
    Abort[]
  ];

  pgct=getPGCharTab[numOrName,"double"->OptionValue["double"]];
  cs=pgct["class"];   ct=pgct["charTab"];   label=pgct["label"];
  cskey=Keys[cs];     cs=Values[cs];        elems=Flatten[cs];
  {snc,nc}=PGinfo[[pgct["number"],{5,8}]];  If[OptionValue["double"]==False,nc=snc]; 

  (*-------check option "elem"---------*)
  elmopt=OptionValue["elem"];
  pper[hs_,tab_]:=Prepend[#2,#1]&@@@Transpose[{hs,tab}]; (*prepend each row*)
  elmerr:=Print["showPGCharTab: \"elem\" aims to reorder the columns and it can be:\n",
           "A list of rotation names to represent their classes, refer to\n  ",
           Grid[pper[Row[{#,":"}]&/@cskey, MapAt[Row[{#,","}]&,Map[InputForm,cs,{2}],{All,;;-2}]], Alignment->Left],
           "\nOr just a list of integers (or a span) for the class sequence numbers."];
  If[elmopt===0, elmerr; Abort[]];
  If[StringQ[elmopt]||IntegerQ[elmopt], elmopt={elmopt}];
  elmidx=elmopt;
  If[VectorQ[elmidx,StringQ], 
    If[SubsetQ[elems,elmidx], elmidx=Position[cs,#][[1,1]]&/@elmidx, elmerr; Abort[]], (*next line else*)
    If[!(VectorQ[elmidx,IntegerQ]||Head[elmidx]===Span||elmidx===All), elmerr; Abort[]]
  ]; 
  Check[elmidx=Range[nc][[DeleteCases[elmidx,0]]], 
    Print["showPGCharTab: out of range! There are ",nc," classes in total."]; Abort[], 
    {Part::partw,Part::take}];

  (*-------check option "irep"---------*)
  txtirl=pgct["label"];
  txtirl=Transpose[{Mulliken2str/@txtirl[[All,1]],GammaLabel2str/@txtirl[[All,2]]}]; 
  iropt=OptionValue["irep"];
  irerr:=Print["showPGCharTab: \"irep\" aims to reorder the ireps (rows) and it can be:\n",
           "A list of text-version irep labels. OR\n",
           "A list of integers (or a span) for the irep sequence numbers. Refer to\n",
           Grid[pper[Range[nc], Map[InputForm,txtirl,{2}]], Frame->All,FrameStyle->Gray] ];
  If[iropt===0, irerr; Abort[]];
  iridx=If[StringQ[iropt]||IntegerQ[iropt], {iropt}, iropt];
  If[VectorQ[iridx,StringQ], 
    If[SubsetQ[Flatten[txtirl],iridx], iridx=Position[txtirl,#][[1,1]]&/@iridx, irerr; Abort[]], (*next line else*)
    If[!(VectorQ[iridx,IntegerQ]||Head[iridx]===Span||iridx===All), irerr; Abort[]]
  ]; 
  Check[iridx=Range[nc][[DeleteCases[iridx,0]]], 
    Print["showPGCharTab: out of range! There are ",nc," ireps in total."]; Abort[], 
    {Part::partw,Part::take}];

   
  showclass=Grid[pper[Row[{#,":"}]&/@cskey, MapAt[Row[{#,","}]&,Map[showRot,cs,{2}],{All,;;-2}]], Alignment->Left];
  ct=ct[[iridx,elmidx]];
  headR=pgct["label"][[iridx]];
  headC=Switch[mode,
    1, cskey[[elmidx]],
    2, Column/@Map[showRot,cs[[elmidx]],{2}],
    3, showRot/@If[VectorQ[elmopt,StringQ], elmopt, cs[[elmidx,1]]],
    4, If[nc==Length[elems], showRot/@elems[[elmidx]], cskey[[elmidx]]]
  ];
  
  tmp=Position[headC,"E"]; (*To adjust the width of "E", find its index, remove index of barE.*)
  idxE=tmp[[All,1]];
  tmp=Position[headC,showRot["barE"]]; 
  idxE=Complement[idxE,tmp[[All,1]]];
  idxE+=3;

  If[mode==1&&clsopt===Automatic, clsopt=On];
  If[mode==2&&clsopt===Automatic, clsopt=Off];
  If[MatchQ[mode,3|4]&&clsopt===Automatic, clsopt=If[nc==Length[elems],Off,On]];
  
  tmp=Row[{showPGInt[#2],"\[VeryThinSpace](",showPGSch[#1],")"}]&@@pgct["symbol"];
  tab=pper[headR[[All,1]],pper[headR[[All,2]],pper[pgct["reality"][[iridx]],ct]]];
  tab=Prepend[tab,{tmp,SpanFromLeft,SpanFromLeft,Sequence@@headC}];

  sty1=Directive[Black,Thickness[OptionValue["linewidth"]]];
  sty2=Directive[Thin,GrayLevel[0.8]];
  bg0={1,1}->Lighter[Red,0.9];
  bg1={{1,1},{2,-1}}->Lighter[Yellow,0.9];
  tmp=Mulliken2str/@headR[[All,1]];
  didx=Select[Range@Length[headR],StringLength[tmp[[#]]]>3&&StringTake[tmp[[#]],3]=="bar"&];
  sidx=Complement[Range@Length[headR],didx];
  bg2={{#,#}+1,{1,-1}}->Lighter[Green,0.95]&/@sidx;
  bg3={{#,#}+1,{1,-1}}->Lighter[Blue,0.95]&/@didx;
  bg4={{#,#}+1,{1,3}}->Lighter[Green,0.90]&/@sidx;
  bg5={{#,#}+1,{1,3}}->Lighter[Blue,0.90]&/@didx;

  grid=Grid[tab, Frame->All, Alignment->{{Center,Center,Left,{Right}}, Center (*,{{1,1},{1,-1}}->Center*) }, 
                 ItemSize->{{Full,{1->Full,2->Full,3->0.6,Sequence@@(#->1&/@idxE)}},{}},
                 Dividers->{{{{sty2}},Join[#->sty1&/@{1,4,-1},#->sty2&/@{2,3}]},
                            {{{sty2}},#->sty1&/@{1,2,-1}}},
                 Background->{None,None,{bg0,bg1,Sequence@@Join[bg2,bg3,bg4,bg5]}}
           ];
  If[clsopt=!=On,showclass=Nothing];
  {showclass,grid}//Column
]


(* ::Subsection:: *)
(*getPGIrepTab*)


Options[getPGIrepTab]={"double"->False, "trace"->False};
getPGIrepTab[numOrName_, OptionsPattern[]]:=Module[{pgno,name1,name2,tmp,nc,agno,gens,dnc,
  dagno,dgens,elem,elemidx,cs,ct,irt,pgct,dcs,delem,delemidx,dct,dirt,dpgct},
  pgno=checkPGinput[numOrName, "getPGIrepTab"];
  {name1,name2,tmp,nc,agno,gens,dnc,dagno,dgens}=PGinfo[[pgno,2;;]];
  cs=Values@getAGClassesByGen[Sequence@@agno,gens,RotTimes];
  elem=Flatten[cs];
  elemidx=Association@Table[elem[[i]]->i,{i,Length[elem]}];
  elem=SortBy[elem, RotNameIndex]; 
  elemidx=elemidx/@elem;
  ct=AGCharTab[Sequence@@agno];
  tmp=Length/@cs;
  (*Initial value of the irep table (irt) is constructed from the character table*)
  irt=Table[Flatten[Table[#1,#2]&@@@Transpose[{ir,tmp}],1],{ir,ct}];
  tmp=AGIrepGen[Sequence@@agno][[All,2]];
  For[i=1,i<=nc,i++, If[OptionValue["trace"]===True, Break[]];
    If[irt[[i,1]]==1, Continue[]]; (*update irep matrix with dimension>1*)
    irt[[i]]=Values@getAGClassesByGen[Sequence@@agno,tmp[[i]],Dot]//Flatten[#,1]&;
  ];
  pgct=getPGCharTab[pgno,"double"->False];
  irt=irt[[pgct["iridx"],elemidx]];
  If[OptionValue["double"]==False,
    Return[<|"number"->pgno, "symbol"->{name1,name2}, "double"->False, 
             "elem"->elem, "label"->pgct["label"], "reality"->pgct["reality"], "irep"->irt|>]
  ];
(*TableForm[irt, TableHeadings->{pgct["label"],elem}]//Print;*)
  (*---------------process double point group--------------*)
  dcs=Values@getAGClassesByGen[Sequence@@dagno,dgens,DRotTimes];
  delem=Flatten[dcs];
  delemidx=Association@Table[delem[[i]]->i,{i,Length[delem]}];
  delem=SortBy[delem, RotNameIndex];
  delemidx=delemidx/@delem;
  dct=AGCharTab[Sequence@@dagno];
  tmp=Length/@dcs;
  dirt=Table[Flatten[Table[#1,#2]&@@@Transpose[{ir,tmp}],1],{ir,dct}];
  tmp=AGIrepGen[Sequence@@dagno][[All,2]];
  For[i=1,i<=dnc,i++, If[OptionValue["trace"]===True, Break[]];
    If[dirt[[i,1]]==1, Continue[]];
    dirt[[i]]=Simplify@Values@getAGClassesByGen[Sequence@@dagno,tmp[[i]],Dot]//Flatten[#,1]&;
  ];
  dpgct=getPGCharTab[pgno,"double"->True];
  dirt=dirt[[dpgct["iridx"],delemidx]];
  (*For the ireps of double point groups with chi(barE)>0, we use the ireps of corresponding single point groups.*)
  dirt[[;;nc,;;Length[elem]]]=irt;
  dirt[[;;nc,Length[elem]+1;;]]=irt;
(*TableForm[dirt, TableHeadings->{dpgct["label"],delem}]//Print;*)
  Return[<|"number"->pgno, "symbol"->{name1,name2}, "double"->True, 
           "elem"->delem, "label"->dpgct["label"], "reality"->dpgct["reality"], "irep"->dirt|>]
]


(* ::Subsection:: *)
(*showPGIrepTab*)


Options[showPGIrepTab]={"double"->True,"rotmat"->True,"elem"->All,"irep"->All,"trace"->False,
                        "spin"->"downup","cartesian"->False,"linewidth"->2};
showPGIrepTab[numOrName_, OptionsPattern[]]:=Module[{pgirt,label,irt,elmopt,tmp,elmidx,dbl,
  elems,nelm,elmerr,nc,snc,txtirl,iropt,irerr,iridx,row1,rots1,rots2,brav,tab,nstart,grid,
  sty1,sty2,sidx,didx,bg0,bg1,bg1a,bg2,bg3,bg4,bg5,reality},
  (*-------check option "double"---------*)
  dbl=OptionValue["double"];
  If[!MemberQ[{True,False,Full},dbl],
    Print["showPGIrepTab: \"double\" should be one of True (default), False, or Full:\n",
          "True:  For ireps of double point groups. Only half of all elements, i.e. the ones without bar, are shown.\n", 
          "Full:  For ireps of double point groups. All elements are shown.\n", 
          "False: For ireps of single point groups.\n"];
    Abort[]
  ];

  pgirt=getPGIrepTab[numOrName,"double"->True, "trace"->OptionValue["trace"]];
  elems=pgirt["elem"];   irt=pgirt["irep"];   label=pgirt["label"];   reality=pgirt["reality"];
  nelm=Length[elems];
  {snc,nc}=PGinfo[[pgirt["number"],{5,8}]];  
  If[dbl===False,
    nc=snc; nelm=nelm/2; elems=elems[[;;nelm]]; irt=irt[[;;nc,;;nelm]]; label=label[[;;nc]]; reality=reality[[;;nc]]
  ]; 

  (*-------check option "elem"---------*)
  elmopt=OptionValue["elem"];
  elmerr:=Print["showPGIrepTab: \"elem\" aims to reorder the columns and it can be:\n",
           "A list of rotation names (elements). OR\n",
           "A list of integers (or a span) for their sequence numbers. Refer to\n",
            Grid[Partition[Grid[{#},ItemSize->{{1.5,Full}},Alignment->Left]&/@Transpose@{Range[nelm],InputForm/@elems},UpTo[10]], 
                 ItemSize->Full, Frame->All, Alignment->Left, FrameStyle->Gray] ];
  If[elmopt===0, elmerr; Abort[]];
  If[StringQ[elmopt]||IntegerQ[elmopt], elmopt={elmopt}];
  elmidx=elmopt;
  If[VectorQ[elmidx,StringQ], 
    If[SubsetQ[elems,elmidx], elmidx=Position[elems,#][[1,1]]&/@elmidx, elmerr; Abort[]], (*next line else*)
    If[!(VectorQ[elmidx,IntegerQ]||Head[elmidx]===Span||elmidx===All), elmerr; Abort[]]
  ]; 
  Check[elmidx=Range[nelm][[DeleteCases[elmidx,0]]], 
    Print["showPGIrepTab: out of range! There are ",nelm," elements in total."]; Abort[], 
    {Part::partw,Part::take}];

  (*-------check option "irep"---------*)
  txtirl=Transpose[{Mulliken2str/@label[[All,1]],GammaLabel2str/@label[[All,2]]}]; 
  iropt=OptionValue["irep"];
  irerr:=Print["showPGIrepTab: \"irep\" aims to reorder the ireps (rows) and it can be:\n",
           "A list of text-version irep labels. OR\n",
           "A list of integers (or a span) for the irep sequence numbers. Refer to\n",
           Grid[Prepend[Transpose@Map[InputForm,txtirl,{2}],Range[nc]], Frame->All,FrameStyle->Gray] ];
  If[iropt===0, irerr; Abort[]];
  iridx=If[StringQ[iropt]||IntegerQ[iropt], {iropt}, iropt];
  If[VectorQ[iridx,StringQ], 
    If[SubsetQ[Flatten[txtirl],iridx], iridx=Position[txtirl,#][[1,1]]&/@iridx, irerr; Abort[]], (*next line else*)
    If[!(VectorQ[iridx,IntegerQ]||Head[iridx]===Span||iridx===All), irerr; Abort[]]
  ]; 
  Check[iridx=Range[nc][[DeleteCases[iridx,0]]], 
    Print["showPGIrepTab: out of range! There are ",nc," ireps in total."]; Abort[], 
    {Part::partw,Part::take}];
  
  If[dbl===True&&elmopt===All, nelm=nelm/2; elmidx=elmidx[[;;nelm]]];
  irt=irt[[iridx,elmidx]];    elems=elems[[elmidx]];  label=label[[iridx]];

  tab=Map[formatRepMat,irt,{2}];
  tab=Map[If[MatrixQ[#],MatrixForm[#],#]&, tab, {2}];

  tab=Transpose[Join@@Transpose/@{label,Transpose@{reality[[iridx]]},tab}];
  
  tmp=Row[{showPGInt[#2],"\[VeryThinSpace](",showPGSch[#1],")"}]&@@pgirt["symbol"];
  row1={tmp,SpanFromLeft,SpanFromLeft,Sequence@@showRot/@elems};
  If[OptionValue["rotmat"]=!=False,
    brav=If[16<=pgirt["number"]<=27, "HexaPrim", "CubiPrim"];
    rots1=getRotMat[brav,StringReplace[#,"bar"->""]]&/@elems;
    If[OptionValue["cartesian"]===True&&brav=="HexaPrim",
      tmp=Transpose@BasicVectors[brav];  rots1=Simplify[tmp.#.Inverse[tmp]]&/@rots1;
      tmp="(cart.)",  (*else:*) tmp=Nothing
    ];
    rots1={Column[{"Rotation","matrix",tmp}], SpanFromLeft,SpanFromLeft, Sequence@@MatrixForm/@rots1};
    rots2=ComplexExpand@First@getSpinRotOp[#]&/@elems;
    tmp="(\[DownArrow]\[UpArrow])";
    If[OptionValue["spin"]==="updown", 
       tmp="(\[UpArrow]\[DownArrow])"; rots2={{0,1},{1,0}}.#.{{0,1},{1,0}}&/@rots2
    ];
    rots2={Column[{"Spin"<>tmp,"rotation","matrix"}], SpanFromLeft,SpanFromLeft, Sequence@@MatrixForm/@rots2};
    tab=Prepend[tab,rots1];  nstart=3;
    If[dbl=!=False, tab=Insert[tab,rots2,2];  nstart=4]; 
    , (*----else----*)
    nstart=2
  ];
  tab=Prepend[tab,row1];

  sty1=Directive[Black,Thickness[OptionValue["linewidth"]]];
  sty2=Directive[Thin,GrayLevel[0.8]];
  bg0={1,1}->Lighter[Red,0.9];
  bg1a={{2,nstart-1},{1,2}}->Lighter[Yellow,0.85];
  bg1={{1,nstart-1},{2,-1}}->Lighter[Yellow,0.9];
  tmp=Mulliken2str/@label[[All,1]];
  didx=Select[Range@Length[iridx],StringLength[tmp[[#]]]>3&&StringTake[tmp[[#]],3]=="bar"&];
  sidx=Complement[Range@Length[iridx],didx];
  bg2={{#,#}+nstart-1,{1,-1}}->Lighter[Green,0.95]&/@sidx;
  bg3={{#,#}+nstart-1,{1,-1}}->Lighter[Blue,0.95]&/@didx;
  bg4={{#,#}+nstart-1,{1,3}}->Lighter[Green,0.90]&/@sidx;
  bg5={{#,#}+nstart-1,{1,3}}->Lighter[Blue,0.90]&/@didx;

  grid=Grid[tab, Frame->All, Alignment->Center, ItemSize->{{Full,{1->Full,2->Full,3->0.6}},{}}, 
                 Dividers->{{{{sty2}},Join[#->sty1&/@{1,4,-1},#->sty2&/@{2,3}]},
                            {{{sty2}},#->sty1&/@{1,2,-1}}},
                 Background->{None,None,{bg0,bg1a,bg1,Sequence@@Join[bg2,bg3,bg4,bg5]}}
           ]
]


(* ::Subsection:: *)
(*PGIrepDirectProduct*)


Options[PGIrepDirectProduct]={"output"->1};
PGIrepDirectProduct[numOrName_, ireps1_, ireps2_, OptionsPattern[]]/;
 !Or@@(VectorQ[Flatten[{#}],MemberQ[{Rule,RuleDelayed},Head[#]]&]&/@{ireps1,ireps2}):=Module[{pgct,snc,dnc,
  ir1idx,ir2idx,txtirl,irerr,ct,cs,label,kc,DP1pair,conjct,kcconjct,nelm,out1,out2,out3,outopt},
  pgct=getPGCharTab[numOrName, "double"->True];
  {snc,dnc}=PGinfo[[pgct["number"],{5,8}]]; 
  ct=pgct["charTab"];   cs=pgct["class"];   label=pgct["label"];
  (*-------check input ireps1 and ireps2--------*)
  txtirl=Transpose[{Mulliken2str/@label[[All,1]],GammaLabel2str/@label[[All,2]]}]; 
  irerr:=Print["PGIrepDirectProduct: Inputs ireps1 and ireps2 can be:\n",
           "A (list of) text-version irep label(s). OR\n",
           "An (a list of) integer(s) for the irep sequence number(s). OR\n",
           "A span such as 1;;5.  OR  All.  Refer to\n",
           Grid[Prepend[Transpose@Map[InputForm,txtirl,{2}],Range[dnc]], Frame->All,FrameStyle->Gray] ];
  If[ireps1===0||ireps2===0, irerr; Abort[]];
  ir1idx=If[StringQ[ireps1]||IntegerQ[ireps1], {ireps1}, ireps1];
  If[VectorQ[ir1idx,StringQ], 
    If[SubsetQ[Flatten[txtirl],ir1idx], ir1idx=Position[txtirl,#][[1,1]]&/@ir1idx, irerr; Abort[]], (*next line else*)
    If[!(VectorQ[ir1idx,IntegerQ]||Head[ir1idx]===Span||ir1idx===All), irerr; Abort[]]
  ]; 
  Check[ir1idx=Range[dnc][[DeleteCases[ir1idx,0]]], 
    Print["PGIrepDirectProduct: out of range! There are ",dnc," ireps in total."]; Abort[], {Part::partw,Part::take}];
  ir2idx=If[StringQ[ireps2]||IntegerQ[ireps2], {ireps2}, ireps2];
  If[VectorQ[ir2idx,StringQ], 
    If[SubsetQ[Flatten[txtirl],ir2idx], ir2idx=Position[txtirl,#][[1,1]]&/@ir2idx, irerr; Abort[]], (*next line else*)
    If[!(VectorQ[ir2idx,IntegerQ]||Head[ir2idx]===Span||ir2idx===All), irerr; Abort[]]
  ]; 
  Check[ir2idx=Range[dnc][[DeleteCases[ir2idx,0]]], 
    Print["PGIrepDirectProduct: out of range! There are ",dnc," ireps in total."]; Abort[], {Part::partw,Part::take}];
  (*--------check option "output"---------------*)
  outopt=OptionValue["output"];
  If[!MemberQ[{1,2,3,4},outopt],
    Print["PGIrepDirectProduct: option \"output\" can be one of {1,2,3,4}:\n",
      "1. Output the Mulliken label.\n2. Output the Gamma label.\n",
      "3. Output the occurrence numbers of all ireps.\n4. Output all the above 1-3 results."
    ]; Abort[]
  ];
  (*--------------------------------------------*)

  kc=Length/@Values[cs];  conjct=Conjugate[ct];  kcconjct=kc*#&/@conjct;
  nelm=Total[kc];
  DP1pair[i_,j_]:=Simplify[(ct[[i]]*ct[[j]]).#/nelm]&/@kcconjct;
  out3=Table[{i,j}->DP1pair[i,j], {i,ir1idx},{j,ir2idx}]//Flatten[#,1]&//Association;
  out1=label[[#,1]]->(If[#[[1]]==1,#[[2]],#]&/@Select[Transpose@{out3[#],label[[All,1]]},#[[1]]!=0&])&/@Keys[out3]//Association;
  out2=label[[#,2]]->(If[#[[1]]==1,#[[2]],#]&/@Select[Transpose@{out3[#],label[[All,2]]},#[[1]]!=0&])&/@Keys[out3]//Association;
  
  Switch[outopt, 1, out1, 2, out2, 3, out3, 4, {out1,out2,out3}]
]
PGIrepDirectProduct[numOrName_, ireps1_, OptionsPattern[]]/;!VectorQ[Flatten[{ireps1}],MemberQ[{Rule,RuleDelayed},Head[#]]&]:=
  PGIrepDirectProduct[numOrName, ireps1, ireps1, "output"->OptionValue["output"]]
PGIrepDirectProduct[numOrName_, OptionsPattern[]]:=PGIrepDirectProduct[numOrName, All, All, "output"->OptionValue["output"]]


(* ::Subsection:: *)
(*showPGIrepDirectProduct*)


Options[showPGIrepDirectProduct]={"label"->1, "double"->True, "linewidth"->2, "emph"->None};
showPGIrepDirectProduct[numOrName_, ireps1_, ireps2_, OptionsPattern[]]/;
 !Or@@(VectorQ[Flatten[{#}],MemberQ[{Rule,RuleDelayed},Head[#]]&]&/@{ireps1,ireps2}):=Module[{pgct,dp,
  ir1,ir2,tmp,label,txtirl,lopt,tab,sir1,dir1,sir2,dir2,sty1,sty2,bg0,bg1,bg2,bg3,bg4,bg5,s1pos,d1pos,
  s2pos,d2pos,emopt,emidx,dnc,emerr},
  lopt=OptionValue["label"];
  If[!MemberQ[{1,2},lopt],
    Print["showPGIrepDirectProduct: option \"label\" can be 1 or 2:\n",
          "1. Output the Mulliken label.\n2. Output the Gamma label."]; Abort[]
  ];

  pgct=getPGCharTab[numOrName, "double"->True];
  label=pgct["label"];  dnc=Length[pgct["class"]];
  (*-------check option "emph"--------*)
  txtirl=Transpose[{Mulliken2str/@label[[All,1]],GammaLabel2str/@label[[All,2]]}]; 
  emopt=OptionValue["emph"];    If[emopt===None,emopt={}];
  emerr:=Print["showPGIrepDirectProduct: option \"emph\" specifies the irep(s) to be emphasized and can be:\n",
           "A (list of) text-version irep label(s). OR\n",
           "An (a list of) integer(s) for the irep sequence number(s). OR\n",
           "A span such as 1;;5.  OR  All  OR  None (default).  Refer to\n",
           Grid[Prepend[Transpose@Map[InputForm,txtirl,{2}],Range[dnc]], Frame->All,FrameStyle->Gray] ];
  If[emopt===0, emerr; Abort[]];
  emidx=If[StringQ[emopt]||IntegerQ[emopt], {emopt}, emopt];
  If[VectorQ[emidx,StringQ], 
    If[SubsetQ[Flatten[txtirl],emidx], emidx=Position[txtirl,#][[1,1]]&/@emidx, emerr; Abort[]], (*next line else*)
    If[!(VectorQ[emidx,IntegerQ]||Head[emidx]===Span||emidx===All), emerr; Abort[]]
  ]; 
  Check[emidx=Range[dnc][[DeleteCases[emidx,0]]], 
    Print["showPGIrepDirectProduct: out of range! There are ",dnc," ireps in total."]; Abort[], {Part::partw,Part::take}];

  dp=PGIrepDirectProduct[numOrName,ireps1,ireps2,"output"->4];
  ir1=Keys[dp[[3]]][[All,1]]//DeleteDuplicates;
  ir2=Keys[dp[[3]]][[All,2]]//DeleteDuplicates;
  sir1=Select[ir1,StringPosition[txtirl[[#,1]],"bar"]=={}&];
  sir2=Select[ir2,StringPosition[txtirl[[#,1]],"bar"]=={}&];
  dir1=Complement[ir1,sir1];
  dir2=Complement[ir2,sir2];
  If[OptionValue["double"]===False, ir1=sir1; ir2=sir2];
  tmp=Join@@(Position[dp,#]&/@label[[emidx,lopt]]);
  If[emidx!={}, dp=MapAt[Style[#,{Red,Bold}]&,dp,tmp]];
  dp=Row[#,"+"]&/@Map[If[ListQ[#],Row[#],#]&, dp[[lopt]], {2}];
  If[emidx!={}, dp=Style[#,Gray]&/@dp];

  tab=Table[dp[{label[[i,lopt]],label[[j,lopt]]}], {i,ir1}, {j,ir2}];
  tab=Transpose@Join[Transpose@label[[ir1]], Transpose@tab];
  tmp=Row[{showPGInt[#2],"\[VeryThinSpace](",showPGSch[#1],")"}]&@@pgct["symbol"];
  tab=Prepend[tab, {tmp,SpanFromLeft,Sequence@@label[[ir2,lopt]]}];

  tmp=Association@Table[ir1[[i]]->i+1,{i,Length[ir1]}];
  s1pos=tmp/@sir1;  d1pos=tmp/@dir1;
  tmp=Association@Table[ir2[[i]]->i+2,{i,Length[ir2]}];
  s2pos=tmp/@sir2;  d2pos=tmp/@dir2;
 
  sty1=Directive[Black,Thickness[OptionValue["linewidth"]]];
  sty2=Directive[Thin,GrayLevel[0.8]];
  bg0={1,1}->Lighter[Red,0.9];
  bg1=With[{c=Lighter[Green,0.85]}, Flatten@Join[{{#,1}->c,{#,2}->c}&/@s1pos,{1,#}->c&/@s2pos]];
  bg2=With[{c=Lighter[Blue,0.85]}, Flatten@Join[{{#,1}->c,{#,2}->c}&/@d1pos,{1,#}->c&/@d2pos]];
  bg3=Table[{i,j}->Lighter[Green,0.9],{i,s1pos},{j,s2pos}]//Flatten[#,1]&;
  bg4=Table[{i,j}->Lighter[Yellow,0.9],{i,d1pos},{j,d2pos}]//Flatten[#,1]&;
  bg5=Join[Table[{i,j}->Lighter[Blue,0.9],{i,s1pos},{j,d2pos}],
           Table[{i,j}->Lighter[Blue,0.9],{i,d1pos},{j,s2pos}]]//Flatten[#,1]&;

  Grid[tab, Frame->All, Alignment->Center, ItemSize->{{{},{1->2.5,2->2.5}},{}}, 
                 Dividers->{{{{sty2}},Join[#->sty1&/@{1,3,-1},#->sty2&/@{2}]},
                            {{{sty2}},#->sty1&/@{1,2,-1}}},
                 Background->{None,None,{bg0,Sequence@@Join[bg1,bg2,bg3,bg4,bg5]}}
       ]
]
showPGIrepDirectProduct[numOrName_, ireps1_, OptionsPattern[]]/;!VectorQ[Flatten[{ireps1}],MemberQ[{Rule,RuleDelayed},Head[#]]&]:=
  showPGIrepDirectProduct[numOrName, ireps1, ireps1, (#->OptionValue[#])&/@{"label","double","linewidth","emph"}]
showPGIrepDirectProduct[numOrName_, OptionsPattern[]]:=
  showPGIrepDirectProduct[numOrName, All, All, (#->OptionValue[#])&/@{"label","double","linewidth","emph"}]


(* ::Section:: *)
(*Get the table of ireps of little group and space group.*)


(* ::Subsection:: *)
(*getLGIrepTab and getLGCharTab*)


Clear[formatRepMatDict];
formatRepMatDict[_]=None
formatRepMat[mat_]:=Module[{norm,arg,fl,pu,poth,num},
  If[MatrixQ[mat], Return@Map[formatRepMat,mat,{2}]];
  If[ListQ,Return[formatRepMat/@mat]];
  If[MachineNumberQ[mat], Return[Round[mat,1.*^-6]]];
  num=formatRepMatDict[mat];
  If[num=!=None, Return[num]];
  num=Simplify[mat];
  If[Flatten[Position[num,#]&/@{t\:2081,t\:2082,t\:2083}]!={}, formatRepMatDict[mat]=num; Return[num]];
  If[Position[num,u]=={}, 
    norm=Norm[num]//FullSimplify;   arg=If[norm=!=0, Arg[num]//FullSimplify, 0];
    (* For AG {48,6},{96,7},{96,8}, there are matrix elements Sqrt[2]+I or Sqrt[2]-I whose arg 
       contains ArcTan or ArcCot, e.g. SG212, "R". In this case we do not use exp form. *)
    If[Position[arg,ArcTan]!={}||Position[arg,ArcCot]!={}, formatRepMatDict[mat]=num; Return[num]];
    num=norm*E^(I*arg);
    If[MemberQ[{0,Pi,Pi/2,-Pi/2,Pi/4,3Pi/4,-Pi/4,-3Pi/4},arg], num=ComplexExpand[num]];
    formatRepMatDict[mat]=num;   Return[num]
  ];
  fl=FactorList[num];
  pu=Position[fl,u][[1,1]];
  poth=DeleteCases[Range[Length[fl]],pu];
  If[Head[fl[[pu,1]]]===Power&&fl[[pu,1,1]]==E, fl[[pu,1]]=Simplify@Exp@Expand@ fl[[pu,1,2]]];
  num=formatRepMat[Times@@(#1^#2&@@@fl[[poth]])]*(#1^#2&@@fl[[pu]]);
  formatRepMatDict[mat]=num; 
  num
]; 

(* "format" controls whether or not format the matrix elements of the ireps. 
   "abcOrBasVec" can be Rules for lattice constants such as {a\[Rule]3,c\[Rule]2}, or the numerical basic vectors,
   or just be None. When "abcOrBasVec" is not None, BZ type is determined, otherwise all possible BZ
   types are considered. *)
Options[getLGIrepTab]={"format"->True, "abcOrBasVec"->None};
getLGIrepTab[sgno_, kNameOrCoordOrInfodict_,OptionsPattern[]]:=Block[{u,brav,ks,BZtypes,kname,tmp,
  kBZs,Gkin,kinfos,kinfodicts,Gkins,kinfo,times,inv,dtimes,dinv,AGno,avRep,gens,reality,reptype,pows,
  abc,repLabel,kname2,fullreps,reps,Sw,GkBC,idx,factor,hskp,HLG,CE,kstd,kLGrep,didx,dfactor,dHLG,dCE,
  dAGno,davRep,dgens,dreality,dreptype,dpows,drepLabel,dfullreps,dreps,dGkBC,GkBCs,mysort,tmp1,
  srepidx,drepidx,basVec,chkbv},
  brav=getSGLatt[sgno];
  BZtypes=Switch[brav,
    "OrthBase", {"OrthBase(a)","OrthBase(b)"},
    "OrthBody", {"OrthBody(a)","OrthBody(b)","OrthBody(c)"},
    "OrthFace", {"OrthFace(a)","OrthFace(b)","OrthFace(c)","OrthFace(d)"},
    "TetrBody", {"TetrBody(a)","TetrBody(b)"},
    "TrigPrim", {"TrigPrim(a)","TrigPrim(b)"},
    _, {brav} ];
  basVec=OptionValue["abcOrBasVec"];
  If[basVec=!=None,
    If[Position[basVec,Rule]!={}, basVec=BasicVectors[brav]/.Flatten[basVec]];
    If[!MatrixQ[basVec,NumericQ],
      Print["getLGIrepTab: The option \"abcOrBasVec\" can be rules which make the basic vectors\n",
            "              numeric, or directly the numeric basic vectors, or just None. \n"
            "              The basic vectors is now ",basVec];
      Abort[];
    ];
    chkbv=checkBasVec[brav,basVec];
    If[chkbv[[1]],
      If[Length[BZtypes]>1, BZtypes={brav<>"("<>chkbv[[2]]<>")"}],
      Print["getLGIrepTab: the basic vectors ",basVec," are not of the BC form for ", 
             brav, ": ",BasicVectors[brav]];  Abort[];
      ]
  ];
      
  If[StringQ[kNameOrCoordOrInfodict], kname=kNameOrCoordOrInfodict;
    ks=Keys[LGIrep[sgno]];
    If[!MemberQ[ks,kname], Print["getLGCharTab: kname should be in ",ks]; Abort[]];
    If[sgno==205&&kname=="Z'", kBZs={{"Z'",{1/2,u,0},{"CubiPrim"}}}; Goto["end kname"]];
    If[kname=="aF", kBZs={{"F",{0,1/2,-1/2},{"TrigPrim(a)"}}}; Goto["end kname"]];
    If[kname=="bF", kBZs={{"F",{1/2,1/2,0}, {"TrigPrim(b)"}}}; Goto["end kname"]];
    tmp={#,Select[BCHighSymKpt[#],#[[1]]==kname&]}&/@BZtypes;
    tmp=Select[tmp,#[[2]]!={}&];
    If[tmp=={}, Print["getLGCharTab: kpoint ",kname," does not exist in ",BZtypes]; Abort[]];
    tmp={#[[2,1,3]],#[[1]]}&/@tmp;
    tmp=Gather[tmp,First[#1]==First[#2]&];
    kBZs={kname,#[[1,1]],#[[All,2]]}&/@tmp;
    Label["end kname"];
    Gkin=getLGElem[sgno,kname]; 
    kinfos=identifyBCHSKptBySG[sgno,StringTake[kBZs[[1,3,1]],{-2,-2}],kBZs[[1,2]]/.u->0.1];
    kinfos={kinfos};   kBZs={kBZs};   Gkins={Gkin};
    Goto["kinfoGot"];
  ];
  
  If[VectorQ[kNameOrCoordOrInfodict],
    If[!VectorQ[kNameOrCoordOrInfodict,NumericQ], 
      Print["getLGIrepTab: the k coordinates ",kNameOrCoordOrInfodict," should be numeric."]; Abort[]];
    If[basVec===None,
      (*tmp={identifyBCHSKptBySG[sgno,StringTake[#,{-2,-2}],kNameOrCoordOrInfodict],#}&/@BZtypes,*)
      tmp=With[{tmp2=identifyBCHSKptBySG[sgno,StringTake[#,{-2,-2}],kNameOrCoordOrInfodict, "allowtwok"->True]},
            If[Length[Dimensions[tmp2]]==1, {tmp2,#}, Sequence@@Table[{kinf,#},{kinf,tmp2}]] ]&/@BZtypes,
      tmp={identifyBCHSKptBySG[sgno,basVec,kNameOrCoordOrInfodict],#}&/@BZtypes
      ];
    If[tmp[[1,1,2]]=="GP",
      kBZs={{{"GP",tmp[[1,1,1]],BZtypes}}};
      kinfos={tmp[[1,1]]};
      Gkins={{{"E",{0,0,0}}}};
      Goto["kinfoGot"];
    ];
    If[tmp[[1,1,2]]=="UN",
      kBZs={{{"UN",tmp[[1,1,1]],BZtypes}}};
      kinfos={tmp[[1,1]]};
      Gkins={Select[getLGElem[sgno,"\[CapitalGamma]"],MemberQ[tmp[[1,1,4]],#[[1]]]&]};
      Goto["kinfoGot"];
    ];    
    tmp=Gather[tmp,#1[[1,6]]==#2[[1,6]]&]; (* Gather items with the same rotation. *)
    (*\:5982\:679c\:6ca1\:6709\:4e0b\:9762\:8fd9\:53e5\:8fdb\:4e00\:6b65\:6309k\:70b9\:540d\:79f0\:5212\:5206\:7684\:8bdd\:ff0c\:5219 155, {0.4,0.4,0.4} \:53ea\:6709\[CapitalLambda]\:6ca1\:6709P\:90e8\:5206*)
    tmp=Join@@(Gather[#,#1[[1,2]]==#2[[1,2]]&]&/@tmp); (* Gather items with the same kname. *)
    kinfos=tmp[[All,1,1]];
    tmp=Transpose[{#[[All,1,2]],#[[All,1,5]],#[[All,2]]}]&/@tmp;
    tmp=Gather[#,#1[[2]]==#2[[2]]&]&/@tmp; (* For each kname, gather items with the same coordiantes.*)
    kBZs=Map[{#[[1,1]],#[[1,2]],#[[All,3]]}&,tmp,{2}];  
    If[brav=="TrigPrim"&&kBZs[[1,1,1]]=="F",
      tmp=If[ValueQ[chkbv], {chkbv[[2]]<>"F"}, {"aF","bF"}];
      GkBCs=getLGElem[sgno,#]&/@tmp,  (* --else-- *)
      GkBCs=getLGElem[sgno,#]&/@kBZs[[All,1,1]]
    ];
    Gkins=Module[{t=SeitzTimes[brav],i=invSeitz[brav]},
      GkBC=#1;  Sw=#2[[6]];  
      SortBy[modone[t[t[Sw,#],i[Sw]]]&/@GkBC, RotNameIndex[#[[1]]]&]
    ]&@@@Transpose[{GkBCs,kinfos}];
    Goto["kinfoGot"];
  ];
  
  If[AssociationQ[kNameOrCoordOrInfodict],   
    kLGrep=<||>;
    kLGrep["symbol"]={SGSymStd[sgno],sgno,SGSymBC[sgno]};
    {kBZs,kinfo,Gkin}=Values[kNameOrCoordOrInfodict];
    kLGrep["kBZs"]=kBZs;   kLGrep["kinfo"]=kinfo;   kLGrep["Gkin"]=Gkin;
    kname=kBZs[[1,1]];   kstd=kBZs[[1,2]];
    (*fix20211227: 
    \:5bf9\:4e8e\:591aBZ\:60c5\:51b5\:4e0b\:540c\:4e00k\:540d\:79f0\:4e0d\:540cBZ\:7c7b\:578b\:4e0b\:7684k\:70b9\:5750\:6807\:76f8\:5dee\:5012\:683c\:77e2\:7684\:60c5\:51b5\:ff08\:53ea\:6709\:4ee5\:4e0b\:56db\:79cd\:ff09\:ff0c\:5bf9\:4e0d\:540cBZ\:8981\:9009\:62e9
    \:76f8\:540c\:7684kstd\:ff0c\:8fd9\:6837\:624d\:80fd\:4fdd\:8bc1\:540c\:4e00\:5c0f\:8868\:793a\:540d\:79f0\:5bf9\:5e94\:7684\:5c0f\:8868\:793a\:4e00\:6837\:ff0c\:5426\:5219\:53ef\:80fd\:4e0d\:4e00\:6837\:ff0c\:539f\:56e0\:5728\:4e8eBC\:662f\:5bf9\:4e2d\:5fc3\:6269\:5c55\:7684
    \:8868\:793a\:547d\:540d\:7684\:ff0c\:4f46\:5b9e\:9645\:63cf\:8ff0\:7684\:5374\:662f\:5c0f\:8868\:793a\:ff0c\:6545\:6362\:7b97\:6210\:5c0f\:8868\:793a\:65f6\:76f8\:5dee\:5012\:683c\:77e2\:7684k\:70b9\:5f97\:51fa\:7684\:7ed3\:679c\:53ef\:80fd\:4e0d\:540c\:3002*)
    If[MemberQ[{{"OrthBase","H"},{"OrthBody","G"},{"OrthBody","F"},{"OrthBody","U"}},{brav,kname}],
      kstd=kBCcoord[sgno,kname][[1,1]]];
    kname2=If[brav=="TrigPrim"&&kname=="F", StringTake[kBZs[[1,3]],{-2,-2}]<>"F", kname];
    
    If[kname=="GP"||kname=="UN", hskp=False;  
      {avRep,reptype,AGno,CE}=calcRep[sgno,kinfo][[3;;]];
      {davRep,dreptype,dAGno,dCE}=calcRep[sgno,kinfo,"DSG"->True][[3;;]];
      GkBC=Gkin;   dGkBC=Gkin;    
      idx=(Flatten[Position[CE,{#[[1]],0}]]&/@GkBC)[[All,1]];   
      factor=Exp[-I*2Pi*kstd.#]&/@GkBC[[All,2]]//Simplify;
      didx=(Flatten[Position[dCE,{#[[1]],0}]]&/@dGkBC)[[All,1]];
      dfactor=factor;
      Goto["got_idx_and_factor"];
     ];
    
    Sw=kinfo[[6]];
    {AGno,gens,avRep,reptype}=LGIrep[sgno,kname2];
    {dAGno,dgens,davRep,dreptype}=DLGIrep[sgno,kname2];
    hskp=VectorQ[gens[[1,2]]];    
    times=SeitzTimes[brav];   inv=invSeitz[brav];
    dtimes=DSGSeitzTimes[brav];   dinv=DSGinvSeitz[brav];
    GkBC=If[Sw[[1]]=="E", Gkin, times[inv[Sw],times[#,Sw]]&/@Gkin];
    dGkBC=If[Sw[[1]]=="E", Gkin, dtimes[dinv[Sw],dtimes[#,Sw]]&/@Gkin];

    If[hskp,
      HLG=getHLGElem[brav,AGno,gens];
      idx=(Flatten[Position[HLG[[All,1]],#[[1]]]]&/@GkBC)[[All,1]];
      factor=Exp[-I*2Pi*kstd.#]&/@(GkBC[[All,2]]-HLG[[idx]][[All,2]])//Simplify;
      dHLG=getHLGElem[brav,dAGno,dgens,"DSG"->True];
      didx=(Flatten[Position[dHLG[[All,1]],#[[1]]]]&/@dGkBC)[[All,1]];
      dfactor=Exp[-I*2Pi*kstd.#]&/@(dGkBC[[All,2]]-dHLG[[didx]][[All,2]])//Simplify,
      (*-----------else: hskp\[Equal]False----------*)
      CE=getCentExt[sgno,kname];   
      idx=(Flatten[Position[CE,{#[[1]],0}]]&/@GkBC)[[All,1]];
      factor=Exp[-I*2Pi*kstd.#]&/@GkBC[[All,2]]//Simplify;
      dCE=getCentExt[sgno,kname,"DSG"->True];   
      didx=(Flatten[Position[dCE,{#[[1]],0}]]&/@dGkBC)[[All,1]];
      dfactor=Exp[-I*2Pi*kstd.#]&/@dGkBC[[All,2]]//Simplify;
    ];
    
    Label["got_idx_and_factor"];
    
    abc=Association[Rule@@@Transpose[{{"a","b","c","d","e","f"},Range[2,7]}]];
    reality=avRep[[All,2]];    avRep=avRep[[All,1]];    
    dreality=davRep[[All,2]];  davRep=davRep[[All,1]];    
    mysort=Sort[#, If[ListQ[#1],#1[[1]]<#2[[1]]||(#1[[1]]==#2[[1]]&&#1[[2]]=="+"),#1<#2]&]&;

    tmp=LGIrepLabel[AGno];
    tmp1=Select[tmp[[abc[reptype],3]],#=!=""&];
    srepidx=Position[tmp1,#]&/@mysort[tmp1]//Flatten;
    repLabel=Transpose[{tmp[[abc[reptype],2]],RepGammaLabel[kname,#]&/@tmp[[abc[reptype],3]]}];
    repLabel=repLabel[[Position[tmp[[1,2]],#]&/@avRep//Flatten]];
    pows=Flatten[AGClasses@@AGno,1];
    fullreps=Table[getAGIrepMat[Sequence@@AGno,ir,#]&/@pows,{ir,avRep}];
    reps=factor*#&/@fullreps[[All,idx]]//Chop//Simplify;

    tmp=DLGIrepLabel[dAGno];
    tmp1=Select[tmp[[abc[dreptype],3]],#=!=""&];
    drepidx=Position[tmp1,#]&/@mysort[tmp1]//Flatten;
    drepLabel=Transpose[{tmp[[abc[dreptype],2]],RepGammaLabel[kname,#]&/@tmp[[abc[dreptype],3]]}];
    drepLabel=drepLabel[[Position[tmp[[1,2]],#]&/@davRep//Flatten]];
    dpows=Flatten[AGClasses@@dAGno,1];
    dfullreps=Table[getAGIrepMat[Sequence@@dAGno,ir,#]&/@dpows,{ir,davRep}];
    dreps=dfactor*#&/@dfullreps[[All,didx]]//Chop//Simplify;
    
    kLGrep["GkBC"]=GkBC;     kLGrep["dGkBC"]=dGkBC;
    If[OptionValue["format"], reps=Map[formatRepMat,reps,{2}]; dreps=Map[formatRepMat,dreps,{2}]];
    (* srepidx and drepidx are used to sort the reps according to thier Gamma labels *)
    kLGrep["slabel"]=repLabel[[srepidx]];   
    kLGrep["sreality"]=reality[[srepidx]];  
    kLGrep["sirep"]=reps[[srepidx]];
    kLGrep["dlabel"]=drepLabel[[drepidx]];  
    kLGrep["dreality"]=dreality[[drepidx]]; 
    kLGrep["direp"]=dreps[[drepidx]];
    Return[kLGrep];
  ];
  
  (* If the program runs to here, something is wrong. *)
  Print["getLGIrepTab: the input k should be either a its name (string type) or its coordinate."];
  Abort[];
  
  Label["kinfoGot"];
  kinfodicts=<|"kBZs"->#1,"kinfo"->#2,"Gkin"->#3|>&@@@Transpose[{kBZs,kinfos,Gkins}];
  getLGIrepTab[sgno,#,"format"->OptionValue["format"],"abcOrBasVec"->basVec]&/@kinfodicts
]

Options[getLGCharTab]={"format"->True, "abcOrBasVec"->None};
getLGCharTab[sgno_, kNameOrCoord_, OptionsPattern[]]:=Block[{u,irepTabs,mytr,toCT,str,dtr},
  irepTabs=getLGIrepTab[sgno,kNameOrCoord,"format"->False,"abcOrBasVec"->OptionValue["abcOrBasVec"]];
  mytr[m_]:=If[MatrixQ[m],Tr[m],m];
  toCT[irepTab_]:=Module[{sirep,direp,charTab},
    sirep=irepTab["sirep"];   direp=irepTab["direp"];
    charTab=Delete[irepTab,{{Key["sirep"]},{Key["direp"]}}];
    str=Map[mytr,sirep,{2}];       dtr=Map[mytr,direp,{2}];
    If[OptionValue["format"], str=Map[formatRepMat,str,{2}]; dtr=Map[formatRepMat,dtr,{2}]];
    charTab["scharTab"]=str;
    charTab["dcharTab"]=dtr;
    charTab
  ];
  toCT/@irepTabs
]


(* ::Subsection:: *)
(*checkLGIrep*)


(* Check whether the representation matrices in the result of getLGIrepTab satisfy correct
   multiplications for LG IR. Things are all right if all the returned numbers are zero.
   e.g.   rep=getLGIrepTab[222,"R"]; 
          checkLGIrep[rep]   or  checkLGIrep/@rep   or  checkLGIrep[rep[[1]]]    *)
checkLGIrep[repinfo_]:=Module[{Gk,mtab,sirep,direp, time, brav, fBZ, rot2elem,n,tmp,sub,k,
  itab,dvtab,ftab,rot2idx, repmtab, reptime, check, dtime,mtab2, bartab, empty},
  If[ListQ[repinfo], Return[checkLGIrep/@repinfo]];
  Gk=repinfo["Gkin"];   n=Length[Gk];
  If[Position[repinfo["kinfo"],u]!={}, sub=repinfo["kinfo"][[9]], sub={}];
  rot2elem=#[[1]]->#&/@Gk//Association;
  rot2idx=Flatten[{Gk[[#,1]]->#,"bar"<>Gk[[#,1]]->#}&/@Range[n]]//Association;
  sirep=repinfo["sirep"];  direp=repinfo["direp"];
  fBZ=repinfo["kBZs"][[1,3]];  If[ListQ[fBZ], fBZ=fBZ[[1]]];
  k=repinfo["kinfo"][[1]]; 
  brav=If[StringTake[fBZ,-1]==")",StringTake[fBZ,1;;-4],fBZ];
  time=SeitzTimes[brav];
  mtab=Table[time[i,j],{i,Gk},{j,Gk}];
  itab=Table[rot2idx[mtab[[i,j,1]]],{i,n},{j,n}];  
  dvtab=Table[tmp=mtab[[i,j]];tmp=tmp[[2]]-rot2elem[tmp[[1]]][[2]],{i,n},{j,n}];
  ftab=Table[Exp[-I*k.dvtab[[i,j]]*2Pi]//Chop,{i,n},{j,n}]; 
  reptime[rep_,i_,j_]:=Module[{m1,m2},
     m1=rep[[i]]; m2=rep[[j]];
     If[MatrixQ[m1],m1.m2, m1*m2]//Simplify[#,u\[Element]Reals]&
  ];
  repmtab[rep_]:=Table[reptime[rep,i,j],{i,n},{j,n}];

  dtime=DSGSeitzTimes[brav];
  mtab2=Table[dtime[i,j],{i,Gk},{j,Gk}];  
  bartab=Table[If[mtab2[[i,j,1]]==Gk[[itab[[i,j]],1]],1,-1],{i,n},{j,n}]; 

  check[rep_,d_]:=Module[{diff, tab},
    tab= Table[rep[[itab[[i,j]]]],{i,n},{j,n}]*ftab;
    If[d=="d",tab=tab*bartab];
    diff=Flatten[(repmtab[rep]-tab)/.sub];
    Total@Abs[diff//N//Chop//Simplify]
  ];
   
  empty=If[sirep=={}||direp=={}, 1, 0];
  {empty, check[#,"s"]&/@sirep, check[#,"d"]&/@direp}//Simplify
]


(* ::Subsection:: *)
(*getRepMat and getLGIrepMat*)


(* Gk is the list of elements of the little group of k, and rep is the corresponding representation
   matrices (or characters) of each elements in Gk. rep can be one representation or a list of 
   representations.*)
getRepMat[k_/;VectorQ[k],Gk_,rep_][RvOrRvList_]:=Module[{trans,ir,id,forOneRv,reps,oneRep,re},
  If[Length[Gk[[1]]]==3, id={1,3}, id={1}];  (*compatible with magnetic little group*)
  trans=Association[#[[id]]->#[[2]]&/@Gk];
  oneRep=Mod[Length@Dimensions[rep],2]==1;   reps=If[oneRep, rep, rep\[Transpose]];
  ir=Association[Rule@@@Transpose[{Gk[[All,id]],reps}]];
  (trans[MapAt["bar"<>#&,#,1]]=trans[#])&/@Gk[[All,id]];
  (ir[MapAt["bar"<>#&,#,1]]=-ir[#])&/@Gk[[All,id]]; 
  forOneRv[Rv_]:=Module[{dv,m}, 
    m=ir[Rv[[id]]];
    If[Head[m]===Missing, Print["getRepMat: no rotation ",
       If[Length[id]==1,Rv[[1]],Rv[[id]]]," in the ",
       If[Length[id]==1,"","magnetic "], "little group of k=",k,"."]; Abort[]];
    dv=Rv[[2]]-trans[Rv[[id]]];   
    If[modone[dv]!={0,0,0}, Print["getRepMat: Warning, the difference between ",Rv," and ",
       Insert[Rv[[id]],trans[Rv[[id]]],2], " is not a lattice vector."]];
    m*Exp[-I*k.dv*2Pi]//Simplify
  ];
  re=If[StringQ[RvOrRvList[[1]]], forOneRv[RvOrRvList], forOneRv/@RvOrRvList];
  If[StringQ[RvOrRvList[[1]]]||oneRep, re, re\[Transpose]]
]

(* repinfo can be the output of getLGIrepTab or getLGCharTab *)
Options[getLGIrepMat]={"uNumeric"->False};
getLGIrepMat[repinfo_,OptionsPattern[]][RvOrRvList_]/;AssociationQ[repinfo]||VectorQ[repinfo,AssociationQ]:=
 getLGIrepMat[repinfo,All,"uNumeric"->OptionValue["uNumeric"]][RvOrRvList]
getLGIrepMat[repinfo_,IRidx_,OptionsPattern[]][RvOrRvList_]/;AssociationQ[repinfo]||VectorQ[repinfo,AssociationQ]:=
 Module[{info,k,kinfo,Gk,nsir,ndir,sirep,direp,usub={},idx,reps,re},
   info=If[ListQ[repinfo], repinfo[[1]], repinfo];
   kinfo=info["kinfo"];   Gk=info["Gkin"];
   If[Position[kinfo,Rule]=!={}, k=kinfo[[7]]+kinfo[[8]]; usub=kinfo[[9]], k=kinfo[[1]]];
   sirep=info["sirep"];   direp=info["direp"];
   If[Head[sirep]===Missing, sirep=info["scharTab"]; direp=info["dcharTab"]];
   nsir=Length[sirep];    ndir=Length[direp];
   If[IntegerQ[IRidx],idx={IRidx}];
   idx=Check[Range[nsir+ndir][[If[IntegerQ[IRidx], {IRidx}, IRidx]]],
             Print["getLGIrepMat: index of irep out of range [1,",nsir,"]\[Union][",nsir+1,
             ",",nsir+ndir,"]."]; Abort[]];
   reps=Join[sirep,direp][[idx]];
   If[IntegerQ[IRidx], reps=reps[[1]]];
   re=getRepMat[k,Gk,reps][RvOrRvList];
   If[OptionValue["uNumeric"],re=re/.usub];
   re
 ] 


(* ::Subsection:: *)
(*showLGIrepTab and showLGCharTab*)


Options[showRot]={"format"->"std", "fullbar"->False};
showRot[R_String,OptionsPattern[]]:=Module[{R0,hasbar,fmt,
  sub,pm,prime,end2,end1,rot,tmp,fullbar},
  fmt=OptionValue["format"];  fullbar=OptionValue["fullbar"];
  If[!MemberQ[{"simple","std","TeX"},fmt],
    Print["showSeitz: option \"format\" should be in ",InputForm/@{"simple","std","TeX"}]; Abort[];
  ];
  If[!MemberQ[{True,False},fullbar],
    Print["showSeitz: option \"fullbar\" should be True or False."]; Abort[];
  ];  
  hasbar=StringLength[R]>3&&StringTake[R,3]=="bar";
  R0=If[hasbar, StringTake[R,{4,-1}], R];
  If[fmt=="simple", Return[If[hasbar,OverBar[R0],R0]]];
  sub=pm=prime="";
  end2=StringTake["x"<>R0,-2];
  If[end2=="pp", prime=If[fmt=="std","\[DoublePrime]","''"]; R0=StringTake[R0,{1,-3}]; Goto["sup over"]];
  end1=StringTake[R0,-1];   
  Switch[end1, "p", prime=If[fmt=="std","\[Prime]","'"], "+"|"-",  pm=end1, _, Goto["sup over"]];
  R0=StringTake[R0,{1,-2}]; 
  Label["sup over"];
  If[StringLength[R0]>1, sub=StringTake[R0,{2,-1}]; R0=StringTake[R0,1]];
  Switch[fmt,
   "std", If[hasbar&&fullbar===False, R0=OverBar[R0]];
          rot=If[sub=="", R0, If[pm<>prime=="",Subscript[R0,sub],Subsuperscript[R0,sub,pm<>prime]]];
          If[hasbar&&fullbar===True, rot=OverBar[rot]],
   "TeX", R0=StringReplace[ToString@TeXForm[R0], "\\text"->""]; 
          If[hasbar&&fullbar===False, R0="\\overline{"<>R0<>"}"]; 
          rot=If[sub=="", R0, tmp=R0<>"_{"<>sub<>"}";If[pm=="", tmp, tmp<>"^"<>pm]]<>prime;
          If[hasbar&&fullbar===True, rot="\\overline{"<>rot<>"}"];
  ];
  rot
]


Options[showSeitz]={"format"->"std", "fullbar"->True};
showSeitz[{R_String,v_}, OptionsPattern[]]/;VectorQ[v]:=Module[{R0,hasbar,vout,fmt,
  sub,pm,prime,end2,end1,rot,tmp,fullbar},
  fmt=OptionValue["format"];  fullbar=OptionValue["fullbar"];
  If[!MemberQ[{"simple","std","TeX"},fmt],
    Print["showSeitz: option \"format\" should be in ",InputForm/@{"simple","std","TeX"}]; Abort[];
  ];
  If[!MemberQ[{True,False},fullbar],
    Print["showSeitz: option \"fullbar\" should be True or False."]; Abort[];
  ];  
  hasbar=StringLength[R]>3&&StringTake[R,3]=="bar";
  R0=If[hasbar, StringTake[R,{4,-1}], R];
  vout=Row[If[NumericQ[#]&&#<0,OverBar[-#],#]&/@v,""];
  If[fmt=="simple", Return[Row[{"{",If[hasbar,OverBar[R0],R0],"|",vout,"}"}]]];
  sub=pm=prime="";
  end2=StringTake["x"<>R0,-2];
  If[end2=="pp", prime=If[fmt=="std","\[DoublePrime]","''"]; R0=StringTake[R0,{1,-3}]; Goto["sup over"]];
  end1=StringTake[R0,-1];   
  Switch[end1, "p", prime=If[fmt=="std","\[Prime]","'"], "+"|"-",  pm=end1, _, Goto["sup over"]];
  R0=StringTake[R0,{1,-2}]; 
  Label["sup over"];
  If[StringLength[R0]>1, sub=StringTake[R0,{2,-1}]; R0=StringTake[R0,1]];
  Switch[fmt,
   "std", If[hasbar&&fullbar===False, R0=OverBar[R0]];
          rot=If[sub=="", R0, If[pm<>prime=="",Subscript[R0,sub],Subsuperscript[R0,sub,pm<>prime]]];
          vout=Row[(If[NumericQ[#],
               tmp=If[#<0,OverBar[-#],#];If[IntegerQ[#],tmp,Style[tmp,Small]],#])&/@v,""];
          If[hasbar&&fullbar===True, rot=OverBar[rot]]; 
          Row[{"{",rot,"|",vout,"}"}],
   "TeX", R0=StringReplace[ToString@TeXForm[R0], "\\text"->""]; 
          If[hasbar&&fullbar===False, R0="\\overline{"<>R0<>"}"]; 
          rot=If[sub=="", R0, tmp=R0<>"_{"<>sub<>"}";If[pm=="", tmp, tmp<>"^"<>pm]]<>prime;
          vout=StringJoin[(If[NumericQ[#],
               tmp=ToString@TeXForm[Abs[#]];If[#>=0,tmp,"\\bar{"<>tmp<>"}"], ToString[#]])&/@v];
          If[hasbar&&fullbar===True, rot="\\overline{"<>rot<>"}"];
          "$\\{"<>rot<>"|"<>vout<>"\\}$"
  ]
]

(* The default linewidth 2 is for display on screen. If export to pdf, a smaller linewith
   such as 0.4 should be used for looking good. *)
Options[showLGIrepTab]={"uNumeric"->False,"irep"->All,"elem"->All,"rotmat"->True,"trace"->False,
                        "spin"->"downup","abcOrBasVec"->None,"linewidth"->2};
showLGIrepTab[sgno_, kNameOrCoord_, OptionsPattern[]]:=Block[{u,irepTabs,showOneK,brav,
  showmat,idxsir,idxdir,idxelm,showrot,sx,saytwok},
  If[OptionValue["trace"]==False,
    irepTabs=getLGIrepTab[sgno, kNameOrCoord, "abcOrBasVec"->OptionValue["abcOrBasVec"]], 
    irepTabs=getLGCharTab[sgno, kNameOrCoord, "abcOrBasVec"->OptionValue["abcOrBasVec"]]  
  ];
  brav=getSGLatt[sgno];
  showmat[m_]:=If[MatrixQ[m],MatrixForm[m],m];
  showrot=OptionValue["rotmat"];
  sx={{0,1},{1,0}};

  showOneK[irepTab_]:=Module[{kname,kBZs,kinfo,Gkin,GkBC,dGkBC,slbl,sre,sirep,dlbl,dre,direp,
    rot,trans,srot,table,nelem,nsir,ndir,sfl,sfa,nc,nr,nfrom,head,h1,h2,h3,h4,h5,grid,sty1,sty2,
    thickHLines,tmp,nsir1,ndir1,bg1,bg2,bg3,bg4,bg5,bg6,nstart,symstd,symBC},
    kBZs=irepTab["kBZs"];      kinfo=irepTab["kinfo"];     kname=kinfo[[2]];
    Gkin=irepTab["Gkin"];        GkBC=irepTab["GkBC"];         dGkBC=irepTab["dGkBC"];
    slbl=irepTab["slabel"];    sre=irepTab["sreality"];    
    sirep=irepTab[If[OptionValue["trace"],"scharTab","sirep"]];
    dlbl=irepTab["dlabel"];    dre=irepTab["dreality"];    
    direp=irepTab[If[OptionValue["trace"],"dcharTab","direp"]];
    If[OptionValue["uNumeric"]&&VectorQ[kNameOrCoord]&&Position[kinfo,Rule]!={}, 
      sirep=sirep/.kinfo[[9]];  direp=direp/.kinfo[[9]]];
    nelem=Length[Gkin];   nsir=Length[sirep];   ndir=Length[direp];
    tmp=Check[Range[nsir+ndir][[If[IntegerQ[tmp=OptionValue["irep"]], {tmp}, tmp]]],
             Print["showSGIrepTab: index of irep out of range [1,",nsir+ndir,"]"]; Abort[]];
    idxsir=Select[tmp,#<=nsir&];    idxdir=Complement[tmp,idxsir];
    nsir1=Length[idxsir];    ndir1=Length[idxdir];
    idxelm=Check[Range[nelem][[If[IntegerQ[tmp=OptionValue["elem"]], {tmp}, tmp]]],
               Print["showLGIrepTab: index of element out of range [1,",nelem,"]"]; Abort[]];

    {symstd,symBC}=irepTab["symbol"][[{1,3}]];
    tmp=If[symBC=!=symstd,Row[{"","(","BC:",symBC,")"}," "], Nothing];    
    h1=Row[{"No.",sgno," ",symstd,tmp,":  k-point name is ",kname}," "];
    h2=If[VectorQ[kNameOrCoord], Row[{"Input: \!\(\*SubscriptBox[\(k\), \(in\)]\)=(",Row[kNameOrCoord,","],")"}], {}];
    If[h2=!={}&&Position[kinfo,Rule]!={}, h2=Row[{h2,"  (u=",kinfo[[9,2]],")"}]];
    h3={"BC standard:", 
        Row[Row[{"  \!\(\*SubscriptBox[\(k\), \(BC\)]\)=(",Row[#[[2]],","],") for ",Row[#[[3]],","]}]&/@kBZs,"\n"]};
    If[Length[kBZs]==1, h3=Row[h3]];
    If[kname!="GP"&&kname!="UN",
      h4=Row[{"{S|w}=",showSeitz[kinfo[[6]]],"("<>kinfo[[-1]]<>") : {S|w\!\(\*SuperscriptBox[\(}\), \(-1\)]\)\!\(\*SubscriptBox[\(G\), \(kin\)]\){S|w}=\!\(\*SubscriptBox[\(G\), \(kBC\)]\) or \!\(\*SubsuperscriptBox[\(G\), \(kBC\), \(d\)]\)"}]; 
      If[kinfo[[6,1]]=="E",h4={}],
      (*-----else------*)
      h4={};  h3={}];
    If[h4==={}, h5={},
      h5=Row[{"\!\(\*SubscriptBox[\(Sk\), \(BC\)]\)=[",kinfo[[6,1]],"](",Row[kinfo[[5]],","],")=(",Row[kinfo[[7]],","],") \[DoubleLeftRightArrow] \!\(\*SubscriptBox[\(k\), \(in\)]\)"}];
      ];
    head={h1,h2,h3,h4,h5}//Flatten//Column;
    
    rot=MatrixForm[getRotMat[brav,#]]&/@Gkin[[idxelm,1]];
    trans=MatrixForm[{InputForm/@#}\[Transpose]]&/@Gkin[[idxelm,2]];
    srot=getSpinRotOp[#][[1]]&/@Gkin[[idxelm,1]];
    If[OptionValue["spin"]=="updown", srot=sx.#.sx&/@srot];
    srot=MatrixForm[Expand[#]]&/@srot;
    sfl=SpanFromLeft;   sfa=SpanFromAbove;
    table={idxelm, showSeitz/@Gkin[[idxelm]], 
           Sequence@@If[showrot,{rot, srot},{}], 
           Sequence@@If[h4=!={}&&nsir1>0,{showSeitz/@GkBC[[idxelm]]},{}], 
           Sequence@@Map[showmat,sirep[[idxsir,idxelm]],{2}],
           Sequence@@If[h4=!={}&&ndir1>0,{showSeitz/@dGkBC[[idxelm]]},{}], 
           Sequence@@Map[showmat,direp[[idxdir-nsir,idxelm]],{2}]};
    {nr,nc}=Dimensions[table];   
    table=Join[Table[sfl,4,nr],table\[Transpose]]\[Transpose];
    table[[1,1]]="Index";   
    table[[2,1]]=If[VectorQ[kNameOrCoord]&&h4=!={},"Elem. in \!\(\*SubscriptBox[\(G\), \(kin\)]\)", "Element"];   
    If[showrot,
      table[[3,1]]=Column[{"Rotation","matrix"}];
      table[[4,1]]=Column[{If[OptionValue["spin"]=="updown","Spin(\[UpArrow]\[DownArrow])","Spin(\[DownArrow]\[UpArrow])"],"rotation","matrix"}]; 
      nstart=5,
      nstart=3
    ];   
    If[h4=!={}&&nsir1>0, table[[nstart,1]]="Elem. in \!\(\*SubscriptBox[\(G\), \(kBC\)]\)";nfrom=nstart+1,  nfrom=nstart];
    table[[nfrom;;nfrom+nsir1-1,1]]=idxsir;
    table[[nfrom;;nfrom+nsir1-1,2]]=slbl[[idxsir,2]];
    table[[nfrom;;nfrom+nsir1-1,3]]=slbl[[idxsir,1]];
    table[[nfrom;;nfrom+nsir1-1,4]]=sre[[idxsir]];  
    If[ndir1>0,
      nfrom=nfrom+nsir1; 
      table[[nfrom,1]]="Elem. in \!\(\*SubsuperscriptBox[\(G\), \(kBC\), \(d\)]\)";
      If[h4=!={},nfrom+=1];
      table[[nfrom;;nfrom+ndir1-1,1]]=idxdir;    
      table[[nfrom;;nfrom+ndir1-1,2]]=dlbl[[idxdir-nsir,2]];
      table[[nfrom;;nfrom+ndir1-1,3]]=dlbl[[idxdir-nsir,1]];
      table[[nfrom;;nfrom+ndir1-1,4]]=dre[[idxdir-nsir]]
    ];     
    
    sty1=Directive[Black,Thickness[OptionValue["linewidth"]]];   
    sty2=Directive[Thin,GrayLevel[0.8]];
    thickHLines=If[h4=!={}, {1,nstart,nstart+1,nstart+1+nsir1,nstart+nsir1+2,-1}, 
                             {1,nstart,nstart+nsir1,-1}];
    bg1={{1,nstart-1},{1,-1}}->Lighter[Yellow,0.9];
    bg2={{2,2},{1,-1}}->Lighter[Yellow,0.9];
    If[h4=!={}&&nsir1>0,
      bg3={{nstart,nstart},{1,-1}}->Lighter[Yellow,0.95];  nfrom=nstart+1,
      bg3={};  nfrom=nstart
    ];
    bg4={{nfrom,nfrom+nsir1-1},{1,-1}}->Lighter[Green,0.95];
    nfrom+=nsir1;  
    If[h4=!={}&&ndir1>0,
      bg5={{nfrom,nfrom},{1,-1}}->Lighter[Yellow,0.98]; nfrom+=1,
      bg5={}
    ];    
    bg6={{nfrom,-1},{1,-1}}->Lighter[Blue,0.95];
    grid=Grid[table,Frame->All,Alignment->{Center,Center},ItemSize->Full,
                    Dividers->{{{{sty2}},Join[#->sty1&/@{1,5,-1},#->sty2&/@{2,3,4}]}, 
                              {{{sty2}},Join[#->sty1&/@thickHLines, {2->sty2}]}},
                    Background->{None,None,{bg1,bg2,bg3,bg4,bg5,bg6}}
             ];
    Column[{head,grid}]
  ];
  
  If[Length[irepTabs]==2,
    saytwok=Row[{"Note: The input k can be identified as two BC standard k-points ",
      irepTabs[[1]]["kBZs"][[1,1]]," and ",irepTabs[[2]]["kBZs"][[1,1]],
      "\n      depending on the BZ types and actual values of lattice constants."}],
    saytwok=Nothing
    ];
  Column[Prepend[showOneK/@irepTabs,saytwok], Spacings->2]
]

Options[showLGCharTab]={"uNumeric"->False,"irep"->All,"elem"->All,"rotmat"->True,"spin"->"downup",
     "abcOrBasVec"->None, "linewidth"->2};
showLGCharTab[sgno_, kNameOrCoord_, OptionsPattern[]]:=showLGIrepTab[sgno,kNameOrCoord,
  Sequence@@(#->OptionValue[#]&/@{"uNumeric","irep","elem","rotmat","spin","abcOrBasVec","linewidth"}), "trace"->True]
  

LGIRtwokRelation[sgno_Integer,k_]:=LGIRtwokRelation[getLGCharTab[sgno,k]]
LGIRtwokRelation[repinfos_]/;AllTrue[repinfos,AssociationQ]:=Module[{reps1,reps2,key,lbs1,lbs2,
  sub1,sub2,idx,as1},
  key=If[MissingQ[repinfos[[1]]["sirep"]], "charTab", "irep"];
  lbs1=Join[repinfos[[1]]["slabel"],repinfos[[1]]["dlabel"]][[All,2]];
  If[Length[repinfos]==1, Return[Association[(#->#)&/@lbs1]]];
  lbs2=Join[repinfos[[2]]["slabel"],repinfos[[2]]["dlabel"]][[All,2]];
  sub1=repinfos[[1]]["kinfo"][[9]];    sub2=repinfos[[2]]["kinfo"][[9]];
  reps1=Join[repinfos[[1]]["s"<>key],repinfos[[1]]["d"<>key]]/.sub1;
  reps2=Join[repinfos[[2]]["s"<>key],repinfos[[2]]["d"<>key]]/.sub2;
  If[key=="irep",
    reps1=Map[If[MatrixQ[#],Tr[#],#]&, reps1, {2}]//Simplify;
    reps2=Map[If[MatrixQ[#],Tr[#],#]&, reps2, {2}]//Simplify;
  ];
  idx=Table[Position[Total@Abs[i-#]<1*^-5&/@reps2,True][[1,1]],{i,reps1}];
  as1=Association@Thread[lbs1->lbs2[[idx]]];
  If[repinfos[[1]]["kBZs"][[1,1]]!=repinfos[[2]]["kBZs"][[1,1]],
    Join[as1, Association@Thread[lbs2[[idx]]->lbs1]],  as1
  ]
]


(* ::Subsection:: *)
(*getSGIrepTab and showSGIrepTab*)


Options[getSGIrepTab]={"format"->True, "abcOrBasVec"->None};
(* Note that here the subscripts of t use special characters, ie. \: plus 2081, 2082, 2083. *)
getSGIrepTab[sgno_Integer, kNameOrCoord_, OptionsPattern[]]:=Block[{u,t\:2081,t\:2082,t\:2083,LGtabs,forOneK,tmp},
  LGtabs=getLGIrepTab[sgno,kNameOrCoord,"format"->OptionValue["format"], "abcOrBasVec"->OptionValue["abcOrBasVec"]];
  
  forOneK[LGtab_]:=Module[{SGtab,kinfo,kname,Gkin,GkBC,dGkBC,k1,Gk1,kstar,k1srep,k1drep,
    dv,factor,barfac,G,tmp,brav,cosetrep,getk1srep,getk1drep,h,sirep,direp,sLbl,dLbl,invcsr,
    dinvcsr,zeros,nir,nelem,dims,ncsr,cosetrot,ggj,i,j,ir,ig,invgi,Ettt,Efac,tm,inv,dtm,dinv,
    repEttt,sstar},
    brav=getSGLatt[sgno];
    kinfo=LGtab["kinfo"];   kname=kinfo[[2]];
    Gkin=LGtab["Gkin"];  GkBC=LGtab["GkBC"];   dGkBC=LGtab["dGkBC"];
    SGtab=<||>;    G=getLGElem[sgno,"\[CapitalGamma]"]; 
    sstar="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\), \(*\)]\)";
    sLbl={kname<>#[[1]],sstar<>#[[2]]}&/@LGtab["slabel"];
    dLbl={kname<>#[[1]],sstar<>#[[2]]}&/@LGtab["dlabel"];
    If[kname=="UN"||kname=="GP", k1=kinfo[[1]]; Gk1=Gkin,
      If[Last[kinfo]=="not in G", k1=kinfo[[7]]; Gk1=Gkin, k1=kinfo[[5]]; Gk1=modone[GkBC]]
    ];
    k1srep=LGtab["sirep"];  k1drep=LGtab["direp"];
    
    If[Last[kinfo]==="in G"&&kinfo[[6,1]]!="E",
      dv=(Gk1[[#,2]]-GkBC[[#,2]])&/@Range[Length[Gk1]];
      factor=Exp[-I*k1.#*2Pi]&/@dv//Simplify;
      barfac=If[StringLength[#[[1]]]>3&&StringTake[#[[1]],3]=="bar",-1,1]&/@dGkBC;
      k1srep=#*factor&/@k1srep;
      k1drep=#*factor*barfac&/@k1drep;
    ];
    
    getk1srep[{R_,v_}]:=Module[{dv,pos,fac,rep}, 
      pos=Position[Gk1[[All,1]],R];
      If[pos!={}, pos=pos[[1,1]], Print["getSGIrepTab: ",R," not in ",Gk1[[All,1]]]; Abort[]];
      dv=v-Gk1[[pos,2]];
      If[!VectorQ[dv,IntegerQ], Print["getSGIrepTab: ",dv, " is not an integer vector"]; Abort[]];
      fac=Exp[-I*k1.dv*2Pi]//Simplify;
      rep=k1srep[[All,pos]]*fac;
      If[OptionValue["format"]&&R!="E", rep=formatRepMat/@rep, rep=rep//Simplify];
      rep
    ];
    getk1drep[{R_,v_}]:=Module[{dv,pos,fac,hasbar,Rnobar,rep},
      hasbar=StringLength[R]>3&&StringTake[R,3]=="bar";
      Rnobar=R;   If[hasbar,Rnobar=StringTake[R,{4,-1}]];
      pos=Position[Gk1[[All,1]],Rnobar];
      If[pos!={}, pos=pos[[1,1]], Print["getSGIrepTab: ",Rnobar," not in ",Gk1[[All,1]]]; Abort[]];
      dv=v-Gk1[[pos,2]];
      If[!VectorQ[dv,IntegerQ], Print["getSGIrepTab: ",dv, " is not an integer vector"]; Abort[]];
      fac=If[hasbar,-1,1]*Exp[-I*k1.dv*2Pi]//Simplify;
      rep=k1drep[[All,pos]]*fac;
      If[OptionValue["format"]&&R!="E", rep=formatRepMat/@rep, rep=rep//Simplify];
      rep
    ];    
    
    tmp={#,getRotMatOfK[brav,#[[1]]].k1}&/@G;
    tmp=Gather[tmp,keqmod[#1[[2]],#2[[2]]]&];
    cosetrep=tmp[[All,1,1]];
    kstar=tmp[[All,1,2]];
    
    tm=SeitzTimes[brav];        inv=invSeitz[brav];
    dtm=DSGSeitzTimes[brav];    dinv=DSGinvSeitz[brav];
    Ettt={"E",{t\:2081,t\:2082,t\:2083}};
    Efac=Exp[-I*2Pi*k1.tm[tm[inv[#],Ettt],#][[2]]]&/@cosetrep//Simplify;
    
    invcsr=inv/@cosetrep;
    ggj=Table[tm[g,gj],{g,G}, {gj,cosetrep}];
    nir=Length[k1srep];    zeros=0*k1srep[[All,1]];
    nelem=Length[G];       ncsr=Length[cosetrep];
    sirep=Table[0, nir, nelem, ncsr, ncsr];   
    cosetrot=Table[tm[gi,#][[1]]&/@Gk1, {gi,cosetrep}];
    For[i=1,i<=ncsr,i++, invgi=invcsr[[i]];
      For[ig=1,ig<=nelem,ig++,
        For[j=1,j<=ncsr,j++,
          If[MemberQ[cosetrot[[i]],ggj[[ig,j,1]]],
            sirep[[All,ig,i,j]]=getk1srep[tm[invgi,ggj[[ig,j]]]],
            sirep[[All,ig,i,j]]=zeros
            ];
    ]]];  
    sirep=Map[ArrayFlatten,sirep,{2}];
    dims=(Length/@zeros)/.{0->1};
    repEttt=DiagonalMatrix@Flatten[Outer[Times,Efac,Table[1,#]]]&/@dims;
    sirep[[All,1]]=repEttt;
    sirep=Map[If[Dimensions[#]=={1,1},#[[1,1]],#]&, sirep, {2}];  
    
    (*
    sirep0=Table[h=SeitzTimes[brav][SeitzTimes[brav][igi,g],gj];
      If[MemberQ[Gk1[[All,1]],h[[1]]], getk1srep[h], 0*k1srep[[All,1]]][[ir]]
      ,{ir,Length[k1srep]}, {g,G}, {igi,invcsr}, {gj,cosetrep}]//Map[ArrayFlatten,#,{2}]&;       
    sirep0=Map[If[Dimensions[#]\[Equal]{1,1},#[[1,1]],#]&, sirep0, {2}];
    Print[direp\[Equal]direp0];
    *)

    dinvcsr=dinv/@cosetrep;      
    ggj=Table[dtm[g,gj],{g,G}, {gj,cosetrep}];
    nir=Length[k1drep];     zeros=0*k1drep[[All,1]];
    direp=Table[0, nir, nelem, ncsr, ncsr];   
    cosetrot=Table[(tmp=dtm[gi,#][[1]];StringReplace[tmp,"bar"->""])&/@Gk1, {gi,cosetrep}];
    For[i=1,i<=ncsr,i++, invgi=dinvcsr[[i]];
      For[ig=1,ig<=nelem,ig++,
        For[j=1,j<=ncsr,j++,
          If[MemberQ[cosetrot[[i]],StringReplace[ggj[[ig,j,1]],"bar"->""]],
            direp[[All,ig,i,j]]=getk1drep[dtm[invgi,ggj[[ig,j]]]],
            direp[[All,ig,i,j]]=zeros
            ];
    ]]];  
    direp=Map[ArrayFlatten,direp,{2}];
    dims=(Length/@zeros)/.{0->1};
    repEttt=DiagonalMatrix@Flatten[Outer[Times,Efac,Table[1,#]]]&/@dims;
    direp[[All,1]]=repEttt;
    direp=Map[If[Dimensions[#]=={1,1},#[[1,1]],#]&, direp, {2}];  
     
    (* 
    direp0=Table[h=DSGSeitzTimes[brav][DSGSeitzTimes[brav][igi,g],gj];
      tmp=h[[1]];
      If[StringLength[tmp]>3&&StringTake[tmp,3]\[Equal]"bar", tmp=StringTake[tmp,{4,-1}]];
      If[MemberQ[Gk1[[All,1]],tmp], getk1drep[h], 0*k1drep[[All,1]]][[ir]]
      ,{ir,Length[k1drep]}, {g,G}, {igi,dinvcsr}, {gj,cosetrep}]//Map[ArrayFlatten,#,{2}]&;     
    direp0=Map[If[Dimensions[#]\[Equal]{1,1},#[[1,1]],#]&, direp0, {2}];
    Print[direp\[Equal]direp0];
    *)
    
    SGtab["symbol"]=LGtab["symbol"];
    SGtab["kBZs"]=LGtab["kBZs"];      SGtab["kinfo"]=LGtab["kinfo"];     
    G[[1]]=Ettt;   SGtab["elements"]=G; 
    SGtab["k1"]=k1;    SGtab["Gk1"]=Gk1;
    SGtab["cosetrep"]=cosetrep;  SGtab["kstar"]=kstar; 
    SGtab["slabel"]=sLbl;   SGtab["sreality"]=LGtab["sreality"];
    SGtab["k1sirep"]=k1srep;    SGtab["sirep"]=sirep;   
    SGtab["dlabel"]=dLbl;   SGtab["dreality"]=LGtab["dreality"];
    SGtab["k1direp"]=k1drep;    SGtab["direp"]=direp;   
    SGtab
  ];
  
  (*\:5bf9\:4e8e\:83f1\:65b9\:683c\:5b50\:6765\:8bf4\:ff0caF\:548cbF\:603b\:662f\:5904\:4e8e\:540c\:4e00\:661f\:4e2d\:ff0c\:56e0\:800c\:5bf9\:4e8eSGIR\:4e0d\:5fc5\:52a0\:4ee5\:533a\:5206\:ff0c\:5426\:5219\:5f97\:5230\:7684\:4e24\:90e8\:5206\:76f8\:540c*)
  If[Length[LGtabs]>1&&getSGLatt[sgno]=="TrigPrim"&&LGtabs[[1]]["kBZs"][[1,1]]=="F",
    tmp=LGtabs[[1]];  
    tmp["kBZs"]={{"F",{0,1/2,-(1/2)},{"TrigPrim(a)"}},{"F",{1/2,1/2,0},{"TrigPrim(b)"}}};
    LGtabs={tmp};
  ];
  forOneK/@LGtabs
]


Options[showSGIrepTab]={"uNumeric"->False,"irep"->All,"elem"->All,"rotmat"->True,"trace"->False,
                        "spin"->"downup","maxDim"->4, "abcOrBasVec"->None, "linewidth"->2};
showSGIrepTab[sgno_, kNameOrCoord_, OptionsPattern[]]:=Block[{u,t\:2081,t\:2082,t\:2083,irepTabs,showOneK,brav,
  showmat,idxsir,idxdir,idxelm,showrot,sx,saytwok,dmax,tmp,rmRe0},
  If[OptionValue["trace"]==False,
    irepTabs=getSGIrepTab[sgno, kNameOrCoord, "abcOrBasVec"->OptionValue["abcOrBasVec"]], 
    irepTabs=getSGIrepTab[sgno, kNameOrCoord, "abcOrBasVec"->OptionValue["abcOrBasVec"], "format"->False]  
  ];
  brav=getSGLatt[sgno];
  dmax=OptionValue["maxDim"];
  rmRe0=Map[If[MachineNumberQ[#],If[Re[#]==0,Which[Im[#]==1,I,Im[#]==-1,-I,True,Im[#]"\[ImaginaryI]"],#],#]&, #, -1]&;
  showmat[m_]:=If[!MatrixQ[m], rmRe0[m], If[Length[m]<=dmax, MatrixForm[rmRe0[m]], 
      tmp=Row[{"("<>ToString[#[[1,1]]]<>","<>ToString[#[[1,2]]]<>")",#[[2]]},":"]&;
      Partition[tmp/@ArrayRules[rmRe0[m]][[;;-2]],UpTo[2]]//Grid[#,Alignment->Left,ItemSize->Full]&
    ]];
  showrot=OptionValue["rotmat"];   
  sx={{0,1},{1,0}};

  showOneK[irepTab_]:=Module[{kname,kBZs,kinfo,Gk1,k1,slbl,sre,sirep,dlbl,dre,direp,kstar,G,
    rot,trans,srot,table,nelem,nsir,ndir,sfl,sfa,nc,nr,nfrom,head,h1,h2,h3,h4,h5,grid,sty1,sty2,
    thickHLines,nsir1,ndir1,bg1,bg2,bg3,bg4,nstart,symstd,symBC},
    kBZs=irepTab["kBZs"];      kinfo=irepTab["kinfo"];     kname=kinfo[[2]];
    k1=irepTab["k1"];          Gk1=irepTab["Gk1"];         kstar=irepTab["kstar"];
    slbl=irepTab["slabel"];    sre=irepTab["sreality"];    sirep=irepTab["sirep"]//Chop;
    dlbl=irepTab["dlabel"];    dre=irepTab["dreality"];    direp=irepTab["direp"]//Chop;
    If[OptionValue["trace"],
      sirep=Map[FullSimplify@If[MatrixQ[#],Tr[#],#]&, sirep, {2}];
      direp=Map[FullSimplify@If[MatrixQ[#],Tr[#],#]&, direp, {2}];
    ];
    If[OptionValue["uNumeric"]&&VectorQ[kNameOrCoord]&&Position[kinfo,Rule]!={}, 
      sirep=sirep/.kinfo[[9]];  direp=direp/.kinfo[[9]]];
    G=irepTab["elements"];
    nelem=Length[G];   nsir=Length[sirep];   ndir=Length[direp];
    tmp=Check[Range[nsir+ndir][[If[IntegerQ[tmp=OptionValue["irep"]], {tmp}, tmp]]],
             Print["showSGIrepTab: index of irep out of range [1,",nsir+ndir,"]"]; Abort[]];
    idxsir=Select[tmp,#<=nsir&];    idxdir=Complement[tmp,idxsir];
    nsir1=Length[idxsir];    ndir1=Length[idxdir];
    idxelm=Check[Range[nelem][[If[IntegerQ[tmp=OptionValue["elem"]], {tmp}, tmp]]],
               Print["showLGIrepTab: index of element out of range [1,",nelem,"]"]; Abort[]];
    
    {symstd,symBC}=irepTab["symbol"][[{1,3}]];
    tmp=If[symBC=!=symstd,Row[{"","(","BC:",symBC,")"}," "], Nothing];    
    h1=Row[{"No.",sgno," ",symstd,tmp,":  k-point name is ",kname}," "];
    h2=If[VectorQ[kNameOrCoord], Row[{"Input: \!\(\*SubscriptBox[\(k\), \(in\)]\)=(",Row[kNameOrCoord,","],")"}], {}];
    If[h2=!={}&&Position[kinfo,Rule]!={}, h2=Row[{h2,"  (u=",kinfo[[9,2]],")"}]];
    h3={"BC standard:", 
        Row[Row[{"  \!\(\*SubscriptBox[\(k\), \(BC\)]\)=(",Row[#[[2]],","],") for ",Row[#[[3]],","]}]&/@kBZs,"\n"]};
    If[Length[kBZs]==1, h3=Row[h3]];
    If[kname=="GP"||kname=="UN", h3={}];
    h4="The k star:";
    If[kinfo[[-1]]==="not in G", 
      h4=Row[{h4," (with \!\(\*SubscriptBox[\(k\), \(1\)]\)=\!\(\*SubscriptBox[\(Sk\), \(BC\)]\) and {S|w}=",showSeitz[kinfo[[6]]]," not in G)"}]];
    h5=Row[Row[{Subscript[If[#==1," k","k"], #],"=(",Row[kstar[[#]],","],")"}]&/@Range@Length[kstar],";  "];
    head={h1,h2,h3,h4,h5}//Flatten//Column;
    
    rot=MatrixForm[getRotMat[brav,#]]&/@G[[idxelm,1]];
    trans=MatrixForm[{InputForm/@#}\[Transpose]]&/@G[[idxelm,2]];
    srot=getSpinRotOp[#][[1]]&/@G[[idxelm,1]];
    If[OptionValue["spin"]=="updown", srot=sx.#.sx&/@srot];
    srot=MatrixForm[Expand[#]]&/@srot;
    sfl=SpanFromLeft;   sfa=SpanFromAbove;
    table={idxelm, showSeitz/@G[[idxelm]], 
           Sequence@@If[showrot,{rot, srot},{}], 
           Sequence@@Map[showmat,sirep[[idxsir,idxelm]],{2}],
           Sequence@@Map[showmat,direp[[idxdir-nsir,idxelm]],{2}]};
    {nr,nc}=Dimensions[table];   
    table=Join[Table[sfl,4,nr],table\[Transpose]]\[Transpose];
    table[[1,1]]="Index";   
    table[[2,1]]="Element";   
    If[showrot,
      table[[3,1]]=Column[{"Rotation","matrix"}];
      table[[4,1]]=Column[{If[OptionValue["spin"]=="updown","Spin(\[UpArrow]\[DownArrow])","Spin(\[DownArrow]\[UpArrow])"],"rotation","matrix"}]; 
      nstart=5,
      nstart=3
    ];   
    nfrom=nstart;
    table[[nfrom;;nfrom+nsir1-1,1]]=idxsir;
    table[[nfrom;;nfrom+nsir1-1,2]]=slbl[[idxsir,2]];
    table[[nfrom;;nfrom+nsir1-1,3]]=slbl[[idxsir,1]];
    table[[nfrom;;nfrom+nsir1-1,4]]=sre[[idxsir]];  
    If[ndir1>0,
      nfrom=nfrom+nsir1;  
      table[[nfrom;;nfrom+ndir1-1,1]]=idxdir;    
      table[[nfrom;;nfrom+ndir1-1,2]]=dlbl[[idxdir-nsir,2]];
      table[[nfrom;;nfrom+ndir1-1,3]]=dlbl[[idxdir-nsir,1]];
      table[[nfrom;;nfrom+ndir1-1,4]]=dre[[idxdir-nsir]]
    ];     
    
    sty1=Directive[Black,Thickness[OptionValue["linewidth"]]];   
    sty2=Directive[Thin,GrayLevel[0.8]];
    thickHLines={1,nstart,nstart+nsir1,-1};
    bg1={{1,nstart-1},{1,-1}}->Lighter[Yellow,0.9];
    bg2={{2,2},{1,-1}}->Lighter[Yellow,0.9];
    nfrom=nstart;
    bg3={{nfrom,nfrom+nsir1-1},{1,-1}}->Lighter[Green,0.95];
    nfrom+=nsir1;  
    bg4={{nfrom,-1},{1,-1}}->Lighter[Blue,0.95];
    grid=Grid[table,Frame->All,Alignment->{Center,Center},ItemSize->Full,
                    Dividers->{{{{sty2}},Join[#->sty1&/@{1,5,-1},#->sty2&/@{2,3,4}]}, 
                              {{{sty2}},Join[#->sty1&/@thickHLines, {2->sty2}]}},
                    Background->{None,None,{bg1,bg2,bg3,bg4}}
             ];
    Column[{head,grid}]
  ];
  
  If[Length[irepTabs]==2,
    saytwok=Row[{"Note: The input k can be identified as two BC standard k-points ",
      irepTabs[[1]]["kBZs"][[1,1]]," and ",irepTabs[[2]]["kBZs"][[1,1]],
      "\n      depending on the BZ types and actual values of lattice constants."}],
    saytwok=Nothing
    ];
  Column[Prepend[showOneK/@irepTabs,saytwok], Spacings->2]
]


(* ::Subsection:: *)
(*getFullRepMat and getSGIrepMat*)


(* G is the list of elements of a (magnetic) space group, and rep is the corresponding representation
   matrices of each elements in G. rep can be one representation or a list of representations.*)
Options[getFullRepMat]={"trace"->False};
getFullRepMat[G_,rep_,OptionsPattern[]][RvOrRvList_]:=Module[{trans,ir,id,forOneRv,
  reps,oneRep,Ettt,re,times},
  If[Length[G[[1]]]==3, id={1,3}, id={1}];  (*compatible with magnetic space group*)
  trans=Association[#[[id]]->#[[2]]&/@G];
  oneRep=Mod[Length@Dimensions[rep],2]==1;   reps=If[oneRep, rep, rep\[Transpose]];
  ir=Association[Rule@@@Transpose[{G[[All,id]],reps}]];
  (trans[MapAt["bar"<>#&,#,1]]=trans[#])&/@G[[All,id]];
  (ir[MapAt["bar"<>#&,#,1]]=-ir[#])&/@G[[All,id]]; 
  Ettt[{v1_,v2_,v3_}]:=reps[[1]]/.{t\:2081->v1,t\:2082->v2,t\:2083->v3};
  times=If[MatrixQ[#1],#1.#2,#1*#2]&;
  forOneRv[Rv_]:=Module[{dv,m,m2,tr}, 
    m=ir[Rv[[id]]];
    If[Head[m]===Missing, Print["getFullRepMat: no rotation ",
       If[Length[id]==1,Rv[[1]],Rv[[id]]]," in the ",
       If[Length[id]==1,"","magnetic "], "space group."]; Abort[]];
    dv=Rv[[2]]-trans[Rv[[id]]];   
    If[(Rv[[1]]=="E"||Rv[[1]]=="barE")&&modone[Rv[[2]]]!={0,0,0}||modone[dv]!={0,0,0}, 
       Print["getFullRepMat: Warning, the difference between ",Rv," and ",
       Insert[Rv[[id]],trans[Rv[[id]]],2], " is not a lattice vector."]];
    m2=If[dv=={0,0,0}||Rv[[1]]=="E"||Rv[[1]]=="barE", m/.Thread[{t\:2081,t\:2082,t\:2083}->Rv[[2]]], 
         If[oneRep, times[Ettt[dv],m], times@@@Transpose[{Ettt[dv],m}]]]//Simplify;
    If[OptionValue["trace"]=!=True, m2,
      tr=If[MatrixQ[#],Tr[#],#]&; If[oneRep, tr[m2], tr/@m2]
    ]
  ];
  re=If[StringQ[RvOrRvList[[1]]], forOneRv[RvOrRvList], forOneRv/@RvOrRvList];
  If[StringQ[RvOrRvList[[1]]]||oneRep, re, re\[Transpose]]
]

(* repinfo can be the output of getSGIrepTab *)
Options[getSGIrepMat]={"uNumeric"->False,"trace"->False};
getSGIrepMat[repinfo_,OptionsPattern[]][RvOrRvList_]:=
 getSGIrepMat[repinfo,All,#->OptionValue[#]&/@{"uNumeric","trace"}][RvOrRvList]
getSGIrepMat[repinfo_,IRidx_,OptionsPattern[]][RvOrRvList_]:=
 Module[{info,G,kinfo,nsir,ndir,sirep,direp,usub={},idx,reps,re},
   info=If[ListQ[repinfo], repinfo[[1]], repinfo];
   G=info["elements"];  kinfo=info["kinfo"];
   If[Position[kinfo,Rule]=!={}, usub=kinfo[[9]]];
   sirep=info["sirep"];   direp=info["direp"];
   nsir=Length[sirep];    ndir=Length[direp];
   If[IntegerQ[IRidx],idx={IRidx}];
   idx=Check[Range[nsir+ndir][[If[IntegerQ[IRidx], {IRidx}, IRidx]]],
             Print["getSGIrepMat: index of irep out of range [1,",nsir,"]\[Union][",nsir+1,
             ",",nsir+ndir,"]."]; Abort[]];
   reps=Join[sirep,direp][[idx]];
   If[IntegerQ[IRidx], reps=reps[[1]]];
   re=getFullRepMat[G,reps,"trace"->OptionValue["trace"]][RvOrRvList];
   If[OptionValue["uNumeric"],re=re/.usub];
   re
 ] 


(* ::Subsection:: *)
(*checkSGIrep*)


(* Check whether the representation matrices in the result of getSGIrepTab satisfy correct
   multiplications for SG IR. Things are all right if all the returned numbers are zero.
   e.g.   rep=getSGIrepTab[222,"R"]; 
          checkSGIrep[rep]   or  checkSGIrep/@rep   or  checkSGIrep[rep[[1]]]    *)
checkSGIrep[repinfo_]:=Module[{G,mtab,sirep,direp, time, brav, fBZ, rot2elem,n,tmp,sub,
  itab,dvtab,rot2idx, repmtab, reptime, check, dtime,mtab2, bartab, empty},
  If[ListQ[repinfo], Return[checkSGIrep/@repinfo]];
  G=repinfo["elements"]/.Thread[{t\:2081,t\:2082,t\:2083}->{0,0,0}];   n=Length[G];
  (* If[Position[repinfo["kinfo"],u]!={}, sub=repinfo["kinfo"][[9]], sub={}]; *)
  sub=u->0.1;
  rot2elem=#[[1]]->#&/@G//Association;
  rot2idx=Flatten[{G[[#,1]]->#,"bar"<>G[[#,1]]->#}&/@Range[n]]//Association;
  sirep=repinfo["sirep"];  direp=repinfo["direp"];
  fBZ=repinfo["kBZs"][[1,3]];  If[ListQ[fBZ], fBZ=fBZ[[1]]];
  brav=If[StringTake[fBZ,-1]==")",StringTake[fBZ,1;;-4],fBZ];
  time=SeitzTimes[brav];
  mtab=Table[time[i,j],{i,G},{j,G}];
  itab=Table[rot2idx[mtab[[i,j,1]]],{i,n},{j,n}];  
  dvtab=Table[tmp=mtab[[i,j]];tmp=tmp[[2]]-rot2elem[tmp[[1]]][[2]],{i,n},{j,n}];
  reptime[m1_,m2_]:=If[MatrixQ[m1],m1.m2, m1*m2]//Simplify[#,u\[Element]Reals]&;
  repmtab[rep_]:=Table[reptime[rep[[i]],rep[[j]]],{i,n},{j,n}];

  dtime=DSGSeitzTimes[brav];
  mtab2=Table[dtime[i,j],{i,G},{j,G}];  
  bartab=Table[If[mtab2[[i,j,1]]==G[[itab[[i,j]],1]],1,-1],{i,n},{j,n}]; 

  check[rep_,d_]:=Module[{diff, tab, Ettt,rep1},
    Ettt=rep[[1]]/.Thread[{t\:2081,t\:2082,t\:2083}->#]&;
    rep1=rep;  rep1[[1]]=Ettt[{0,0,0}];
    tab=Table[reptime[Ettt[dvtab[[i,j]]],rep1[[itab[[i,j]]]]],{i,n},{j,n}];
    If[d=="d",tab=tab*bartab];
    diff=Flatten[(repmtab[rep1]-tab)/.sub];
    Total@Abs[diff//N//Chop//Simplify]
  ];
   
  empty=If[sirep=={}||direp=={}, 1, 0];
  {empty, check[#,"s"]&/@sirep, check[#,"d"]&/@direp}//Simplify
]


(* ::Subsection:: *)
(*Generate libLGIrep.mx file containing LG IR data for all named k*)


 (*Note that this function will take somewhat a long time to regenerate the libLGIrep.mx file. 
  By default, the file will be output to the file libLGIrep.mx under the directory Directory[]
  and this can be changed by designating a user-defined file. *)
generateLibLGIrep[]:=generateLibLGIrep["libLGIrep.mx"];
generateLibLGIrep[filename_String]:=Module[{sgno,brav,knames,kcoords,lib,kstars,fullsymSG,fullsymPG,fsPG,i,k,
  kco,kcos,nukcos,tmp,usub=u->1/10,kBZs,tryabc,ir,j,dictkco,dictk,dictLG,dictLabel,dictType,dictIrep},
  tryabc=<|"OrthBase(a)"->{a->3,b->2,c->4}, "OrthBase(b)"->{a->2,b->3,c->4},
      "OrthBody(a)"->{a->4,b->2,c->3}, "OrthBody(b)"->{a->3,b->4,c->2},
      "OrthBody(c)"->{a->2,b->3,c->4}, "OrthFace(a)"->{a->3,b->3.5`,c->4},
      "OrthFace(b)"->{a->3,b->4,c->1.8}, "OrthFace(c)"->{a->4,b->1.8,c->3},
      "OrthFace(d)"->{a->1.8,b->3,c->4}, "TetrBody(a)"->{a->3,c->2},
      "TetrBody(b)"->{a->2,c->4}, "TrigPrim(a)"->{a->5,c->2},
      "TrigPrim(b)"->{a->2,c->3}|>;
  fullsymSG=<|"Tric"->2,"Mono"->10,"Orth"->47,"Tetr"->123,"Trig"->166,"Hexa"->191,"Cubi"->221|>;
  fullsymPG[sg_]:=getSGElem[fullsymSG[StringTake[getSGLatt[sg],4]]][[All,1]];
  lib=<||>;   dictLG=dictLabel=dictType=dictIrep=<||>;
  ParallelNeeds["SpaceGroupIrep`"];
  SetSharedVariable[lib,tryabc,fullsymSG,usub];
  SetSharedFunction[fullsymPG];
  ParallelDo[
    brav=getSGLatt[sgno];
    knames=LGIrep[sgno]//Keys;   
    kcoords=kBCcoord[sgno,#][[1,1]]&/@knames;
    kBZs=kBCcoord[sgno,#][[1,2,1]]&/@knames;
    fsPG=fullsymPG[sgno];
    dictk=<||>;
    For[i=1,i<=Length[knames],i++, k=knames[[i]]; kco=kcoords[[i]];
      kcos=Gather[getRotMatOfK[brav,#].kco&/@fsPG,keqmod][[All,1]];
      nukcos=modone[kcos/.usub];
      dictkco=<||>;
      For[j=1,j<=Length[nukcos],j++,
        tmp=tryabc[kBZs[[i]]];  
        ir=First@getLGIrepTab[sgno,nukcos[[j]],"abcOrBasVec"->If[MissingQ[tmp],None,tmp]];
        tmp={ir["sirep"],ir["direp"]};
        dictkco[nukcos[[j]]]=<|"k"->kcos[[j]], "LG"->ir["Gkin"], 
            "label"->{ir["slabel"],ir["dlabel"]}, "reality"->{ir["sreality"],ir["dreality"]},
            "trace"->Map[If[MatrixQ[#],Tr[#],#]&,tmp,{3}]//Simplify,  "irep"->tmp|>;
      ];
      dictk[k]=dictkco;
    ];
   (*lib[sgno]=dictk;*)
   AppendTo[lib,sgno->dictk];
  ,{sgno,Range[230]}];  
  lib=KeySort[lib];
  Export[filename,lib];
]


(* ::Section:: *)
(*Calculate the direct product of two space group ireps*)


Options[SGIrepDirectProduct]={"abcOrBasVec"->None};
SGIrepDirectProduct[sgno_Integer, kin1_, kin2_, OptionsPattern[]]:=Block[{u,Ireps1,Ireps2,getchi,err,
  directProduct,brav,G,G0,times,inv,dtimes,dinv,rightCoset,doubleCosetRep,fixk3BZs,kcoord1,kcoord2,HSKpt,fixTrigF},
  brav=getSGLatt[sgno];
  HSKpt=Select[LGIrep[sgno], VectorQ[#[[2,1,2]]]&]//Keys;
  err[]:=Print["SGIrepDirectProduct: k-name is only supported for high-symmetry k-points, i.e. one of ",HSKpt,", not k-lines. "<>
            "Or use numeric coordinates for k-points."];
  If[!StringQ[kin1], kcoord1=kin1, If[MemberQ[HSKpt,kin1], kcoord1=kBCcoord[sgno,kin1][[1,1]], err[]; Abort[]] ];
  If[!StringQ[kin2], kcoord2=kin2, If[MemberQ[HSKpt,kin2], kcoord2=kBCcoord[sgno,kin2][[1,1]], err[]; Abort[]] ];
  If[!VectorQ[kcoord1,NumericQ]||!VectorQ[kcoord1,NumericQ],
    Print["SGIrepDirectProduct: The input k-point coordinates should be numeric."];
    Abort[];
  ];
  Ireps1=getLGCharTab[sgno,kcoord1,"abcOrBasVec"->OptionValue["abcOrBasVec"]];
  Ireps2=If[keqmod[kcoord1,kcoord2], Ireps1, getLGCharTab[sgno,kcoord2,"abcOrBasVec"->OptionValue["abcOrBasVec"]]];
  G=getLGElem[sgno,"\[CapitalGamma]"];        G0=G[[All,1]];
  times=SeitzTimes[brav];       inv=invSeitz[brav];
  dtimes=DSGSeitzTimes[brav];   dinv=DSGinvSeitz[brav];
  
  (* calculate the right cosets and associate them to each element of G *)
  rightCoset[subG_]:=Module[{elemleft,tmp,j},
    If[subG=={{"E",{0,0,0}}}, Return[Association[#->{#}&/@G]]];
    If[seteq[subG,G], Return[Association[#->G&/@G]]];
    elemleft=G;  j=0;  tmp=<||>;
    While[elemleft!={},
      tmp[++j]=modone[times[#,First[elemleft]]&/@subG];
      elemleft=Complement[elemleft,tmp[j]];
    ]; 
    Association[Join@@(Table[#[[j]]->#,{j,Length[subG]}]&/@Values[tmp])]
  ];
  
  (* Note that G2Lcst is a list of left cosets of G2 *)
  doubleCosetRep[G1_,G2Lcst_]:=Module[{G1Rcst,elemleft,j,tmp},
    If[G1=={{"E",{0,0,0}}}, Return[G2Lcst[[All,1]]]];
    If[seteq[G1,G], Return[{{"E",{0,0,0}}}]];
    If[Length[G2Lcst]==1, Return[{{"E",{0,0,0}}}]];
    G1Rcst=rightCoset[G1];
    If[Length[G2Lcst]==Length[G], Return[DeleteDuplicates[Values[G1Rcst]][[All,1]]]];
    elemleft=G2Lcst;   j=0;  tmp=<||>;
    While[elemleft!={},
      tmp[++j]=elemleft[[1,1]];
      elemleft=Select[elemleft,Intersection[#,G1Rcst[tmp[j]]]=={}&];
    ];
    Values[tmp]
  ];
  
  getchi[reptab_, Rv_]:=Module[{Gk,k,facbar,Rnobar,pos,dv,srep,drep,fac},
    facbar=If[StringLength[Rv[[1]]]>3&&StringTake[Rv[[1]],3]=="bar", -1, 1];
    Rnobar=StringReplace[Rv[[1]],"bar"->""];
    Gk=reptab["Gkin"];   k=reptab["kinfo"][[1]];
    pos=Position[Gk,Rnobar][[1,1]]; 
    dv=Rv[[2]]-Gk[[pos,2]];
    fac=Exp[-I*2Pi*k.dv]//Simplify;
    srep=fac*reptab["scharTab"][[All,pos]];
    drep=facbar*fac*reptab["dcharTab"][[All,pos]];
    Join[srep,drep]
  ]; 
  
  (*\:4fee\:6b63\:83f1\:65b9\:683c\:5b50\:7684F\:70b9\:ff0c\:4e24\:7ec4\:6570\:636ek\:70b9\:540d\:79f0\:4e00\:6837\:ff0c\:4e14\:5750\:6807\:5c5e\:4e8e\:540c\:4e00\:661f\:ff0c\:53ea\:4fdd\:7559\:4e00\:7ec4\:6570\:636e*) 
  fixTrigF[irep_]:=Module[{tmp},
    If[Length[irep]>1&&brav=="TrigPrim"&&irep[[1]]["kBZs"][[1,1]]=="F",
      tmp=irep[[1]];  tmp["kBZs"]={{"F",{0,1/2,-(1/2)},{"TrigPrim(a)","TrigPrim(b)"}}};
      Return[{tmp}];
    ];
    irep
  ];
  
  fixk3BZs[{kn1_,u1_},{kn2_,u2_},k3BZs_,k3BZs0_]:=Module[{kpair0,kpair,prec=1*^-6,k3BZ,i,nk3,tmp,
    kus,kusmax,ku2,selected},
    If[!MemberQ[{"OrthBase","OrthBody","OrthFace","TetrBody","TrigPrim"},brav], Return[k3BZs]];
    kpair0=Switch[brav,
       "OrthBase", <|"\[CapitalDelta]"->"F","F"->"\[CapitalDelta]","B"->"G","G"->"B", "A"->"E","E"->"A","\[CapitalSigma]"->"C","C"->"\[CapitalSigma]"|>,
       "OrthBody", <|"\[CapitalLambda]"->"G","G"->"\[CapitalLambda]","\[CapitalDelta]"->"U","U"->"\[CapitalDelta]","\[CapitalSigma]"->"F","F"->"\[CapitalSigma]"|>,
       "OrthFace", <|"G"->"H","H"->"G","C"->"A","A"->"C","D"->"B","B"->"D",
                     "\[CapitalLambda]"->"Q","Q"->"\[CapitalLambda]","\[CapitalDelta]"->"R","R"->"\[CapitalDelta]","\[CapitalSigma]"->"U","U"->"\[CapitalSigma]"|>,
       "TetrBody", <|"\[CapitalLambda]"->"V","V"->"\[CapitalLambda]","\[CapitalSigma]"->"F","F"->"\[CapitalSigma]","U"->"Y","Y"->"U"|>,
       "TrigPrim", <|"\[CapitalLambda]"->"P","P"->"\[CapitalLambda]","B"->"Y","Y"->"B","\[CapitalSigma]"->"Q","Q"->"\[CapitalSigma]"|> 
    ];
    kpair[k_]:=With[{kp=kpair0[k]}, If[MissingQ[kp],"-",kp]];
    If[kn2==kpair[kn1]&&u1+u2>=0.5-prec, Return[{}]];
    nk3=Length[k3BZs0];
    selected={};
    Do[
      tmp=k3BZs0[[#,k3BZ[[1,#]],{2,3}]]&/@Range[nk3];
      kus=Join[{{kn1,u1},{kn2,u2}},tmp];
      tmp=MaximalBy[#,Last]&/@GatherBy[kus,First]//Flatten[#,1]&;
      kusmax=Association[Rule@@@tmp]; 
      AppendTo[kusmax,"-"->-1];
      ku2=(tmp=kusmax[kpair[#]];If[MissingQ[tmp],-1,tmp])&/@Keys[kusmax];
      If[!Or@@(#>=0.5-prec&/@(Values[kusmax]+ku2)), AppendTo[selected,k3BZ]];
    ,{k3BZ,k3BZs}];
    selected
  ];
  
  directProduct[reptab1_,reptab2_]:=Module[{kstar1,kstar2,k1k2,ktest,startest,i,j,tmp,kinfo1,kinfo2,
    sub1,sub2,k3s,BZs1,BZs2,BZs,BZtypes,k3reps0,k3BZs,k3BZ,nk3,k3reps,k3subs,k3,Gk1,Gk2,Gk3,dalpha,
    bbeta,Gk1Lcst,Gk2Lcst,Gk3Rcst,da,bb,La,LaRcst,alphabeta,Nab,k1,k2,ir1ir2,trichi,ir1,ir2,
    tmp1,tmp2,k1csr,k2csr,sstar,DP,nsir1,nsir2,ndir1,ndir2,nir1,nir2,Lb1,Lb2,Lb3,kname1,kname2,kname3,
    DPs,strki,dims1,dims2,dims3,k3dict,k3BZs0,tmp0},

    kinfo1=reptab1["kinfo"];    kinfo2=reptab2["kinfo"];
    Gk1=reptab1["Gkin"];         Gk2=reptab2["Gkin"];
    k1=kcoord1;    k2=kcoord2;   kname1=kinfo1[[2]];    kname2=kinfo2[[2]];
    
    (* calculate the left cosets, coset representatives, and k star of Gk1 and Gk2*)    
    tmp=With[{R=getRotMatOfK[brav,#[[1]]]},{{#,R.k1},{#,R.k2}}]&/@G;
    tmp1=Gather[tmp[[All,1]],keqmod[#1[[2]],#2[[2]]]&];
    tmp2=Gather[tmp[[All,2]],keqmod[#1[[2]],#2[[2]]]&];
    Gk1Lcst=tmp1[[All,All,1]];    k1csr=Gk1Lcst[[All,1]];   kstar1=tmp1[[All,1,2]];
    Gk2Lcst=tmp2[[All,All,1]];    k2csr=Gk2Lcst[[All,1]];   kstar2=tmp2[[All,1,2]]; 
    
    sub1=If[Position[kinfo1,Rule]!={}, kinfo1[[9]], {}];
    sub2=If[Position[kinfo2,Rule]!={}, kinfo2[[9]], {}];
    BZs1=Join@@(reptab1["kBZs"][[All,3]]);
    BZs2=Join@@(reptab2["kBZs"][[All,3]]);
    BZs=Intersection[BZs1,BZs2];   
    If[BZs=={}, Return[Nothing]];
    BZtypes=If[StringTake[#,-1]==")",StringTake[#,{-2,-2}],""]&/@BZs;

    (* Print["kstar1=",kstar1];    Print["kstar2=",kstar2]; (*for debug*) *)
    k1k2=If[Length[kstar1]<Length[kstar2], k2+#&/@kstar1, k1+#&/@kstar2];
    i=0; k3s=<||>;  
    While[k1k2!={}, ktest=First[k1k2];
      startest=getRotMatOfK[brav,#].ktest&/@G0;
      k3s[++i]=Intersection[k1k2,startest,SameTest->keqmod]; 
      k1k2=Complement[k1k2,startest,SameTest->keqmod];
    ];
    k3s=Values[k3s][[All,1]]//Chop;      nk3=Length[k3s];
    k3s=modone[k3s+(0.5-1*^-14)]-(0.5-1*^-14);
    (* Print["k3s=",k3s];  (*for debug*) *)

    k3reps0=fixTrigF@getLGCharTab[sgno,#,"abcOrBasVec"->OptionValue["abcOrBasVec"]]&/@k3s;
    k3BZs=<||>;   k3BZs0=<||>;
    For[i=1,i<=nk3,i++,
      k3BZs0[i]={Join@@(#["kBZs"][[All,3]]),  #["kinfo"][[2]], 
                  If[Position[#["kinfo"],Rule]!={}, #["kinfo"][[9,2]], -1]}&/@k3reps0[[i]]; 
      k3BZs[i]=Position[k3BZs0[i],#][[All,1]]&/@BZs;
    ];   
    k3BZs={BZs,Values[k3BZs]\[Transpose]}\[Transpose];    k3BZs0=Values[k3BZs0];
    k3BZs={#[[1]], tmp=Outer[List,Sequence@@#[[2]]];Flatten[tmp,Length@Dimensions[tmp]-2]}&/@k3BZs;
    k3BZs=Flatten[Table[{#[[1]],tmp},{tmp,#[[2]]}]&/@k3BZs, 1];
    k3BZs={#[[1,2]],#[[All,1]]}&/@GatherBy[k3BZs,Last];  
    tmp={#["kinfo"][[2]],If[Position[#["kinfo"],Rule]!={}, #["kinfo"][[9,2]], -1]}&/@{reptab1,reptab2};
    k3BZs=fixk3BZs[Sequence@@tmp,k3BZs,k3BZs0];

    sstar="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\), \(*\)]\)";
    nsir1=Length[reptab1["scharTab"]];    ndir1=Length[reptab1["dcharTab"]];
    nsir2=Length[reptab2["scharTab"]];    ndir2=Length[reptab2["dcharTab"]];
    nir1=nsir1+ndir1;    nir2=nsir2+ndir2;
    ir1ir2=Flatten[Table[{ir1,ir2},{ir1,nir1},{ir2,nir2}],1];
    If[Or@@(keqmod[k1,#]&/@kstar2), ir1ir2=Select[ir1ir2,#[[1]]<=#[[2]]&]]; (*in the same star*)
    Lb1=Join[reptab1["slabel"],reptab1["dlabel"]];
    Lb2=Join[reptab2["slabel"],reptab2["dlabel"]];
    dims1=Join[reptab1["scharTab"][[All,1]],reptab1["dcharTab"][[All,1]]]*Length[kstar1];
    dims2=Join[reptab2["scharTab"][[All,1]],reptab2["dcharTab"][[All,1]]]*Length[kstar2];
    dims1=Rationalize[dims1];   dims2=Rationalize[dims2];
    DPs=<||>;
    Do[
      k3reps=k3reps0[[#,k3BZ[[1,#]]]]&/@Range[nk3];
      k3subs=If[Position[#["kinfo"],Rule]!={}, #["kinfo"][[9]], {}]&/@k3reps;
      (* Print[#["kinfo"]&/@k3reps//Column]; (*for debug*) *)
      DP=<||>;   (DP[#]=<|"label"->"","dim"->{},"irep"-><||>|>)&/@ir1ir2;  
      k3dict=<||>;
      For[i=1,i<=nk3,i++, k3=k3s[[i]];
        Gk3=k3reps[[i]]["Gkin"];
        (* Print["i=",i,", k3=",k3]; 
           Print["Gk1=",Gk1]; Print["Gk2=",Gk2]; Print["Gk3=",Gk3]; (*for debug*) *)
        (* calculate the double coset representatives of Gk3 and Gk2: dalpha *)
        dalpha=doubleCosetRep[Gk3,Gk2Lcst];
        (* Print["dalpha=",dalpha];   (*for debug*) *)
        alphabeta={};
        Do[ (*for each elem in dalpha calculate bbeta, the double coset representatives of La and Gk1*)
          La=Intersection[Gk3,modone[times[times[da,#],inv[da]]&/@Gk2]];
          (* Print["La=",La];  (*for debug*) *)  
          bbeta=doubleCosetRep[La,Gk1Lcst];
          (* Print["bbeta=",bbeta];  (*for debug*) *)
          Do[ (* find alpha and beta satisfying beta.k1+alpha.k2 \[DoubleLeftRightArrow] k3 *)
            If[keqmod[getRotMatOfK[brav,bb[[1]]].k1+getRotMatOfK[brav,da[[1]]].k2,k3],
              Nab=Intersection[La,modone[times[times[bb,#],inv[bb]]&/@Gk1]];
              AppendTo[alphabeta,{da,bb,Nab}];
            ];
          ,{bb,bbeta}];                    
        ,{da,dalpha}];
        (* Print["alpha,beta,Nab=",alphabeta]; (*for debug*) *)
 
        (* Here, alpha, beta, and Nab are found. Then use the equation 4.7.29 in BC book. *)
        dims3=Join[k3reps[[i]]["scharTab"][[All,1]],k3reps[[i]]["dcharTab"][[All,1]]]*Length[G]/Length[Gk3];
        dims3=Rationalize[dims3];
        trichi=Table[{da,bb,Nab}=tmp;
          {getchi[reptab1,dtimes[dtimes[dinv[bb],#],bb]]/.sub1, 
           getchi[reptab2,dtimes[dtimes[dinv[da],#],da]]/.sub2,
           getchi[k3reps[[i]],#]\[Conjugate]/.k3subs[[i]]}&/@Join[Nab,{"bar"<>#1,#2}&@@@Nab]
           ,{tmp,alphabeta}]//Chop;          
        kname3=k3reps[[i]]["kinfo"][[2]];
        Lb3=Join[k3reps[[i]]["slabel"],k3reps[[i]]["dlabel"]];
        Lb3={sstar<>#2,kname3<>#1}&@@@Lb3;
        strki=strSub["k",ToString[i]];
        
        tmp1=If[k3reps[[i]]["kinfo"][[-1]]==="not in G",1,0];
        tmp2=Length[G]/Length[k3reps[[i]]["Gkin"]];
        k3dict[strki]={{kname3,tmp1,tmp2},k3};
        
        For[j=1,j<=Length[ir1ir2],j++, {ir1,ir2}=ir1ir2[[j]];
          DP[{ir1,ir2},"label"]={{sstar<>Lb1[[ir1,2]],sstar<>Lb2[[ir2,2]]}, 
                                 {kname1<>Lb1[[ir1,1]],kname2<>Lb2[[ir2,1]]}}; 
          DP[{ir1,ir2},"dim"]={dims1[[ir1]],dims2[[ir2]]};
          tmp=Total[Mean[#[[All,1,ir1]]*#[[All,2,ir2]]*#[[All,3]]]&/@trichi]//Rationalize[#,0.1]&;
          tmp1=Switch[#1,0,Nothing,1,#2,_,ToString[#1]<>#2]&@@@Transpose[{tmp,Lb3[[All,1]]}];
          tmp2=Switch[#1,0,Nothing,1,#2,_,ToString[#1]<>#2]&@@@Transpose[{tmp,Lb3[[All,2]]}];
          tmp0=If[#1==0,Nothing,{#1,#2}]&@@@Transpose[{tmp,Range[Length[tmp]]}]; (*tmp0\:4e2d\:6bcf\:4e00\:5bf9\:6570\:524d\:4e00\:4e2a\:662f\:91cd\:6570\:ff0c\:540e\:4e00\:4e2a\:662f\:8868\:793a\:7684\:5e8f\:53f7*)
          (* tmp1=StringRiffle[tmp1,"+"];   tmp2=StringRiffle[tmp2,"+"]; *)  
          tmp=dims3[[Select[Range[Length[tmp]],tmp[[#]]!=0&]]];       
          DP[{ir1,ir2},"irep",strki]={tmp0,tmp1,tmp2,tmp};
        ];
	 ]; (* end For of i for k3s *)
	tmp={{kname1, If[kinfo1[[-1]]==="not in G",1,0], Length[kstar1]},
	     {kname2, If[kinfo2[[-1]]==="not in G",1,0], Length[kstar2]}};
	DPs[k3BZ[[1]]]={k3BZ[[2]],tmp,{{nsir1,ndir1},{nsir2,ndir2}},k3dict,DP};
    ,{k3BZ,k3BZs}];  
    Values[DPs]
  ];
 
  Ireps1=fixTrigF[Ireps1];   Ireps2=fixTrigF[Ireps2];
  Flatten[Table[directProduct[k1rep,k2rep],{k1rep,Ireps1},{k2rep,Ireps2}],2]
]

Options[showSGIrepDirectProduct]={"label"->1,"abcOrBasVec"->None,"linewidth"->2};
showSGIrepDirectProduct[sgno_Integer, kin1_, kin2_, OptionsPattern[]]:=Module[
  {DPs,forOneBZ,showk,bold,lbtype,kcoord1,kcoord2},
  DPs=SGIrepDirectProduct[sgno,kin1,kin2,"abcOrBasVec"->OptionValue["abcOrBasVec"]];
  If[StringQ[kin1], kcoord1=kBCcoord[sgno,kin1][[1,1]], kcoord1=kin1];
  If[StringQ[kin2], kcoord2=kBCcoord[sgno,kin2][[1,1]], kcoord2=kin2];
  showk[k_]:="("<>StringRiffle[ToString[Chop@Round[#,1.*^-5]]&/@k,","]<>")";
  bold=Style[#,Bold]&;
  
  lbtype=OptionValue["label"];
  If[lbtype!=1&&lbtype!=2,
    Print["showSGIrepDirectProduct: Value of the option \"label\" can either be 1 or 2.\n",
          "   1 for \[CapitalGamma] label, 2 for Mulliken-like label."];
    Abort[];
  ];
  
  forOneBZ[DPdat_]:=Module[{BZ,ktype,nsir1,ndir1,nsir2,ndir2,DP,k3dict,h0,h1,h2,kname1,kname2,kt1,kt2,
    skp,sstar,dag,h3,h4,k3label,tmp,narm1,narm2,head,ir1ir2,table,key1,key2,key3,n1,n2,n3,
    show1,show2,sty1,sty2,bg0,bg1,bg2,bg3,tab,maxNitem},
    {BZ,ktype,{{nsir1,ndir1},{nsir2,ndir2}},k3dict,DP}=DPdat;
    {{kname1,kt1,narm1},{kname2,kt2,narm2}}=ktype;

    skp="\!\(\*SuperscriptBox[\(k\), \(\[Prime]\)]\)";
    sstar=\!\(\*
TagBox[
StyleBox["\"\<\\!\\(\\*SuperscriptBox[\\(\\), \\(*\\)]\\)\>\"",
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\);
    dag="\!\(\*SuperscriptBox[\(\), \(\[Dagger]\)]\)"; 
    h0=Row[{"For Brillouin zone type", If[Length[BZ]>1,"s: ",": "], bold[Row[BZ,","]]}];
    h1=Row[{"Direct products of ireps of space group ",bold[SGSymStd[sgno]]," (No. ",bold[sgno],"):"}];
    h2=Row[{sstar<>"k \[CircleTimes] "<>sstar<>skp<>":  k=",showk[kcoord1],"[",kname1,If[kt1==1,dag,""],",",narm1,
           "],  "<>skp<>"=",showk[kcoord2],"[",kname2,If[kt2==1,dag,""],",",narm2,"]"}];
    If[Length[DP]<(nsir1+ndir1)(nsir2+ndir2),   h2=Row[{h2," (in the same star)"}]];
    k3label=Keys[k3dict];
    h3="The results contain stars "<>sstar<>First[k3label]<>"~"<>sstar<>Last[k3label]<>":";
    tmp=StringJoin[{#, "=", showk[k3dict[#][[2]]], "[", k3dict[#][[1,1]], 
                    If[k3dict[#][[1,2]]==1,dag,""],",",ToString[k3dict[#][[1,3]]], "]"}]&/@k3label;
    h4=TableForm[Partition[tmp,UpTo[3]]];
    head=Column[{h1,h0,h2,"",h3,h4,""}];  
    ir1ir2=Keys[DP];
        
    table=Table["", Length[ir1ir2]+1, 4];
    table[[1]]={"Direct Product",SpanFromLeft,SpanFromLeft,"Results"};
    key1=Select[ir1ir2, #[[1]]<=nsir1&&#[[2]]<=nsir2&];
    key2=Select[ir1ir2, #[[1]]<=nsir1&&#[[2]]>nsir2||#[[1]]>nsir1&&#[[2]]<=nsir2&];
    key3=Select[ir1ir2, #[[1]]>nsir1&&#[[2]]>nsir2&];
    {n1,n2,n3}=Length/@{key1,key2,key3};
    
    maxNitem=Max[Length/@(Join@@@(Values[#["irep"]][[All,1]]&/@Values[DP]))];
    
    show1[key_]:=Module[{dp,L1,L2,d1,d2}, dp=DP[key];
      {L1,L2}=dp["label"][[lbtype]];   {d1,d2}=ToString/@dp["dim"];
      {L1<>"(k,"<>d1<>")", "\[CircleTimes]", L2<>"("<>skp<>","<>d2<>")"}
    ];
    
    show2[key_]:=Module[{rep,kis,reps,g},
      rep=DP[key,"irep"];      kis=Keys[rep];
      reps=Table[Row[{#1,Style[Row[{"(",ki,",",#2,")"}],Gray]}]&@@@Transpose[rep[ki][[{lbtype+1,4}]]], {ki,kis}];
      reps=Flatten[reps];
      (*
      If[maxNitem<=6, Return[Row[reps,"+"]]];
      reps=Partition[reps,UpTo[5]];
      If[Length[reps]\[Equal]1, Return[Row[reps[[1]],"+"]]];
      Append[Row[{Row[#,"+"],"+"}]&/@reps[[;;-2]], Row[reps[[-1]],"+"]]//Column
      *)
      If[maxNitem<=6, g={Riffle[reps,"+"]},
        reps=Partition[reps,UpTo[5]];
        If[Length[reps]==1, g={Riffle[reps[[1]],"+"]},
          g=Riffle[#,"+",{2,-1,2}]&/@reps[[;;-2]];
          AppendTo[g,Riffle[reps[[-1]],"+"]]
        ]
      ];
      Grid[g, Spacings->{0.2, 0.2}]
    ];
    
    table[[2;;1+n1,1;;3]]=show1/@key1;     
    table[[2;;1+n1,4]]=show2/@key1;
    table[[2+n1;;1+n1+n2,1;;3]]=show1/@key2;     
    table[[2+n1;;1+n1+n2,4]]=show2/@key2;
    table[[2+n1+n2;;-1,1;;3]]=show1/@key3;     
    table[[2+n1+n2;;-1,4]]=show2/@key3;
    
    sty1=Directive[Black,Thickness[OptionValue["linewidth"]]];   
    sty2=Directive[Thin,GrayLevel[0.8]];
    bg0={{1,2},{1,-1}}->Lighter[Gray,0.9];
    bg1={{2,1+n1},{1,-1}}->Lighter[Green,0.95];
    bg2={{2+n1,1+n1+n2},{1,-1}}->Lighter[Yellow,0.95];
    bg3={{2+n1+n2,-1},{1,-1}}->Lighter[Blue,0.95];
    tab=Grid[table,Alignment->{Left,Center,{{1,1}->Center,{1,4}->Center}},
               Dividers->{{{None},{1->sty1,4->sty1,-1->sty1}},
                         {{{sty2}},#->sty1&/@{1,2,2+n1,2+n1+n2,-1}}},
               Background->{None,None,{bg0,bg1,bg2,bg3}},
               ItemSize->{{{},{4->Full}},{}}, Spacings->{{}, 0.75}
     ];
    Column[{head,tab}]    
  ];
  
  Column[forOneBZ/@DPs, Spacings->2]
]


(* ::Section:: *)
(*Read the trace.txt file generated by vasp2trace and determine the representations.*)


readVasp2trace[filename_String]:=Module[{dat,nelec,soc,nsym,i,j,rot,trans,srot,nk,kpt,nband,
  trace,deg,ene,knsym,kisym,ik,ib},
  
  If[!FileExistsQ[filename],Print["readVasp2trace: Cannot read file ",filename,"!"]; Abort[]];
  dat=Import[filename,"Table"];  
  {nelec,soc,nsym}=dat[[1;;3]]\[Transpose][[1]];
  
  rot=trans=srot=Table[0,{nsym}];
  For[j=4,j<=3+nsym,j++,i=j-3;
    rot[[i]]=Partition[dat[[j,1;;9]],3];
    trans[[i]]=dat[[j,10;;12]];
    srot[[i]]=Partition[(#[[1]]+I #[[2]])&/@Partition[dat[[j,13;;20]],2],2];
  ];
  
  nk=dat[[4+nsym]]//First;        (* the number of k points *)
  kpt=dat[[5+nsym;;4+nsym+nk]];   (* kpoints *) 
  nband=Module[{},i=7+nsym+nk;    (* the number of bands *)
    While[i<=Length[dat]&&Length[dat[[i]]]>1, i++];
    i--;  dat[[i,1]]+dat[[i,2]]-1 
   ];
  
  (*ene[[ik,ib]] : the energy of the k-point ik and band ib
    deg[[ik,ib]] : the degeneracy of the state
    trace[[ik,ib]] : character of the little group for the states
    knsym[[ik]] : the order of the little co-group of k-point ik
    kisym[[ik]]: the indexes of the operations in the little co-group   *)
  trace=deg=ene=Table[0,{nk},{nband}]; 
  knsym=kisym=Table[0,{nk}];
  j=5+nsym+nk;
  For[ik=1,ik<=nk,ik++,
    knsym[[ik]]=dat[[j++,1]];
    kisym[[ik]]=dat[[j++]];
    For[ib=1,ib<=nband,ib++,
      deg[[ik,ib]]=dat[[j,2]];
      ene[[ik,ib]]=dat[[j,3]];
      trace[[ik,ib]]=(#[[1]]+I #[[2]])&/@Partition[dat[[j,4;;]],2];
      If[ib>=dat[[j,1]]+deg[[ik,ib]]-1,j++];
    ];
  ];

  trace=Chop[trace,1*^-5];
  <|"nelec"->nelec, "soc"->soc, "nsym"->nsym, "rot"->rot, "trans"->trans, "srot"->srot,
    "nk"->nk, "kpt"->kpt, "nband"->nband, "ene"->ene, "deg"->deg, "knsym"->knsym, "kisym"->kisym,
    "trace"->trace|>
]

Options[getBandRep]={"CompressDegeneracy"->True, "showdim"->True};
(* Note that the parameter traceData here corresponds to primitive cell of BC setting. *)
getBandRep[sgno_Integer,BZtypeOrBasVec_,traceData_, ikOrListOrSpan_, ibOrListOrSpan_, OptionsPattern[]]/;
 And@@(Position[#,Rule]=={}&&(IntegerQ[#]||ListQ[#]||Head[#]==Span||#==All)&/@{ikOrListOrSpan,ibOrListOrSpan}):=
 Module[{bandRepOneK,kinfoList,bv,BZtype="",brav,rotName,iks,iik,ibs0,iball,kpathstr,re,dsg,sx},
  iks=Check[Range[traceData["nk"]][[ikOrListOrSpan]],
        Print["getBandRep: ik ",ikOrListOrSpan," out of range [1,",traceData["nk"],"]."]; Abort[],
        {Part::partw,Part::pkspec1,Part::take}];
  If[IntegerQ[ikOrListOrSpan], iks={iks}];
  iball=Range[traceData["nband"]];
  ibs0=Check[iball[[ibOrListOrSpan]],
        Print["getBandRep: ib ",ibOrListOrSpan," out of range [1,",traceData["nband"],"]."]; Abort[],
        {Part::partw,Part::pkspec1,Part::take}];
  If[IntegerQ[ibOrListOrSpan], ibs0={ibs0}]; 
  iik=Association[Rule@@@Transpose[{iks,Range[Length[iks]]}]]; 
  kinfoList=identifyBCHSKptBySG[sgno,BZtypeOrBasVec,traceData["kpt"][[iks]]];
  brav=getSGLatt[sgno];
  If[brav=="TrigPrim",
    If[MatrixQ[BZtypeOrBasVec], 
       bv=BZtypeOrBasVec; a=bv[[1,2]]//Abs; c=bv[[1,3]]; BZtype=If[a>Sqrt[2]c,"a","b"],
       BZtype=BZtypeOrBasVec]
    ];
  dsg=If[traceData["soc"]==1, True, False];
  rotName=If[!dsg, 
    getRotName[brav,#]&/@traceData["rot"],
    (*-------else: dsg--------*)
    (*Note that the bases of spin rotation matrices of BC are {down,up}, different from the
      usual {up,down} we used. So, a transformation by sx is needed. *)
    sx={{0,1},{1,0}};
    getSpinRotName[brav,{sx.#1.sx,Det[#2]}]&@@@Transpose[{traceData["srot"],traceData["rot"]}]
  ];
          
  kpathstr=StringRiffle[If[#1[[2]]!=""||#1[[1]]=="UN"||#1[[1]]=="GP", #1[[1]],
                          "["<>#1[[1]]<>"("<>ToString[#2]<>")]"]&
             @@@Transpose[{kinfoList[[All,{2,3}]],iks}],"-"];
  
  (* Format of kinfo is like:
  {{-0.2,0.6,0.1},"GP","","C1"}
  {{-0.2,0.6,0.1},"UN","",{"E","\[Sigma]z"}}
  {{0,0.1,0.5},"Z","XM","C2v",{u,0.5,0},{"C31+",{0,0,0}},{0,u,0.5},{0,0,0},u\[Rule]0.1`,0.5,"in G"} *)
  bandRepOneK[ik_]:=Module[{kinfo,k,ktip,kname,kname2,S,w,AGno,gens,avRep,reptype,LG,HLG,
    Gk,Gk1,Gk2,idx,idxcls,kisym,times,inv,chars,factor,dt,kBD,CE,g,chars2,repR,nc,ib,icEle,
    tmp,tmp1,deg,ibs2,rep,i,iR,repLabel,abc,as,rs,iball2,deg2,ibidx,ibs,facbar,Gk1rots},
    kinfo=kinfoList[[iik[ik]]];
    {k,kname,ktip}=kinfo[[1;;3]];  
    kisym=traceData["kisym"][[ik]];
    Gk={rotName[[kisym]],traceData["trans"][[kisym]]}\[Transpose];
    deg=traceData["deg"][[ik]];
    ibs=ibs0;
    If[OptionValue["CompressDegeneracy"],
      {ibs2,deg2}=Transpose@
           ((Reap@For[i=1,i<=Last[iball],i++,Sow[{i,deg[[i]]}];i+=deg[[i]]-1;])[[2,1]]);
      iball2=Range[#1,#1+#2-1]&@@@Transpose[{ibs2,deg2}];   
      ibidx=DeleteDuplicates[Position[iball2,#][[1,1]]&/@ibs0]; 
      ibs=ibs2[[ibidx]];
    ];
    
    If[kname=="GP"||kname=="UN",
      {avRep,reptype,AGno,CE}=calcRep[sgno,kinfo,"DSG"->dsg][[3;;6]]; 
      avRep=Association[Rule@@@avRep];
      g=Max[CE[[All,2]]]+1;
      If[!dsg, 
        idx=Position[Gk[[All,1]],#[[1]]][[1,1]]&/@CE;
        facbar=Table[1,Length[CE]],
        (*-------else: dsg-------*)
        tmp=Join[Gk[[All,1]],SpinRotTimes[brav]["barE",#]&/@Gk[[All,1]]];
        idx=Position[tmp,#[[1]]][[1,1]]&/@CE;
        facbar=If[#>Length[Gk],-1,1]&/@idx;
        idx=With[{n=Length[Gk]},If[#>n,#-n,#]]&/@idx;
      ];
      (* idx makes|Gk2|\[Equal]|CE| and all rotations are in the same order, i.e. Gk2[[All,1]]\[Equal]CE[[All,1]]. *)
      Gk2=Gk[[idx]];
      factor=With[{v=Gk2[[#,2]],aa=CE[[#,2]]},Exp[I*(k.v+aa/g)*2Pi]]&/@Range[Length[CE]]//Chop;
      factor=factor*facbar;
      Goto["determine reps"];
    ];

    {S,w}=kinfo[[6]];  
    kname2=If[brav=="TrigPrim"&&kname=="F", BZtype<>kname, kname];
    {AGno,gens,avRep,reptype}=If[!dsg, LGIrep[sgno], DLGIrep[sgno]][kname2];
    avRep=Association[Rule@@@avRep];
    If[!dsg,
      times=SeitzTimes[brav];   inv=invSeitz[brav],
      times=DSGSeitzTimes[brav];  inv=DSGinvSeitz[brav]
      ];
    (* Convert the little group (LG) of k, Gk, to its conjugate LG Gk1. Gk1 is the LG of kBD
       and kBD is in the star of k and in the basic domain. Gk1 is used to determine the reps. *)
    Gk1=If[S=="E",Gk,times[times[inv[{S,w}],#],{S,w}]&/@Gk];
    LG=getLGElem[sgno,kname2,"DSG"->dsg];   
    Gk1rots=If[!dsg, Gk1[[All,1]], Join[Gk1[[All,1]],SpinRotTimes[brav]["barE",#]&/@Gk1[[All,1]]]];
    (* Print["{S,w}=",{S,w},"\nGk=",Gk,"\nGk1=",Gk1,"\nGk1rots=",Gk1rots,"\nLG=",LG]; (*for debug*)*)
    If[!seteq[Gk1rots,LG[[All,1]]],
      Print["getBandRep: ik=",ik," (",k,"), Rots=",Gk1rots," not equal to Gk0=",LG[[All,1]]];
      Return[];
    ];
                
    If[VectorQ[gens[[1,2]]],  (* for high-symmetry point *)
      kBD=kinfo[[5]]; (* the k in basic domain *)
      HLG=getHLGElem[brav,AGno,gens,"DSG"->dsg];  
      idx=Position[Gk1rots,#[[1]]][[1,1]]&/@HLG;
      facbar=Table[1,Length[HLG]];
      If[dsg, 
        facbar=If[#>Length[Gk1],-1,1]&/@idx;
        idx=With[{n=Length[Gk1]},If[#>n,#-n,#]]&/@idx;
      ];            
      (* idx makes|Gk2|\[Equal]|HLG| and all rotations are in the same order, i.e. Gk2[[All,1]]\[Equal]HLG[[All,1]]. *)
      Gk2=Gk1[[idx]];
      factor=If[dt=HLG[[#,2]]-Gk2[[#,2]];dt!={0,0,0},Exp[-I*kBD.dt*2Pi],1]&/@Range[Length[HLG]]//Chop;  
      factor=factor*facbar;    
    ];
    
    If[IntegerQ[gens[[1,2]]],  (* for high-symmetry line *)
      (*see fix20211227*)
      If[MemberQ[{{"OrthBase","H"},{"OrthBody","G"},{"OrthBody","F"},{"OrthBody","U"}},{brav,kname}],
        kBD=kBCcoord[sgno,kname][[1,1]]/.kinfo[[-3]],
        kBD=kinfo[[5]]/.kinfo[[-3]]; 
      ]; (* the k in basic domain *)
      CE=getCentExt[sgno,kname,"DSG"->dsg];  g=Max[CE[[All,2]]]+1;
      idx=Position[Gk1rots,#[[1]]][[1,1]]&/@CE;
      facbar=Table[1,Length[CE]];
      If[dsg, 
        facbar=If[#>Length[Gk1],-1,1]&/@idx;
        idx=With[{n=Length[Gk1]},If[#>n,#-n,#]]&/@idx;
      ];            
      (* idx makes|Gk2|\[Equal]|CE| and all rotations are in the same order, i.e. Gk2[[All,1]]\[Equal]CE[[All,1]]. *)
      Gk2=Gk1[[idx]];
      factor=With[{v=Gk2[[#,2]],aa=CE[[#,2]]},Exp[I*(kBD.v+aa/g)*2Pi]]&/@Range[Length[CE]]//Chop;
      factor=factor*facbar;    
     ];

    Label["determine reps"]; (* Use AGno, avRep, reptype, idx, factor *)
    chars=traceData["trace"][[ik]];
    nc=Length/@AGClasses@@AGno;
    idxcls=If[AGno!={1,1},FoldList[Plus,1,nc[[;;-2]]],{1}];
    chars2=chars[[#,idx]]*factor&/@iball;  
       
    (* For debug. To check if characters in the same class equal to each other. *)
    (* -------for debug --------
    icEle=#1;;(#1+#2-1)&@@@Transpose[{idxcls,nc}];
    For[i=1,i<=Length[ibs],i++, ib=ibs[[i]];
      tmp=DeleteDuplicates[chars2[[ib,#]],Abs[#1-#2]<2*^-2&]&/@icEle;
      tmp1=Flatten@Position[Length[#]==1&/@tmp,False];
      If[tmp1!={}, Print["Warning: getBandRep: ik=",ik,", ib=",ib," characters in the same"<>
         " class do not equal to each other for classes ",tmp1,": ",tmp[[tmp1]]]]
    ]; 
     -------------------------*) 
    
    repR={#,reduceRep[AGno,chars2[[#,idxcls]]]}&/@ibs;   
    repR=repR//Chop//Rationalize[#,0.1]&;  
        
    abc=Association[Rule@@@Transpose[{{"a","b","c","d","e","f"},Range[2,7]}]];
    tmp=If[!dsg, LGIrepLabel[AGno], DLGIrepLabel[AGno]];
    repLabel=Association@(Rule@@@Transpose[{tmp[[1,2]],
     Transpose[{tmp[[abc[reptype],2]],RepGammaLabel[kname,#]&/@tmp[[abc[reptype],3]],tmp[[1,3]]}]}]);
 
    rep=Table[0,Length[repR]];
    For[i=1,i<=Length[ibs],i++, ib=ibs[[i]];
      iR=Select[Range[Length[idxcls]],repR[[i,2,#]]!=0&]; 
      tmp1=If[OptionValue["CompressDegeneracy"],iball2[[ibidx[[i]],{1,-1}]],ib];
      If[!SubsetQ[Keys[avRep],iR], 
        rep[[i]]={tmp1,traceData["ene"][[ik,ib]],deg[[ib]],{"??","??"}};
        (* Print["Warning: getBandRep: ik=",ik,", ib=",ib," reps ",iR,
                " not in available reps ",Keys[avRep]]; (* for debug *) *)
        Continue[]
      ];  
      as=If[#==1,"",ToString[#,InputForm]]&/@repR[[i,2,iR]];
      rs=repLabel/@iR;  
      tmp=StringRiffle[#1<>#2&@@@Transpose[{as,rs[[All,1]]}],"\[CirclePlus]"];
      If[OptionValue["showdim"],
        tmp={tmp,StringRiffle[#1<>#2<>"("<>ToString[#3]<>")"&@@@Transpose[{as,rs[[All,2]],rs[[All,3]]}],"\[CirclePlus]"]},
        tmp={tmp,StringRiffle[#1<>#2&@@@Transpose[{as,rs[[All,2]]}],"\[CirclePlus]"]}
      ];
      If[VectorQ[repR[[i,2]],IntegerQ], (* The multiplicities of reps should be integers. *)
        rep[[i]]={tmp1,traceData["ene"][[ik,ib]],deg[[ib]],tmp},
        (* rep[[i]]={tmp1,traceData["ene"][[ik,ib]],deg[[ib]],"??"<>#<>"??"&/@tmp}; (*for debug*);
        Print["Warning: getBandRep: ik=",ik,", ib=",ib," multiplicities of reps are not integers ",
               repR[[i,2]]];  (*for debug*)   *)
        rep[[i]]={tmp1,traceData["ene"][[ik,ib]],deg[[ib]],{"??","??"}};
      ]
    ];    
    
    If[IntegerQ[ibOrListOrSpan],rep[[1]],rep]
  ];
  
  re=<||>;  
  re["kpath"]=kpathstr;
  re["rep"]=If[IntegerQ[ikOrListOrSpan], bandRepOneK[iks[[1]]], bandRepOneK/@iks];
  re["kinfo"]=kinfoList;  
  re
]

getBandRep[sgno_Integer,BZtypeOrBasVec_,traceData_, ikOrListOrSpan_, OptionsPattern[]]/;
  And@@(Position[#,Rule]=={}&&(IntegerQ[#]||ListQ[#]||Head[#]==Span||#==All)&@ikOrListOrSpan):=
  getBandRep[sgno,BZtypeOrBasVec,traceData, ikOrListOrSpan, All, "CompressDegeneracy"->OptionValue["CompressDegeneracy"], "showdim"->OptionValue["showdim"]]
getBandRep[sgno_Integer,BZtypeOrBasVec_,traceData_, OptionsPattern[]]:= 
  getBandRep[sgno,BZtypeOrBasVec,traceData, All, All, "CompressDegeneracy"->OptionValue["CompressDegeneracy"], "showdim"->OptionValue["showdim"]]


(*rep should be the returned value of getBandRep[sgno, BZtypeOrBasVec, tr] without specifying ik and ib. 
  Although result will be given when rep=getBandRep[sgno, BZtypeOrBasVec, tr, ik], a warning will be issued. *)
SetAttributes[showBandRep,HoldFirst];
Options[showBandRep]={"bottomUp"->True};
showBandRep[rep_, ik_Integer, OptionsPattern[]]:=showBandRep[rep,ik,All,"bottomUp"->OptionValue["bottomUp"]]
showBandRep[rep_, ik_Integer, ibOrListOrSpan_, OptionsPattern[]]:=Module[{ir,irik,ibmax,iball,ibs,ibs0,ibs1,irout,tab},
  ir=rep["rep"];
  If[IntegerQ[ir[[1]]]||VectorQ[ir[[1]],IntegerQ],
    Print["showBandRep: the rep should be the returned value of getBandRep without specifying band index."]; Abort[]
  ];
  If[Length[ir[[1]]]==4 && (IntegerQ[ir[[1,1]]]||VectorQ[ir[[1,1]],IntegerQ]),
    Print["Warning: there is only one k-point in the data of "<>StringTake[ToString@Hold[rep],{6,-2}]<>"."];
    irik=ir,  (* for the case in which ik has been specified in getBandRep, i.e. rep=getBandRep[sgno, "a", tr, ik]*)
    (*---- else: for the case rep=getBandRep[sgno, "a", tr] ------*)
    Check[irik=ir[[ik]], Print["showBandRep: ik out of range [1,",Length[ir],"]!"]; Abort[],
      {Part::partw,Part::take}];
  ];
  ibs0=irik[[All,1]];  ibmax=Max@Flatten@ibs0;  iball=Range[ibmax];
  ibs=Check[iball[[ibOrListOrSpan]],
        Print["showBandRep: ib ",ibOrListOrSpan," out of range [1,",ibmax,"]."]; Abort[],
        {Part::partw,Part::pkspec1,Part::take}];
  If[IntegerQ[ibOrListOrSpan], ibs={ibs}];
  If[ibOrListOrSpan===0, ibs=iball];
  ibs1=(Position[Range@@@ibs0,#][[1,1]]&/@ibs)//Union;
  irout=If[OptionValue["bottomUp"]===True, irik[[ibs1]]//Reverse, irik[[ibs1]]];
  tab=MapAt[If[IntegerQ[#], #, If[#[[1]]==#[[2]],#[[1]],Row[#,"-"]]]&, irout, {All,1}];
  tab=MapAt[Sequence@@Reverse[#]&, tab, {All,4}];
  tab=Prepend[tab,{"Band","Energy","Degeneracy","GammaLabel","Mulliken"}];
  Grid[tab, Alignment->{{Left,Left,Center,Left,Center}}, Spacings -> {{Default, 3, 1, 1,1}},
            Dividers->{{},{2->Directive[Thickness[0.4],Gray]}}, Frame->Thickness[0.4]]
]

  
(* Convert the traceData from any primitive input cell to the trace data for BC cell. 
   P, p0, and stdR are the dataset['transformation_matrix'], dataset['origin_shift'] 
   and dataset['std_rotation_matrix'] from spglib for the input cell respectively. *)
(* Note that when soc=0, stdR has no use. But when soc=1, stdR has to be given correctly. *)
convTraceToBC[sgno_Integer,traceData_,P_,p0_]:=convTraceToBC[sgno,traceData,P,p0,IdentityMatrix[3]]
convTraceToBC[sgno_Integer,traceData_,P_,p0_,stdR_]:=Block[{\[Gamma],trdat,Q,S,U,iP,iQ,iU,
  t0,rot,newrot,trans,newtrans,Mk,newkpt,Raxis,Rang,Saxis,Sang,tmp1,tmp2,SR,srot,newsrot},
  trdat=traceData;
  {t0,U}=SGGenElem[sgno][[{2,3}]];
  {Q,S}=getQandS[sgno];
  {iP,iQ,iU}=Inverse/@{P,Q,U};
  rot=traceData["rot"];   trans=traceData["trans"];
  newrot=iU.iQ.P.#.iP.Q.U&/@rot//Round;
  (* Note: srot need to be transformed also for SOC case. *)
  srot=traceData["srot"];   newsrot=srot;
  If[trdat["soc"]==1,
    {tmp1,Raxis,Rang}=rotAxisAngle[stdR];    
    S=S/.\[Gamma]->Pi/2;  (*For monoclinic system, the parameter \[Gamma] can be any non-zero value. They are all
    equivalent in terms of converting C2y to C2z.*)
    {tmp2,Saxis,Sang}=rotAxisAngle[S];  
    tmp1=MatrixExp[-I*Rang*Raxis.(PauliMatrix/@{1,2,3})/2]//Simplify;
    tmp2=MatrixExp[-I*Sang*Saxis.(PauliMatrix/@{1,2,3})/2]//Simplify;
    SR=tmp2.tmp1//Simplify;
    newsrot=SR.#.SR\[ConjugateTranspose]&/@srot;
  ];
  newtrans=iU.iQ.(P.trans[[#]]-P.rot[[#]].iP.p0+p0)-newrot[[#]].t0+t0&/@Range[Length[rot]];
  Mk=Transpose@Inverse[iU.iQ.P];
  newkpt=Mk.#&/@traceData["kpt"];
  trdat["rot"]=newrot;  trdat["trans"]=newtrans;   trdat["kpt"]=newkpt;  trdat["srot"]=newsrot;
  trdat
]

readPOSCAR[filename_String]:=Module[{poscar,basVec,a,pos,elem,nelem,type,n,ch,atnum,atsym,sym2num},
  If[!FileExistsQ[filename],Print["readPOSCAR: Cannot read file ",filename,"!"]; Abort[]];
  poscar=Select[Import[filename,"Table"],#!={}&];
  a=poscar[[2,1]];
  basVec=poscar[[3;;5]]*a;
  If[NumberQ[poscar[[6,1]]], 
    nelem=poscar[[6]]; elem=Table["H",Length[nelem]];  n=7,  
    elem=poscar[[6]]; nelem=poscar[[7]];  n=8 ];
  ch=StringTake[poscar[[n,1]],1];
  If[ch=="S"||ch=="s", n++];   type=StringTake[poscar[[n++,1]],1];
  pos=poscar[[n;;,1;;3]];
  If[MemberQ[{"c","C","k","K"},type], pos=#.Inverse[basVec]&/@pos];
  If[Length[pos]<Total[nelem], 
    Print["readPOSCAR: Error! The number of atomic positions ",Length[pos]," does not equal to the ",
          "sum of numbers of atoms for each element ",nelem,"."];
    Abort[] ];
  pos=pos[[;;Total[nelem]]];
  atsym=Flatten[Table[#1,#2]&@@@Transpose[{elem,nelem}]];
  sym2num=<|"H"->1,"He"->2,"Li"->3,"Be"->4,"B"->5,"C"->6,"N"->7,"O"->8,"F"->9,"Ne"->10,
    "Na"->11,"Mg"->12,"Al"->13,"Si"->14,"P"->15,"S"->16,"Cl"->17,"Ar"->18,"K"->19,"Ca"->20,
    "Sc"->21,"Ti"->22,"V"->23,"Cr"->24,"Mn"->25,"Fe"->26,"Co"->27,"Ni"->28,"Cu"->29,"Zn"->30,
    "Ga"->31,"Ge"->32,"As"->33,"Se"->34,"Br"->35,"Kr"->36,"Rb"->37,"Sr"->38,"Y"->39,"Zr"->40,
    "Nb"->41,"Mo"->42,"Tc"->43,"Ru"->44,"Rh"->45,"Pd"->46,"Ag"->47,"Cd"->48,"In"->49,"Sn"->50,
    "Sb"->51,"Te"->52,"I"->53,"Xe"->54,"Cs"->55,"Ba"->56,"La"->57,"Ce"->58,"Pr"->59,"Nd"->60,
    "Pm"->61,"Sm"->62,"Eu"->63,"Gd"->64,"Tb"->65,"Dy"->66,"Ho"->67,"Er"->68,"Tm"->69,"Yb"->70,
    "Lu"->71,"Hf"->72,"Ta"->73,"W"->74,"Re"->75,"Os"->76,"Ir"->77,"Pt"->78,"Au"->79,"Hg"->80,
    "Tl"->81,"Pb"->82,"Bi"->83,"Po"->84,"At"->85,"Rn"->86,"Fr"->87,"Ra"->88,"Ac"->89,"Th"->90,
    "Pa"->91,"U"->92,"Np"->93,"Pu"->94,"Am"->95,"Cm"->96,"Bk"->97,"Cf"->98,"Es"->99,"Fm"->100,
    "Md"->101,"No"->102,"Lr"->103,"Rf"->104,"Db"->105,"Sg"->106,"Bh"->107,"Hs"->108,"Mt"->109,
    "Ds"->110,"Rg"->111,"Cn"->112,"Nh"->113,"Fl"->114,"Mc"->115,"Lv"->116,"Ts"->117,"Og"->118|>;
  (*atnum=ElementData[#,"AtomicNumber"]&/@atsym;*)
  atnum=sym2num/@atsym;
  <|"basVec"-> basVec,"pos"->pos,"atnum"->atnum, "atsym"->atsym|>
]

spglibGetSym[{basVec_,pos_,atnum_}]:=spglibGetSym[{basVec,pos,atnum},1*^-5]
spglibGetSym[{basVec_,pos_,atnum_}, prec_]:=Module[{ss, cell, ds=<||>, topy, dat=<||>},
  topy[expr_]:=ExportString[expr,"PythonExpression"];
  ss=StartExternalSession["Python"];
  ExternalEvaluate[ss,"from numpy import *"];
  ExternalEvaluate[ss,"from spglib import *"];
  cell=topy[{basVec,pos,atnum}];
  cell="("<>topy[basVec]<>","<>topy[pos]<>","<>topy[atnum]<>")";
  cell="tuple("<>topy[{basVec,pos,atnum}]<>")";
  ExternalEvaluate[ss,"ds=get_symmetry_dataset("<>cell<>",symprec="<>topy[prec]<>")"];
  dat["symbol"]=ExternalEvaluate[ss,"ds['international']"];
  dat["number"]=ExternalEvaluate[ss,"ds['number']"];
  dat["P"]=ExternalEvaluate[ss,"ds['transformation_matrix'].tolist()"]//Chop;
  dat["p0"]=ExternalEvaluate[ss,"ds['origin_shift'].tolist()"]//Chop;
  dat["R"]=ExternalEvaluate[ss,"ds['std_rotation_matrix'].tolist()"]//Chop;
  dat["rot"]=ExternalEvaluate[ss,"ds['rotations'].tolist()"]//Chop;
  dat["trans"]=ExternalEvaluate[ss,"ds['translations'].tolist()"]//Chop;
  DeleteObject[ss]; (*release memory after usage*)
  dat
]

Options[autoConvTraceToBC]={"cellData"->False};
autoConvTraceToBC[poscarFile_,traceData_,OptionsPattern[]]:=
  autoConvTraceToBC[poscarFile,traceData,1*^-5,"cellData"->OptionValue["cellData"]]
autoConvTraceToBC[poscarFile_,traceData_,prec_,OptionsPattern[]]:=
 Module[{cell,BCcell,sym,ops1,ops2,ops3,brav,ckbv,trdat,sgno,U,Q,t0,S,R},
  cell=readPOSCAR[poscarFile];
  sym=spglibGetSym[Values[cell][[1;;3]],prec];
  ops1=Transpose[{sym["rot"],modone@Round[sym["trans"],prec]}];
  ops2=Transpose[{traceData["rot"],modone@Round[traceData["trans"],prec]}];
  If[!seteq[ops1,ops2], 
    Print["autoConvTraceToBC: symmetry operations for POSCAR are not equal to those in trace.txt: ",
      Complement[ops1,ops2],"!=",Complement[ops2,ops1]];
    Abort[];
  ];
  sgno=sym["number"];   brav=getSGLatt[sgno];
  ops3={getRotMat[brav,#1],#2}&@@@getLGElem[sgno,"\[CapitalGamma]"];  
  ckbv=checkBasVec[brav,cell[[1]]];
  If[ckbv[[1]]&&seteq[ops1,ops3], 
     If[OptionValue["cellData"]===True, Return[<|"trace"->traceData,"input_cell"->cell,"BCcell"->cell,"sym"->sym|>], Return[traceData]]
    ];
  {t0,U}=SGGenElem[sgno][[{2,3}]];
  {Q,S}=getQandS[sgno];
  trdat=convTraceToBC[sgno,traceData,sym["P"],sym["p0"],sym["R"]];
  BCcell=cell;
  BCcell["basVec"]=Transpose@(S.sym["R"].Transpose[cell["basVec"]].Inverse[sym["P"]].Q.U)//Chop;
  BCcell["pos"]=Inverse[Q.U].(sym["P"].#+sym["p0"])+t0&/@cell["pos"]//Chop;
  If[OptionValue["cellData"]===True, <|"trace"->trdat,"input_cell"->cell,"BCcell"->BCcell,"sym"->sym|>, trdat]
]


(* ::Section:: *)
(*Compare with the k points and reps of BCS (Bilbao Crystallographic Server)*)


Off[General::shdw];
<<"allBCSkLGdat.mx";
allBCSkLGdat=Global`allBCSkLGdat/.{Global`u->u,Global`v->v,Global`w->w};
Remove[Global`u, Global`v, Global`w, Global`allBCSkLGdat];

Pabc=IdentityMatrix[3];
Cabc={{1/2,1/2,0},{-1/2,1/2,0},{0,0,1}};
Aabc={{1,0,0},{0,1/2,-1/2},{0,1/2,1/2}};
Rabc={{2,-1,-1},{1,1,-2},{1,1,1}}/3;
Fabc={{0,1,1},{1,0,1},{1,1,0}}/2;
Iabc={{-1,1,1},{1,-1,1},{1,1,-1}}/2;

BCSanotherKvalue[fullBZtype_]:=Block[{u,v,w,sub},
 sub={u->0.6,v->0.6,w->0.6};
 Switch[fullBZtype,
  "OrthBase(a)", <|"\[CapitalDelta]"->sub,"F"->sub,"B"->sub,"G"->sub|>,
  "OrthBase(b)", <|"A"->sub,"E"->sub,"\[CapitalSigma]"->sub,"C"->sub|>,
  "OrthBody(a)", <|"\[CapitalLambda]"->sub,"G"->sub,"\[CapitalDelta]"->sub,"U"->sub|>,
  "OrthBody(b)", <|"\[CapitalLambda]"->sub,"G"->sub,"\[CapitalSigma]"->sub,"F"->sub|>,
  "OrthBody(c)", <|"\[CapitalDelta]"->sub,"U"->sub,"\[CapitalSigma]"->sub,"F"->sub|>,
  "OrthFace(a)", <|"A"->sub,"C"->sub,"B"->sub,"D"->sub,"H"->sub,"G"->sub|>,
  "OrthFace(b)", <|"\[CapitalLambda]"->sub,"Q"->sub,"H"->sub,"G"->sub|>,
  "OrthFace(c)", <|"\[CapitalDelta]"->sub,"R"->sub,"B"->sub,"D"->sub|>,
  "OrthFace(d)", <|"A"->sub,"C"->sub,"\[CapitalSigma]"->sub,"U"->sub|>,
  "TetrBody(a)", <|"\[CapitalLambda]"->sub,"V"->sub|>,
  "TetrBody(b)", <|"Y"->sub,"U"->sub,"\[CapitalSigma]"->sub,"F"->sub|>,
  "TrigPrim(a)", <|"\[CapitalLambda]"->{w->0.9},"P"->{w->0.9}|>,
  "TrigPrim(b)", <|"Y"->sub,"B"->sub,"\[CapitalSigma]"->sub,"Q"->sub|>,
  "HexaPrim",    <|"T"->sub,"T'"->sub,"S"->sub,"S'"->sub|>,
  "CubiBody",    <|"\[CapitalLambda]"->sub,"F"->sub|>,
  "CubiFace",    <|"\[CapitalSigma]"->{u->0.8},"S"->{u->0.8}|>,
  _,             <||>
 ]
]

kptBCStoBC[sgno_Integer]:=kptBCStoBC[sgno, "a"]
kptBCStoBC[sgno_Integer, BZtype_String]:=Block[{u,v,w,\[Alpha],BCSknames,BCSkc,M,Mabc,t0,U,Q,S,tmp,tab,i,j,
  kifs,kif,kBCSc,kBCSp,kBCS2BC,kBC,kd,sub1,BCSkc2,BCSk2BCn,brav,fullBZtype,kn,kn1},
  sub1={u->0.1,v->0.2,w->0.3};
  tmp=allBCSkLGdat[[sgno]]["kirepinfo"][[All,Key["kconv"]]];
  BCSkc=Values[tmp];     BCSknames=Keys[tmp];
  {t0,U}=SGGenElem[sgno][[{2,3}]];
  {Q,S}=getQandS[sgno];
  Mabc=ToExpression["SpaceGroupIrep`Private`"<>StringTake[allBCSkLGdat[[sgno]]["spgsym"],1]<>"abc"];
  M=Q.U;
  
  brav=getSGLatt[sgno];   fullBZtype=brav;
  If[MemberQ[{"OrthBase","OrthBody","OrthFace","TetrBody","TrigPrim"},brav], 
     fullBZtype=brav<>"("<>BZtype<>")"];
  tmp=BCSanotherKvalue[fullBZtype];
  BCSk2BCn=(#.M/.sub1)&/@BCSkc;   BCSkc2=BCSkc;
  kifs=identifyBCHSKptBySG[sgno,BZtype,BCSk2BCn]/.u->\[Alpha];    
  For[j=1;i=1, i<=Length[BCSkc], i++;j++,  
    kn=BCSknames[[j]];  kn1=kifs[[j,2]];   
    If[!MemberQ[Keys[tmp],kn1], Continue[]];
    j++;
    BCSk2BCn=Insert[BCSk2BCn, BCSkc[[i]].M/.tmp[kn1], j];
    BCSknames=Insert[BCSknames, kn, j];
    BCSkc2=Insert[BCSkc2, BCSkc[[i]], j];
    kif=identifyBCHSKptBySG[sgno,BZtype,BCSk2BCn[[j]]]/.u->\[Alpha]; 
    kifs=Insert[kifs, kif, j];  
  ];
  
  kifs=Rationalize[kifs,0.0001];
  tab=Table[0,Length[BCSkc2]];
  For[i=1,i<=Length[BCSkc2],i++,
    kBCSc=BCSkc2[[i]]//Rationalize[#,0.0001]&;
    kBCSp=kBCSc.Mabc//Simplify;
    kBCS2BC=kBCSc.M//Simplify;
    kif=kifs[[i]];
    If[kif[[2]]=="UN"||kif[[2]]=="GP",tmp={kif[[2]],"","",""},
      kBC=kif[[5]];
      If[kif[[6,1]]=="E",   tmp={kif[[2]],kBC,kif[[6,1]],kif[[8]]},
        If[kif[[3]]=="",  (*high-symmetry point*)
          kd=kBCS2BC-kBC;
          tmp=If[VectorQ[kd,IntegerQ], {kif[[2]],kBC,"E",kd}, {kif[[2]],kBC,kif[[6,1]],kif[[8]]}],
          (* Else: high-symmetry line*)
          tmp={kif[[2]],kBC,kif[[6,1]],kif[[8]]};
          ];
        ];
      ];
    (* Print[kif]; *)
    tab[[i]]={BCSknames[[i]],kBCSc,kBCSp,kBCS2BC,Sequence@@tmp};
    If[!MemberQ[{"","E"},tab[[i,7]]], tab[[i,5]]=tab[[i,5]]<>If[Last[kif]=="in G", "*","**"]];
  ];
  tmp=#.Inverse[M].Mabc&/@BCSk2BCn;
  <|"table"->tab, "BCSknames"->BCSknames, "BCSkp"->tab[[All,3]], "BCSkpn"->tmp|>
]

showKptBCStoBC[sgno_Integer]:=showKptBCStoBC[sgno, "a"]
showKptBCStoBC[sgno_Integer, BZtype_String]:=Block[{u,v,w,\[Alpha],tab,tab0,i,knsub,TAB, bztp},
  tab=tab0=kptBCStoBC[sgno,BZtype]["table"];
  knsub={"DT"->"\[CapitalDelta]","GM"->"\[CapitalGamma]","LD"->"\[CapitalLambda]","SM"->"\[CapitalSigma]","ZA"->"Z'"};
  For[i=1,i<=Length[tab],i++,
    If[tab[[i,5]]=="UN"||tab[[i,5]]=="GP",
      tab[[i]]=Style[#,Blue]&/@tab[[i]] ,
      If[(tab[[i,1]]/.knsub)!=StringReplace[tab[[i,5]],"*"->""],  tab[[i]]=Style[#,Red]&/@tab[[i]] ]
     ]
   ];
  For[i=1,i<=Length[tab]-1,i++,
    If[tab0[[i,1]]==tab0[[i+1,1]], 
      tab[[i]]=Style[#,Background->LightYellow]&/@tab[[i]];
      tab[[i+1]]=Style[#,Background->LightYellow]&/@tab[[i+1]];
    ]
  ];
  TAB=TableForm[tab,TableDepth->2, TableHeadings->
    {None,{"BCS","BCSconv","BCSprim","BCStoBCprim","BC","BCprim","rot","gn"}}];
  If[MemberQ[{"OrthBase","OrthBody","OrthFace","TetrBody","TrigPrim"},getSGLatt[sgno]],
    bztp="BZ type ("<>BZtype<>")",
    bztp=Nothing]; 
  Column[{
    Row[{Row[{"No. ",sgno}], SGSymStd[sgno], bztp}, ",   "], TAB},
    Dividers->{None,2->Black}, Alignment->Left, Spacings->1]    
]


Options[buildTr4BCSrep]={"DSG"->False};
buildTr4BCSrep[sgno_Integer, BZtype_String, OptionsPattern[]]:=Block[{u,v,w,BCSknames,Mabc,tmp,
  tab,i,j,BCSkp,BCSkpn,kn,chtab,trdat=<||>,kLGdat,rot,trans,srot,deg,ene,trace,nb,BCSrepname,
  dsg,sx,sC4z},
  kLGdat=allBCSkLGdat[[sgno]];
  Mabc=ToExpression["SpaceGroupIrep`Private`"<>StringTake[kLGdat["spgsym"],1]<>"abc"];
  {tmp,BCSknames,BCSkp,BCSkpn}=kptBCStoBC[sgno,BZtype]//Values; 
  dsg=OptionValue["DSG"];
  
  trdat["nelec"]=0;   (* no use *)
  trdat["soc"]=If[dsg,1,0];
  trdat["nsym"]=kLGdat["kirepinfo","GM","nsym"];
  trdat["rot"]=kLGdat["SymElemRp"][[;;trdat["nsym"]]];
  trdat["trans"]=kLGdat["SymElemtp"][[;;trdat["nsym"]]];
  If[sgno<143||sgno>194,
    trdat["srot"]=kLGdat["SymElemS"][[;;trdat["nsym"]]],
    sx={{0,1},{1,0}};   sC4z=MatrixExp[-I(Pi/2){0,0,1}.(PauliMatrix/@{1,2,3})/2];
    trdat["srot"]=sC4z.sx.#.sx.Inverse[sC4z]&/@kLGdat["SymElemS"][[;;trdat["nsym"]]]
    ];
  trdat["nk"]=Length[BCSkpn];
  trdat["kpt"]=BCSkpn;
  trdat["knsym"]=kLGdat["kirepinfo",#,"nsym"]&/@BCSknames;
  trdat["kisym"]=kLGdat["kirepinfo",#,"symidx"]&/@BCSknames;
  chtab=kLGdat[[Key["charTable"],All,Key[If[dsg,"dchartab","schartab"]]]];
  nb=Max[Total[#[[All,1]]]&/@Values[chtab]];  trdat["nband"]=nb;
  ene=deg=trace=BCSrepname=Table[0,Length[BCSknames]];
  For[i=1, i<=Length[BCSknames], i++, kn=BCSknames[[i]];
    tmp=Flatten[Table[#,#]&/@chtab[kn][[All,1]]];
    deg[[i]]=Flatten[Table[tmp,Ceiling[nb/Length[tmp]]]][[;;nb]]//Round;
    (*tmp=Flatten[Table[#1,#2]&@@@Transpose[{kLGdat["charTable",kn,"srep"],chtab[kn][[All,1]]}]];
    BCSrepname[[i]]=Flatten[Table[tmp,Ceiling[nb/Length[tmp]]]][[;;nb]];*)
    BCSrepname[[i]]=kLGdat["charTable",kn,If[dsg,"drep","srep"]];
    ene[[i]]=Range[nb];  (* no use *)
    If[!VectorQ[BCSkp[[i]],NumericQ],  (* k has u,v,w *)
      tmp=BCSkpn[[i]]-BCSkp[[i]]/.{u->0,v->0,w->0};
      tmp=Exp[-I*2Pi*tmp.trdat["trans"][[#]]]&/@trdat["kisym"][[i]]//Chop;
      tmp=tmp*#&/@(chtab[kn]\[Conjugate]),  (* Note that the BCS small reps are the conjugate of the BC ones. *)
    (* else: k is numeric *)
      tmp=chtab[kn]\[Conjugate]
    ];
    tmp=Flatten[Table[#,#[[1]]]&/@tmp,1];
    trace[[i]]=Flatten[Table[tmp,Ceiling[nb/Length[tmp]]],1][[;;nb]];
  ];
  trdat["ene"]=ene;
  trdat["deg"]=deg;
  trdat["trace"]=trace;
  <|"trdat"->trdat, "BCSknames"->BCSknames, "BCSkp"->BCSkp, "Mabc"->Mabc, "BCSrepname"->BCSrepname|>
]


Options[krepBCStoBC]={"DSG"->False};
krepBCStoBC[sgno_Integer, OptionsPattern[]]:=krepBCStoBC[sgno, "a", "DSG"->OptionValue["DSG"]]
krepBCStoBC[sgno_Integer, BZtype_String, OptionsPattern[]]:=Block[{u,v,w,trdat,BCSknames,BCSkp,
  Mabc,BCSrepname,s0conv,BCtrdat,repinfo,rep,i,nrep,ff,re,BCknames,BCir,BCirL,ML,ML2r,BCrlt,BCSrlt,
  aUs,dsg,keyrlty},
  s0conv[n_]:=Switch[n, 48,{1,1,1}/4, 50,{1,1,0}/4, 59,{1,1,0}/4, 68,{0,1,1}/4,
    70,{-1,-1,-1}/8, 85,{-1,1,0}/4, 86,{-1,-1,-1}/4, 88,{0,-2,-1}/8, 125,{-1,-1,0}/4,
    126,{-1,-1,-1}/4, 129,{-1,1,0}/4, 130,{-1,1,0}/4, 133,{-1,1,-1}/4, 134,{-1,1,-1}/4,
    137,{-1,1,-1}/4, 138,{-1,1,-1}/4, 141,{0,2,-1}/8, 142,{0,2,-1}/8, 201,{-1,-1,-1}/4,
    203,{-1,-1,-1}/8, 222,{-1,-1,-1}/4, 224,{-1,-1,-1}/4, 227,{-1,-1,-1}/8,
    228,{-3,-3,-3}/8, _,{0,0,0}];
  dsg=OptionValue["DSG"];
  {trdat,BCSknames,BCSkp,Mabc,BCSrepname}=Values@buildTr4BCSrep[sgno,BZtype,"DSG"->dsg];
  (* Print["Mabc=",Mabc,"-s0conv=",-s0conv[sgno]]; *)
  BCtrdat=convTraceToBC[sgno,trdat,Mabc,-s0conv[sgno]];  
  repinfo=getBandRep[sgno,BZtype,BCtrdat];  
  rep=repinfo["rep"];                 
  BCknames=repinfo["kinfo"][[All,2]];
  ff=Sequence@@{StringTake[#[[2]],{-2,-2}],StringTake[#[[2]],{1,-4}],#[[1]]}&;
  re=Table[0,Length[BCSrepname]];
  For[i=1,i<=Length[BCSrepname],i++,
    nrep=Length[BCSrepname[[i]]];
    (*-------check reality----------*) 
    If[getSGLatt[sgno]=="TrigPrim"&&BCknames[[i]]=="F",
      BCir=If[dsg,DLGIrep,LGIrep][sgno,BZtype<>BCknames[[i]]],
      If[BCknames[[i]]!="GP"&&BCknames[[i]]!="UN",
        BCir=If[dsg,DLGIrep,LGIrep][sgno,BCknames[[i]]],
        BCir=calcRep[sgno,repinfo["kinfo"][[i]],"DSG"->dsg]; BCir=BCir[[{5,6,3,4}]]
        ]
    ];
    BCirL=If[dsg,DLGIrepLabel,LGIrepLabel][BCir[[1]]];
    ML=DeleteCases[Select[BCirL,#[[1]]==BCir[[4]]&][[1,2]],""];
    ML2r=Association[Rule@@@Transpose[{ML,BCir[[3,All,2]]}]];   
    BCrlt=ML2r/@rep[[i,;;nrep,4,1]];  
    aUs=allBCSkLGdat[[sgno]]["kirepinfo",BCSknames[[i]],"antiUsym"]; 
    If[aUs==0, 
      BCSrlt=Table["x",nrep],
      keyrlty=If[dsg,"dreality","sreality"];
      BCSrlt=allBCSkLGdat[[sgno]]["charTable",BCSknames[[i]],keyrlty]/.{-1->2,0->3}
      ]; 
    If[BCrlt!=BCSrlt, Print[If[dsg,"D",""]<>"SG ",sgno,": ",BCSknames[[i]],",",BCknames[[i]],
        " reality not equal for BCS and BC ",BCSrlt,"!=",BCrlt]];
    (*------------------------------*)
    re[[i]]={BCSrepname[[i,#]],ff[rep[[i,#,4]]],BCrlt[[#]]}&/@Range[nrep];
  ];
  re
]

Options[showKrepBCStoBC]={"DSG"->False};
showKrepBCStoBC[sgno_Integer, OptionsPattern[]]:=showKrepBCStoBC[sgno, "a", "DSG"->OptionValue["DSG"]]
showKrepBCStoBC[sgno_Integer, BZtype_String, OptionsPattern[]]:=Module[{tab,tabs,ncol=3,maxrow,
  hsep=0.5,h1,htot,tabs2,i,j,hs,htmp, bztp,ws,bg,Nk,dsg,tmp},
  dsg=OptionValue["DSG"];
  tab=krepBCStoBC[sgno, BZtype, "DSG"->dsg];
  ws={2.9,1,1.8,2.3,1};   
  Nk=Length[tab];
  bg=Table[White,Nk];
  For[i=1,i<=Nk,i++,  
    If[i<Nk&&tab[[i,1,1]]==tab[[i+1,1,1]], bg[[i]]=bg[[i+1]]=LightYellow];
    tmp={"\!\(\*SubscriptBox[\(UN\), \(1\)]\)","\!\(\*SubscriptBox[\(GP\), \(1\)]\)",
         "\!\(\*SubscriptBox[\(UN\), \(3\)]\)","\!\(\*SubscriptBox[\(GP\), \(2\)]\)"};
    If[Intersection[tab[[i,All,3]],tmp]!={}, bg[[i]]=LightBlue]
  ];  
  tabs={Grid[tab[[#]],ItemSize->{ws},Frame->True,FrameStyle->Thin,Background->bg[[#]]],
        Length[tab[[#]]]}&/@Range[Nk];
  htot=Total[tabs[[All,2]]]+(Length[tabs]-ncol)*hsep;
  h1=N[htot/ncol];
  tabs2=Table[{},ncol];   hs=Table[-hsep,ncol];
  For[i=1,i<=Nk,i++,
    htmp=hs+hsep+tabs[[i,2]];
    j=First@Ordering[htmp]; 
    hs[[j]]+=hsep+tabs[[i,2]];
    AppendTo[tabs2[[j]],tabs[[i,1]]];
  ];
  If[MemberQ[{"OrthBase","OrthBody","OrthFace","TetrBody","TrigPrim"},getSGLatt[sgno]],
     bztp="BZ type ("<>BZtype<>")",
     bztp=""]; 
  Column[{
    Grid[{{Row[{"No. ",sgno,If[dsg," (double)",""]}], SGSymStd[sgno], bztp}},
      ItemSize->Total[ws]+5*0.69,Alignment->{{Left,Center,Right}}],
    Grid[{Column/@tabs2},Alignment->{Left,Top}]},
    Dividers->{None,2->Black}, Alignment->Center]
]


(* ::Section:: *)
(*End of `Private`*)


End[]


(* ::Section:: *)
(*Another part of usages which use functions and variables*)


(*Note that this part of usages have used the functions/variables such as RotMat, SpinRotName, and str2Mulliken. 
So they have to be defined before these usages call them. And this is why this part is moved to the end of file.
IN PARTICLUAR, SpinRotName and str2Mulliken are still in the context `Private`, therefore we have to use the 
`Private` prefix to call them. *)
RotTimes::usage="RotTimes[Rname1, Rname2]    OR    RotTimes[Rname1, ..., Rnamen]\n"<>
   "performs the multiplication of two or more rotations by their name strings "<>
   "defined in the BC book, i.e. the names in\n"<>
   ToString@DeleteDuplicates@Join[RotMat[[1]]["CubiPrim"],RotMat[[1]]["HexaPrim"]];
DRotTimes::usage="DRotTimes[Rname1, Rname2]    OR    DRotTimes[Rname1, ..., Rnamen]\n"<>
   "performs the multiplication of two or more double-point-group rotations "<>
   "by their name strings defined in the BC book, i.e. the names in\n"<>
   ToString@DeleteDuplicates@Join[`Private`SpinRotName[[1]]//Values, `Private`SpinRotName[[2]]//Values];
str2Mulliken::usage="str2Mulliken[str]  converts the plain text string str to the Mulliken-form string. e.g.\n"<>
   "    str2Mulliken[\"2E1g'\"]  ==>  "<>str2Mulliken["2E1g'"]<>
   "   and   str2Mulliken[\"barA1''\"]  ==>  "<>str2Mulliken["barA1''"];
Mulliken2str::usage="Mulliken2str[MulStr]  converts the the Mulliken-form string MulStr to the plain text string e.g.\n"<>
   "    Mulliken2str["<>str2Mulliken["2E1g'"]<>"]  ==>  \"2E1g'\""<>
   "   and   Mulliken2str["<>str2Mulliken["barA1''"]<>"]  ==>  \"barA1''\"";
str2GammaLabel::usage="str2GammaLabel[str]  converts the plain text string str to the Gamma label string. e.g.\n"<>
   "    str2GammaLabel[\"GM3+\"]  ==>  "<>str2GammaLabel["GM3+"]<>
   "   and   str2GammaLabel[\"DT2\"]  ==>  "<>str2GammaLabel["DT2"];
GammaLabel2str::usage="GammaLabel2str[MulStr]  converts the the Gamma label string GMLstr to the plain text string e.g.\n"<>
   "    GammaLabel2str["<>str2GammaLabel["GM3+"]<>"]  ==>  \"GM3+\""<>
   "   and   GammaLabel2str["<>str2GammaLabel["DT2"]<>"]  ==>  \"DT2\"";


(* ::Section:: *)
(*Ending*)


EndPackage[]
