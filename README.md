# SpaceGroupIrep
A mathematica package for irreducible representations of space group in BC convention. Here, "BC convention" means the convention used in the the famous
book “The mathematical theory of symmetry in solids” by C. J. Bradley & A. P. Cracknell (called the BC book hereafter).

## Please refer to the following paper for details of the code:
* [arXiv:2012.08871](http://arxiv.org/abs/2012.08871)

## Capabilities of SpaceGroupIrep
* Elements of any space group, little group, Herring little group, or central extension of little co-group can be easily obtained. 
* Identify k-points. Support the cases in which one k-point may have two different names for different shapes of Brillouin zones due to different ratios of lattice constants.
* Show the little-group (LG) irreducible representations (IRs) for any k-point in  intuitive table form,.
* Show the space-group (SG) IRs for any k-stars in intuitive table form.
* Both single-valued and double-valued IRs are supported. 
* Calculate the decomposition of the direct product of SG IRs for any two k-stars. 
* Determine the LG IRs of Bloch states in energy bands in BC convention. And this works for any input primitive cell thanks to its ability to convert any input cell to a cell in BC convention. 
* Provide the correspondence of k-points and LG IR labels between BCS (Bilbao Crystallographic Server) and BC conventions.

In a word, the package SpaceGroupIrep is a database and tool set for IRs of space group in BC convention, and it is very useful for both study and research.

## Files
* **SpaceGroupIrep.wl** The main file containing most functions and data.
* **AbstractGroupData.wl** The abstract group data in BC-Tab. 5.1 (meaning the Tab. 5.1 in the BC book).
* **LittleGroupIrepData.wl** This file contains the data of LG IRs in BC-Tabs. 5.7, 5.11, 6.13, and 6.15.
* **allBCSkLGdat.mx** This file contains the BCS data of LG IRs collected from the output of [irvsp](https://github.com/zjwang11/irvsp). 
* **test/test.nb**  A simple test file or demo file.

## Installation
Place the directory **SpaceGroupIrep** containing at the four files (SpaceGroupIrep.wl, AbstractGroupData.wl, LittleGroupIrepData.wl, and allBCSkLGdat.mx) under any of the following paths:
* `$InstallationDirectory`/AddOns/Packages/
* `$InstallationDirectory`/AddOns/Applications/
* `$BaseDirectory`/Applications/
* `$UserBaseDirectory`/Applications/

where `$InstallationDirectory` is the installation directory of Mathematica (**version ≥ 11.2**), and `$BaseDirectory`
and `$UserBaseDirectory` are the directories containing respectively systemwide and user-specific files loaded
by Mathematica. The concrete values of `$InstallationDirectory`, `$BaseDirectory`, and `$UserBaseDirectory`
can be obtained by running them in Mathematica because they are all built-in symbols. 

Then one can use the package after running ```<<"SpaceGroupIrep`"```.
