# SpaceGroupIrep
A mathematica package for irreducible representations of space group in BC convention. Here, "BC convention" means the convention used in the the famous
book “The mathematical theory of symmetry in solids” by C. J. Bradley & A. P. Cracknell (called the BC book hereafter).

## Communications
* Users can join WeChat group for communications: [the QR code](https://2h437cg9.kuaizhan.com/a/xobAfRbwGe/qrcode). If you cannot join the WeChat group, please contact me through gbliu(AT)bit(dot)edu(dot)cn.
* 点击上面链接加入SpaceGroupIrep使用交流微信群，如无法加入可邮件联系。

## Please refer to the following paper for details of the code:
* [Comput. Phys. Commun. **265** , 107993 (2021)](https://doi.org/10.1016/j.cpc.2021.107993)   ([arXiv:2012.08871](http://arxiv.org/abs/2012.08871))

If you use this package in your research, please cite the above paper.

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
* **examples/test.nb**  A simple test file or demo file.
* **examples/document and examples.nb**  A more detailed example file including some explanations.
* **doc/SpaceGroupIrep程序包说明.docx**  A Chinese version of the introduction of this package.
* **getBCsymmetry.py** A python tool to analyze the space group symmetries of vasp POSCAR file or cif file and convert the structure to BC convention.

## Dependence
* The functions `spglibGetSym` and `autoConvTraceToBC` depend on external python package [spglib](https://github.com/spglib/spglib). If these two functions are not used, **SpaceGroupIrep** works fine without spglib.
* `getBCsymmetry.py` depends on python packages: numpy, PyCifRW, and spglib. They can be install by

```bash
  pip3 install numpy PyCifRW spglib
```

## Installation
Place the directory **SpaceGroupIrep** containing at least the four files, i.e. SpaceGroupIrep.wl, AbstractGroupData.wl, LittleGroupIrepData.wl, and allBCSkLGdat.mx, under any of the following paths:
* `$InstallationDirectory`/AddOns/Packages/
* `$InstallationDirectory`/AddOns/Applications/
* `$BaseDirectory`/Applications/
* `$UserBaseDirectory`/Applications/

where `$InstallationDirectory` is the installation directory of Mathematica (**version ≥ 11.2**), and `$BaseDirectory`
and `$UserBaseDirectory` are the directories containing respectively systemwide and user-specific files loaded
by Mathematica. The concrete values of `$InstallationDirectory`, `$BaseDirectory`, and `$UserBaseDirectory`
can be obtained by running them in Mathematica because they are all built-in symbols. 

Then one can use the package after running **```<<"SpaceGroupIrep`"```**.

**Tips**: Use ```?SpaceGroupIrep`*``` to obtain a list of all public functions and vairables in the package. And similarly, you can also use ```?SpaceGroupIrep`*Rot*``` to obtain a list of all public functions and variables whose names contain "Rot".

## The steps to obtain LG IRs using the `getBandRep` function
### Choice one
1. Use `getBCsymmetry.py` to generate a BC-standard `POSCAR_6.vasp` file from `your_POSCAR`:
```bash
  getBCsymmetry.py -f your_POSCAR -p 0.001 -o  # this step can be omitted if your_POSCAR is alread BC-standard
```
2. Use `POSCAR_6.vasp` to do vasp energy-band calculation 
3. Use [`vasp2trace`](https://www.cryst.ehu.es/html/cryst/topological/vasp2trace.tar.gz) to calculate the `trace.txt` file 
4. Use `readVasp2trace` to get the trace data from the `trace.txt` file
5. Use `getBandRep` to obtain the LGIRs from the trace data
```mathematica
(* In mathematica after SpaceGroupIrep is loaded: *)
tr1=readVasp2trace["path-to-the-trace.txt"]; 
rep1=getBandRep[sgno, BZtype, tr1];  (*sgno is the SG number, and refer to the paper for BZtype*)
(* Then the LG IR data is stored in rep1["rep"] and can be shown for specific k point band bands: *)
rep1["rep"][[4]]//TableForm[#,TableDepth->2]&  (*show the LG IRs at the 4th k point*)
rep1["rep"][[1,3;;6]]//TableForm[#,TableDepth->2]&  (*show the LG IRs at the 1st k point and for energy levels from 3 to 6*)
```

Note (i): 1-3 are *pre-SpaceGroupIrep* steps and 4-5 are performed in SpaceGroupIrep. <br>
Note (ii): By default, the number of energy bands for the trace data calculated by `vasp2trace` is `NELECT` (the papameter for the number of electrons in VASP). In the case without spin-orbit-coupling (SOC) this may output all bands since there usually exists `NBANDS < NELECT` due to the double degeneracy of spin. But in the case with SOC, there always be `NELECT < NBANDS`, which results in only occupied bands are calculated by `vasp2trace`. In fact, `vasp2trace` can be runned with an argument that specifys the number of bands. For example, if one want to calculate 80 bands, just run `vasp2trace 80`. If want to all bands are calculated, one can also give a large number which is larger than `NBANDS`, say, 10000, and just run `vasp2trace 10000`. If one doesn't want to specify the number of bands every time, two tiny changes have to made to the source code of `vasp2trace`: <br>
```fortran
     if(IE<=ne) then   ! changed nele to ne in the 30th line of wrtir.f90
!    IF(IE>nele) EXIT  ! delete or comment the 55th line of chrct.f90
```

### Choice two
1. Use any primitive cell to do vasp energy-band calculation (supercell is not supported)
2. Use [`vasp2trace`](https://www.cryst.ehu.es/html/cryst/topological/vasp2trace.tar.gz) to calculate the `trace.txt` file 
3. Use `readVasp2trace` to get the trace data from the `trace.txt` file
4. Use `convTraceToBC` or `autoConvTraceToBC` to convert the trace data to those conforming to the BC convention
5. Use `getBandRep` to obtain the LG IRs from the *converted* trace data
```mathematica
tr0=readVasp2trace["path-to-the-trace.txt"]; 
tr1=convTraceToBC[sgno, tr0, P, p, stdR]; (* OR *)
tr1=autoConvTraceToBC[poscarFile, tr0]; (* this function depends on spglib *)
rep1=getBandRep[sgno, BZtype, tr1]; 
```
For the details of the arguments, please refer to the [paper](https://doi.org/10.1016/j.cpc.2021.107993) or run `?getBandRep`, `?convTraceToBC`, and `?autoConvTraceToBC` for help.

## Newly discovered typos in the BC book
In the supplementary material of the paper [Comput. Phys. Commun.  **265**  , 107993 (2021)](https://doi.org/10.1016/j.cpc.2021.107993) we list the typos found in the BC book. However, I found new typos in the BC book after the publication of the paper. They are listed here:
* In BC-Tab 5.1, for the abstract group $G_{96}^4$ (p.271), the generator $S$ for reps R7, R8, and R9 is wrong because it does not satisfy $S^2=P^3$. There should be $S=\kappa$ for them all. This affects the single-valued LG IR of $H$ point of the No. 230 space group.
* In BC-Tab 5.1, for the abstract group $G_{192}^2$ (p.281), the generator $S$ of R18 does not satisfy $S^2=E$. There should be $S=\{\{0,\varepsilon\},\{\varepsilon,0\}\}$. This affects the double-valued LG IR of $R$ point of the Nos. 222 and 223 space groups and $H$ point of the No. 230 space group.
