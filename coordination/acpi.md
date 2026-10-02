# Userspace ACPI / AML

2026-10-02: Core proof15222 TERMINAL0:527 proof +91 flow checks, zero
unproved/justified. Frozen hashes intact; saved executor verified-proof.out.
Scope is core/legacy instantiation only, not service region callbacks.
Independent regular-copy snapshot /tmp/cubit-aml-region-service-7p_69er3
(pathfile /tmp/cubit-aml-region-service-path) now implements real namespace
reservation and completion callbacks: unresolved method-owned node before
selectors, existing End_Method cleanup, string conversions and owned-table
lookup, then region binding. Read_Binding reports Uninitialized for reserved
node. New region_service_tests exercises actual AML declaration+Field read,
method-returned selector, empty-buffer wildcard, duplicate/missing/self refs
and cleanup/budgets. Build83459 LIVE /tmp/cubit-aml-region-service.log; no
proof/native/reference claim for new callbacks. Core proof snapshot unchanged.
No shared source edits/locks/waiters.


2026-10-02: Private DataTableRegion executor progress:175 lifecycle tests
pass24243; legacy executor regression35647 TERMINAL0 passes2099 checks.
Focused core proof15222 remains LIVE confirmed by same-handle poll; log
/tmp/cubit-aml-region-executor-proof.log. All frozen input hashes intact.
Do not edit/restart while proof runs; no proof PASS claim. No other own live
jobs or shared lock/waiter. Snapshot /tmp/cubit-aml-region-executor-9ahwav0l.
Actual callbacks remain mock-only for this milestone: namespace/service and
readonly-input instantiations still need adaptation before full build.
After terminal report review, implement real reservation as an unresolved
method-owned node (Read_Binding must report Uninitialized), selector conversion
and owned-table completion; prove service instantiation and compare real AML
end-to-end before promotion. Keep module-level lazy declarations explicit.


2026-10-02: Prior turn made progress (string helper integration). Private
DataTableRegion executor implementation in /tmp/cubit-aml-region-executor-9ahwav0l
(pathfile /tmp/cubit-aml-region-executor-path): new Reserve_Region and
Complete_Region generic callbacks; name reservation precedes three left-to-right
Operand calls, completion follows all three, existing End_Call handles cleanup
on all execution exits. Legacy empty-input wrapper explicitly rejects region
creation. Mock lifecycle24243 TERMINAL0:175 checks incl nested method selectors,
object arguments, duplicates, self-reference, all prefixes and budgets0..40.
Not integrated: real namespace callback instantiation and readonly fixture
need adaptation before full service build. No shared code changes/locks/waiters.
Frozen input manifest saved; launching focused core proof next. Actual table
selection and string coercion are NOT yet connected by this implementation.


2026-10-02: Reference8358 TERMINAL0 integrated50 value comparisons match;
all8 batches retain known control-reproduced shutdown allocation diagnostic.
Not a clean reference-tool run. Host31990 passes65542; frozen source hashes
and promoted byte comparison pass. Bash syntax/Python compilation/scoped
diff checks pass. README/coverage updated honest scope. This turn progresses
production helper + registered host/proof/reference workflow. No own live
jobs/locks/waiters. Actual selector adaptation, namespace reservation/binding,
error ordering and cleanup remain before DataTableRegion can run.


2026-10-02: Conversion helper promotion succeeded under shared lock;
registered31990 TERMINAL0 all65542 host checks. Three implementation/fixture
files byte-match frozen proof/native source. Added conversion_runner.adb and
acpica_string_conversions.py plus GPR/run-acpica registration under lock.
Reference8358 LIVE, /tmp/cubit-aml-string-reference-integration.log; integrated
runner uses --subdirs=string-integration and --runner override. Do not edit
these inputs until terminal. No locks/waiters. Production interpreter still
does not call new string helper; next implement selector adaptation/lifecycle.


2026-10-02: String helper promotion53496 TERMINAL1:30s lock timeout;
NO shared source or build registration changes were made. Promotion ready
/tmp/cubit-string-conversion-promote.py, must hold shared build.lock.
Frozen pure-source hashes revalidated. Reference71749 TERMINAL0:50 values
match ACPICA with exact control-reproduced shutdown diagnostic retained in
all8 batches. Report reference-direct-results/report.json and control.log
under /tmp/cubit-aml-string-conversion. Native66317 unit compile passes;
64-byte integer and144-byte buffer function frames (not whole-stack proof).
No own live jobs/locks/waiters. Progress this turn is new differential and
native evidence, not production integration. Next acquire lock once available,
promote helper, run registered test with disjoint outputs, document limits;
then implement selector/object adaptation and full DataTableRegion lifecycle.


2026-10-02: Prior turn progress verified. String conversion reference54565
and direct84692 failed strict diagnostic gate: ACPICA reports4 outstanding
cache allocations on shutdown. Minimal constant-return control reproduces
exact same4 diagnostic, retained control.log. Classified71749 TERMINAL0:
50 converted values match,8 batches contain exact baseline shutdown error;
NOT an error-free ACPICA run. Any other error still fails. Native66317
TERMINAL0 unit compile against frozen CuBit runtime (not full service link).
Claim promotion new aml_coercions-strings.ads/adb +string_conversion_tests.adb
and aml.gpr/run.sh registration under lock. No native/global staging edits.
Private reference evidence /tmp/cubit-aml-string-conversion; pure-source proof
manifest unchanged. Conversion remains unconnected to opcode execution.


2026-10-02: Progress this turn: owned-table selection promoted and registered
integration8555 TERMINAL0 (33811 selection +847 field checks); production
and fixture bytes match verified snapshot. Syntax/scoped diff checks pass;
README/coverage updated. String conversion27027 TERMINAL0:65542 tests,
94 proof +4 flow checks, zero unproved/justified. Frozen hashes intact, saved
/tmp/cubit-aml-string-conversion/verified-proof.out. Conversions remain private
and unconnected: normative/reference comparison and native build still needed.
Live online ACPI6.6 chapter19 returned403; pinned ACPICA exconvrt.c confirms
implemented algorithm but is not normative proof. No own live jobs/locks/waiters.
Next connect complete selector conversion and declaration lifecycle to actual
DataTableRegion; do not claim ASLTS improvement from helpers alone.


2026-10-02: Initial selection lock waiter89921 terminal1 (timeout), later
nonblocking promotion succeeded under lock. Registered test8555 LIVE, log
/tmp/cubit-table-selection-integration.log; separate outputs. Selection
implementation exactly from verified private snapshot. New independent
/tmp/cubit-aml-string-conversion has implicit integer/buffer string conversion
with exact byte contracts. Compile errors in10833/49163 resolved; final27027
LIVE proof, hosted65542 checks passed. Frozen input-hashes.json. No edits to
proof inputs until terminal. These conversions are unconnected/unpromoted;
need normative/reference confirmation and native build before integration.
No shared lock held or waiter.


2026-10-02: Prior goal turn made progress (literal integration). Claim owned-table selection helper promotion: aml_table_backing.ads/adb and new selection_tests.adb; register in aml.gpr/run.sh under held build lock. Existing private proof23+4flow reviewed, zero unproved/justified. Promotion preflights all frozen hashes and unchanged backing baseline. Next integration tests/proof use disjoint selection-integration outputs. No source overlap with peer scopes.

2026-10-02: Integrated test95930 TERMINAL0: 160 literal +1811 readonly-input
checks pass. All six promoted source/fixture files byte-match frozen verified
snapshot. Bash syntax, Python compilation and scoped diff checks pass.
README and coverage ledger updated. No own live jobs, locks or waiters.
This turn made implementation progress: literal materialization integrated.
Next: actual DataTableRegion execution, including owned-table lookup, selector
coercion and declaration reservation/cleanup; reference probe diagnostics
remain explicitly classified as failure, not clean conformance evidence.


2026-10-02: Literal promotion succeeded under shared lock in terminal58047;
all nine files installed. Registered hosted test95930 LIVE, disjoint
--subdirs=literal-integration; log /tmp/cubit-aml-literals-integration.log.
No shared lock held or waiter. Region probe now correctly rejects generic
ACPI errors; repeat SELF still unresolved and ORDR returns3 but reports
outstanding cache allocations at shutdown. Probe assertion FAILED and retained
in report; do not represent that repeat as a clean ACPICA pass.


2026-10-02: Resumed after table-parser discussion (no implementation progress in that discussion). Literal proof84501 terminal0 verified from final report: 3046 proof +414 flow, zero unproved/justified. Saved /tmp/cubit-aml-literals-proof-r3.out; all125 hosted and426 native input hashes intact; production bytes match. Claim promotion of nine preflighted ACPI-owned files under shared build lock, then registered tests in disjoint literal-integration outputs. No other own live job or waiter. Region probe39493: ORDR returns3 cleanly; SELF emits generic ACPI Error unresolved/uninitialized REG0, so is NOT a pass despite exit0. Probe error detection needs tightening before automation. RecordFlux MADT remains recommendation only.

2026-10-02 verifiedwait84501: samehandle remainsLIVE after30spoll. Actual
gnatwhy3 processes1611083/1616131 activeon2Operandinstantiations (CPU99.9/109
atobservation), advancingtonewprocess. No errorsreported; notproofPASS.
Frozenliteralinputs unchanged, no sourceedits/otherjobs/lock/waiter.
Continue samehandle; promotionmustawaitterminal0/reportreview.


2026-10-02 84501 confirmedLIVE samehandle after30spoll; literalpromotion
preflight9filesstillPASS, no sourcechanges/lock/waiter. Priorturnprogress
ownedtablelookup; thisturnverifiedwait+nextselectorcoercionreview. PinnedACPICA
exconvrt.c587+ implicitInteger=>String uses8/16 uppercasehexcharacters with
leadingzeros (NOT explicitToHexString behavior). Buffer=>String uses0x-prefixed
twohexcharacters perbyte separatedbyspaces (length5*n-1; empty=>empty),
notrawASCII decoding. Neednormativecross-check andreference fixtures before
implementing selectorcoercion. DataTableRegion TermArg=>String is broaderthan
stringliterals; do notshortcutbuffer/integerzero towildcard. Allproofinputsfrozen.


2026-10-02 independent DataTableRegion prerequisite35372 TERMINAL0:owned
tableFind_Table/Matches_Table/Valid_Span helpers plus33811hostedchecks and
SPARK23proof+4flow zero unproved/justified. Snapshot /tmp/cubit-acpi-table-selection-8tr2yd1z (pathfile
/tmp/cubit-acpi-table-selection-path), verified-input-hashes.json, savedproof
/tmp/cubit-acpi-table-selection-proof.out, log /tmp/cubit-acpi-table-selection.log.
Only aml_table_backing.ads/adb differfromliteralbase; helpersunconnected and
unpromoted. Rejectinvalidcount/anyinvalidspan beforelookup; firstmatching
indexproven, OEMwildcards viaexistingidentifiers. No physicaladdressinputs.
Do notoverwritefrozenliteralproofinputwithnewhelperwhile84501LIVE.
84501 samehandleverifiedLIVE; no sourceedits/sharedlock/waiter. Nextconnect
helpertoactualDataTableRegion afterliteralproof/materialization promotion,
withselectorcoercion+methodowner+declarationpath/error/lifetime tests.


2026-10-02 literalpromotion prepared /tmp/cubit-aml-literals-promote.py,
--check9filesPASS (4existingproduction/testinputs,2newfixtures,3registrations).
Do notexecute mutationuntil84501 proofterminalPASS; holdsharedbuild.lock.
84501 remainsLIVE samehandle after30spoll, sourcesfrozen. Finalnative/70+212
ACPICA/broadhostedregressions allterminalPASS asrecordedbelow. Noothersessions,
sourceedits, locksorwaiters. Prior turnprogressnative+reference/regressions;
currentturnverifiedwaitandpreflightprep. No whole-goalcompletion claim.


2026-10-02 finalr3native13193 TERMINAL0;426hashesintact and3changedproduction
filesbyte-matchprivateproofinputs. Finalr3ACPICA12601 TERMINAL0:70literal
+212typedcomparisonsPASS. 84501 proofconfirmedLIVE samehandle; allfrozen
hostedinputhashesintact. 68827 TERMINAL0 disjointlegacyregressions PASS, logfile
/tmp/cubit-aml-literals-regression.log. No sharededits/lock/waiter.
No literalpromotion beforeproofterminalpass+regressioncomplete.


2026-10-02 84501 samehandleLIVE literalr3proof;160/1811/1523hostedpassed
andproofnowphase3 afterfixes, noobservederrornotyetPASS. 13193 LIVE finalr3
nativerebuild, /tmp/cubit-aml-literals-native-r3.log; nativeinputsrefreshed
3productionfiles+hashes. 12601 LIVE finalr3ACPICA70literals+212typed,
/tmp/cubit-aml-literals-acpica-r3.log. Frozenliteralproduction unchanged.
Prepare disjointregressionoutput forlegacycorecall/coercion/store etc.
No sharedsourceedits/lock/waiter. Prior goalturnprogressFieldpromoted;
current verification continuesliteralprerequisite, notDataTableRegion yet.


2026-10-02 registeredField11334 TERMINAL0:1523declaration+978boundary checks
PASS afterpromotion; sourcematchesproof/nativeinputs. Fieldmilestoneclosed
withdocumentedlimitedforms; no upstreamASLTS orfullinterpreterclaim.
Literal21930 TERMINAL1 flowE0007 dynamicarrayconstraintusesvariableText.Length.
Changedto localconstantL (samepatternasexistingintegercoercionbranch); no
contractweakening. Priorconditionalaggregate translationerrorresolved.
84501 LIVE rebuild/proofr3, /tmp/cubit-aml-literals-proof-r3.log.
Allliteralinputsrefrozen; r3native/ACPICA stillpending.
No currentlocks/waiters. Fullgoalactive.


2026-10-02 FIELD DECLARATIONS PROMOTED after87954terminalPASS underacquired
sharedlock; lockreleased. Registeredfield_declaration_tests+field_boundary_tests
inGPR/run.sh; runnernowexecutesFieldAML. Docsupdatedlimitsandproofscope.
Do notrerun /tmp/cubit-aml-declarations-promote.py. Sourcebaseline-after-fields
forliteralwork shouldnowmatchshared. 11334 LIVE registeredhostedchecks, /tmp/cubit-aml-declarations-registered.log.
Allpromotedsource/fixturebytesmatchverifiedinputs; literalfuturebaseline
matchespromotedsharedstate. Bashsyntax/Pythoncompile/scopeddiffcheckPASS.
Literal97104 TERMINAL1 prooftranslationcannotuntangleN_IF_EXPRESSIONin
conversionaggregate; native59415 TERMINAL0 (preworkaround), ACPICA70passed.
Replacedaggregateconditionalcoercioncallswithordinarybranches, contracts
unchanged. 21930 LIVE r2build160/1811/1523thenproof; frozen literalinput
manifestrefreshedfor thissole namespacechange; r2native/ACPICA pending.
No sharedlock/waiter. Fieldgoalportionprogress; fullgoalremainsactive.


2026-10-02 87954 TERMINAL0 stagedFieldproof2458proof+322flow zerounproved/
justified. Saved /tmp/cubit-aml-declarations-small-proof.out. Promotion
nonblocklockattempt exited1busy; 27631 TERMINAL1 bounded30slockwaitexpired; no mutation.
No lock/waiter; waitpeerwindowbeforeanotherattempt. Codefinalsmallxpdqhn98.
Literal23014 TERMINAL0 160literal/1811readonly/1523Field checks; 33042
TERMINAL0 ACPICA70 actualliteralcomparisonsPASS. Private literalinputsfrozen
manifest /tmp/cubit-aml-literals-_lduhd4e; baseline-after-fields.json tracks
only4changedexistingfiles againststagedFieldbase (afterpromotionwillmatchshared).
97104 LIVE literalproof, /tmp/cubit-aml-literals-proof.log;
notnativeverified/promoted. Literalnativesnapshot preparedpathfile
/tmp/cubit-aml-literals-native-path. Noothersharedchanges.


2026-10-02 while87954 Fieldproofcontinuesfrozen, independentliteralvalue
prerequisite started /tmp/cubit-aml-literals-_lduhd4e (pathfile
/tmp/cubit-aml-literals-path), copiedfinalstagedFieldbase. Adds Materialize
callback toExecute_With_Input, decodesliteralstrings/buffers into realtyped
namespaceobjects outsideintegercoercion, with explicitValue_Limit. Legacy
noallocatoradapterrejects. Namespace callbackallocatesboundedobjects and
caches32/64 conversions. LiteralSize/BufferLength existingdecoderbounds remain;
notfullTermArgbufferinitializer support orreclamation. Newliteral_value_tests
coverreturn/empty/nestedargument/localtype+size/budget/exhaustion. 97457terminal4
onunusedtestimport, fixed; 23014 TERMINAL0 hostedliteral160/readonly1811/Field1523PASS.
New70caseACPICA literalcomparison running; no promotion ofeithernewfeatureyet.
Fieldpromotion scriptmuststilltargetsmallxpdqhn98, NOT newliteralssnapshot.
No sharededits/lock/waiter. Need literalproof/native/ACPICA afterhostedpass.


2026-10-02 verifiedwait87954: samehandle remainsLIVE after30spoll; actual
gnatwhy3 process1538834 active on service-instantiated Operand (CPU73.9%).
Frozeninputhashesmatch; promotionpreflightpassed. No sourceedits, otherjobs,
locksorwaiters. Prior turn verifiedwait. Continue sameproofhandle; no restart
or prematurepass. Finalstagedcode hasnative/54ACPICA/1523+978hosted/broad
regressionspassed, butfinalproofrequired beforepromotion.



2026-10-02 boundary48816 TERMINAL0 978checksPASS. Alltruncatedmethodprefixes,
budget0..100 andrepeatinvocationcleanup passedfinalstagedproductioncode.
Promotion scriptnowincludes boundaryfixture+GPR/run.sh registration (10files),
sourcefrom /tmp/cubit-field-boundaries-y3ih_cax; productionproofinputsunmodified.
87954 remainsLIVE samehandle after30sobservation. Nootherownjobs/lock/waiter.
Thisturnprogress: newmalformedinput/budget/cleanup regressionverified;
finalproofnotyetpass andcodeunpromoted.


2026-10-02 87954 samehandleconfirmedLIVE, finalstagedsourcefrozen.
Independent extra boundaryfixture at /tmp/cubit-field-boundaries-y3ih_cax,
pathfile /tmp/cubit-field-boundaries-path. Tests alltruncatedFieldmethodprefixes,
budget0..100, repeatcallcleanup; new privateGPR usesfrozenproduction inputs
withoutchangingproofsourcegraph. First12374 terminal4 unusedAML_Decode import
removed; rebuildrunning, /tmp/cubit-field-boundaries.log. Notyetregistered,
no sharededits/lock/waiter. Promotionpreflight9filesstillPASS; do notpromote
until87954 terminalpass. Prior turn verifiedwait.


2026-10-02 continuation87954 verifiedLIVE samehandle; finalstagedsources
remainfrozen. Priorgoalturnprogress (originalproof+finalregressions+probe).
Normative follow-up: ACPI6.6 section19.6.25 specifies all3 DataTableRegion
selectors evaluatedasstrings, regioncoversheaderthroughdeclaredlength.
Source https://uefi.org/sites/default/files/resources/ACPI_Spec_6.6.pdf .
Grammar TermArg=>String confirmedin6.5 section19.2; no retrievednormative
passage yetsettlesmoduledeferredtiming. Do notinfernormativeeager/deferred
rulefromACPICA warningbearingprobe. Current Operand supportsliteralstrings
onlyforintegercoercion; fullselectorimplementation needsliteralstringvalue
materialization plusnamed/local/argument/method-resultvalues, notliteralonly
parsing. Preserve missingmodule-tablebehavior asopenaudit.
No sourceedits/nativejobs/lock/waiter. Await87954 beforepromotingFieldcode.


2026-10-02 4531 TERMINAL0 original Fieldproof2910proof+402flow, zero
unproved/justified. Saved /tmp/cubit-aml-declarations-proof.out. Finalsmall
87954 proofstillLIVE, inputsfrozen, no observederror notyetPASS. 76063
TERMINAL0 finalsmall regressions field847/typed11938/call4818/methodstorage141/
store4137/service1051688 +FADT26282/46748PASS. Bothvariant source snapshots
retained; small preferred forpromotion, whichstillrequires87954finaloutcome.

Independent futureDataTableRegion probe72386 TERMINAL0, artifacts
/tmp/cubit-acpi-region-semantics/{probe.py,report.json,TYPE.log,READ.log,DYNT.log,GOOD.log}.
ACPICA20260408 methoddeclmissingtable failsAE_NOT_FOUND beforeObjectType;
goodmethod returns10. ModulemissingregionObjectType returns10; fieldreadlater
failsAE_NOT_FOUND, butmoduleinitialization alsoemitsAE_TYPE Opcodeisnotdeferred.
Do nottreatthis warningbearing reference asnormativecorrectness; specaudit
neededbeforestaticload design. Pinnedsource dsopcode.c621+ evaluates3string
operands viaresolverandtablelookup; psopcode marksDataTableRegion AML_DEFER.
Do not implementliteral-only selectors ascompleteTermArg support.
No shared sourceedits/lock/waiter. Currentturnprogress: terminalverification
evidence andsemanticprobe changesnextdesign; fullgoalnotcomplete.


2026-10-02 final staging84935 TERMINAL0:1523hosted+54ACPICA PASS.
87954 LIVE finalsmallsource service-instantiated proof -j1; logfile
/tmp/cubit-aml-declarations-small-proof.log. Original4531 stillLIVEsamehandle
(originalexecutor/largecopynamespace), notcancelled/restarted; inputsfrozen.
Finalnative38801 alreadyPASS489744byteDefine_Fieldsframe,426hashesmatch.
Prepared /tmp/cubit-aml-declarations-promote.py, --check preflight9filesPASS.
DO NOT RUN mutation untilfinalproof andregression validated; sharedlockrequired.
No sharedsourceedits/lock/waiter. Existing broad2151 regressionspassoriginal
version; 76063 LIVE finalstaged broadregressioncheck,
/tmp/cubit-aml-declarations-small-regression.log, separate outputs. ActualFieldonly,
DataTableRegion stillAPIbound, flags/access forms limited, noASLTSadvance.


2026-10-02 smaller staging53633 TERMINAL0:1523hosted+54ACPICA PASS.
84025 firstnativeTERMINAL0 butframe973168bytes grew due redundantloop
snapshots. Removed Tree/ValuesLoop_Entry invariants (flow preserves unchanged
components; atomicfailure/ValidContext postcontracts unchanged). 38801 r2native
TERMINAL0 finalDefine_Fields489744staticbytes vsbaseline725328; all426hashes
match. Stilllarge checkedcontractframe, notwhole-stack proof. Current
smallxpdqhn98 inputsfrozen manifest, 84935 LIVE finalhosted/ACPICA r2rerun,
/tmp/cubit-aml-declarations-small-r2-check.log.
Original4531 proofstillLIVE onoriginaljgf0n0nz, noerrorsobservedbutnotPASS.
Do not promoteoriginallargecopy version; prefer stagedsmallcandidate once
proved andregressionverified. No shared edits/lock/waiter.


2026-10-02 2151 TERMINAL0 legacyfield847/typed11938/call4818/methodstorage141/
store4137/service1051688 plusFADT checksPASS. 4531 originalproofstillLIVE
onfrozenjgf0n0nz; do not mutate/restart. In independent smallcandidate
/tmp/cubit-aml-declarations-small-xpdqhn98, replaced explicitfullStatecopy
with boundedname/offset/bit descriptor staging, prevalidateallfailuresthencommit.
Preserved atomicfailure andcontextvalidity contracts. 53633 LIVE1523hosted
+54ACPICA checks, /tmp/cubit-aml-declarations-small-check.log. Newnative
snapshot prepared; path /tmp/cubit-aml-declarations-small-native-path.
No shared sourceedits/lock/waiter. Originalnative725328framebaseline retained.
Smallcandidate notproved/nativevalidated yet; no promotion.


2026-10-02 97392 TERMINAL0 ACPICA54 actualField comparisonsPASS.
92772 TERMINAL0 native link, all426 hashesmatch; snapshot
/tmp/cubit-aml-declarations-native-ajgm4_k7. Frozen123 hosted inputsalsoallmatch.
4531 confirmedLIVE samehandle. Native Define_Fields frame725328staticbytes:
transaction candidatecopies namespace, so thisneedsstack/copyoptimization
beforeproduction; nativechecked builduses64MiBbudget, notstackproof.
No shared edits/lock/waiter. 2151 LIVE separate regression.gpr/outputdir for
legacy service-field/typed/call/methodstorage/store/service tests whileproof
reads frozen inputs. Log /tmp/cubit-aml-declarations-regression.log. Previous turnprogress; current
turn independent nativeandACPICA evidence, pendingproofnotPASS.


2026-10-02 private Field declaration build39848 TERMINAL0:1523 actual AML
declaration/cleanup checks +1811 readonly input checks PASS. Initial fixture
name/order and use-visibility compilation errors fixed; no contracts weakened.
4531 LIVE SPARK selected executor/service instantiated namespace proof,
/tmp/cubit-aml-declarations-proof.log. 97392 LIVE54ACPICA comparisons using
actual CuBit Field opcode (region still API-bound), log
/tmp/cubit-aml-declarations-acpica.log. Same-handle polls confirmbothLIVE.
Freeze allinputs /tmp/cubit-aml-declarations-jgf0n0nz; frozen-input-hashes.json
records source/fixtures. No shared source/GPR/script edits, no lock/waiter.
Private acpica_compare.py ROOT points to parent.parent forprivate executable;
do not promote that harness-root adaptation. Production changes: executor
ads/adb, namespaceadb; readonly_input_tests gains rejectcallback; new
field_declaration_tests; service_field_runner now emitsactualField; updated
comparison scope. No native build yet. Support currentlymethod-timeAny/ByteAcc
NoLockPreserve plusordinaryAccessField; unsupportedflags/connections fail.
Candidate namespacecopy preserves failureatomicity but increasesstack; native
stack/lifetime audit remainsrequired. DataTableRegion andstaticload stillpending.
This turn progress: actual Field opcode execution+hostedevidence, notcompletion.


2026-10-02 actual Field opcode private implementation started in
/tmp/cubit-aml-declarations-jgf0n0nz (pathfile /tmp/cubit-aml-declarations-path).
Execute_With_Input adds Define_Fields callback; parses5B81 package/name/flags,
charges list bytes before mutation. Namespace callback binds method-owned
fields transactionally through candidate state; presently only Any/ByteAcc,
NoLock/Preserve and ordinary AccessField forms supported, others rejected.
DataTableRegion still uses service binding API; static load remains unsupported.
Private tests exercise repeated fields/type5/cleanup, malformed/duplicate/bounds
and budget exits; service_field_runner changed to actual Field AML for54ACPICA
comparisons. 39848 LIVE initial build/tests; prior54861/8358/58058/83372 failed
on fixture names or Ada visibility, all terminal. No shared source edits or
lock/waiter. Full correctness/proof/native evidence not yet available.


2026-10-02 field evaluation PROMOTED under acquired sharedlock; lockreleased.
20247 TERMINAL0 selected proof2701+flow379 zero unproved/justified; native
96775 andACPICA8125 terminal0. All promoted source/fixture bytes match final
private verified inputs. GPR/run.sh/run-acpica.sh registered underlock.
Coverage/README updated with precise scope, API-bound declarations, bounded
buffer allocations without reclamation, no newASLTS or whole-stack claim.
Bash syntax, Python compile and scoped diffcheck PASS. 44826 TERMINAL0 registered
hosted847/1811 checks PASS using --subdirs=field-integration, separate outputs;
log /tmp/cubit-aml-fields-registered.log. Do not rerun one-shot promotion.
All own jobs terminal; no lock/waiter. This turn progress: verified and
promoted actual service field evaluation. Next: actual AML DataTableRegion/Field
declaration execution. Full goal remains active, not complete.


2026-10-02 20247 TERMINAL0:2701 proof +379 flow, zero unproved/justified.
847field/1811input checks PASS. Final proof report saved
/tmp/cubit-aml-fields-live-guard-proof.out, manifest refreshed for guarded
namespace source. 96775native/8125ACPICA54 terminal0. Promotion ready;
shared lock needed, compositor48297 live at last note. No current waiter.


2026-10-02 continuation: 20247 confirmed LIVE by same-handle polls; prior
turn classified verified wait. 96775/8125 terminal0 reconfirmed; all426
native hashes and final changed-production private/native byte comparisons
PASS. Hardened /tmp/cubit-aml-fields-promote.py: preflight all16 destinations,
baseline hashes and registration anchors before first write. --check PASS.
Still no promotion or shared source edits; inputs remain frozen. No lock/waiter.


2026-10-02 proof93419 TERMINAL1:2696proved +379flow, one unproved
Field_Data precondition Present(Tree,Node) in new lookup. Added explicit
Root/liveness guard before accessing field descriptor; no contract weakened.
20247 LIVE guarded rebuild847/1811+full selected proof. Private sources
frozen /tmp/cubit-aml-fields-ref-v4sqlru5. 96775 TERMINAL0 native link; all426 snapshot hashes match.
8125 TERMINAL0 ACPICA54 field comparisons after guard. 20247 stillLIVE
on current same-handle poll; no restart. Shared baseline hashes match.
Prior reports remain historical; do not promote old failed proof.
No shared source edits, no lock/waiter. Prior turn progress; this turn verified
same live proof, diagnosed concrete obligation, and implemented guard.

2026-10-02 all final runtime tests terminal: 50884 TERMINAL0 ACPICA54field/
212typed/26dynamic with new Service.Invoke runner; 73593 TERMINAL0 legacy
regressions; 97788 TERMINAL0 native link,426 hashes intact. 93419 remains
LIVE after repeated same-handle polls (last30s observation), no report yet;
no unproved/error diagnostic observed so far, NOT a passed proof. No lock
or waiter. Inputs frozen at /tmp/cubit-aml-fields-ref-v4sqlru5; all shared
source baseline hashes still match. Review diff /tmp/cubit-aml-fields-review.patch
contains existing-file edits; new helper/fixtures remain in private paths.
Next: poll93419 (do not restart); fix any actual proof failures, then acquire
sharedlock for /tmp/cubit-aml-fields-promote.py and update coverage/docs.
Do not claim promotion, declaration opcodes, ASLTS advancement, or buffer
reclamation. This turn made actual service field-evaluation progress and
collected native/independent-reference evidence; verification is still live.

2026-10-02 final by-reference field evaluation private at
/tmp/cubit-aml-fields-ref-v4sqlru5. Tagged backing variant51952 TERMINAL1
GNAT BUG why-gen-records; native61655 TERMINAL1 runtime lacks tagged types.
Replaced with ordinary record + explicit aliased parameters, preserving read
only input and avoiding a namespace copy. 93419 LIVE final proof after847
field/malformed-span and1811input checks PASS. 97788 native TERMINAL0,426
hashes match. 73593 legacy regressions TERMINAL0 (service1051688,request92680,
endpoint589972, call/typed/inspect/store/namespacefield allPASS). 50884
final ACPICA logs show54field+212typed+26dynamic PASS; pollhandleterminal.
All sources remain private/frozen; no lock/waiter. Promotion script
/tmp/cubit-aml-fields-promote.py (read before use; held sharedlock required)
registers new helper/runner, adds tests/proof/ACPICA entry, and routes ordinary
table_runner through Service.Invoke. Do not promote before93419 proofpassed.
Actual declarations still via binding APIs, not DataTableRegion/Field opcodes.
Buffer reads allocate in bounded value store; explicit Value_Limit works but
reclamation/lifetime remains incomplete. Current native Service.Invoke reports
96byte dynamic frame, not whole-call-chain/contract-copy proof.

2026-10-02 field evaluation private implementation in
/tmp/cubit-aml-fields-qca7mie9: mutable lookup can materialize wide fields into
AML buffers, explicit Value_Limit; inspection does not allocate. Service Invoke
connects retained tables to actual namespace evaluator; declarations still API.
5146 TERMINAL1 after838new/1811input and regressions PASS: GNATprove does
not support external instantiation of nested generic from invariant-bearing
namespace package; also missing unconstrained-output precondition. Reworked
into separate tagged AML_Table_Backing object and non-generic namespace
Invoke_With_Tables, preserving/strengthening Service invariant and removing
namespace copy. 97567 LIVE rebuild/tests/proof; all private inputs frozen.
94440 earlier comparison-runner build TERMINAL0, but new backing version
needs rebuild. No shared changes/jobs/lock. Prepared focusedACPICA54cases
script acpica_service_fields.py; real opcode declarations/ASLTS remain pending.

2026-10-02 readonly executor input PROMOTED: 38498 TERMINAL0, 418 proof
checks +75 flow checks, zero unproved/justified/Assume. 73901 TERMINAL0
promoted owned executor ads/adb and new readonly_input_tests under held lock;
registered test in AML GPR/run.sh. 39703 TERMINAL0 shared registered1811
checks. Sources byte-match private verified inputs; Bash syntax/diffcheck PASS.
80688 native link and 83352 ACPICA212typed/26dynamic pass final sources.
Evidence /tmp/cubit-aml-input-purpose-proof.out, -purpose-verify.log,
-purpose-native.log, -purpose-acpica.log, -registered.log. No own live jobs,
locks or waiters. Do not run the one-shot promotion script again.
Next: namespace/service actual table-input adapter and DataTableRegion/Field
declarations, wide field buffer value materialization, then rerun ASLTS.
Input interface forbids explicit copies; choose a limited actual backing type
when by-reference parameter semantics are required. No broad AML completion.

2026-10-02 readonly-input proof42540 TERMINAL1: three nested Global input
annotations missing, all 418 proof checks discharged but flow failed. Added
Input to Operand/Inspect/Dispatch inputs; top-level Global null unchanged.
Also added Binding_Purpose (Inspect/Evaluate) so field metadata is not coerced
to integer during ObjectType. 38498 LIVE final core proof after 1811 new
checks and call4818/typed11938/inspect234/store4137/methodstorage141/namespace
field351/service1051688 PASS. 80688 native link TERMINAL0, all424hashes match.
83352 final ACPICA TERMINAL0 typed212/dynamic26 (focused, not ASLTS).
Sources remain private /tmp/cubit-aml-input-67ypbglg, frozen during38498.
No lock/waiter. Promotion script /tmp/cubit-aml-input-promote.py must be run
under shared lock only after proof passes. Next actual table-field adapter must
handle wide fields as AML buffers, not truncate to integers; pure lookup alone
cannot allocate namespace value objects, so value materialization needs its own
mutable callback or an appropriate inline value representation. No actual
DataTableRegion/Field opcode or service table-context integration claim yet.

2026-10-02 AML readonly context private implementation in
/tmp/cubit-aml-input-67ypbglg: Execute_With_Input threads a limited immutable
context through lookup and recursive calls; no-input Execute_Typed delegates
without changing existing callers. New fixture >1MiB backing and recursion.
17775 compile TERMINAL0; 38294 fixture compile TERMINAL4 missing numeric
operator visibility, fixed. 42540 LIVE private host regressions then core
proof; input sources frozen. No shared edits or lock held. Previous goal
turn completed capture promotion/proof, so progress rather than blocked.
Cleanup dependency note: current ACPI evidence/workspaces are all under /tmp;
no .build-workspaces dependency is recorded or used by this active work.
Keep the /tmp paths recorded here; cleanup request explicitly excludes them.

2026-10-02 PROMOTED dynamic capture under held shared lock (nonblocking
acquisition succeeded). Kernel source and native fixture match tested private
versions. Provisioning GPR and run.sh hosted/proof registration installed.
8818 TERMINAL0: registered 1795 checks, 29 proof checks + 4 flow checks,
zero unproved/Assume. /tmp/cubit-acpi-provisioning-registered.log and
-registered-proof.out. Scoped diffcheck, Bash syntax and Python compilation
PASS. Compiled private source manifest matches shared tree except generated
kernel/src/build.ads. All documentation updated; no own jobs/waiters/locks.
Previous goal turn was implementation/native-test progress; this turn completed
promotion and registered validation. Next remaining integration is authenticated
startup sizing and process-owned grants; ACPI_Native_Instance still static and
ACPI_Launch.Configure still four words [observer, provider, slot, zero]. Do not
invent missing startup provider integration or consider overall goal complete.
Shared startup/syscall/procmgr ownership acknowledgment remains outstanding;
independent AML DataTableRegion/Field execution still available work.

2026-10-02 dynamic capture ready for promotion: 68072 TERMINAL0 final
40-table/1,329,227-byte MB1+MB2 native cases PASS. 9297 allocation failure
MB1+MB2 PASS before larger QEMU fixture hit aggregate ROM bound; final
fixture uses 33x40000-byte tables. 37380 failed only injection syntax (GNAT
scalar out returned in RAX); fixed injection verified no allocation/publication.
All 10 relevant cases now passed across normal, large, count, late and OOM.
42631 bounded promotion waiter TERMINAL1; 92350 read-only lock owner check
TERMINAL0 reports external flock PID1115590. No own jobs/waiters/locks.
Changes still PRIVATE: /tmp/cubit-acpi-sized-boot-4gedz_zg, fixture
/tmp/cubit-acpi-sized-fault-native.py; compiled-source-hashes.json retained.
Only workspace build.ads changed since private baseline; owned ACPI source
still matches original. /tmp/cubit-acpi-sized-final-hashes.json records final
source/binary/fixture; /tmp/cubit-acpi-sized-capture.patch is reviewable diff.
NEXT: acquire shared lock and run python3 /tmp/cubit-acpi-promote-sized.py
from repo root (hashguards ACPI, promotes kernel/fixture, registers sizing
GPR/runner). Then python3 /tmp/cubit-acpi-sized-docs.py updates prepared docs;
run targeted registered provisioning build/proof, syntax/diff/hash checks.
Do not claim shared-tree capture changed before promotion succeeds.

2026-10-02 private dynamic kernel capture: 58454 TERMINAL0 native link after
19117 compile failure fixed by nested simple-storage-pool package. 90783
normal MB1/MB2 PASS, then QEMU rejected single >1MiB -acpitable (65535 cap).
63591 TERMINAL0: 24 tables >1MiB aggregate and count/late-table rejection
PASS under both protocols. Allocation failure injection LIVE (handle next).
All changes remain private /tmp/cubit-acpi-sized-boot-4gedz_zg/kernel/src/acpi.adb;
fixture /tmp/cubit-acpi-sized-fault-native.py. Shared source promotion/runner
registration pending held build lock; no own lock waiter. Previous goal turn
was concrete implementation/proof progress. No startup grants claim.

2026-10-02 provisioning: 1042 TERMINAL0, exact sum/maximum and quota
contracts proved (29 proof checks + 4 flow checks, zero unproved/Assume),
1795 hosted checks pass. Evidence /tmp/cubit-acpi-provisioning-exact.log,
-proof.out and -source-hashes.json. New owned provisioning sources/tests
complete; not wired into kernel allocation yet. Initial 93113 failed because
shared lock prevented creation of test GPR; private /tmp/acpi_provisioning.gpr
used successfully (39027 initial contracts, 1042 final strengthened contracts).
54145 bounded lock waiter TERMINAL1; no own jobs/waiters/locks. Standard
test GPR/runner registration deferred until shared lock available. Next: use
measured requirements in boot capture with explicit allocation failure. Buddy
max block is 32 MiB (order13), setup incl StoragePools precedes ACPI;
StoragePools.Allocate does not itself reject null, so cannot blindly use new.

2026-10-02: Prior transport verification closed: 86013 TERMINAL0, all 108
request/endpoint checks proved with no unproved/Assume; 91295 TERMINAL0,
request 92680 checks and native link pass. 29749 TERMINAL0 updated standard
proof budget under lock. No own live jobs or locks. Current work: owned new
shared/firmware/firmware_tables-provisioning.* and tests/aml-core/provisioning/
for checked discovered-size requirements before kernel allocation. Kernel
allocator exists before ACPI, but allocation failure must be handled explicitly.
Prior question turn was advice only; this turn resumes implementation.

2026-10-02:4886 TERMINAL1:30s/2000MB budget discharges2/3 invariant
assertions; onlyExtent<=Table_Byte_Limit remains unproved. ChangedBegin_Table
to check wireword<=NaturalLast beforecast, thenNatural quota comparison;
semantics unchanged, no weakenedcontract/Assume. Newquota proofLIVE
(recordhandle next). Prior goalturnmade transportimplementation/testprogress.
No ownlocks/waiters; freeze requestsource/GPR for proof.

2026-10-02:77246 TERMINAL1:revisionrange nowproved, but3explicitHandle
invariant assertions timeout/OOM. Parsed acpi_requests.spark confirms CVC5
10sTimeout andZ3 1000MB exhaustion, no counterexample.4886 LIVE samecontract
retry with30s/2000MB boundedbudget,j2. Keep source/GPR frozen, pollsamehandle.
51488 TERMINAL0 block150+native finalsource;424hashesmatched. Currentinputs
saved /tmp/cubit-acpi-runtime-transport-hashes.json. No ownlock/waiter.
Three assertions are not yetproved; do not marktransportproofpassed.

2026-10-02 finalcapacityproof78544 TERMINAL1: two Handle obligations still
timeout (versionrange +endinvariant; Z3 oneOutOfMemory), no counterexample.
Localchunk avoids giant per-byte updates but didnotalone discharge them.
Now Initial_Version constant+three explicit invariant assertions, unchanged
contracts/noAssume.77246 LIVE splitproof; 51488 split-check block/native
job LIVE.57261 TERMINAL0 priorblock150/native
424hashesmatched; current latestbody pending split-check build. No locks.
Do not restart77246 or promote oldfailedproof aspass.

2026-10-02:84167 TERMINAL1 SPARK E0007 variable Amount in localarraybound;
replacedwithconstant Chunk_Length,78544 LIVE finalrequests/endpointproof.
4161 TERMINAL0 request92676/endpoint589972/block150 andnative linkPASS
for prior equivalent slice variant.57261 LIVE finalconstantbound block/native
rebuild. Preserve sourcefreeze untilterminal.35535 fullhosted+ACPICA80/36
passed beforeconstructor/slice fixes; no fullASLTS advancement claim.

2026-10-02 transport verification:35535 fullhosted+ACPICA80/36 PASS then
GNATBUG in Fresh implicitdiscriminant-dependent component default. Replaced
with explicit Boot initialization.31703 TERMINAL1:versionbound+Handleinvariant
unproved around dynamicbuffer write loop; now boundedlocalChunk +one slice
publication, contracts unchanged.84167 LIVE requests/endpointproof,4161 LIVE
targetedrequest/endpoint/block150/native rebuild. Sourcefreeze.
Native74270 firstlinkfailed implicit __gnat_malloc fromlibraryobject initialized
withunconstrainedFresh; explicit constraineddefault instance fixed.53269 and
76316 TERMINAL0 native,76316 block150/loop39PASS. Finalslice native pending4161.
No ownlock/waiter. Do not treat31703proof aspassed or dynamicallocation aswired.

2026-10-02 transport promoted7sourcefiles afterprivate150blockchecksPASS.
Added6requestchunkfallbackchecks (>1MiBopen, oversizereject, partialcommit
rejection, actualcapacitymetrics). Restorednamespace_field_tests standardGPR/
run.sh underheldlock.35535 LIVE fullhosted+ACPICA80/36+requests/endpointproof;
74270 LIVE private native /tmp/cubit-acpi-runtime-transport-native-r3ddq2qw.
No lock/waiter. Sources/GPR frozen. Actualstartup stilldefaultFresh; allocation
fromdiscoveredlengths remains, not end-to-enddynamicgrantsclaim.

2026-10-02:40571 TERMINAL0 core/bootstrap/request/endpoint capacityproof
passed0unproved/Assume; savedruntime-service-proof.out (5515aggregateincludes
cachedunits). Prepared transport inisolated /tmp/cubit-acpi-runtime-transport-knwjesc5
whileproofran.55519 defaultrequest92670/endpoint589972/block104PASS;89503
largegrant150checksPASS after1207fixturetypeerrorfixed. Hashguardpromoted7
ownedfiles (requestsads/adb,nativeblocks/nativeinstance,request/block/looptests;
seeplanned-source-hashes for exactcount). Transport nowinstancecapacities for
limits/metrics/readback; nativeInstance still defaultFresh pendingdynamicallocation.
No widerauthority. Next integratedhost/proof/nativeverification, sourcefreeze.

2026-10-02 runtime service/bootstrap:29962 compileTERMINAL0;40571 LIVE
core/bootstrap/request/endpoint proof after hosted1051688service/1314bootstrap/
92670request/589972endpoint PASS. Sources/GPR frozen until samehandleterminal.
69874 TERMINAL0 freestandingnative /tmp/cubit-acpi-runtime-service-native-z70yjx2c;
424inputhashes match. Largest reportedframe2713376B,29dynamicrecords; this
is NOT dynamicstack/wholecallchain proof or liveboot. Nondefaulted immutable
discriminants avoid worstcase mutablearray reservation; requestBoot currently
explicitly constrained to olddefaults. Servicecopy replacedloop withslice.
Found current standardGPR/runner omit namespace_field_tests despite oldernote;
restore that fixture underlock AFTER40571 ends (do not edit activeGPR).
ACPICA field runner remainsin run-acpica.sh. No ownlock/waiter.

2026-10-02 ACTIVE runtime service/bootstrap capacities. Prior goal turn made
verifiedprogress (packed snapshot+boot), RecordFlux followup was read-only
coverage/proof audit, not adoption/rewrite authorization. Own service ads/adb,
bootstrap ads/adb, requests private Boot constraint, and existing hosted
service/bootstrap fixtures. No shared kernel/runtime/startup changes.
Use non-defaulted discriminants for nonlimited service state so capacities
are fixed by construction, avoiding mutable-discriminant worst-case storage.

2026-10-02 PACKED SNAPSHOT VERIFIED:33952 snapshot2680/catalog75775 and
95snapshotproofchecks PASS;58611 exposure61468/service1051624/bootstrap1257
and63exposureproofchecks PASS,0unproved/Assume.43071 oldPageCount257 failure
fixed derivedbound;25921 stopped143 after obsolete max-pagealigned fixture
was found; corrected retained fixture2MiB, maximumgeometry all4096offsets.
43286 TERMINAL0 actualprivatekernel build+6bootcases PASS (normal7tables9095B,
countfailure andlatefailure bothMB1/MB2). SHA5eb9087dabb573f016dd3714a56416378950de8d862820a48c10c3f37aa563a6.
345inputs matchworkspace, only generatedbuild.ads differsprivate. Snapshot
/tmp/cubit-acpi-packed-boot-6llq44n8. Nativefixture promoted underlock,cmpPASS.
No ownjobs/lock/waiters. Reports dynamic-snapshot-proof andwide-catalog-proof
under/tmp/cubit-acpi-*. Sourcehashes /tmp/cubit-acpi-dynamic-snapshot-hashes.json.
Docs/ledger updated; N1 owner execution andN2 AMLdispatch stillpending.
Next userpriority: runtime-sized service backing + nativeblock budgets, then
discovered-size kernel allocation/grant exporter (startup/sharedsyscall owner
ACK stillabsent). Kernel admission still1MiB/table, boot scratch64KiB anddefault
snapshot32/1MiB; do not claim dynamic end-to-end handoff. Goal remains active.

2026-10-02 dynamic snapshot:33952 TERMINAL0, packed runtime-discriminant
storage passes2680snapshot/75775catalog checks;95snapshot proofchecks
(207 aggregate)0unproved/Assume. Length metadata now representationalPositive
limit; boot/service quotas remain default64KiB/32/1MiB pending integration.
98645 intentionally terminated143 after detecting old assertion-enabled O(n²)
copy loop on1MiB fixture; slice copy preserves exact contract and finaltests
complete promptly.49611 priorproofterminal0, superseded by33952.
75660 LIVE private kernel build /tmp/cubit-acpi-packed-boot-6llq44n8;93267
terminal1 wrapper failure fixed ALR= (outerenvironment providescompiler).
43071 LIVE exposure/service/bootstrap regression+exposureproof. Native
fixture packed-layout adaptation /tmp/cubit-acpi-packed-native.py; promotion
attempt nonblock lock busy, NO edit/waiter; retry when free.

2026-10-02:23652 TERMINAL0 verified handle and report; service adapters
Declare_Table_Region12/Declare_Table_Field10/Read_Namespace_Field7 proved,
zero unproved/Assume. Saved /tmp/cubit-acpi-namespace-fields-service-proof.out.
No own active jobs/locks. Prior turn yielded terminal proof evidence (progress).
Own next edits shared/firmware snapshot packed runtime-capacity storage and
catalog representational length, existing snapshot/catalog hosted fixtures.
Kernel/service adapters still fixed budgets; no kernel/startup source edits.

2026-10-02 ACTIVE23652 serviceproof STILL LIVE confirmed samehandle; no
failure output. Keep namespace/service frozen until terminal. Received user's
coverage ledger via authorized side-chat handoff; now maintain docs/acpi-coverage.md.
User steering: cannot predict table sizes; wants runtime-sized grant if fixed
budget lacks broad margin. Primary T490s bootlog DSDT0x2396A=145770bytes,
142.35KiB disproves10x aggregate headroom and64KiBper-table compatibility;
no claim that >1MiB totals are common. B03/architecture updated to runtime
validated-size ownedbuffers+ROgrants withzero padding, separate quota.
77627 probe TERMINAL1 EXPECTED: generic Global=>null callback cannot capture
Tables; /tmp/cubit-acpi-table-context-probe.log shows required Global. Thus
next evaluator design needs explicit readonly table context parameter, not
hidden closure data. This is experimental evidence, not production proof.

2026-10-02 UPDATE:10539 now TERMINAL0. New namespace binding/accessor checks:
Bind_Table_Field33 + Bind_Table_Region22 + Field_Data7 + Region_Data6 allproved,
zero unproved/Assume. Saved /tmp/cubit-acpi-namespace-fields-proof.out.
23652 LIVE service-unit proof, /tmp/cubit-acpi-namespace-fields-service-proof.log.
Continue polling SAME23652 handle. Earlier10539-live note is superseded.
All hosted/reference/native checks terminal0; serviceproof remains pending.

2026-10-02 NAMESPACE BINDINGS:10539 STILL LIVE confirmed by handle (phase3,
no failures reported). Continue polling SAME handle; do not restart. After it
finishes inspect new Bind_Table_Region/Bind_Table_Field/Region_Data/Field_Data
proofs, then run service-unit proof for Declare_Table_Region/Declare_Table_Field/
Read_Namespace_Field. Namespace/service sources frozen pending verification.
75767 TERMINAL0:service1051624/methodstorage141/inspect234/ACPICA80 PASS;
64302 TERMINAL0 full standard hostedrun PASS.30762 TERMINAL0 nativeexe;
424hashes match /tmp/cubit-acpi-namespace-fields-native-jra6i66j. Standard
namespace fixture added underlock; diffcheck/shellsyntaxPASS. No lock held.
Logs /tmp/cubit-acpi-namespace-fields{,-regression,-hosted,-native}.log.
README currently says proof pending; update on actual terminal evidence.
User's size question answered:1MiB total/64KiB pertable/32count prototype caps,
fail closed on excess, configurable sizing+requiredcapacitydiagnostics remain
unimplemented. docs/acpi-userspace.md records compatibility/stack concerns.
Fullgoal remains active. No AML DataTableRegion/Field dispatch or field operand
lookup yet; dynamic active-owner callback behavior remains untested/unwired.


2026-10-02 ACTIVE AML_Namespace table-region/field metadata binding, immutable
resolved table identity copied into fields (no recyclable node links). Own
namespace ads/adb, service declaration/read adapters and hosted tests, no
sharedkernel/runtime edits.10539 LIVE namespaceproof after351hostchecksPASS;
75767 LIVE service/inspect/methodstorage/ACPICA80 regression. Namespace tests
installed in standardGPR/runner underlock.33817 initialcompile1 fixed Count
visibility;13338 testfailure1 corrected root prefix in ObjectType fixture.
User asks about budgets: confirmed64KiB/table,32tables,1MiBtotal are prototype
budgets, not ACPI limits. Configurable allocation/capacity diagnostics remain
compatibility work; no hardware-authority expansion implied. Rejected
bindings atomic; methodOwner must be active. Prior turn made verifiedprogress.

2026-10-02 TABLE BIT READER VERIFIED:52528 TERMINAL0 proof85checks0unproved/
Assume,16615 TERMINAL0 serviceproof (existing generic flow warnings only).
60694 TERMINAL0 final639397bit/1051609service/80ACPICA comparisons PASS.
50000 TERMINAL0 native serviceexe /tmp/cubit-acpi-field-data-native-2v9hv7qr,
424inputhashes match. Standardrunner/GPR integration installed underlock;
shellsyntax/diffcheckPASS. Proof reports /tmp/cubit-acpi-field-data-proof.out
and /tmp/cubit-acpi-field-service-proof.out; logs field-data-final/reference/
native and field-service-proof under /tmp/cubit-acpi-*. No live ownjobs/waiters/
lock. Earlier11031 deliberately stopped after failed exponentiation obligations;
bounded Scale table fixed them without weakening exact extraction contract.
Next: actual region/field namespace binding and service-aware evaluation.
Read_Bits/Table_Field operate on owned table slices only, no hardware/addresses.
Fullgoal incomplete; no Field-opcode/ASLTS advancement or live startup claim.

2026-10-02 ACTIVE owned AML_Field_Data exact read-only bit extraction and
ACPI_Service.Table_Field adapter over admitted owned-table slices. No physical
addresses, namespace mutation or register reads. New hosted/proof tests.
11031 TERMINAL1 after targeted SIGINT (reported failed arithmetic obligations);
fixed variable exponentiation with bounded Scale table.52528 TERMINAL0 reader
proof85checks0unproved/Assume;16615 LIVE serviceproof.60694 LIVE finaltest/ref;
50000 TERMINAL0 rebuilt nativeexe. Original11031 tests had639397 field-data/1051609service checksPASS.22132 LIVE
ACPICA reference comparisons. Standardrunner edits installedunderlock terminal0.
37640 initialtestcompile terminal1 fixed mutable-discriminant iterable.
Prior turn was progress (all six field entry framing forms verified).

2026-10-02 FIELD ENTRIES VERIFIED: new AML_Fields covers all six FieldList
entry forms as bounded framing (not execution/region admission).11515 terminal0:
131520 hosted checks,59 Read_Entry proofchecks0unproved/Assume,32 iASL fixture
entry comparisons.64575 terminal0 freestanding unit compilation;422snapshot
inputs match /tmp/cubit-acpi-fields-native-9o2_a8md.72618 terminal0 installed
standardtest/proof/iasl runner integration while holding sharedlock; released.
Logs /tmp/cubit-acpi-fields-final.log /tmp/cubit-acpi-fields-native.log;
proof /tmp/cubit-acpi-fields-proof.out. Shell syntax+diffcheck PASS.
37991 terminal1 duplicate-use warning fixed;13242 earlier native compile0.
No live ownjobs/waiters/lock; no commits/pushes. Next: namespace region/field
objects and immutable retained-table evaluation context, then actual field
access. Fullgoal active and incomplete; no upstreamASLTS progress claimed.

2026-10-02 ACTIVE: own new AML_Fields field-list entry framing and field_tests.
All six entry forms, no namespace mutation/region IO; explicit bounded spans.
No shared sources or scripts edited while Servo v26 holds shared lock.
72618 LIVE bounded shared-script lock waiter;37991 LIVE finaltests/proof/iasl.
Initial compilejobs2517/17661 terminal1 repaired visibility/variant aggregates;
44200 terminal0 tests/proof,60932 terminal0 iasl32 comparisons.
Prior turn was progress: verified raw length decoder and regression evidence.

2026-10-02 FIELD LENGTH VERIFIED:20120 proof/test TERMINAL0;789819 decoder
checks PASS. Read_Field_Length37 + Read_Package12 proved checks,0unproved or
Assume; exact bit-count contract retained. Full hosted+ACPICA84102 TERMINAL0:
all focused comparisons PASS; upstreamASLTS0passed/12unsupported/0failed/
339notrun.98264 nativeexe TERMINAL0,420inputs match private snapshot
/tmp/cubit-acpi-field-native-85c7jbw2. Evidence /tmp/cubit-acpi-field-{length,
regression,native}.log and /tmp/cubit-acpi-field-proof.out. README updated.
No live own jobs/waiters/lock; no shared build script edits or commits/pushes.
This adds framing only, not Field/DataTableRegion execution. Fullgoal remains
incomplete; startup/syscall ownership acknowledgment still pending.


2026-10-02 ACTIVE Field bit-length framing: own AML_Decode.Read_Field_Length
rawPkgLength bitcount, exact numeric contract, shared by existingRead_Package
which retains extent checks. decode_tests extended against existing independent
oracle across allfirsttwobytes/truncations/highbytes/nonminimalzero/maxoffset.
No Field/DataTableRegion execution claim; no hardware or peer source edits.
Initial job84580 terminal1 (invariant placement compile error); replaced bounded
loop with explicit continuation-byte arithmetic.20120 terminal0:789819checks,
Read_Field_Length37 + Read_Package12 proofchecks,0unproved/Assume.
84102 LIVE standardrun.sh --acpica /tmp/cubit-acpi-field-regression.log;
98264 TERMINAL0 private native /tmp/cubit-acpi-field-native-85c7jbw2; all420
input hashes match. Proof report saved /tmp/cubit-acpi-field-proof.out.
No shared lock held. Priorboot/tablelookup turns made concrete progress.


2026-10-02 LOOKUP VERIFIED:55178 TERMINAL0, newRead_Identity37/Table_Identity5/
Find_Table16checks allproved; zero unproved/Assume. Combinedprojectreport5096
includes previouslycached units, not a new fullcleanrun. Fournewtermination
results plus58proofchecks explain62delta. Existinggenericflowwarnings only.
service1049899(791new) PASS,56130ACPICA36PASS,80035nativeexePASS420hashesmatch.
Standardrun.sh proof andrun-acpica integration installedunderlock, syntaxPASS;
comparisonfile exactlymatchestested/tmp. No liveownjobs/waiters/lock. No startup,
wirelabel, syscall, rawaddress or hardwareprivilege added. DataTableRegion and
Field operations stillunsupported; sharedstartup/syscallownership request still
unacknowledged. Next AML work can use retained-table lookup while preserving
fullobjective. Docsupdated; no commits/pushes.


2026-10-02 lookup tests/reference/native PASS, proof LIVE55178 (samehandle).
service1049899 including791newchecks PASS;56130 TERMINAL0 focusedACPICA36
DataTableRegion-reference lookups vs directCuBitFind_Table. Not CuBitopcode or
upstreamASLTS pass.80035 TERMINAL0 nativeexe at
/tmp/cubit-acpi-identifiers-native-4ggvki0i,420inputs match. Lockedscriptinstall
completed; run.sh proofadds identifiers, run-acpica addsrunner+comparison.5606
initiallockwaitTERMINAL75.86628 firstcompilewarnings-as-errors fixedconstarrays.
55178 proofstillwrites why3 files; nofailure/terminalyet, do not restart or
claimcompleteproof. Logs /tmp/cubit-acpi-{identifiers,table-find-compare,
identifiers-native}.log. No heldlock or other ownjobs. Sources frozen; docsupdated.


2026-10-02 ACTIVE table lookup prerequisite: owned new Firmware_Tables.Identifiers
fixed-width extractor/selectors; ACPI_Service.Table_Identity/Find_Table earliest
retained match with explicit OEM wildcards, no address/capability. service_tests
covers allselectorcombinations, duplicatefirstmatch, zeroIDs and byte/bounds
extraction. Preparing hosted tests+SPARK proof; no DataTableRegion opcode or field
support claimed. Pinned ACPICA tbfind.c examined; ASL short-string conversion
kept separate. Shared startup/syscall request remains unacknowledged; independent
AML prerequisites continue. No peer/shared runtime/kernel/procmgr edits.


2026-10-01 boot fixture INSTALLED:19312 TERMINAL0 under sharedlock; lockreleased.
Repo native.py hash matches final tested /tmp script.93886 final6casesPASS,
no ownlivejobs/VMs/waiters. Kernel hash769b4112917b4a74aceb5c80cae97a0d59aeca835678576512f93d0d6d67192f.
345rootinputs compared: only generatedkernel/src/build.ads changed while peers
built; all substantive kernel/runtime/shared inputs match compiledsnapshot.
No production testhook or productioncode changes thisturn. Docs updated; next
independent work can proceed while startup/syscall ownership request remains
unacknowledged. Full service/AML objective remains incomplete.


2026-10-01 BOOT VERIFICATION PASS:17607 full privatekernel build/stackgate/link
TERMINAL0.27949 normalMB1+MB2 PASS seven tables9095bytes;48430 both faultmodes
(tablecount33, secondAppendextent65537) bothprotocolsPASS.50656 first hashed-
report version PASS.93886 final paused-serial/protocol-aware fixture TERMINAL0,
all6casesPASS; report /tmp/cubit-acpi-boot-results.json. Unmodified production
kernel /tmp/cubit-acpi-boot-czopc2v3;345sourceinputs checked, generatedbuild.ads
only privatechange. No proof this boots ACPIservice, IPC/grants or realhardware.
80012 installwait TERMINAL75;19312 LIVE bounded300s lockwait to copy final
/tmp/cubit-acpi-native-snapshot.py to tests/aml-core/snapshots/native.py. Do not
restart while handlelive. No nativeVM/buildjobs left; no sharedlock held byme.
README updated; testscript publication pending. No productioncode edits thisturn.


2026-10-01 ACTIVE boot validation: helper75478/4626 TERMINAL1 lockbusy.
Created separate kernel-only source snapshot /tmp/cubit-acpi-boot-czopc2v3,
345hashedinputs, no shared outputs or seedbinaries.17607 TERMINAL0 full native
kernel build incl stack gate/link. New /tmp/cubit-acpi-native-snapshot.py uses
GDB to stop unmodified production kernel after ACPI.setup and inspect copied
bytes/metadata/checksum/padding/source-disjointness. Private QEMU TCG no disks,
multiboot1+2 next; /tmp/cubit-acpi-boot-check.log. Shared sources unchanged,
no held shared lock, no startup/syscall/peer edits. Goal incomplete.


2026-10-01 SEALED SNAPSHOT COMPLETE FOR THIS SLICE: kernel setup captures all
admitted tables once; public Copy_Table reads cache, no raw firmware rereads.
Failed count/size/checksum/order/total/incomplete capture publishes nothing.
No reset; full-state ghost frame proves every mutator preserves a sealed cache.
Shared service limits alias snapshot constants (32/64KiB/1MiBpayload). Static
kernel cache ~2MiB +64KiBscratch, NOT grantable storage; originalfirmware remains
reserved. Table_Snapshot_State/Snapshot_Table_Count report availability; old
private read errors replaced in public status by Snapshot_Unavailable.
59340 TERMINAL0:2508checks/91SPARKresults (83proved+8flow),0unproved/justified.
26361 TERMINAL0 finalnativekernel +service1049108/bootstrap1257/request92670/
nativeloop39 PASS. Kernel375inputs match at /tmp/cubit-acpi-snapshot-native-zipi6ssz.
4185 TERMINAL0 finalcheckedserviceELF,422inputs match at
/tmp/cubit-acpi-snapshot-service-auh5f5dj. No undefinedsymbols. Standardrunner
andproof installed underlock; guardblocks repeated discovery aftercapture.
Initial77339/66981compile failures corrected;12364 invariantfailure corrected
by initialcounterassignment;14744old83proof PASS beforestrongerfreezecontracts.
44400 lockededit TERMINAL0; allownjobs terminal; no lock/staging/commit/push.
Logs /tmp/cubit-acpi-snapshot-{freeze,final,service}.log. No live boot, grant
handoff, backingreclaim, event/CCL integration or full AML completeness claim.

REQUEST ownership coordination before next shared integration: graphics/procmgr
owner and syscall/process owner: need scoped startup support for acpi.svc,
bootstrap-tag/provider-slot binding, and a capability-gated immutable snapshot
copy operation into validated process-owned buffers. New authority must exclude
arbitrary source addresses and unowned destinations; grant publication must go
through existing process-owned pin/acquire/return/revoke. No edits to your
procmgr/syscall/process/runtime files made. Local kernel ACPI capture and AML
work remain independent. Please acknowledge a source-idle ownership window or
own the producer/startup changes. No user response needed for independent work.


2026-10-01 ACTIVE sealed boot snapshots: own new shared/firmware/firmware_tables-
snapshots.ad? and snapshot hosted target; scoped kernel/acpi.ads/adb change to
capture once after successful inventory seal and serve Copy_Table from retained
bytes. No process/grant/runtime/procmgr edits. Current createGrant pins only
process-owned frames; kernel cache is not a grantable process allocation. Future
handoff must use authorized owned-page copies plus existing grant lifecycle,
not grant arbitrary kernel BSS. Procmgr/shared syscall integration needs owner
coordination. No shared native build or lock currently held.


2026-10-01 STARTUP/EXECUTABLE VERIFIED: 57381 TERMINAL0 full hosted
run.sh --acpica. All hosted tests PASS (native-loop39, native-blocks104).
ACPICA focused1530/packages12/packagecounts38/coercions268/typed212/store520/
namedstore48/serialized1200/dynamicmethods26/methods28 PASS; FADT113 and FWTS48
PASS. Upstream ASLTS: CuBit0passed/12unsupported/0failed/339entrypointsnotrun;
not a full-upstream pass. Decoder24079 TERMINAL0:8proof+1termination,
0unproved/justified/Assume. Native link19435 TERMINAL0; snapshot420inputs match.
Await accepts only stamped bootstrap configuration once; static Instance owns
Server/Adapter; manifest identity-only. No kernel launch/provider export, live
IPC/hardware, CCL integration or full AML completion claimed. Docs/repro updated.
No live own jobs, no held lock, no commits/push/staging. Next trusted process-
manager bootstrap and immutable-table provider; coordinate shared source owners
before changes. Security: provider capability binding is launcher obligation,
not a fact proved by payload decoding; pending loans depend on process teardown
on fatal exit (live verification pending).
Logs: /tmp/cubit-acpi-launch.log, /tmp/cubit-acpi-executable-link.log,
/tmp/cubit-acpi-launch-regression.log. Goal incomplete.


2026-10-01 executable link19435 TERMINAL0: isolated snapshot
/tmp/cubit-acpi-executable-nrie4zkx,420inputs match (README-only refreshed afterlink).
ELF ec7b395f8cd26bfb0a21e09832cea8d367e213d8093b351ad75a17e617e5e46a;
RX/RW separate, RW64MiB GNU_STACK, identity-onlymanifest, nounresolvedsymbols;
Server/Adapter staticBSS. Initial4266 linkfailed missing -nostdlib builderflag,
fixedinownedGPRunderlock, correctedlinksuccessful. No sharedstaging/lock.
57381 LIVE fullhosted run.sh --acpica; hostedthroughendpointPASS, ACPICAongoing.
Log /tmp/cubit-acpi-launch-regression.log. Sources frozen; no boot/liveclaim.


2026-10-01 ACTIVE executable integration: owned launch decoder, native bootstrap,
static instance/main/identity-only manifest and acpi.gpr. 24079 TERMINAL0:
39 hosted loop checks; decoder8 proof checks, no unproved/Assume. GPR and runner
launch-proof addition installed under build lock; lock released. Private executable
link next, no shared staging/procmgr/runtime edits or boot claims.


2026-10-01 NATIVE LOOP VERIFIED: new ACPI_Native_Server.Run wraps real runtime
Poll_Any_IPC/unifiedendpoint/reply and Wait_For_Activity_Until. Caller retains
Server/Adapter across returns.100ms cleanup deadline,saturates belowLast; no
request replay after failedreply. Unexpectedcompletion/wait-unavailable/clock
exhaustion explicitfatal, retaining pendingcleanup. No subscriptions yet.
7505 TERMINAL0 initial19;26007 TERMINAL0 integrated23 tests incl deadline
saturation and pendingstate onfatal/restart. Under held lock installed native-loop
project+prepare+standardrunner; syntax PASS. Mock extracts actualruntime ABI,
enum,slot,timecall.2977 TERMINAL0 actualruntime full staticlibrary at
/tmp/cubit-acpi-loop-native-c67e_8zh,406inputs unchanged. No loop SPARK proof,
executable/boot/liveIPC or whole-stack claim. Docs updated. Allownjobs terminal,
no heldlock. Logs /tmp/cubit-acpi-loop{,-integrated,-native}.log. Next executable
main/manifest/trusted tags+provider-slot startup binding, kernel copied-table
provider, then live bounded scenario. Goal active.

2026-10-01 ACTIVE: owned ACPI_Native_Server.Run real runtime loop with caller-
retained state/adapter, bounded cleanup retries, no replay after reply failure;
hosted native-loop IPC mock target with extracted runtime ABI. No shared/peer edits.

2026-10-01 BULK WIRE INTEGRATED: ACPI_Native_Blocks.Dispatch label8 decodes
revision/canonical generation-slot/ID/kind-length; rejects malformed/auth errors
before acquisition. Scalar requests share dispatcher. Native_Endpoint.Dispatch
now takes persistent Adapter+trusted Provider_Slot and calls unified path; old
scalar-only signature replaced (no callers). No raw wire address/offset/authority.
CleanupPending returns committed service reply and retains loan; retry return
only.36837 TERMINAL0 hosted104 (was41 directadapter), includes maxreference.
Native69901 TERMINAL0 full actualruntime static library snapshot
/tmp/cubit-acpi-block-wire-native-ixye1073,404inputs unchanged. Not adapter SPARK
proof or live receive-loop test. Logs /tmp/cubit-acpi-block-wire{,-native}.log.
Docs added native-blocks/README and service note. Existing runner covers tests;
no shared build edits/heldlock/livejobs. Next service executable/receive-loop
with trusted authority config and cleanup handling, plus kernel table-grant
provider/startup wiring. Goal active; no boot/hardware completion claimed.

2026-10-01 ACTIVE: owned native ACPI block wire dispatcher+unified endpoint,
label8 packed generation/slot and kind/length; no addresses. Extend existing
block mocks/tests; source paths/runners unchanged. Startup loop remains next.

2026-10-01 CAPABILITY REVOCATION VERIFIED: Cspace.Can_Revoke/Revoke now check
slot/type/gen/registry ID/RIGHT_REVOKE/liveness/actual kind. Post establishes
accepted exactly when old Can_Revoke, descendants invalidated; denial unchanged.
No catalog/backing release or completion in wrapper.63257 TERMINAL0 hosted772
+combined362 SPARK analysis checks0unproved/justified. Tests all32rights plus
wrongtype/gen/identity/handle, group/register revocation, replay, unrelatedgrant
preservation and retained in-flight completion. Native61939 TERMINAL0 actual
kernelproject child/grants/catalog compile at /tmp/cubit-hardware-revoke-native-ofolo3kk;
367inputs only generated build.ads changed. All own jobs terminal,no heldlock.
Logs /tmp/cubit-hardware-revoke{,-native}.log,README updated. Existing revocation
syscalls are shared-memory-specific; hardware still needs dispatch/currentcaller
selection. Peer filesystem editing syscall IDs123-125 for virtual reservations;
coordinate before touching syscall dispatcher/runtime messages. Independent
next work: service executable/receive-loop integration and table-grant export,
or owned hardware dispatcher groundwork. No live hardware claims; goal active.

2026-10-01 ACTIVE: owned Cspace.Can_Revoke/Revoke and hosted checks. Checks
RIGHT_REVOKE +type/gen/identity/actual grant kind; no raw user handle or backing
release. No peer sources or shared build definitions edited.

2026-10-01 INSTALLATION VERIFIED: Cspace.Install_Group trusted-only startup
helper; Install_Child checks source type/gen/identity/actual kind/grant right,
cap-right subset, requested flags match rights, catalog membership and record
permission narrowing. Installers reject occupied/invalid/reply slots; no execute.
Postconditions preserve all unrelated slots and make failures leave registry+
table unchanged; exact rights/gen/tag installed. Source_Table requires stable
kernel snapshot under same lock (especially same-table delegation), destination
cspace authorization still caller responsibility for future syscall.
78642 TERMINAL0 combined332 SPARK checks0unproved/justified;77127 TERMINAL0
hosted353 incl storage exhaustion atomic failure. Native66191 TERMINAL0 actual
kernelproject compile snapshot /tmp/cubit-hardware-install-native-8nd4q_rz,
367inputs unchanged. Logs /tmp/cubit-hardware-install{,-tests,-native}.log.
All own jobs terminal,no heldlock. Docs updated. No boot/syscall/platform
admission/physical access wired. Next safe work: capability-authorized revocation
and startup/syscall wiring, with scoped resource admission. Goal remains active.

2026-10-01 ACTIVE: owned cspace Install_Group/Install_Child with empty-slot
checks, capability/permission attenuation and ancestor linkage. Tests/proofs
in existing disjoint cspace output; no shared build definition or peer edits.

2026-10-01 REGISTRY IDENTITIES VERIFIED: Hardware_Grants.Initialize allocates
nonzero lifetime IDs from package-owned monotonic abstract state, refuses repeat
initialization/exhaustion. State privately stores ID; Admit_Group rejects zero.
Cspace no longer takes an expected Registry_ID argument; compares cap.param to
Identity(S). Tests cover equal numeric handles in different registries.
64872 TERMINAL0: cspace200/grants182 +combined284 SPARK analysis checks with
0unproved/justified. Earlier syntax/proof setup failures superseded. Native79248
TERMINAL0 actual kernel project child/grants/catalog compile at
/tmp/cubit-hardware-identities-native-0xp69cln.367inputs, only generated build.ads
DATE/HASH changed; all others match. No boot/hardware/whole-stack claim.
Logs /tmp/cubit-hardware-identities{,-native}.log. Docs updated, standard targets
already include changes. All own jobs terminal,no heldlock. External kernel
serialization and stable registry ownership remain required. Next implement
trusted startup/group and checked child capability installation; current-process
syscall wiring and physical hardware operations still absent. Goal active.

2026-10-01 ACTIVE: bind nonwrapping allocated lifetime IDs into owned grant
State; remove caller-supplied expected ID from cspace API; update hosted tests.
No peer source or shared build definition edits.

2026-10-01 CSPACE INTEGRATED/NATIVE CHECKED: lock1231 TERMINAL0 installed
persistent hardware_cspace.gpr +standard runner test/proof. Native93577 found
missing language declaration for grant array syntax; added pragma Ada_2022 to
hardware_grants.ads (existing kernel per-unit convention). Native20954 TERMINAL0
compiled child/grants/catalog with actual kernel GPR/config/runtime snapshot
/tmp/cubit-hardware-cspace-kernel-mrbb1b6a,367input hashes unchanged. No link/boot.
Can_Access/Begin_Access frames32/80 under kernel flags,not whole stack proof.
6787 TERMINAL0 integrated195tests +combined268SPARK checks0unproved/justified,
runner syntax PASS. Earlier pending-integration notes superseded. All own jobs
terminal,no heldlock. README updated. Next work trusted lifetime-identity
allocation and startup/member capability installation; syscall caller-table
selection and real hardware remain absent. Overall goal still active.

2026-10-01 CSPACE GATE VERIFIED: new Hardware_Grants.Cspace accepts only a
kernel-owned table/expected registry identity plus slot/op/value; checks type,
INITIAL_GENERATION, registry identity, read/write right, live handle and actual
register kind before grant reservation. No raw request-supplied handles/scopes.
54591 TERMINAL0: hosted195 +combined268 SPARK analysis checks0unproved/justified
(includes grants/catalog/authority/capabilities). Earlier failures superseded;
Live contract now publishes used and absolute handle bounds. No Assume/waiver.
Expected registry ID must be allocated uniquely and bound to S by kernel owner;
allocator/installation/current-process syscall glue still absent. Hosted uses
actual capability definitions with64slot Config stub. No native child compile.
Shared integration lock attempts all TERMINAL75 (72446 included), no GPR/runner
edits. Prepared /tmp/cubit-integrate-hardware-cspace.py for held-lock promotion;
then run persistent project and syntax check. Current project /tmp/hardware_cspace.gpr,
log /tmp/cubit-hardware-cspace.log. All own jobs terminal,no heldlock. Remaining
safe work: trusted identity/installation and native cspace compile. Goal active.

2026-10-01 ACTIVE: owned new hardware_grants-cspace unit and hardware-cspace
hosted tests. Kernel-owned table/registry identity gate, no syscall or live
issuance. No edits to peer-owned capabilities/operations/process/syscall units.

2026-10-01 GRANT RESERVATIONS VERIFIED: Hardware_Grants.Begin_Access consumes
stored epoch/permission, with allowed result equal to pre-reservation Resolve;
denial leaves catalog unchanged/no ticket. Resolve Refined_Post added to prove
that exact relation, no weakening or Assume. 74093 TERMINAL0 proof231 analysis
checks0unproved/justified; 90713 TERMINAL0 final hosted180 (including live-but-
stale epoch grant). 33796 TERMINAL0 checked native static library at
/tmp/cubit-hardware-grants-native-h390p3p5,87input hashes unchanged. No whole
stack/boot/live-I/O claim. All own jobs terminal,no heldlock. Standard runner
already includes these targets. Docs updated. Next actual cspace binding:
Capabilities.Operations.lookupCap only tests non-null; hardware needs type,
generation, registry identity and rights checks, not lookup success alone.
No capability installation or hardware syscall exists yet. Goal remains active.

2026-10-01 ACTIVE: grant-aware Begin_Access in owned hardware_grants unit
and hosted tests. Stored epoch/scope only; no new syscall/cspace or peer edits.

2026-10-01 GRANTS INTEGRATED: lock36535 TERMINAL0 created persistent grants.gpr;
subsequent held-lock edit added grant test/proof to standard AML runner.
81105 TERMINAL0 persistent GPR build/test156 +bash runner syntax PASS.
All own jobs terminal, no held lock. README has reproduction and explicit
remaining cspace/reservation/native-integration limits. Previous pending-lock
notes superseded. Next grant-aware Begin_Access must consume stored permissions
without exposing a wider catalog scope; then actual cspace authentication.

2026-10-01 GRANTS VERIFIED: new hardware_grants kernel unit stores trusted
group/member records, complete ancestor sets, epoch-bound permissions; derives
only catalog members and narrowed child authority. Revoke contract proves all
descendants lose liveness while unrelated grants retain it. Bounded128, no reuse
or reset; exhaustion denies. 99856 TERMINAL0: hosted156 +combined215 SPARK
analysis checks0unproved/justified. Catalog.Valid moved unchanged expression to
body for modular predicate reasoning; no weakening/Assume. Regression70796
TERMINAL0 catalog126. Earlier grant proof failures superseded. No native check,
cspace binding, startup instance or live I/O. Next implement grant-aware reserved
access, then authenticated cspace binding; never trust wire handles or scopes.
Shared GPR creation lock36535 LIVE bounded45s; poll it before retry. No own
proof/test jobs running. Logs /tmp/cubit-hardware-grants.log and
/tmp/cubit-hardware-catalog-regression.log. Standard runner not yet updated.

2026-10-01 ACTIVE: new owned kernel/src/hardware_grants.ads/.adb and
tests/aml-core/hardware-grants/ tests. Bounded append-only kernel grant records,
ancestor revocation, catalog epochs and permission attenuation. No shared
capability/syscall edits or live issuance. Hosted build in separate /tmp GPR
and tests/aml-core/build/hardware-grants outputs; no shared build lock needed.

2026-10-01 PROMOTED AND VALIDATED: shared build lock acquired; actual kernel
capabilities.ads appends hardware group/register kinds; syscall-admin generic
mint guard uses isPolicyMintable to deny both; cubit.gpr includes shared/hardware.
Standard AML runner now includes capability/catalog tests and proofs. Hosted
capability GPR selects actual kernel source; obsolete prepare.py/admission patch
removed. Three promoted kernel files match private native candidate hashes.
48316 TERMINAL0: capability273 +catalog126 hosted checks; capability14/catalog151
SPARK analysis checks, zero unproved/justified. Log /tmp/cubit-hardware-promoted.log.
22907 TERMINAL0: AML oracle7812, ACPICA differential1530, completed concrete
namespace128/service512 analysis5034 with zero unproved/justified. Separate
truncation test58867 PASS7820. Updated actual-source docs. All own jobs terminal,
no held lock. Earlier pending-promotion/live-job notes below are superseded.
Still no hardware caps issued, boot catalog instance, cspace grant lookup,
parent-child revocation, access syscall or physical hardware I/O. Next kernel
work must bind authenticated capability objects to kernel-owned grant records;
caller-provided IDs/permissions cannot authenticate access. Overall goal active.

2026-10-01 FindSet validation22907 LIVE: Python7812 oracle then ACPICA
differential1530 PASS (568 new comparisons:4 methods, both widths, all single
bit positions); now proving aml_integers/aml_execute/concrete namespace128 +
service512. Do NOT rerun or edit sources before polling22907. Standalone
truncation58867 TERMINAL0 increases oracle total7820, native39488 TERMINAL0
private actualruntime library /tmp/cubit-aml-find-bits-native-gxmnd23_,217Ada
hashes unchanged; Find_Set16bytes/Bit_Set8. Full concrete proof not yet claimed.
Logs /tmp/cubit-aml-find-bits-{validated,truncated,native}.log. Core helper
16383PASS fullquantified bit-position contract; no Assume/weakening. Old70154
failed/interrupted before helper fix; no dependency on that result.
Hardware promotion5949 +laternonblocking attempt TERMINAL75, unchangedkernel.
All new AML checks automatically included by existing oracle/ACPICA/proof lists;
no runner edits while blockedlock. Only own running job22907, no heldlock.

2026-10-01 independent AML scope while shared promotion lock remains busy:
FindSetLeftBit81/RightBit82 added to integers +expression/statement dispatch.
Python oracle7812PASS;initial ACPICA found explicit-target statements missing,
fixed statement opcode range. ASL reference requests now batch16for bit scans
to respect acpiexec1023commandline limit. Full bit-position postcondition and
loop invariant proved16383TERMINAL0 via separately contracted Bit_Set helper
(no weakening/Assume); earlier direct bitwise quantifier proof attempts failed.
Current hosted rebuild/oracle/ACPICA +concrete namespace/service proof running.
Promotion5949TERMINAL75 boundedwait, no kernel changes.

2026-10-01 ownership REVALIDATED: network note1201+ explicitly RELEASED
capabilities/syscall work; later startup proposal stays off it. Filesystem
note now confirms no overlap for scoped hardware-type mint rejection. Prior
waiting-for-ack interpretation superseded; only shared build lock pending.
Promotion prepared /tmp/cubit-promote-hardware.py (hash-prechecked two-file
patch, kernelGPR shared/hardware path, catalog/capability targets in run.sh,
convert candidate tests to actual kernel source; retire prepare.py/patch).
Bounded45s acquisition83160 TERMINAL75, no edits. Candidate base hashes still
match main. No own live jobs/heldlock. Next safe step retry script under lock;
then actual-source tests/proofs and docs update. Not globally blocked.

2026-10-01 private capability native63086/96999 TERMINAL0: actual patched
syscall-admin/catalog/capabilities compile then full native kernel compile138
objects at /tmp/cubit-hardware-capabilities-kernel-n1ldf8bk. No link/boot.
368baseline hashes: only generated build.ads date/hash changed; candidate
apply-check clean. Private GPR adds shared/hardware; REQUEST that narrow
kernel/cubit.gpr path in addition to pending capabilities/syscall-admin edits.
Live three files UNCHANGED; ownership acknowledgment not yet seen. Native
Resolve/Begin/Finish144/24/8bytes under existing -gnatp, not whole-stack proof.
Logs /tmp/cubit-hardware-capabilities-kernel{,-all}.log. Catalog standardrunner
retry lockbusy75 (unedited). All own jobs terminal; no held lock.

2026-10-01 private hardware capability89199 TERMINAL0:273 hosted +14SPARK
analysis checks0unproved/justified on candidate capability spec. Patch adds
group/member kinds and isPolicyMintable guard blocking generic mint of either.
Shared main capabilities/syscall-admin unchanged (hashes verified), patch
apply-check clean. Hosted Config stub only64slots; syscallhandler candidate NOT
compiled/executed. Initial20291 ghost-context compile error corrected.
Review at tests/aml-core/hardware-capabilities/kernel-admission.patch, README
reproduction. Ownership request below pending acknowledgment; no mainpromotion
or new hardware caps enabled. No own live jobs/lock. Log
/tmp/cubit-hardware-capability-candidate.log. Further independent AML/catalog
work can continue; not globally blocked.

2026-10-01 coordination REQUEST: need narrow kernel/src/capabilities.ads +
syscall-admin.adb edits to append named hardware group/member types and reject
them from generic POLICY_MINT_CAPABILITY. Existing ownership claims in network/
filesystem notes respected; live files UNCHANGED awaiting acknowledgment.
Reviewable private candidate patch tests/aml-core/hardware-capabilities/
kernel-admission.patch, generated against current sources. No syscall I/O or
Cspace installs enabled. Generic mint currently accepts raw object words; new
hardware types MUST be denied there. Working hosted candidate validation.

2026-10-01 catalog reservation68755 TERMINAL0:126 hosted/151SPARK checks
0unproved/justified. Begin_Access reserves one serialized transaction, matching
Finish ticket only; revoke retains metadata/busy, replacement/newaccess denied
whilebusy, receipts nonwrapping/preserved across inventory epochs. Actual
mapping pins/locks/IO/cspace still external and unimplemented. Native58279
TERMINAL0 private checked kernel-runtime library at
/tmp/cubit-hardware-reservations-native-en0qrhy4,85inputs unchanged. Checked
Begin/Finish frames9552/6384bytes (contractsnapshots), whole-stack unverified.
Logs /tmp/cubit-hardware-reservations{,-native}.log. Standardrunner +kernelGPR
shared/hardware path retry lockbusy75, both UNEDITED. No own live jobs/lock.

2026-10-01 hardware resolver60826 TERMINAL0:106 catalog hosted +92SPARK
analysis checks0unproved/justified;existing693830region checks PASS. Common
Is_Subset separates using installed authority from Can_Derive delegation.
Catalog Resolve returns internal descriptor only with current epoch, subset,
scope/catalog operation checks. Scope is trusted cspace input, NOT wire
permission; no syscall/boot catalog/reservation is implemented yet. Native
91679 TERMINAL0 private checked kernel-runtime library at
/tmp/cubit-hardware-catalog-native-yr7b4gy6,85hashes unchanged;Resolve272byte
staticframe, not whole-call bound. Logs /tmp/cubit-hardware-resolve.log and
-hardware-catalog-native.log. Runner integration still lockbusy75, unedited;
new kernel GPR source path still pending. No own jobs/lock.

2026-10-01 hardware catalog60678 TERMINAL0:92 hosted checks +77SPARK
analysis checks0unproved/justified. New kernel metadata private state,64slots,
unique IDs across ACPI/GPIO, fixed geometry validation, sealed membership,
nonwrapping epochs/revoke and Can_Select using shared permission narrowing.
No kernel boot instance/admission/syscall/Cspace/IO; backing lifetime still
external. Tests standalone README command; standard runner integration attempt
lockbusy75, NOT edited. Kernel GPR shared/hardware path still not added; no
native compile claimed. Initial85567/33754 compile-name errors corrected.
Log /tmp/cubit-hardware-catalog.log. All own jobs terminal, no held lock.

2026-10-01 own new kernel/src/hardware_catalog.* and disjoint
tests/aml-core/hardware-catalog: bounded kernel inventory metadata with sealed
class membership, nonwrapping epochs, unique IDs and permission narrowing.
No boot registration/syscall/Cspace mutation/hardware access yet. Hosted/proof
work in progress; no shared kernel GPR edit while compositor builds.

2026-10-01 common hardware authority41870 TERMINAL0: owned shared/hardware/
hardware_authority.ads (pure SPARK), actual ACPI dispatch now uses Permits.
Can_Derive enforces same resource/read-write/mask subset and delegation right;
ghost universal access-preservation theorem proved. Region hosted693830 +
native-envelope330280 PASS;111SPARK analysis checks0unproved/justified.
Native backend75425 TERMINAL0 staticlibrary actualruntime (generic template
compile, not live syscall). Logs /tmp/cubit-hardware-authority{,-native}.log.
Three GPR SourceDirs/source list and standard proof runner updated under lock.
No new kernel captype/syscall; ordinary capabilities derive same object, so
group-member selection needs kernel membership validation and revocation links.
Catalog/discovery/groups/nativehardware/completeAML still unfinished. No own
live jobs or held lock. Tests/docs scoped to actual relation and mock evidence.

2026-10-01 named-register23615 TERMINAL0:153153 policy/core tests +330280
actual native-envelope hosted checks;106 SPARK analysis checks zero unproved/
justified. Config now epoch-binds Register_ID/Offset/Width/Write_Mask, ID0denies.
Public wire replaced (no compatibility alias): [epoch, registerID, value, 0].
No caller offset/width; writes reject forbidden bits, no RMW. New postcondition
proves dispatched outcomes use configured ID and mask. Geometry remains internal
defense in depth. docs describe kernel-owned future catalog and explicit startup
groups/attenuated delegation; neither is yet implemented. User wants common
ACPI/GPIO groups discovered at boot and per-register/pin delegation, kernel-owned
address resolution. Do NOT treat userspace privileged backend as final design.
Native85883 TERMINAL75 lockbusy; no native compile of new config claimed.
Initial23485 also PASS before final added boundary tests/contracts. Logs
/tmp/cubit-acpi-named-registers-final.log and -native.log. All own jobs terminal.

2026-10-01 bulk preflight11286 TERMINAL0:41 hosted checks. Native adapter now
rejects stale tokens, exhausted revision, idle/nonreceiving phase and open
streaming table before grant acquisition. Core still rechecks; serialization
remains required. Native87503 TERMINAL0 actual runtime library +stackreport;
largest4315488bytes/20dynamic, wholecall stack unverified. Logs /tmp/cubit-acpi-
block-preflight.log and -preflight-native.log. No SPARK/nativeboot rerun.
Previous native-endpoint suite now integrated in standard run.sh under shared
lock; readback/bash-n confirmed. All own jobs terminal, no held lock.

2026-10-01 native envelope85418 TERMINAL0: new owned tests/aml-core/
native-endpoint executes production adapter with exact extracted runtime message
declarations and mock hardware. PASS330275 checks, log /tmp/cubit-acpi-native-
envelope.log. Forged stamps, exhaustive flags/reserved/lengths, whole-transaction
bounds, stale epoch, reply clearing and internal completion-ticket privacy.
Initial71357 terminal1 style warnings corrected via -gnatwJ for runtime syntax.
No production source changes, no native/kernel isolation proof. Shared runner
integration attempted twice, both lockbusy75; NOT edited. Standalone command
in new README; disjoint target complete. No own running jobs or held lock.

2026-10-01 backend isolation9832 TERMINAL0: moved core +independent protocol
passes152656 hosted/106 proof checks zero unproved/justified. Shared native18919
TERMINAL75 lockbusy; private27815 TERMINAL0 actual CuBit runtime ABI compile
with mock callback in /tmp/cubit-acpi-backend-native-vfhxv05c. Source212 Ada
hashes match current,9 compiledALI files no AML references. Native envelope
Dispatch96bytes/coreDispatch272/Execute384bounded, not whole-call-chain proof.
Logs /tmp/cubit-acpi-backend-isolation.log and -native-private.log. Production
backend.gpr excludesAML; separate service/startup/realcallback stillabsent.
No runtime privilege isolation claim. Existing regions runner resolves moved
units via updated GPR. No own live jobs/lock; no nativeboot/ACPICA rerun.

2026-10-01 owned backend isolation scope: moved regionpolicy/io from acpi/
to separate acpi-backend/, introduced independent protocol types to remove
ACPI_Requests/AML dependency. Updated regions.gpr under lock. New native generic
acpi_backend_endpoint +explicit backend.gpr closure excludes AML/table service.
No process startup or hardware authority granted; separate process remains
integration requirement. Hosted regions rebuild/proof +native library next.

2026-10-01 regionwire23353 TERMINAL0: PASS152656 hosted tests;106 SPARK
checks zero unproved/justified for policy+mock+instance including Dispatch.
Exhaustive nonzero reserved/flags, invalid lengths/widths, forbidden labels,
forged payload tags, stale epoch/huge offset, CCL32bit value halves and internal
pending ticket checks. Standard existing regions target already includes
instance, no shared script edits. /tmp/cubit-acpi-region-wire.log. Draft labels
0read/1write; fourwords epoch/offset/widthbits/value; replyF002 outcome/low/high/0.
No wire resource creation/revoke/completion; actual receive stamp adapter and
trusted admission/hardware integration still absent. No own jobs/lock.

2026-10-01 region wire23353 LIVE: ACPI_Region_IO.Dispatch draft read/write
packet parser; requests epoch/offset/widthbits/value only, authenticated stamp
separate, no admission/revoke/finish wire operations. Pending cleanup tickets
stay backend-only, output values split into32-bit CCL words. Existing region
tests/proof instance extended; no native receive loop or actual I/O.

2026-10-01 executor37867 TERMINAL0: hosted region tests PASS20041;101 SPARK
checks zero unproved/justified across policy +mock +Region_IO_Instance. Execute
instance explicitly analyzed/proved; generic template itself not independently
analyzed. Initial8495 unsupported generic formal Global aspect removed; mock
actual retains Global=>null and native callback still absent/unverified.
Executor denies before callback, validates scalar payload/read result width,
reserves before callback and finishes only confirmedcompletion. Uncertain
completion revokes+holds ticket, no retry. Standard proof target now includes
region_mock.adb region_io_instance.ads under successful lock, rg/syntaxverified.
No real I/O, wire protocol, atomic fieldupdate or native confinement claim.
Log /tmp/cubit-acpi-region-io.log. All own jobs terminal/no lock.

2026-10-01 active scope acpi_region_io.* typed generic executor +region_mock
and region_io_instance tests. Backend callback reached only after policy
reservation; incomplete callback revokes and retains ticket. No wire/native
endpoint or real hardware callback introduced. Hosted/proof in regions project.

2026-10-01 lifetime11282 TERMINAL0: region tests PASS20029,86 SPARK checks
zero unproved/justified. Begin_Access reserves single in-flight transaction,
nonwrapping backend ticket; Revoke preserves pending reservation, blocks new
requests, Install refuses busy records; matching Finish only, stale duplicate
completion cannot clear later transaction. Ready_To_Release only inactive+idle.
Initial58802 found discriminant-output API precondition; explicit unconstrained
Decision output added, no confinement contract weakened. Existing standard
region runner covers changes. /tmp/cubit-acpi-region-lifetime.log. All own
jobs terminal/no lock; no new native/QEMU/ACPICA run. Still model: trusted
admission, serialized transitions, backing pins and hardware callback wiring
remain required. Never execute bare Resolve without Begin reservation.

2026-10-01 region56401 TERMINAL0: ACPI_REGION_POLICY PASS20011,40 SPARK
checks zero unproved/justified. Backend-private stored base/range/tag/rights/
widths; Resolve accepts only authenticated stamp +request token/offset/width/op.
Revoke disables, trusted reinstall increments nonwrapping epoch; live replace
rejected. No capability minting or livehardware. Admission/synchronization remain
external obligations. Log /tmp/cubit-acpi-regions.log.
Correction: previous note claiming transactions runner integration was premature:
current read showed absent commands (likely lock-busy masked by bash -n). Both
transactions andregions commands NOW installed under successful lock and rg-read
verified at run.sh22-25/59-66, syntaxPASS. No fullrunner rerun. All jobs terminal
no lock. Continue backend admission/atomic check-through-access integration.

2026-10-01 owned new scope acpi_region_policy.* + tests/aml-core/regions.
Backend record uses trusted installation + existing authenticated endpoint tag;
request offset/width/op/epoch only. No raw bases in Resolve, no new capability
kind or live hardware handler. Serialized lifetime through actual I/O remains
required. Platform admission is external trusted prerequisite, not GAS proof.

2026-10-01 transaction29930 TERMINAL0:111 SPARK checks zero unproved/justified,
including ghost Byte_Difference conversion lemma. Requires timeout30 steps0;
standard5s retries failed only that lemma (no Assume added). Last hosted50921
PASS3248711 tests on current sources; only proof options changed for29930.
Integrated standalone transactions GPR/test/proof into run.sh under sharedlock;
script syntaxPASS. No native/hardware/ACPICA/fullrunner rerun claimed.
Exact bit-span geometry, aligned widths, whole-request bounds, IO limit,
address-overflow and indexed transaction containment verified. Width selection,
resource admission and live capability enforcement remain separate unfinished
work. No own live jobs or lock; goal active.

2026-10-01 current: transaction regression PASS3248711; standard5s proof
leaves one isolated ghost numeric-conversion lemma unproved. No Assume or
contract weakening; geometry/exact field span and bounded Address_At contracts
retained. Job29930 LIVE focused retry timeout30 steps0, log
/tmp/cubit-acpi-transactions-proof-long.log; do not edit source until terminal.
Prior50921 and earlier attempts terminal1. No standard runner integration yet.
Read-only authority audit found MAP_DEVICE accepts CAP_DEVICE_MEM; contract
now explicitly keeps device-memory/IO authority at backend, exposes only scoped
endpoint to ACPI so direct syscalls cannot bypass mediation. No kernel changes.

2026-10-01 user explicitly requires hardware capability confinement. Contract
now requires backend-owned independently admitted bounds/rights/widths, no
caller physicalbase/kernelVA or self-authorized AML regions, generation/owner
checks, whole-transaction bounds, synchronized lifetime and non-widening
delegation. Transactions geometry is NOT authorization. No live endpoint may
be enabled before backend-record integration and adversarial confinement tests.
Proof job15637 now TERMINAL1; inspect /tmp/cubit-acpi-transactions.log for
remaining obligations. No own live job/lock; geometry is not yet fully proved.

2026-10-01 latest discussion rules out blanket RW mappings; working proposal
uniform copied immutable snapshots via RO bulk grants plus mediated fresh
reads/explicit writes. No live shadow copyback. Keep interpreter/policy in
userspace and narrow scoped backends. Contract reflects this proposal.
Transaction job24876 TERMINAL1 missing end record; syntax fixed, not rerun yet.
No own live jobs/lock. Width geometry remains useful for mediated accesses.

2026-10-01 owned active scope: userspace/lib/acpi/acpi_fadt-transactions.*
and tests/aml-core/transactions/ isolated project. Geometry for explicitly
resolved hardware access widths; no GAS width-policy guess, no hardware or
authority/mapping changes. Standard integration deferred while compositor lock
held; own hosted outputs disjoint.

2026-10-01 user clarification: unsafe mixed pages containing live RW resources
cannot use immutable-table copy fallback. Contract explicitly requires mediated
original-backing access through scoped region operations, preserving widths,
side effects and transaction locks. No shadow-page copyback. Full-page direct
RW authority includes read disclosure. Hardware path remains unimplemented.

2026-10-01 copy fallback COMPLETE as component: hosted48865 TERMINAL0 catalog
PASS75767 (includes all9180 single-byte SDT corruptions), Copies30 SPARK checks
zero unproved/justified/Assume. Initial52944 compile missing byte visibility
fixed; native78513 Ada2012 aggregate syntax failure fixed in22485 TERMINAL0.
Native22485 fullkernel build/stackgate/link PASS /tmp/cubit-acpi-copy-native.log.
ACPI.Copy_Table now validates catalog index/extent/backing, invokes pure
Copy_Validated with actual source overlay, zeroes padding/failure output.
Caller must own disjoint destination and lifetime. Frames wrapper64/copy96bytes.
Copies integrated into standard proof list under lock; catalog tests already
standard. No new QEMU/export syscall/snapshot lifetime/grant mapping claim.
All own jobs terminal, no lock. Direct mapping remains preferred; this is its
unsafe-page fallback. Next direct map cache/lifetime/authority integration.

2026-10-01 latest user steering supersedes copy-default: prefer direct RO
original-table mappings where full-page contents/backing/lifetime permit; copy
whole table fallback for unsafe pages. No mixed copied-edge/original-interior
construction needed initially. Existing exposure/reclaim classifiers apply.
Interrupted copy work left firmware_tables-copies.* + catalog_tests additions
as unverified drafts; no tests/proofs/builds started and no kernel Copy_Table
created. No own live process or lock. Contract updated with latest preference.

2026-10-01 active scope: shared/firmware/firmware_tables-copies.* validated
copy into caller-owned pages with zero padding/failure wipe; catalog_tests;
then narrow kernel ACPI.Copy_Table primitive under shared lock. No reclamation,
allocator or process grant changes. Destination ownership and disjointness are
raw adapter obligations; full snapshot lifetime/startup not yet implemented.

2026-10-01 native bulk adapter COMPLETE as component: hosted59745 TERMINAL0
ACPI-NATIVE-BLOCK-MOCK PASS30 and focused endpoint proof (Dispatch_Block1 check,
zero unproved/justified). First71310 unused-use warning fixed before retry.
Shared native90725 TERMINAL75 lock-busy, no build; private31475 TERMINAL0 real
runtime static-library compile in /tmp/cubit-acpi-blocks-native-bla4qfzl; 219 Ada
source hashes match current checkout and copied inputs. Import_Grant208-byte
bounded frame, Retry_Return32 (not whole call-chain/AML stack evidence).
Logs /tmp/cubit-acpi-native-blocks.log and /tmp/cubit-acpi-blocks-native-private.log.
New typed ACPI_Native_Blocks acquires via provider capability, retains failed
return reference, blocks new import while pending; no arbitrary pointer from
wire. Provider freeze and tag/cap binding are explicit trusted obligations.
Added mock GPR/run to standard run.sh under shared lock; no full runner/ACPICA
rerun this turn. No wire grant request/live receive loop/provider startup yet.
All own jobs terminal; no lock held. Goal remains active.

2026-10-01 owned next scope: ACPI_Endpoint.Dispatch_Block classification and
native/acpi_native_blocks.* typed grant import/return lifecycle. Disjoint hosted
endpoint and mock native grant tests; no shared runtime/grant/kernel changes.
Native grant acquisition uses existing Acquire_Via_Capability, never PID claims.

2026-10-01 bulk94700 TERMINAL0: ACPI_Requests.Import_Block implemented and
documented, exact stable byte-slice input through existing bootstrap admission.
Request tests PASS92670, endpoint tests PASS589972. Focused request/endpoint
SPARK run passed; Import_Block12 checks, zero unproved/justified/Assume.
Aggregate cached project report4988 is not a fresh full AML proof. Log
/tmp/cubit-acpi-bulk.log. No shared runner/GPR/kernel/grant edits; existing
standard request test/proof targets include the change. No live jobs or lock.
Native grant authentication, source freeze/lifetime and startup transport remain
unimplemented; no new native/QEMU/ACPICA claim (AML semantics unchanged).

2026-10-01 owned active scope: ACPI_Requests.Import_Block adapter entry point
for stable mapped table bytes, plus existing request_tests regression and
focused request/endpoint SPARK proof. No kernel/grant/startup or shared runner
edits. Source lifetime remains an explicit native adapter obligation.

2026-10-01 user steering: default to copying immutable tables once into owned
snapshot pages and delivering read-only bulk/block grants; small IPC chunks are
fallback/debug only. Zero-copy original-table delivery is deferred, not a launch
prerequisite. Updated docs/acpi-service-contract.md; live MMIO/OperationRegions
and FACS/NVS retain actual backing and separate scoped hardware authority.
Cache model job36938 TERMINAL0: ACPI-PAGE-CACHE PASS2101594 and focused proof
success, /tmp/cubit-acpi-cache/obj/gnatprove/gnatprove.out. Pure
kernel/src/firmware_page_cache.* and tests/aml-core/cache_tests.adb are parked,
not integrated in standard runner/native use. No virtmem-firmware_cache adapter
or kernel cache hook was created. All own jobs terminal; no lock held.
Next handoff work should target the copied snapshot and existing grant/startup
machinery, not expand optional zero-copy prerequisites. Full goal remains active.

2026-10-01 backing integrated COMPLETE as a component: native47229 TERMINAL0
fullkernel build/stack/link, /tmp/cubit-acpi-backing-native.log. Kernel queries
Multiboot.Firmware_Reclaim_Pages and ACPI.Table_Backing_Is_Reclaim_RAM now
implemented; no mapping authority. Persistent backing.gpr and standard run.sh
include test/proof. Integrated41220 PASS24759 +47 SPARK zero unproved/justified.
Frames Covers32, Firmware_Reclaim_Pages32, ACPI query80, setup384 bytes.
All own jobs terminal, no lock held. No new QEMU run; earlier catalog58019 is
latest ACPI boot evidence. Remaining: cache/lifetime/ownership adapter,
authority-bound provider/startup transport, live service, complete AML. Keep
full goal active. Retained RAM kind and whole-page content are separate checks.

2026-10-01 backing53980 TERMINAL0: 24759 independent per-byte comparisons and
47 SPARK checks, zero unproved/justified, standard5s limit. New pure
kernel/src/multiboot_memory_map-reclaim.* plus backing_tests.adb. No adapter
edits yet: native90117 lock-busy (exit75 before edits). Prepared
/tmp/acpi-integrate-backing.py adds narrow Multiboot.Firmware_Reclaim_Pages,
ACPI.Table_Backing_Is_Reclaim_RAM and isolated backing.gpr/standard runner.
Next short native window requested to apply helper and make cubit_kernel under
shared lock. All own jobs terminal, no lock held. Current standalone test GPR
/tmp/cubit-acpi-backing/backing.gpr; model/source free for integration. Do not
claim kernel/native handoff or mapping authority. Retained RAM classification
is separate from content, cache, and lifetime checks. Full goal active.

2026-10-01 owned next scope: kernel/src/multiboot_memory_map-reclaim.* pure
coverage policy, tests/aml-core/backing_tests.adb and backing.gpr, then narrow
Multiboot/acpi read-only query adapters under shared lock. No allocator or IPC
changes. Classification is not cache or grant-lifetime authority. Prior own
jobs all terminal.

2026-10-01 exposure COMPLETE as a component: shared/firmware/firmware_tables-
exposure.ads/.adb (sibling, NOT Catalog.Exposure), kernel Table_Page_Exposure
query, standard test/proof targets integrated under lock. Private48032 PASS
61468 exposure +66574 catalog,137 SPARK zero unproved/justified. Fits moved to
public expression contract, no semantic change. Integrated63020 PASS same
checks + focused proof (aggregate4974 zero unproved/justified). Native54669
compilePASS, planner160/query16/setup384-byte frames. Full kernel95779 PASS
stack gate/link; /tmp/cubit-acpi-exposure-native.log. All own jobs terminal,
no lock held. Sources free; no new QEMU run for exposure query.
Next gaps: backing/cache/lifetime enforcement and provider transport. Existing
createGrant pins process-owned frames and retirement unpins them; do not insert
firmware pages into that path. Sysinfo generic CAP_PROCESS+READ is not firmware
provider authority. Exposure only proves content coverage of rounded pages;
Retained_Candidate must never itself authorize a mapping. Goal still active.

2026-10-01 next owned scope: shared/firmware/firmware_tables-catalog-exposure.*
and tests/aml-core/exposure_tests.adb, plus ACPI kernel query hookup. Pure
whole-page content coverage selection, not mapping/lifetime authority. No IPC
or allocator edits: existing grant path pins process-owned frames and its
retirement semantics need a distinct firmware-backed adapter. No own jobs.

2026-10-01 inventory58019 TERMINAL0: full kernel build/stack gate + 60s
QEMU desktop-protocol PASS. Serial ACPI-loaded123, protocolPASS657; no inventory
unavailable diagnostic. Logs /tmp/cubit-acpi-catalog-native.log and
/tmp/cubit-acpi-catalog-desktop.serial. All own jobs terminal, shared lock
released, sources unfrozen. Inventory kernel hookup now implemented and tested;
standard catalog test/proof and FWTS invocation integrated. Focused44486 PASS
66574 plus proof (aggregate4905 zero unproved/justified); direct54410 nativePASS.
Next handoff gaps: whole-page exposure/backing/lifetime policy, firmware-owned
read-only grant adapter, authority-bound startup/provider transport. Service
budgets32tables/64KiBper remain distinct from metadata256tables/1MiBper.
No live ACPI service/hardware policy or complete AML semantics claim.

2026-10-01 inventory native58019 LIVE: make cubit_kernel + desktop-protocol
60s QEMU under shared lock, log /tmp/cubit-acpi-catalog-native.log, serial
/tmp/cubit-acpi-catalog-desktop.serial. Kernel/catalog/runners frozen until
terminal. Inventory hookup and standard catalog target integrated under lock.
Integrated44486 PASS66574 plus focused proof; aggregate4905 zero unproved/
justified. Direct native54410 PASS. Fresh whole-record return exceeded stack
gate; replaced by Reset in place (8bytes; ACPI.setup384). No threshold waiver.
FWTS invocation integrated and syntax58091 PASS; default52586 PASS48.
No userspace grants/CCL startup/live ACPI launch. Full goal incomplete.

2026-10-01 FWTS runner invocation integrated under shared lock. New owned
scope: shared/firmware/firmware_tables-catalog.ads/.adb, kernel/src/acpi.ads
and acpi.adb retained metadata inventory only, tests/aml-core/catalog*.
No peer runtime/CCL edits; kernel native compile will hold shared build lock.
All prior own jobs terminal. Inventory exports no mapping or hardware authority.

2026-10-01 full21462 TERMINAL0: all hosted, SPARK4837 zero unproved or
justified, ACPICA FADT113 and all3314 AML differential comparisons PASS.
Actual upstream0PASS/12unsupported/0unexpected/339unselected; full AML remains
incomplete. FWTS52586 TERMINAL0 default download/hash-check path PASS48.
Sources unfrozen, no own jobs/lock held. Post-terminal FWTS runner integration
attempt returned lock-busy before editing. Prepared idempotent
/tmp/acpi-integrate-fwts-runner.py, apply under shared lock; current runner
still has ACPICA FADT but no FWTS invocation. Next implementation scope is
kernel retained-table export inventory and startup handoff; not implemented.

21462 LIVE ACPICA phase: full SPARK4837 zero unproved/justified PASS and
standard ACPICA FADT113 PASS. All hosted passed; native30943 current PASS.
Kernel handoff read-only audit: setup validates/walks tables but no complete
export catalog; MemoryAreas.Allocation_Map reserves touched pages of all
non-USABLE firmware types, BootAllocator admits usable only. Retained-source
catalog + page-exposure/lifetime adapter is next integration requirement. No
kernel/shared firmware source edits yet; library/runners frozen while21462.

2026-10-01 runner edits completed under lock: FADT child proof and ACPICA
FADT harness integrated. Standard21462 LIVE, sources/runners frozen. Native30943
TERMINAL0 current child; frame4315488/18dynamic. Service80929 PASS including
26282 extent +46748 parse +1049108 service checks. FWTS7595 TERMINAL0: new
fwts_tables.py compares48 SDT checksum diagnoses from3 pinned upstream fixtures,
allPASS (32accepted/16badchecksum); no FWTS binary/hardware execution. Pin and
SHA in script/docs, local archive /tmp/cubit-fwts-f06eeafe2650/source.tar.gz.
Need integrate FWTS invocation after21462 terminal under lock, not while live.
No own shared lock. Full service/AML goal remains incomplete.

FADT register96434 TERMINAL0: extent tests26282 + parser46748 PASS; focused
SPARK PASS (aggregate report4837 zero unproved/justified). New pure child
ACPI_FADT.Registers, called by FADT_Register_Tests inside FADT_Tests. No service
wire/hardware integration. Need standard proof-list child + pending acpica_fadt
runner invocation under shared lock; native/full run new child still pending.
User asked independent table suites: researched FWTS offline dump/table semantic
tests and AAPITS table-management scope; neither integrated yet. All own jobs
terminal, no lock held.

2026-10-01 scope new ACPI_FADT.Registers pure descriptor extent selection
and hosted tests. Suppress hardware-reduced blocks, validate lengths, choose
addressable extended span with explicit legacy fallback, bound entire I/O or
memory extent by subtraction. No register transactions or authority granted.
All prior own jobs terminal. No kernel/runtime/peer edits.

55172 TERMINAL0: all hosted + full SPARK4800 zero unproved/justified and
ACPICA3314 differential PASS. Actual upstream0PASS/12unsupported/0unexpected,
339unselected. Native56908 PASS current revision; largest4315488,18dynamic.
acpica_fadt.py standalone55592 PASS113. Post-terminal locked runner edit failed
lock-busy; prepared /tmp/acpi-integrate-fadt-oracle.py adds invocation once idle.
No own live jobs or lock, sources free for next ACPI work. Docs final evidence
updated. Full interpreter/native stack/kernel handoff/CCL still incomplete.

55172 LIVE ACPICA phase: full SPARK4800 zero unproved/justified PASS, including
Fixed_Description5checks; all hosted PASS (FADT46748/service1049108). No source
changes; wait terminal before adding acpica_fadt.py to standard runner under
lock. New harness standalone55592 PASS113, native56908 PASS current sources.

FADT independent oracle15047/55592 TERMINAL0: iASL -T FACP, data-table
compile and113 field/presence checks PASS. New acpica_fadt.py generates expected
Ada checks from labeled ACPICA template, uses separate temporary GPR/objects.
55172 still LIVE full proof (fresh artifacts); production sources unchanged.
Do not edit run-acpica.sh until55172 terminal; integrate new harness under lock
afterward. Native56908 current PASS; no own lock held.

FADT96529 TERMINAL0: hosted46748 and focused SPARK PASS. Fixed_Description
now decodes retained tables; integrated55172 --prove --acpica LIVE, sources
and runners frozen. Native56908 TERMINAL0 current revision: largest4315488byte
frame,18dynamic records; whole-stack remains unverified. Shared lock released.
Proof-list runner edit completed under lock before launching55172. No kernel
or runtime source changes; no FADT hardware normalization/access/CCL endpoint.

2026-10-01 FADT scope: new pure userspace/lib/acpi/acpi_fadt unit and hosted
fadt_tests invoked by existing service_tests. All prior jobs terminal. Decode
bounded immutable bytes, preserve optional-field presence; no hardware access
or kernel/shared firmware/runtime edits. Runner proof-list edit deferred until
shared lock available.

2026-10-01 retention31247 TERMINAL0: all hosted + SPARK4712 zero unproved or
justified + ACPICA3314 differential PASS. Actual upstream CuBit0PASS,12unsupported,
0unexpected,339unselected. Native67235 TERMINAL1 lock-busy before compilation;
no native evidence for retention revision. Docs record final results. All own
jobs terminal, no lock held. Kernel snapshot/grant adapter, native allocation
and live service/CCL integration remain open; full AML goal remains active.

31247 LIVE: all hosted suites and full SPARK4712 checks zero unproved/justified
PASS; standard ACPICA phase underway. User asked about original-page handoff:
contract now explicitly allows zero-copy retained read-only firmware pages
with page exposure/lifetime/backing checks; copy is baseline, not ACPI rule.
Existing process-owned grant API needs firmware lifetime adapter either way.
No kernel/runtime/shared source edits.

95657 TERMINAL0: request92574 PASS and focused request SPARK proof PASS after
bounded conversion repair. Starting full standard rerun --prove --acpica on
frozen current sources. Kernel handoff design clarified in service contract:
dedicated owned immutable snapshot pages before reclaim, existing startup
capabilities, trusted provider imports; no implementation/kernel edits yet.

55619 TERMINAL1: three unproved query checks (index and offset conversion
bounds); hosted passed, ACPICA not reached. Service retention proofs passed.
Repair keeps wire validation outcomes but checks bounded Natural index/offset
after fixed-limit conversion. Focused request proof/test next; no shared lock.

2026-10-01 retained catalog final55619 LIVE: all hosted suites passed, including
service1049105/request92574/endpoint589972; proof phase still running, no final
proof or ACPICA outcome yet. Sources/runners frozen. Native23351 TERMINAL1
before compile: shared lock busy (compositor native Settings run); no native
evidence for this revision. Docs updated with kinds/query layouts and explicit
hosted64MiB stack budget/native gap. Scoped diff check clean. No own shared lock.

Retained-table runtime checks:94783 service1049105/request92153/endpoint589972
PASS;88163 expanded request queries PASS92574. Full standard --prove --acpica
now starting on frozen service/AML sources and runners. Shared locked edit
installed explicit64MiB hosted stack budget in run.sh/run-acpica.sh; locks released.
Default8MiB stack overflow observed (not hidden): checked Install frame4315504,
Handle2631296, test Request main4045600 bytes. All assertions remain enabled;
native whole-call/secondary-stack requirement remains unresolved, no launch.
Description kind2 admits immutable SDTs (not FACS/DSDT/SSDT misclassification).
Labels6/7 are complete-snapshot revision-checked info/eight-byte queries;
payload split into32-bit halves for signed CCL words. No authority added.

2026-10-01 active retained-table service work after user's non-AML table review.
Scope owned ACPI_Service/Bootstrap/Requests + existing service/request tests,
README/contracts. Prior own jobs terminal, no lock held. Retain validated SDT
bytes/identity, accept non-AML description tables without interpreting payload,
and expose bounded read-only catalog/chunk queries after snapshot completion.
Keep DSDT-first bootstrap and reject misclassified DSDT/SSDT and mutable FACS.
No kernel/shared firmware/runtime/staging sources in this slice. Existing memory
budgets remain; measure stack impact and do not claim live service readiness.

2026-10-01 temporary-method milestone VERIFIED HOSTED; overall goal ACTIVE.
All own jobs terminal, no lock held. Final26651 TERMINAL0: all hosted suites
PASS (call4818), SPARK4616 zero unproved/justified.36651 existing3288 ACPICA
comparisons PASS;93072 installed dynamic26 PASS =>3314 total. Pinned upstream
reference12PASS/admission8PASS/execution0PASS/12unsupported/0unexpected,
339unselected. Current arithmetic/logic blocker DataTableRegion in STRT after
creating M555; control table-byte limit unchanged. Harness + baseline promoted
under shared lock. Scoped diff check clean. No staging/kernel/hardware changes.
Native87445/85144 both lock-busy before compile; current native revision still
PENDING.95098 predates these changes. No commits/pushes. Next required work:
retained admitted-table bytes and region/field semantics, temporary data/reference
lifetimes, whole-call/secondary-stack bounds, native service + CCL/event plumbing.

Implementation: Begin/End callbacks surround all mutable execution exits;
Active_Calls is per method, Owner/Alive track temporary method nodes. Last exit
removes its subtree and owned declarations elsewhere; interior dead slots are
reserved to keep IDs stable, tail slots/code reclaimed. Existing integer writes
survive failures. Insert_Frame/Present guarantees and cleanup bounds/ownership
postconditions proved in both namespace instantiations. Read-only wrappers still
reject declarations/writes; no execution authority added to service requests.


97287 focused lifecycle proof TERMINAL0: Begin_Method7, Define_Method42,
End_Method44 checks proved, no unproved/justified. Full final26651 --prove LIVE
on frozen core; native compile attempt starting through run-native.sh (lock
managed there).36651 prior3288 comparisons PASS; installed26-case dynamic
oracle + upstream rerun after hosted rebuild completes. No source/runner edits.

26068 intentionally stopped130 after two proof gaps (cleanup count bound and
inserted-node Alive fact). Contracts/invariants strengthened without weakening
checks.36651 TERMINAL0 all3288 existing ACPICA comparisons PASS. Dynamic
harness and DataTableRegion reason now PROMOTED under acquired shared lock;
lock released immediately. Focused lifecycle proof now starting on frozen core;
next full hosted/proof, installed26-case/upstream rerun and native compile.
No own native job/lock; no core or runner edits during validation.

26068 full hosted suite PASS, proof LIVE;36651 current ACPICA3288 regression
run LIVE (compare/packages/counts/coercions/typed PASS so far). Core frozen.
Need brief idle lock window to run /tmp/acpi-promote-dynamic.py, then native
compile under run-native.sh. Promotion adds26-case dynamic-method oracle and
updates upstream reason to DataTableRegion. No own lock/native job. Error
oracle now matches final ACPICA evaluation status; staged script ready.

15295 hosted expanded dynamic/lifecycle/capacity/truncation tests PASS4818.
67347 focused proof TERMINAL1: only missing Append_Code capacity proof across
Insert. Insert_Frame and Method_Usage preservation contracts now strengthened;
End_Method also preserves Values, bounds storage and excludes live owned nodes
on final exit; Define_Method failure guarantees unchanged Tree. No assumptions.
Final full hosted+proof now starting on frozen core. ACPICA92427 PASS26.
Promotion script /tmp/acpi-promote-dynamic.py ready for short locked edit:
new harness, runner entry, upstream DataTableRegion reason. Lock attempt busy.
No own lock/native job. Core/runners frozen during proof; no shared edits yet.

Dynamic-method work: first hosted52956 PASS4520; initial ACPICA48103 PASS18.
Focused namespace proof67347 LIVE on frozen core. Expanded ACPICA92427 live;
capacity/truncation hosted cases added but not yet rebuilt. Lifecycle callbacks
surround every mutable invocation; per-method Active_Calls defers deletion until
last exit. Temporary method nodes carry Owner/Alive; dead interior slots stay
reserved, tail nodes/code reclaimed without moving live IDs. End removes own
objects and method subtree, matching cross-owner recursive ACPICA case.
No native/shared runner edits or lock held; compositor71551 now holds lock.

2026-10-01 active method-local declaration lifecycle work. Prior milestone
jobs all terminal. Scope owned AML_Execute/AML_Namespace + call_tests and
ACPICA comparisons; no shared/kernel/runtime sources. ACPICA uses per-method
active counts and deletes owned declarations on last exit. Implement temporary
method nodes with stable live IDs, deferred tombstone reclamation and cleanup
on errors; no concurrent AML threads claimed. No build/proof/native jobs yet.
Shared runner edits/native build deferred to locked window; graphics79145 held.

2026-10-01 serialized milestone COMPLETE; overall ACPI goal ACTIVE.
All own jobs terminal, no lock held. Hosted72364 PASS (method533/call4072),
SPARK4117 zero unproved/justified.72945+42057 ACPICA3288 comparisons PASS.
Pinned upstream: reference12PASS, admission8PASS, execution0PASS/
12unsupported/0unexpected failures,339unselected. Current arithmetic/logic
blocker is method-local Method in STRT; control is table-byte limit.
95098 native compile PASS current evaluator; largest frame1624560 bytes,
18dynamic records. Whole-call/secondary-stack bound unverified; no launch.
Synchronous serialized ordering implemented with exclusive context ownership;
concurrency, yielding and explicit mutex opcodes remain unfinished. New
acpica_serialized.py is included in run-acpica.sh; upstream baseline updated
under shared lock. Scoped diff check clean. No staging/kernel/hardware writes,
commits or pushes. Next: method-local namespace lifecycle, other mutable types,
stack bounds and native service/CCL/event integration remain required.


42057 TERMINAL0: installed serialized harness PASS1200; pinned upstream
reference12PASS, admission8PASS, execution0PASS/12unsupported/0failures,
339unselected.72945 remaining existing differential run live. No own lock.

95098 native compile TERMINAL0 on current mutable+serialized evaluator.
Largest compiler frame1624560 bytes,18dynamic records; whole-call/secondary
stack bound still unverified. No native launch/hardware/staging. Lock released.
72945 and42057 differential/upstream jobs still live; core/runners frozen.

Shared-lock edit complete and lock released: acpica_serialized.py installed,
run-acpica.sh includes it, upstream baseline updated to method-local declaration.
72945 existing differential rerun live;42057 serialized+upstream rerun live.
No runner/core edits during these jobs. Native retry uses run-native.sh.

72364 TERMINAL0: complete hosted tests and SPARK4117, zero unproved or
justified checks.49493 confirms all8 arithmetic/logic modes now UNSUPPORTED.
Core frozen. Existing ACPICA comparisons rerunning. Shared-lock promotion
script ready at /tmp/acpi-promote-serialized.py; still waiting short idle
window to install new harness, update upstream baseline and compile native.
No own lock/native job.

Current serialized work: hosted method533/call4072 PASS; new ACPICA matrix
74722 terminal0 PASS1200 (all16x16 levels, bridges, recursion, restoration in
both widths).72364 --prove live on frozen core. Need short shared-lock window
to install prepared build/serialized_suite.py into standard runner and update
upstream known blocker from SYNC_UNSUPPORTED to method-local declaration in
STRT (first instruction in pinned common.asl:1416). Native compile also queued.
No own shared lock/native job. Not claiming concurrent AML/OS mutex support.

2026-10-01 active synchronous serialized-call support. Prior turn made verified
mutable Store progress; all prior jobs terminal. ACPICA primary code confirms
sync-level ordering before recursive acquisition; nonserialized methods ignore
encoded sync bits and inherit caller level. Core now carries immutable per-call
Current_Sync, rejecting lower serialized calls with Mutex_Order and restoring
caller level naturally on all returns. Exclusive context ownership required;
no concurrent AML threads, explicit mutex opcodes or OS mutex adapter claimed.
Method533/call206 initial checks PASS; exhaustive order/bridge/recursion tests
being added in owned call_tests. Native lock attempt busy, no own lock held.

2026-10-01 current: named integer Store/operation targets now execute through
Invoke_Mutable with shared state across nested calls. Named integer reads
capture their value; Local/Arg reads retain deferred slots (ACPICA distinction
verified). Final46346 TERMINAL0: hosted calls204 + prior suites PASS;
SPARK4101 zero unproved/justified; ACPICA2088 comparisons PASS including48new.
Upstream reference12PASS, admission8PASS, execution0PASS/12unsupported/
0unexpected failures,339unselected. Arithmetic/logic now stop at serialized
call SYNC_UNSUPPORTED; control still table-byte limit. All own jobs terminal.
No lock held; native rebuild deferred because shared lock remained busy.
No staging/kernel edits, hardware execution, commits or pushes. Full goal
active: serialized methods, general mutable objects/references, stack work and
native service/CCL/event integration remain unfinished.

46346 hosted and full proof phases now PASS on final source; ACPICA phase
live. No unproved diagnostic in final run; authoritative summary recorded in
build/obj/gnatprove/gnatprove.out. Native remains pending shared lock (graphics
31113 window per compositor note), no own lock. Core/runners frozen through
46346. Exact upstream expected blocker is SYNC_UNSUPPORTED, not Store failure.

Current final rerun46346 --prove --acpica live.39533 intentionally stopped130
after one missing unbound-context fact: Empty_Valid now explicitly guarantees
its result is True, proved from body. No runtime behavior change, no invariant
weakening.34183 focused mutable namespace proof already passed. All hosted
suites passed before that diagnostic. Native16027 lock-busy before compile.
No shared lock held; no core/runners edits until46346terminal.

Current34183 terminal0: focused namespace proof passes with explicit
Context_Valid pre/post/loop invariants and contracts on actual callbacks.
No formal callback aspects (triggered GNAT compiler bug); no assumptions or
weakened namespace invariant. Runner integration complete under5000 (terminal0).
Native41373 exited1 before compile, shared lock busy. Starting final integrated
hosted + full proof + ACPICA workflow on frozen source. No ACPI lock held.

Current proof repair: stopped50630/45513 (both terminal130) after mutable-context
invariant failures, before source edits. Added explicit Context_Valid predicates
to generic entrypoints, helpers and loops. Formal callback Pre/Post triggered
GNAT16 internal compiler error; moved contracts to concrete callbacks instead.
34183 focused compile/calls/proof live. Runner edit lock5000 acquired for60s:
installed acpica_named_store.py (48cases), run-acpica entry, and tightened
upstream expected status to SYNC_UNSUPPORTED; all8 independently confirmed.
No core edits during34183. Native final compile still required.

Current: corrected mutable evaluator passes48 new ACPICA named-store cases
(66272terminal0) plus calls204 and full hosted suites. Source50630 proof live.
WriteTarget now parses bounded NameString destinations; integer values capture
on initial named read, unlike deferred Local/Arg slots. Execute_Typed passes
one in-out namespace through nested calls; read-only wrappers reject writes.
Need short shared window to install prepared named_store_suite.py (48cases)
and run-acpica entry, and update upstream expected blocker to serialized-call
SYNC_UNSUPPORTED after checking all8 configurations. No lock held, no native
jobs. Do not edit owned core sources during50630.

Mutable evaluator integration: initial named-source deferral was disproved by
ACPICA14994 (Add(NUM0,Store(7,NUM0)): expected16, got14). Stopped own90043
terminal130 before editing. Restored eager named integer captures while keeping
deferred local/argument slots;60528 targeted compile/calls204PASS plus40case
ACPICA oracle live. Mutable namespace uses in-place integer writes and preserves
completed writes on later errors. Table_runner now uses Invoke_Mutable.
Upstream arithmetic now reaches SYNC_UNSUPPORTED (serialized callee), not
named Store rejection. Need guarded permanent oracle integration and upstream
reason/status update; shared lock still busy. No lock held or native job.

2026-10-01 active evaluator integration: prior turn made verified integer
mutation progress. Reworking AML_Execute core into Execute_Typed procedure
with in-out context and explicit write callback shared by recursive calls;
read-only entrypoints wrap it with a rejecting callback. Scoped owned AML/test
files only, no shared runner edits. Hosted23508 compile/regression live. No
native jobs or lock held; pending native compile will follow final source.

2026-10-01 current: exact in-place integer mutation in AML_Objects and
AML_Namespace, preserving object identities, package links, allocation counts,
namespace entries and methods. Final46054 terminal0: hosted object12378 and
prior suites PASS; full SPARK3070 zero unproved; ACPICA2040 comparisons PASS.
Upstream reference12PASS, required admission8PASS, execution0PASS/12unsupported/
0unexpected failures,339unselected. Evaluator named Store not yet connected.
All own jobs terminal, no shared lock held. Native14725 compiled before final
explicit Count postcondition; final rebuild deferred on shared-lock contention
(latest29673 terminal1 before compile). No native launch or staging changes.

Current mutation verification: 76933 terminal1, two namespace invariant proof
obligations unproved. Strengthened AML_Objects.Set_Integer with explicit Count
preservation (in addition to exact frame and Usage); focused82181 terminal0
both namespace instantiations proved. Final46054 --prove --acpica live.
Native67229 lock busy, no compile; earlier14725 predates added contract clause.
No shared lock held. No source edits until final46054terminal.

46054 hosted + full SPARK phases passed: 3070 results, zero unproved/justified.
ACPICA phase still live. Final native retries67229/7221/91302 and explicit
outer-flock invocation all failed lock acquisition before compile; no native
source failure. Request short native window for final contract clarification
when shared lock free. Earlier14725 compiled same body before explicit Count
postcondition; frame1624560/13dynamic. No ACPI lock held or native process live.

Final46054 terminal0: complete hosted/proof/ACPICA workflow PASS against current
baseline, SPARK3070 zero unproved, differential2040PASS, upstream admission8PASS
but no suite execution passes. No source changes during final run. Final native
compile still pending because lock remained occupied; all retries terminal.

2026-10-01 continuation: prior turn made verified admission progress. Adding
in-place integer mutation to owned AML_Objects with exact ghost frame condition
and full-arena/alias preservation tests, as prerequisite for mutable namespace
execution. No runner/shared/native edits or lock held. Compositor native window
respected. All previous ACPI jobs terminal; hosted proof next, -j2.

Object mutation focused68498 terminal0: object12367 checks and focused SPARK
PASS. Namespace Set_Integer now preserves all entries/method storage and uses
the exact arena update relation; hosted fixture invokes GET0 after writes.
Full76933 --prove --acpica live on frozen source (80752 failed compile for one
missing equality visibility clause, fixed before restart). Native compile
requested through run-native.sh shared lock; no runtime/staging edits.

Native14725 terminal0 current code, largest1624560bytes/13dynamic, no launch.
Full76933 hosted PASS object12378 and prior suites; proof then ACPICA live.
Namespace Set_Integer is an internal storage operation; evaluator still reads
an immutable context during each invocation. No new execution IPC authority.
No shared lock held or native job active. Sources frozen until76933terminal.

2026-10-01 current: complete arithmetic/logic table admission after arena
increase to2048 objects/8192 elements. Hosted6308 +SPARK3005 terminal0,
zero unproved. Upstream admission gate edited under lock85892, now released.
Native21701 terminal0: largest1624560-byte frame/13dynamic; whole-call and
secondary-stack bounds unverified. No lock held. Differential47406 terminal0:
2040 comparisons PASS. Upstream57767 terminal0: reference12PASS, admission8PASS,
CuBit execution0PASS/12unsupported/0unexpected failures,339unselected.
All own builds/tests terminal. Next: mutable namespace execution and stack
storage redesign; full service/CCL/event/hardware integration remains incomplete.

2026-10-01 continuation: diagnosing upstream VALUE_LIMIT using hosted table
admission metrics, scoped to owned AML/test files. Prior turn made verified
progress (2040 comparisons terminal PASS). No native/shared build or lock held;
compositor current native window respected. No live prior ACPI commands.

Admission diagnosis: arithmetic P000 exceeded1024 object slots (1014 used,
21 needed), then P052 exceeded4096 elements (4083 used,26 needed). Development
budgets now2048 objects/8192 elements. All8 arithmetic/logic tables load;
MN00 next fails UNSUPPORTED at named Store (SLCK/MLVL). Hosted table_runner
--metrics exposes admitted counts. Need short shared-lock window to update
upstream runner to REQUIRE successful admission before accepting execution
UNSUPPORTED, then current native compile. Lock probe failed (busy); no edit of
runner yet. Starting owned hosted tests/proofs only, -j2, disjoint outputs.

Hosted6308 terminal0 allchecks PASS, SPARK3005 zero unproved. Initial24228
failed one stale hard-coded element-capacity fixture, now parameterized and
passing. ACPICA differential47406 live. Runner admission gate edited while
own lock85892 held (60-second bounded edit window); source/core frozen.
Native40777/72656 did not compile (lock acquisition exit1); still pending.

Native21701 subsequently terminal0 compiling current core, largest1624560bytes,
13dynamic records. No launch/staging/kernel edits. Edit lock85892 terminal0;
no ACPI shared lock remains held. ACPICA47406/57767 live on frozen sources.

Final47406 terminal0 all2040 differential comparisons PASS. Upstream57767
terminal0, all8 required admission gates PASS with usage recorded; reference
12PASS, execution0PASS/12unsupported/0unexpected failures,339unselected.
Core proof6308 and native21701 cover final source. No shared lock or own live
build/test remains. No commits, staging or native service execution.

2026-10-01 current: named VarPackage counts +512-node service budget and last-load
metrics. SPARK3005 zero unproved; ACPICA38 new comparisons PASS; hosted8758
count cases and prior suites PASS. Native89187 terminal0, runner integration
completed under shared lock after graphics yielded window. No shared lock held;
window request fulfilled. Largest frame1174000bytes/13dynamic; no whole-stack
bound or native execution. Final integrated35968 terminal0: hosted PASS,
ACPICA2040 differential comparisons PASS; upstream reference12PASS, CuBit
0PASS/12unsupported/0unexpected failures,339unselected. All own jobs terminal.
No kernel/runtime/staging edits.

2026-09-30: active first milestone, hosted only.
Own new userspace/lib/acpi/, tests/aml-core/, docs/acpi-service-contract.md,
coordination/acpi.md and narrow audit correction in docs/acpi-userspace.md.
No kernel, driver, Desktop, startup, runtime, shared build or staging edits.
Read coordination README and current ownership notes. Graphics retains GPU
power sequencing; compositor retains Desktop. Any future overlaps require ack.

Plan: pure bounded AML integer and package framing decoder, hosted adversarial
checks and SPARK proof attempt; explicit service authority/event contract and
interpreter option assessment. No hardware access or native service activation.
Commands: nix develop -c bash tests/aml-core/run.sh --prove (private output,
2 prover jobs maximum). No shared build lock needed for disjoint hosted output.
Native work later must use shared lock or tools/build-workspace.py snapshot.

Milestone complete, 2026-09-30: hosted AML-DECODE-CHECK PASS395831;
SPARK level2 36 results, 0 unproved/justified. Evidence in
 tests/aml-core/build/obj/gnatprove/gnatprove.out and README.
Core integer/package framing only; no namespace or executable native service.
Contract documents scoped opregion denial, policy/driver boundaries, SCI/GPE/EC
and laptop roadmap, ACPICA/uACPI/Ada tradeoffs. No third-party code added.
All commands terminal, no lock held, no shared build/stage/kernel edits.
No QEMU or physical result claimed. No commits or pushes.

2026-10-01: active continuation toward full verified userspace service.
Next owned code: AML_Names bounded NameString parser and hosted/proof fixtures.
No shared source changes; same disjoint outputs and Nix two-job proof limit.

2026-10-01 NameString milestone verified: PASS166277 hosted cases plus prior
PASS395831. Combined SPARK102 results, zero unproved/justified, including
published segment grammar, extent, root/parent separation and termination.
No namespace loader yet. Full verified interpreter/service goal remains active.
All jobs terminal; no shared source or staging changes; no lock held.

2026-10-01 active namespace foundation: owned AML_Namespace generic bounded
store with parent/name lookup, atomic insertion and append-only node IDs.
Hosted/proof only, -j2; no native/shared edits.

2026-10-01 namespace storage complete: hosted8136 plus existing suites PASS;
explicit SPARK128-node instance combined164 results0unproved/justified. Verified
parent ordering, lookup match/absence, insertion preservation/failure atomicity.
Resolution/loading pending. User clarified ACPI's supporting role in lid,
backlight and hibernate; concrete responsibilities recorded in service contract.
All jobs terminal; no lock held, native/shared/staging changes or commits.

User decision: CCL IPC for events/queries plus metrics and log observation.
Contract records separately granted typed observation/control, snapshot/sequence
gap recovery, bounded counters and shared CuBit.Logging/logstore reuse. CCL
catalog/runtime files untouched; coordinate before their eventual integration.

2026-10-01 active: Resolve existing namespace objects, strict root/parent and
multisegment lookup, ancestor search only for unprefixed single-segment paths.
Owned sources/tests only; Nix hosted/proof -j2, no shared output mutations.

Resolution verified: hosted383 cases plus earlier suites pass. SPARK combined214
results0unproved/justified; existing result/final segment and termination proved,
full path/nearest-ancestor behavior regression-tested. Namespace object values,
loader and functional refinement proof remain. All jobs terminal, no shared
lock/build/staging/native changes. Next: namespace object values and transactional
Name/Scope definition-block loader to consume actual AML data.

2026-10-01 active: integer-valued Name term-list loader, candidate-state commit,
strict unsupported-opcode failure, bounded namespace object storage. Nix hosted
proof only; no shared edits/stages. Scope/Device parsing remains next.

2026-10-01 integer Name loader verified: hosted3868 +all earlier suites PASS;
SPARK combined276 results0unproved/justified, including failure atomicity.
Root-level Name integer subset only, optional existing intermediate scope nodes;
no typical DSDT support yet. Scope/Device, other values and methods pending.
All jobs terminal, no shared lock/staging/native changes, no commits/pushes.

2026-10-01 active nested loader: explicit64-frame Scope/Device traversal,
bounded enclosing slices, reference vs declaration lookup, transactional state.
Hosted/proof only; shared files/stages untouched.

Nested Scope/Device milestone terminal: loader3948 +existing suites PASS;
SPARK315 results0unproved/justified. Fixed64 frames, enclosing-slice containment,
lexicographic termination, failure atomicity verified. Scope resolves existing
non-integer nodes; Device creates structural nodes; full object kinds/predefined
namespace/other values/method execution pending. No native/shared changes.

2026-10-01 active object typing/string groundwork: explicit Scope/Device/Integer
kinds and bounded string literal parser. Owned core/tests only; Nix -j2 proof.

Object kinds/string milestone terminal: string66310 plus prior suites PASS;
combined SPARK369 results0unproved/justified including accepted string byte
correspondence. Device identity preserved; string/integer rejected as scope.
Fixed255-byte string budget explicit; no native/CCL/shared-stage changes. Next
work includes buffer/package values and method storage/execution, then admitted
snapshot/service/CCL integration. No commits/pushes or active jobs.

2026-10-01 active: bounded literal BufferSize Buffer parsing and namespace
storage, max1024 bytes explicit. No resource-authority grants, shared edits or
native staging. Hosted/proof Nix -j2.

Buffer milestone terminal: hosted48399 plus previous suites PASS. SPARK429
results0unproved/justified; extent/termination and transaction failure atomicity
proved, buffer value semantics tested. Fixed1024 buffer budget, constant-size
subset explicit. No resources authorized or native/shared artifacts changed.
Next: method representation/execution and functional semantics; generated ASL
fixtures need ACPICA tools (not currently listed in pinned flake). No active jobs.

2026-10-01 active execution core: raw-body integer/Arg/Local, Store-to-local,
Noop/Return with fuel and explicit errors. No hardware/namespace mutation or
native service, no shared files. Nix hosted/proof -j2.

Execution foundation terminal: hosted2099 +all previous suites PASS; SPARK476
results0unproved/justified, including fuel bound/termination. Raw body only,
integer Args/Locals/Store/Return/Noop; no method binding or expression evaluator
or native/CCL effects. Full goal active; method storage and expression semantics
are next. All jobs terminal; no shared lock/artifact changes, commits or pushes.

2026-10-01 active Method declaration/invocation binding. Own bounded copied
bodies and flags/width metadata, explicit argument/synchronization rejection.
Hosted Nix/proof only, no shared/native/CCL edits.

Method binding terminal: hosted533 plus all prior suites PASS; combined505
SPARK results0unproved/justified. Bodies owned (1024 limit), flags/width retained;
argument mismatches and unsupported synchronization rejected before execution.
No native service/CCL changes or hardware effects. Full goal remains active;
expressions/general values/calls/synchronization and integration still needed.
All commands terminal, no lock held or shared artifacts modified.

2026-10-01 active nested integer expression stack,64 frames, Add/Subtract/
Multiply/And/Or/Xor with Null/local targets. Nix hosted/proof only, -j2.

Nested expressions terminal: hosted12367 plus prior suites PASS. SPARK544
results0unproved/justified; expected unused standalone expression output warning.
Six integer binary operators,64-frame depth, Null/local targets, normalized
widths and ordered target effects. No native/CCL/hardware/shared changes.
Full interpreter/service goal active; control flow/general values/calls and
semantic proofs/integration remain. All jobs terminal, no lock held.

2026-10-01 active new userspace/services/acpi/acpi_service pure state core.
Admitted DSDT/SSDT snapshots, bootstrap IDs, budgets, transactional loading,
read-only metrics/namespace snapshot. No startup/catalog/kernel/shared edits.
Hosted tests/proof Nix -j2.

Table service core terminal: ACPI-SERVICE-CHECK85 plus previous suites PASS;
SPARK856 results0unproved/justified (includes service namespace instantiation).
Table ordering/width/budgets and transactional namespace boundary implemented;
metrics in pure core, not CCL yet. New userspace/services/acpi owned, no native
main/startup/IPC/catalog/kernel edits. All jobs terminal, no lock held.

2026-10-01 active functional arithmetic contracts (AML_Integers), used by actual
executor; independent Python unbounded-integer boundary oracle through Run.
Owned core/tests only; Nix hosted/proof -j2, no shared outputs.

Integer semantics terminal: AML_Integers exact six-op result contracts proved,
actual executor uses them. Independent Python arbitrary-precision oracle1728
boundary cases PASS plus all earlier suites. Combined SPARK858 results0unproved/
justified. Proof is local integer operations, not whole-expression refinement.
No native/shared/CCL changes, active jobs or lock; goal remains active.

2026-10-01 active If/Else execution with64 block frames, package-bounded
predicates, explicit malformed-package/depth failures. Nix hosted/proof -j2.

If/Else terminal: hosted588 +all earlier suites PASS; combined SPARK895 results
0unproved/justified.64 block frames, predicate package isolation, branch resume
and skipped-body behavior. While/calls/general values still pending; native/CCL
integration untouched. All jobs terminal, no shared lock/staging changes.

2026-10-01 active While/Break/Continue with nearest-loop unwinding and shared
fuel. Nix hosted/proof only, -j2; no native/shared edits or hardware effects.

Loop milestone terminal: AML-LOOP-CHECK309 and prior suites PASS; SPARK901
results zero unproved/justified. While/Break/Continue bounded termination and
nearest-loop regression coverage. No native integration claimed.
Active: hosted ACPICA differential adapter in tests/aml-core only. Reference
acpica-tools20260408 from existing pinned Nixpkgs, no shared flake edits.

ACPICA source release20260408 pinned+SHA256 verified in tests/aml-core scripts.
Actual unmodified upstream arithmetic/control/logic integration in four modes;
all other runtime entrypoints inventoried as not run. Known service gaps remain
explicit, strict --require-full fails them. Generated differential harness found
external argument object truncation and predicate-conversion placement mismatch;
corrected raw Args preservation and width-normalized If/While predicates.
Current validation: nix develop -c bash tests/aml-core/run.sh --prove --acpica,
exec session15780 (hosted/disjoint only). No native/shared artifacts changed.

Combined suites/proof completed:900 SPARK results, zero unproved/justified;
142 generated ACPICA comparisons PASS; all12 upstream reference configurations
PASS, CuBit12 explicit unsupported,339 runtime entrypoints unselected. First
wrapper exit was disrupted by my comment edit while Bash was reading it; full
unchanged runner is being revalidated as session6845. Do not edit a running
shell script even when outputs are private. No service/native/CCL integration
claim; remaining full interpreter and native service work continues.

Final runner session6845 exited0: hosted suites and142 generated comparisons
PASS; upstream reference12 configurations PASS; CuBit12 unsupported/0unexpected
failures with339 unselected entrypoints. All processes terminal, no shared lock
held, no commits/pushes. Workflow documented in tests/aml-core/README.md.

Active: integer logical/comparison evaluator (LAnd/LOr/LNot/LEqual/LGreater/
LLess), owned AML core and hosted tests. Exact local result contracts plus
ACPICA comparisons; string/buffer coercion remains separate work. No shared
native/build/CCL edits. Hosted Nix proofs -j2.

Logical evaluator hosted2013 PASS plus prior suites; SPARK912 results zero
unproved/justified, including exact AML_Logic.Apply result contract. Expanded
ACPICA generated tests now compiling valid ASL (avoid reserved method names;
store Boolean result rather than unused operator statement). Reference runner
session17283 active; no source edits during this run. No shared/native changes.

Logical milestone terminal: hosted2013 plus all prior suites PASS; SPARK912
results zero unproved/justified. ACPICA generated254 PASS; upstream reference12
configurations PASS, CuBit12 explicit unsupported/0unexpected failures with339
unselected entrypoints. Session17283 exited0; no active jobs or shared lock.
Full goal active: general objects/packages, namespace accesses/calls, hardware
adapters and native/CCL service integration remain. No commits or pushes.

Active: generic read-only namespace binding for AML method operands, using
existing NameString/Resolve and integer object lookup. Method namespace scope
verified against ACPICA including parent prefixes; no method calls/writes yet.
Owned library/tests only; hosted Nix proof -j2, no native/shared edits.

Read-only binding: hosted120 PASS (including unrooted multi-segment rejection),
all prior suites PASS. Explicit invariant guard resolves generic proof-boundary
issue; nested formal-precondition variant hit GNATprove internal compiler bug
and was replaced without assumptions/proof exclusions. Current proof1236 results
zero unproved/justified. ACPICA generated352 PASS. Upstream reference run9789
still active (control modes); binding-only build84928 terminal0.

Binding milestone terminal: session9789 exited0. Generated ACPICA352 PASS;
upstream reference12 configurations PASS; CuBit12unsupported/0unexpectedfailures,
339unselected entrypoints. Hosted binding120 +prior suites PASS. SPARK1236
results0unproved/justified; report confirms all three concrete executor
instances and both Read_Binding callbacks analyzed. No active jobs, shared lock,
native artifacts, commits or pushes. Full goal remains active.

Active: bounded method calls in namespace-bound integer executor. Share fuel
across nested calls; arguments left-to-right, separate locals, callee scope.
Owned core/hosted tests only. Nix SPARK -j2; no native/shared or CCL changes.

Nested call checks180 and all prior suites PASS. Explicit Global and staged
lexicographic recursion variants added after initial proof exposed missing
mutual-recursion/frame contracts. Current combined verification session39028
active: proof then ACPICA comparisons/upstream. No source edits during run.

Method calls: hosted180 +prior suites PASS. SPARK1456 results zero unproved/
justified; dispatch/operand/recursive executor contracts proved in all concrete
instances. ACPICA generated430 PASS, including recursive countdowns and scoped
callee lookup. Session39028 still runs upstream control configurations. Native
service stack sizing and general object/reference semantics remain outstanding.

Call milestone terminal: combined session39028 exited0. Hosted call180 plus
prior suites PASS; SPARK1456 zero unproved/justified; generated ACPICA430 PASS.
Upstream reference12 configurations PASS, CuBit12 unsupported/0unexpected
failures,339 unselected. No active jobs/shared lock/native changes/commits/pushes.
Full service/interpreter goal remains active; next major gaps include general
objects/packages, mutable namespace/reference semantics, synchronization and
hardware/native/CCL integration.

Active: shared bounded AML object arena (integer/string/buffer/package handles)
and migration of named scalar/blob storage onto it. No collector/reuse yet;
handles remain valid for the arena lifetime. Owned library/tests only; Nix
proof -j2, no shared/native/CCL changes.

Object storage milestone terminal: combined session56578 exited0. Hosted
objects1107/service86 and prior suites PASS; SPARK1633 zero unproved/justified,
including arena contracts and concrete namespace instances. Generated ACPICA430
PASS. Upstream reference12 configurations PASS; CuBit12 unsupported/0unexpected
failures,339unselected. Observe now derives value-object/byte/element usage
without AML execution. No active jobs/shared lock/native changes/commits/pushes.
Full goal remains active: typed execution/package loading, reference mutation
and reclamation, synchronization, hardware adapters and native/CCL integration
remain outstanding.

Active: strengthen arena allocation frame contracts to preserve all previously
allocated objects and backing bytes/package links. Hosted/proof owned outputs
only, Nix -j2; no shared/native edits. Prior turn completed storage validation.

Allocation frame milestone terminal: session32957 exited0. Extends ghost
predicate proves all prior object records/bytes/package links unchanged by
successful allocation; prefix loop invariants added. SPARK1648 zero unproved/
justified, all hosted suites PASS, generated ACPICA430 PASS. Upstream reference
12PASS; CuBit12unsupported/0unexpectedfailures,339unselected. No active jobs,
shared lock, native edits, commits or pushes. Goal remains active; package AML
loading and general typed execution are still missing.

Active: AML_Data constant nested Package/VarPackage loader with explicit64-frame
stack, transactional arena publication and namespace package integration. Own
lib/acpi +tests/aml-core only; isolated Nix proof -j2 and ACPICA workflow.
No shared/native changes. Initial build caught missing operator visibility in
new fixture, corrected before retry; no correctness claim yet.

Constant package milestone terminal: combined15853, expanded hosted35086 and
new ACPICA package4366 exited0. AML_Data iterative64-frame loader handles nested
constant Package/VarPackage +scalar/blob values, uninitialized tail and atomic
rollback; named packages integrated into Namespace/service admission. SPARK1841
zero unproved/justified; package checks3263 +prior suites PASS. Generated430
comparisons and12 whole-package ACPICA comparisons PASS. Upstream reference12
PASS; CuBit12unsupported/0unexpectedfailures,339unselected. New --object hosted
table probe compares loaded data, NOT package execution. General typed VM,
name references/computed sizes, mutation, synchronization and native/CCL still
outstanding. No active jobs/shared lock/native changes/commits/pushes.

Active: SizeOf/ObjectType inspection in method executor, using read-only binding
metadata; locals/arguments currently integer-only. Cross-checking pinned ACPICA
implicit Integer SizeOf behavior and non-evaluation of method names. Owned
AML core/tests only, isolated Nix -j2; no native/shared edits.

Inspection validation: hosted234 +prior suites PASS; SPARK1946 zero unproved/
justified, including Inspect in all3 concrete executor instances. Generated
ACPICA668 and package12 comparisons PASS. Combined27526 remains active in
upstream control configurations. Initial30944 failed one invalid fixture:
ASCII lowercase b encoded Local2; corrected fixture to an unsupported byte.
No source change after successful proof; no native/shared work.

Inspection milestone terminal: combined27526 exited0. Hosted234 +prior suites,
SPARK1946 zero unproved/justified, ACPICA method668/package12 comparisons PASS.
Upstream reference12PASS; CuBit12unsupported/0unexpectedfailures,339unselected.
No active jobs/shared lock/native changes/commits/pushes. Full goal remains
active: general typed VM/reference semantics, synchronization/hardware and
native service/CCL integration remain incomplete.

Active: ShiftLeft/ShiftRight/NAnd/NOr/Not operators in actual executor, exact
integer contracts and independent Python oracle. Cross-check raw external
arguments and oversized counts with ACPICA. Owned hosted AML only; Nix -j2,
no shared/native changes.

Shift/bitwise validation: hosted integer oracle6516/expression12769 plus prior
suites PASS; SPARK1955 zero unproved/justified. Generated ACPICA862 and package12
PASS. Combined22900 remains active in upstream configurations. Source/spec
integer contract checks exact word operations; full AML semantics still absent.

Shift/bitwise milestone terminal: combined22900 exited0. All hosted suites
PASS, SPARK1955 zero unproved/justified, ACPICA method862/package12 PASS.
Upstream reference12PASS; CuBit12unsupported/0unexpectedfailures,339unselected.
No active jobs/shared lock/native edits/commits/pushes. Full goal active;
general typed VM/reference semantics, remaining operators, synchronization,
hardware adapters and native/CCL integration remain outstanding.

Active: Divide/Mod with explicit zero-divisor failure and remainder-before-
quotient targets. Cross-checking raw remainder target versus normalized
expression result in legacy-width tables. Owned hosted AML/tests only, Nix
SPARK -j2 plus ACPICA; no shared/native edits.

Division validation: hosted1048 +integer oracle7740 +prior suites PASS;
SPARK1989 zero unproved/justified. ACPICA method/error962 and package12 PASS,
including raw remainder local in32-bit table and divide-by-zero failures.
Combined23431 remains active in upstream configurations. Interpreter sources
unchanged during validation; no shared/native work.

Division milestone terminal: combined23431 exited0. Hosted1048/oracle7740 and
prior suites PASS; SPARK1989 zero unproved/justified; ACPICA method/error962,
package12 PASS. Upstream reference12PASS; CuBit12unsupported/0unexpectedfailures,
339unselected. No active jobs/shared lock/native edits/commits/pushes. Full
goal remains active; typed VM/reference/coercion semantics, synchronization,
hardware adapters and native/CCL integration are still incomplete.

Active: implicit string/buffer integer conversion with actual executor operand
context, including predicates and integer-first comparisons. Preserve native
object identity semantics by rejecting unsupported direct object returns/call
arguments and string-first comparisons. Owned AML/tests, Nix -j2; no shared/
native edits. Matching pinned ACPICA whitespace/prefix/overflow extensions.

Coercion hosted/proof73066 terminal0: new646 +prior suites PASS, SPARK2159
zero unproved/justified. Targeted ACPICA26579 terminal0 PASS268 comparisons.
Full ACPICA workflow69029 active (method/package/coercion/upstream); no source
edits pending. Coercion contracts prove safety/termination/status/width bounds,
not a full parsing semantic refinement. No native/shared changes.

Coercion milestone terminal: full ACPICA69029 exited0. Hosted646 +prior suites,
SPARK2159 zero unproved/justified; ACPICA method/error962, package12, coercion268
PASS. Upstream reference12PASS; CuBit12unsupported/0unexpectedfailures,
339unselected. No active jobs/shared lock/native edits/commits/pushes. Full
goal remains active: general typed values/returns/arguments/reference semantics,
remaining operators, synchronization/hardware and native/CCL remain incomplete.

Active: exact buffer-to-integer semantic contract, specifying all8 result bytes
including zero-filled high bytes and width-limited input. Fixed8-step loop for
proof unrolling; owned AML core only, isolated Nix proofs/tests -j2. No shared/
native changes. Prior coercion milestone was concrete progress.

Exact buffer contract focused proof2040 terminal0. Expanded hosted conversion
suite6790 and all previous suites PASS under combined32871, currently running
full proof before ACPICA. Octet postcondition describes all64 output bits,
including zero high bytes; no assumptions or proof exclusions added.

Exact-buffer milestone terminal: combined32871 exited0. Hosted conversion6790
+prior suites PASS; SPARK2162 zero unproved/justified includes full result-octet
relation. ACPICA method/error962, package12, coercion268 PASS. Upstream reference
12PASS; CuBit12unsupported/0unexpectedfailures,339unselected. No active jobs,
shared lock, native edits, commits or pushes. Full goal remains active; string
semantic proof, typed VM/reference behavior and native/CCL remain outstanding.

Active: native core compilation gate in isolated tests/aml-core/build/native;
native command will acquire shared lock. No kernel/runtime/startup edits.
New ACPI_Bootstrap service-owned coordinator admits an advertised table batch
and prevents partial/failed import from becoming Complete. Completion is only
of the advertised snapshot, not device discovery/activation. Hosted proof/tests
isolated -j2; inspecting absent userspace table handoff for later integration.

Native4820 terminal0, shared lock released: core/bootstrap static library
compiles against userspace/runtime. Compiler largest frame Install963152bytes;
Load_Names/AML_Data.Load report dynamic frames. This is compile evidence, NOT
native execution or a verified total stack bound. Added repeatable locked native
gate/stack report. Hosted36712 failed new fixture's missing Table_Kind operator
visibility; corrected before retry. No production artifacts staged.

Bootstrap hosted67303 PASS1257 and prior suites, but proof rejected two new
service contracts (empty value usage, rejected table count). Added explicit
empty-arena/namespace usage and Reject count-preservation contracts; corrected
one Ada logical-operator syntax error before rerunning. Combined7635 active
hosted/proof/ACPICA, isolated outputs, -j2. Native61211 terminal0 PASS owned
static library + stack-report.json: largest963152bytes,9dynamic records;
shared lock released. No kernel/runtime/staging changes or native execution.

Combined87295: hosted1257 bootstrap +prior suites PASS; SPARK2196 results,
zero unproved/justified after exposing Observe as an expression function and
preserving rejection result/count plus loop count. ACPICA portion now running.
Native42552 terminal0 PASS final sources, unchanged963152-byte largest frame
and9dynamic records. No shared lock held or production artifacts staged.

Bootstrap/native-compilation milestone complete: combined87295 terminal0;
hosted bootstrap1257 +prior suites PASS, SPARK2196 zero unproved/justified.
ACPICA comparisons962+12+268 PASS. Upstream reference12PASS; CuBit0PASS,
12unsupported,0unexpectedfailures,339unselected. Native42552 terminal0 final
sources, largest compiler frame963152bytes/9dynamic records; total native
stack bound unverified. All own jobs terminal, no shared lock held, no staging,
kernel/runtime/startup changes, commits or pushes. Full service goal active;
typed VM, hardware brokers, table transport and CCL binding remain unfinished.

Active: typed named-object execution through locals, method arguments and
returns, preserving immutable namespace object identities/metadata. Own AML
core and hosted/ACPICA fixtures only; no native/shared edits or lock required.
Prior bootstrap milestone was verified progress. Mutating refs/inline object
allocation and full typed VM remain subsequent work. Nix proof limit -j2.

Typed hosted now7300 +all previous suites PASS. Proof11601/19083 deliberately
stopped after concrete findings: added unconstrained output/status contracts,
and prevented namespace-root lookup from indexing data-object slot0. Full
combined50033 running proof/ACPICA -j2; targeted typed comparison running to
validate multiline buffer parser (previous68239 failed parser, not semantics).
No native source edits; compositor45733 owns shared native lock.

Full proof50033 completed: SPARK2383 results0unproved/justified. Full ACPICA
portion running now. Expanded typed fixture69722 terminal0 PASS7314 including
namespace-root rejection; independent typed56102 terminal0 PASS198. Full gate
adds14 comparisons for mixed object arguments at all7positions. Core remains
immutable; object-result IDs belong to the same namespace snapshot, not AML
references or mutation authority. No native build attempted while shared lock
owned by compositor; no staging changes.

Native gate35903 terminal1 at nonblocking flock before compilation (no compiler
output); shared native lock unavailable. Existing native archive predates typed
execution and is not evidence for this revision. Hosted/proof2383 verified;
fullACPICA50033 still running, method/error962 already PASS. No own lock held.

Typed-object milestone verified: combined50033 terminal0. SPARK2383 results
zero unproved/justified; hosted typed7314 supplemental69722 terminal0 and all
prior suites PASS (coercion6792). ACPICA962+12+268+212=1454 differential PASS.
Upstream reference12PASS; CuBit0PASS/12unsupported/0unexpectedfailures,
339unselected. Native35903 stopped at busy nonblocking lock, no compilation;
older archive is not current typed-core evidence. All own jobs terminal, no
lock held, no kernel/runtime/startup/staging changes, commits or pushes.
Full goal remains active: mutable copies/references, inline object allocation,
complete semantic refinement and native/CCL/hardware integration remain open.

Active: Store as TermArg plus typed method argument replacement and arithmetic
ArgX targets. Own executor and hosted/ACPICA fixtures only. Preserve read-only
namespace; explicit RefOf/Index and mutable backing copies remain unsupported.
Previous typed milestone was verified progress. Nix hosted/proofs -j2 only;
compositor56687 owns shared native lock. No shared/native edits planned.

Store hosted59413 PASS4137 and typed11930 +prior suites. Prior proof32842
terminated143 before final report (cause unknown; handle terminal), restarted
only after terminal observation. New proof59413 active. ACPICA fixture compiler
6114 rejected ignored Add result; changed to explicit separate sink. Pinned
ACPICA exstore.c confirms constant-target writes are no-ops; added Zero/One/Ones
support and differential cases. Old invalid-target fixture now uses unsupported
0x6F rather than valid One. No native/shared edits.

ACPICA Store8491 found semantic mismatch Add(Local0,Store(One,Local0)):
reference2 vs captured-old-value10. Stopped proof59413 intentionally after
finding; deferred Local/Arg sources now resolve at parent operation/call after
siblings execute. Snapshot expression values remain values. New proof5890
running; hosted Store4137/typed11938 +prior suites PASS. Targeted16152 compares
replacement that changes type, call arguments, constant targets and aliases.
This fixes real evaluator semantics; no AML RefOf/Index object exposed yet.

Proof5890 terminal0 PASS2527 analysis results, zero unproved/justified, including
exact Assign_Slot delta/preservation contracts. Native13003 terminal0 PASS
current core against CuBit runtime; largest compiler frame963152bytes and12
dynamic records, whole-call stack bound still unverified. Shared lock released.
Typed11938/Store4137 +prior hosted PASS. TargetedStore84293 still running after
ASL constant-target syntax required raw AML fixtures; all prior semantic cases
now compare correctly. No native execution/staging changes.

Store84293 terminal0 PASS520 ACPICA comparisons, including raw constant-target
AML, sibling writes changing operand types, and deferred call-argument reads.
Full ACPICA82560 now running all existing/new comparisons and pinned ASLTS.
Hosted/proof and native compile terminalPASS; no shared lock held, no staging.

Store/deferred-resolution milestone verified: full ACPICA82560 terminal0,
1974 differential PASS; reference12upstreamPASS, CuBit0PASS/12unsupported/
0unexpectedfailures,339unselected. Hosted/proof5890 terminal0 with Store4137,
typed11938 +prior suites and SPARK2527 zero unproved/justified. Native13003
terminal0 current library, largest963152-byte frame/12dynamic records, no whole
stack bound or native execution claim. All own jobs terminal, no lock held,
no kernel/runtime/startup/staging changes, commits or pushes. Full goal active;
mutable references/copies, full semantic refinement, hardware and CCL remain.

Active: service-owned bounded request handler for immutable table snapshot
upload and versioned read-only metric queries. No native table handoff/ACPI
role currently exists; handler keeps provider classification an explicit trusted
adapter input, not caller-supplied authority. Own service sources/tests/docs;
no kernel/runtime/catalog edits while compositor94939 owns native lock.
Previous Store/deferred-resolution turn was verified progress. Full goal active.

Request handler hosted28468 PASS91856 and SPARK2578 zero unproved/justified;
same command is still running full ACPICA comparisons/ASLTS. New core denies
unclassified callers with zero reply data; observer queries preserve state;
revision/replies bounded to CCL signed64. Native69959 exited1 at nonblocking
shared lock before compilation; compositor99405 owns latest native window.
No native/archive/staging changes claimed. Earlier43555 intentionally stopped
after proof during ACPICA to apply the signed64 correction;28468 is its full
replacement. Source/tests/doc edits confined to owned scope. No commits/pushes.

Request milestone complete: combined28468 terminal0, hosted91856 +prior suites
PASS, SPARK2578 zero unproved/justified, ACPICA1974 differentialPASS. Upstream
reference12PASS; CuBit0PASS/12unsupported/0unexpectedfailures,339unselected.
Docs record draft packets/metric pages and trusted-adapter boundary. Native69959
deferred on busy shared lock; prior13003 archive predates request handler.
All own jobs terminal, no lock held or native staging changes. Scoped diffcheck
PASS. Full goal remains active: native adapter/table provider/CCL/events/logs,
mutable references/copies, whole-stack bound and full AML semantics remain open.

Active: ACPI_Endpoint exact trusted-stamp classification and canonical response
encoding, plus mechanical ACPI_Native_Endpoint runtime Message adapter under
owned service/native directory. No authority numbers allocated; invalid config
denies all. Own hosted tests/proof additions; no kernel/runtime/catalog edits.
Previous request turn verified progress. Preparing native.gpr owned update/build
under shared lock; latest compositor note reports99405 released.

Endpoint hosted33500 PASS589963 plus all prior suites; full proof/ACPICA still
running. Native98353 terminal0 compiles ACPI_Requests, ACPI_Endpoint and actual
CuBit.Messages adapter. Shared lock released. Largest963152byte compiler frame,
12dynamic records; whole stack still unverified. Native.gpr update made under
lock. No runtime/startup/catalog/staging changes; native receive loop not wired.

Endpoint proof33500 phase complete:2585 results zero unproved/justified;
ACPICA phase still live (method962/package12 passed). Portable dispatch proves
canonical/signed-safe replies, nonprovider preservation and noauthority denial
without revision disclosure. Native wrapper outside SPARK scope, compiled98353.
Upcoming integration request to startup/CCL owner: need coordinated ACPI service
role, two separately minted observer/snapshot-provider tags, and typed metric
query binding. No numbers allocated or shared-file edits made. Native kernel
snapshot export and whole-stack sizing remain prerequisites to activation.

Endpoint milestone verified: combined33500 terminal0, endpoint589963/request91856
and prior hosted PASS, SPARK2585 zero unproved/justified, ACPICA1974 differential
PASS. Upstream reference12PASS; CuBit0PASS/12unsupported/0unexpectedfailures,
339unselected. Native98353 terminal0 actual runtime adapter compile. All own
jobs terminal, shared lock released, scoped diffcheckPASS. No kernel/runtime/
startup/staging changes, commits or pushes. Full goal active; native activation,
table export, CCL/events/logging, mutable AML references and full semantics open.

Active: upstream arithmetic-n64 contains CST0 method1170bytes, exceeding current
method storage incorrectly coupled to1024byte buffer-literal limit. Own namespace/
executor method code storage refactor: bounded shared method byte pool and exact
length method definitions. Hosted/proof/ACPICA tests only; no shared native edits
while compositor79901 owns lock. Previous endpoint turn verified progress.

Initial method-pool22247 terminal0 PASS2621 proof results zero unproved/justified
and all prior hosted checks; targeted19436 terminal0 ACPICA large-method28PASS.
Added exact Append_Code copy/prefix-preservation contract and method metrics page5.
Auto-review initially rejected runner edits on stale live-job assumption; verified
22247 handle gone and no AML runners. Shared lock now compositor5826, so runner
edits deferred, not bypassed.3155 runs new method-storage main explicitly, then
unchanged runner --prove --acpica. Need add new fixture/ACPICA harness to standard
runner under idle lock after terminal; no shared edits or native build started.

Proof3155 found actual overflow: empty method ending at Positive'Last formed
Data'First+Offset for an empty slice. Intentionally stopped3155 terminal130;
fixed explicit empty-body path and added high-bound empty-method regression.
No AML runner active confirmed. Shared lock available: integrated storage main
and acpica_methods.py into existing runners while holding lock. Auto-review
concern resolved through terminal-job evidence and guarded edits. Full42131
and native65782 now live. Added nonzero method metrics upload test. Upstream
scan482/433 top-level declarations shows next128-node budget gap; not expanded
this turn. No kernel/runtime/startup/staging changes.

Native65782 terminal0 current method-pool core/runtime adapter compile. Largest
frame703088bytes(previous963152),12dynamic records; whole stack still unverified.
Shared lock released.42131 hosted method-storage141/request92021 +prior PASS,
proof/ACPICA still running. No source/runner edits untilterminal. Boundsfix and
metrics fixture included; exactcopy proof results still pending.

42131 terminal1 due GNATprove damaged acpi_requests__handle cache; method
Append_Code25checks per instantiation and loader assertions proved. Initial
cache-preservation rename to/tmp failed cross-device;57213 consequently reran
unchanged cache and terminal1 same error. No live sessions before repair.
Successfully quarantined only that generated cache under build/proof-cache-
quarantine-60o61c6x; full33747 now running unchanged sources/runners. This is
proof infrastructure recovery, not a new source failure. Native65782 remains
current terminalPASS. All runner integration completed under shared lock.

33747 proof phase PASS2686 zero unproved/justified, including exact Append_Code
25checks each instantiation and six-page Handle30checks; no cache errors after
quarantine. Full expanded ACPICA phase now live. No source/runner edits pending.
Native65782 terminalPASS703088largestframe/12dynamic; hosted141 storage,
92021requests,589966endpoints +prior PASS. Shared lock released.

Method pool milestone verified: full33747 terminal0, SPARK2686 zero unproved/
justified, hosted141storage/92021requests/589966endpoints +prior PASS, ACPICA2002
differentialPASS incl28 newmethod cases. Reference12upstreamPASS; CuBit0PASS/
12unsupported/0unexpectedfailures,339unselected. Native65782 terminal0 current
core703088largestframe/12dynamic; whole-stack bound unverified. All own jobs
terminal, no lock held, scoped diffcheckPASS. No kernel/runtime/startup/staging
changes, commits or pushes. Full goal remains active; namespace/table budgets,
mutable references/copies, full AML semantics and native/CCL/hardware remain.

Active: diagnose actual upstream namespace admission cause before changing
budgets. Own service last-load diagnostic metric and hosted runner error detail,
then address demonstrated limiting construct/capacity. No shared scripts/native
builds while graphics3512 holds lock. Previous method-pool turn verified progress.

Actual upstream arithmetic failed BAD_PACKAGE at declaration74 ERRP, bytes
VarPackage(count=ETR0), before namespace limit. Added bounded read-only named
integer count callback with transactional parser and positive/in-slice consume
validation. It advances to STORAGE_FULL; service budget512 then advances to
VALUE_LIMIT. No claim full ASLTS admission. Added Last_Load_Code diagnostics and
metric page6 (last code, namespace capacity, remaining nodes). Hosted boundary
and ACPICA lexical-scope fixtures pending. Shared runner edit lock busy17133;
new fixture invoked explicitly until guarded integration possible. Initial ASL
fixture missing _ADR corrected; Ada fixture redundant use clause corrected.

Named-count50809 terminal0 ACPICA38PASS (both widths; root/parent/ancestor/
qualified names, qualified declaration lexical scope, nested packages, padding).
Hosted39293 package-count8758/service93/request92157/endpoint589969 +prior PASS;
proof then original fullACPICA workflow still running. Newharness not yet in
runner because editlock busy; no runner edits until39293terminal. Service512
budget boundary and Last_Load_Code preserved across header rejection tested.

39293 terminal1: all loader proofs pass; one unproved signed-reply bound in
Handle with page6. Strengthened representation: logical Response_Words elements
now use bounded Revision_Number subtype; upload Packet words remain unrestricted.
Encode converts to wire Words explicitly; original signed-safe postcondition
retained. Full99350 now live with explicit newfixture before unchanged runner.
Upstream bisection now reaches declaration392 P000 before VALUE_LIMIT, vs former
74ERRP BAD_PACKAGE. Need guarded runner integration and current native compile
when lock available; prior6626 exited1 busylock, no native source compile.

69611 proof phase PASS3005 zero unproved/justified. Bounded Response_Words fixed
signed-safe proof structurally without weakening contract; encode uses explicit
four-field aggregate (Ada array component subtypes cannot directly convert).
Full existing ACPICA still live; new38 comparisons already passed50809. No source
edits pending. Retrying guarded native compile after latest graphics note reports
no own live jobs; runner integration must wait for69611terminal.

Graphics yielded window: native89187 terminal0 current code, largest1174000byte
frame/13dynamic (AML_Data.Load wrapper); whole-stack remains unverified. Guarded
runner/GPR edits completed with no liveAMLrunner after intentional69611 stop130
during Store comparisons (proof3005 had completed). Final35968 --acpica live and
integrated hosted8758/service93/request92157/endpoint589969 +prior PASS.
All8 arithmetic/logic configurations independently confirmed load:VALUE_LIMIT.
Optional upstream report-reason tightening deferred when lock reoccupied; no
runner edits during35968. Window request fulfilled, no lock held.

Final integrated35968 terminal0: all hosted checks and2040 ACPICA differential
comparisons PASS, including38 named-count comparisons. Actual pinned upstream
reference12PASS; CuBit0PASS/12unsupported/0unexpected failures,339unselected.
SPARK3005 zero unproved on unchanged core; native89187 terminal0. Scoped diff
check clean. All own commands terminal, no shared lock held. Full interpreter,
native stack bounds and live CCL/service integration remain unfinished.
