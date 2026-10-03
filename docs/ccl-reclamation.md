# CCL: reclaiming storage within an evaluation (proposal)

Status: **proposal**, not implemented. Written 2026-10-02 after the console's fractal demo hit the limit.

## The problem

An evaluation allocates from four bounded regions:
- the value arena (512 records, 2048 component slots);
- list storage (4096 elements);
- text storage;
- the VM's equivalents (`CCL.VM` arena, `List_Regions`, `Text_Regions`).

Nothing is freed until the evaluation ends. A computation that makes a record or a list on every step therefore exhausts a region even when no value escapes the step:

```lisp
(fold (fn ((z Z) (i Integer)) (step cx cy z)) (Z 0 0 0) (range 1 24))   ; a Z per step
(each (fn ((i Integer)) (escape ...)) (range 0 223))                   ; a (range 1 24) per pixel
```

The demo in `docs/ccl-console.md` works around this. It packs its state into one Integer and keeps the iteration count low. CCL code should not have to know about it: "everything CCL" includes compute.

## Proposal: mark at a call, release at its return

Every function call is a natural region boundary: the callee's temporaries die when it returns, except what its result refers to.

1. **On entry**, a call records the high-water mark of each region: arena nodes and slots, list elements, text bytes.
2. **On return**, the result is either self-contained or refers into the regions.
   - A **scalar result** (Integer, Boolean, Character, enumeration member, function value) holds nothing above the marks. Every region drops back to its mark.
   - A **compound result** (record, list, string) built above the marks is copied down. It is evacuated depth first into the space starting at the marks, in the order the original allocation used, so references still point backwards (to earlier nodes). The regions then end just after the copy.
   - Anything the result refers to below the marks (a caller's value, a captured value) stays where it is; only parts above the marks move.
3. **Fold, each, where, sort-by and any/all** call their function once per element, so each element's temporaries are released before the next. The accumulator survives because it is the call's result.

Bounded cost: the evacuation copies only what the result reaches above the marks, and at most once per return.

### Why this is safe

- Values are immutable and acyclic: references always point to older nodes. That is the existing invariant behind `Print_Value`'s and `Append_Runtime`'s termination measures.
- So after evacuation, nothing below the marks can refer to anything that was released above them.
- A caller's bindings were all made before the call, so they are below its marks.
- Captured values are copied in when the closure is built (the existing `Make_Closure` semantics), so a closure never refers into a released region.
- Host objects (`Object_Owner` views) live in their own bounded table and are not affected.

### What must be proved (interpreter and VM, per the parity rule)

1. **Evacuation preserves the value:** the copy prints the same literal (`Print_Value`) and compares equal, element for element.
2. **References still point backwards after evacuation:** the acyclic and termination invariants hold.
3. **Release never frees anything a live binding can reach:** bindings below the marks are untouched.
4. **Region accounting:** the marks are restored exactly, with no leak and no double use.

The VM gets the same rule in `Enter_Call`/return and in `Run_Apply`'s per-element calls. The differential tests (`Same` in `userspace/ccl/tests/native`) gain allocation-heavy cases. Each must succeed in both engines, with the same results and the same fuel.

## Order of work

1. Interpreter: marks and release for scalar results only. This is the common case (the fractal's `escape` returns an Integer) and needs no evacuation.
2. VM: the same, with the differential tests.
3. Evacuation of compound results, in both engines.
4. Proofs at level 1, or level 2 where needed; then rerun the console fractal with records and 24 iterations as the regression.
