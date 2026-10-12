with CCL.VM.Verifier_State;

--  The verifier's checks outside the instruction sweep: the program's
--  tables (constant pool, imports, locals, match tables, functions) before
--  it, and after it the whole-program stack bound and the ownership pass
--  (CCL.Ownership.Bytecode) over the main body.
private package CCL.VM.Verifier_Tables with SPARK_Mode => On is
   use CCL.VM.Verifier_State;

   --  Every table entry is well formed; Error is the first that is not.
   procedure Check_Tables (Candidate : Program; Error : out Validation_Error)
     with Global => null;

   --  The deepest chain of named calls fits the machine's stack. Calls
   --  through function values are bounded when they run (the frame table
   --  and the stack: Call_Depth_Exhausted), since which function a value
   --  holds is not a static fact.
   function Stack_Fits
     (Own_Max : Region_Demands; Base_Max : Call_Demands; Calls : Call_Presence) return Boolean
     with Global => null;

   --  The ownership pass over the main body; Switches holds each
   --  Switch_Variant's control flow as the sweep recorded it.
   procedure Check_Ownership
     (Candidate : Program; Switches : Switch_Table; Error : out Validation_Error)
     with Global => null;
end CCL.VM.Verifier_Tables;
