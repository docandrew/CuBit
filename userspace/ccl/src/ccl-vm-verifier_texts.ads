with CCL.VM.Verifier_State;

--  The verifier's transfer functions for text, lists and streams: the text
--  opcodes and string built-ins, the list opcodes and list built-ins, and
--  stream values and views. Each does nothing once Error is set.
private package CCL.VM.Verifier_Texts with SPARK_Mode => On is
   use CCL.VM.Verifier_State;

   procedure Step_Text
     (Candidate : Program; Ins : Instruction; State : in out Abstract_State; Error : in out Validation_Error)
     with Global => null,
          Pre => Ins.Op in Push_Text | Concat_Text | Equal_Text | Length_Text | Text_At | Integer_To_Text |
                   Variant_To_Text | Text_Builtin,
          Post => (if Error'Old /= Valid then Error = Error'Old);

   procedure Step_List
     (Candidate : Program; Ins : Instruction; State : in out Abstract_State; Error : in out Validation_Error)
     with Global => null,
          Pre => Ins.Op in New_List | Fill_List | Length_List | List_At | List_Builtin,
          Post => (if Error'Old /= Valid then Error = Error'Old);

   procedure Step_Stream
     (Candidate : Program; Ins : Instruction; State : in out Abstract_State; Error : in out Validation_Error)
     with Global => null,
          Pre => Ins.Op in Push_Stream | Stream_View,
          Post => (if Error'Old /= Valid then Error = Error'Old);
end CCL.VM.Verifier_Texts;
