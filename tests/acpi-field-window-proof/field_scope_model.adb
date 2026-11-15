pragma Ada_2022;
package body Field_Scope_Model with SPARK_Mode is
   function Admission return Boolean is (Configuration.Admission);
   function Due (Item : Lease) return Boolean is (Item.Prepared);
   procedure Inspect_Time (Deadline : out Word; Available : out Boolean) is
   begin Deadline := Configuration.IO_Deadline; Available := Configuration.IO_Available; end Inspect_Time;
   procedure Window (Deadline : out Word; Budget : out P.Attempt_Budget; Available : out Boolean) is
   begin Deadline := Configuration.Lock_Deadline; Budget := P.Attempt_Budget'First; Available := Configuration.Lock_Available; end Window;
   procedure Acquire (Item : in out Lease; Deadline : Word; Budget : P.Attempt_Budget; Result : out P.Lock_Outcome) is
      pragma Unreferenced (Budget);
   begin Item.Prepared := Configuration.Acquire_Due; Result := Configuration.Acquire_Reply; end Acquire;
   procedure Validate (Item : Lease; Result : out P.Lock_Outcome) is
      pragma Unreferenced (Item);
   begin Result := Configuration.Access_Reply; end Validate;
   procedure Release (Item : in out Lease; Result : out P.Lock_Outcome) is
   begin Item.Prepared := Configuration.Release_Due; Result := Configuration.Release_Reply; end Release;
end Field_Scope_Model;
