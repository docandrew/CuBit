pragma Ada_2022;
package body Local_Scope_Proof with SPARK_Mode is
   use type Interfaces.Unsigned_64;
   function Admission return Boolean is (True);
   function Due (Item : Lease) return Boolean is (Item.Prepared);
   procedure Window (Deadline : out Interfaces.Unsigned_64; Budget : out P.Attempt_Budget;
      Available : out Boolean) is
   begin Deadline := 100; Budget := P.Attempt_Budget'First; Available := True; end Window;
   procedure Acquire (Item : in out Lease; Deadline : Interfaces.Unsigned_64;
      Budget : P.Attempt_Budget; Result : out P.Lock_Outcome) is
      pragma Unreferenced (Budget);
   begin
      if Item.Prepared or else Deadline = 0 then Result := P.Not_Ready;
      else Item.Prepared := True; Result := P.Acquired; end if;
   end Acquire;
   procedure Validate (Item : Lease; Result : out P.Lock_Outcome) is
   begin Result := (if Item.Prepared then P.Configured else P.Not_Ready); end Validate;
   procedure Release (Item : in out Lease; Result : out P.Lock_Outcome) is
   begin
      Result := (if Item.Prepared then P.Released else P.Not_Ready);
      Item.Prepared := False;
   end Release;
end Local_Scope_Proof;
