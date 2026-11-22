with AML_Delays;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_Objects;
with AML_References;
with AML_Table_Backing;
procedure Retained_Invoke_Tests is
   package N is new AML_Namespace (32, AML_Delays.Unavailable_Provider, Max_Retained_Roots => 1);
   use N; use N.Owned;
   use type AML_References.Reference;
   use type AML_Decode.Integer_Value;
   A : Arena;
   Input : aliased AML_Table_Backing.State (1, 36);
   Pin, Copied_Pin : Retained_Root;
   Admission : Retain_Status;
   Retention : Invocation_Retention_Status;
   Released_Status : Release_Status;
   Result : Execution_Result;
   Value : Datum;
   Read_Status : Execution_Status;
   Loaded_Status : Load_Status;
   OK : Boolean;
   Checks : Natural := 0;
   Counter_Name : constant Bytes := [67, 78, 84, 48];
   Prefix : constant Bytes := [16#08#,67,78,84,48,0];
   Increment_Counter : constant Bytes := [16#75#] & Counter_Name;
   Return_Buffer : constant Bytes := [16#A4#,16#11#,4,16#0A#,1,16#7F#];
   Zero_Args : constant Value_Arguments := [others => (Integer_Datum,0,Ordinary_Integer)];
   procedure Check (Good : Boolean; Label_Text : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Label_Text; end if;
   end Check;
   procedure Prepare (Code : Bytes) is
   begin
      Reset (A, OK); Check (OK, "reset");
      Load (A, Prefix & Bytes'[16#14#, Byte (6 + Code'Length),84,69,83,84,0] & Code,
        Bits_64, Loaded_Status);
      Check (Loaded_Status = Loaded and then Node_Count (A) = 2, "load fixture");
   end Prepare;
   procedure Call is
   begin
      Invoke_Retained (A, Input, 2, Zero_Args, 0, 1000, Result, Pin, Retention);
   end Call;
   procedure Drop is
   begin
      Release (A, Pin, Released_Status);
      Check (Released_Status = Released and then Pin = No_Retained_Root, "release result");
   end Drop;
begin
   Prepare (Increment_Counter & Return_Buffer);
   Retain (A, (Integer_Datum, 9, Ordinary_Integer), Pin, Admission);
   Check (Admission = Retained, "occupy only pin"); Copied_Pin := Pin;
   declare
      Before : constant N.State := Snapshot (A);
      Pins_Before : constant Retention_State := Retention_Model (A) with Ghost;
   begin
      Call;
      Check (Retention = Result_Root_Limit and then Result.Status = Value_Limit
        and then Result.Charged = 0 and then Pin = No_Retained_Root
        and then Snapshot (A) = Before and then Integer_Data (Snapshot (A), 1) = 0,
        "pin quota denied before side effect");
      pragma Assert (Retention_Model (A) = Pins_Before);
   end;
   Pin := Copied_Pin; Drop;
   Call;
   Check (Retention = Result_Retained and then Result.Status = Object_Returned
     and then Pin /= No_Retained_Root and then Retained_Count (A) = 1
     and then Integer_Data (Snapshot (A), 1) = 1, "retained compound result and effect");
   Read_Retained (A, Pin, Value, Read_Status);
   Check (Read_Status = Returned and then Value.Value_Kind = Object_Datum
     and then Has_Source (A, Value.Object.Source)
     and then AML_Objects.Byte_Data (Value_Store (Snapshot (A)), Value.Object.ID) = Bytes'[16#7F#],
     "read retained buffer");
   Copied_Pin := Pin; Drop;
   Release (A, Copied_Pin, Released_Status); Check (Released_Status = Invalid_Root, "result token replay rejected");
   Prepare (Increment_Counter & Bytes'[16#A4#,1]); Call;
   Check (Retention = No_Root_Required and then Result.Status = Returned
     and then Result.Value = 1 and then Pin = No_Retained_Root
     and then Retained_Count (A) = 0 and then Integer_Data (Snapshot (A), 1) = 1,
     "integer return releases reserved slot");
   Call; Check (Retention = No_Root_Required and then Integer_Data (Snapshot (A), 1) = 2,
     "released result capacity can be reused");
   Prepare (Increment_Counter & Bytes'[16#FE#]); Call;
   Check (Retention = No_Root_Required and then Result.Status = Unsupported
     and then Retained_Count (A) = 0 and then Integer_Data (Snapshot (A), 1) = 1,
     "execution failure keeps AML effects, releases pin");
   Prepare (Bytes'(1 .. 0 => 0)); Call;
   Check (Retention = No_Root_Required and then Result.Status = No_Return
     and then Retained_Count (A) = 0, "no-return releases pin");
   Prepare ([16#70#,16#0A#,7,16#60#,16#A4#,16#71#,16#60#]); Call;
   Check (Retention = No_Root_Required and then Result.Status = No_Return
     and then Retained_Count (A) = 0, "current frame reference suppressed before export");
   Prepare (Bytes'[16#A4#,16#71#] & Counter_Name); Call;
   Check (Retention = Result_Retained and then Result.Status = Reference_Returned,
     "named reference result");
   Read_Retained (A, Pin, Value, Read_Status);
   Check (Read_Status = Returned and then Value = Datum'(Reference_Datum, Result.Ref),
     "reference result read as descriptor"); Drop;
   -- Method-owned names disappear at cleanup; a returned object's independent
   -- lifetime is owned by the result pin, not the vanished namespace node.
   Prepare (Bytes'[16#08#,84,69,77,80,16#11#,4,16#0A#,1,16#55#,16#A4#,84,69,77,80]); Call;
   Check (Retention = Result_Retained and then Result.Status = Object_Returned,
     "method-owned object exported");
   Read_Retained (A, Pin, Value, Read_Status);
   Check (Read_Status = Returned and then Has_Source (A, Value.Object.Source)
     and then AML_Objects.Byte_Data (Value_Store (Snapshot (A)), Value.Object.ID) = Bytes'[16#55#],
     "export survives method cleanup"); Drop;
   Prepare (Bytes'[16#08#,84,69,77,80,1,16#A4#,16#71#,84,69,77,80]); Call;
   Check (Retention = Result_Retained and then Result.Status = Reference_Returned,
     "method-owned reference descriptor exported");
   Read_Retained (A, Pin, Value, Read_Status);
   Check (Read_Status = Returned and then Value.Ref = Result.Ref, "expired descriptor preserved");
   declare Ref : constant AML_References.Reference := Value.Ref; begin
      Resolve_Value (A, Ref, Value, Read_Status);
   end;
   Check (Read_Status /= Returned, "pin does not resurrect expired namespace location"); Drop;
   -- A foreign descriptor can be carried as argument data, but cannot be
   -- exported as this arena's retained authority. Committed effects remain.
   declare
      Foreign : Arena;
      Ref : AML_References.Reference;
      Args : Value_Arguments := Zero_Args;
   begin
      Reset (Foreign, OK); Check (OK, "foreign reset");
      Load (Foreign, Prefix, Bits_64, Loaded_Status); Check (Loaded_Status = Loaded, "foreign name");
      Make_Named_Reference (Foreign, 1, Ref, OK); Check (OK, "foreign descriptor");
      Reset (A, OK); Check (OK, "argument reset");
      Load (A, Prefix & Bytes'[16#14#,13,84,69,83,84,1] & Increment_Counter & Bytes'[16#A4#,16#68#],
        Bits_64, Loaded_Status); Check (Loaded_Status = Loaded, "argument method");
      Args (0) := (Reference_Datum, Ref);
      Invoke_Retained (A, Input, 2, Args, 1, 1000, Result, Pin, Retention);
      Check (Retention = Invalid_Result and then Result.Status = Unsupported_Value
        and then Result.Charged > 0 and then Pin = No_Retained_Root
        and then Retained_Count (A) = 0 and then Integer_Data (Snapshot (A), 1) = 1,
        "foreign result rejected, effects preserved, slot freed");
      Make_Named_Reference (A, 1, Ref, OK); Check (OK, "local descriptor");
      Args (0) := (Reference_Datum, Ref);
      Invoke_Retained (A, Input, 2, Args, 1, 1000, Result, Pin, Retention);
      Check (Retention = Result_Retained and then Result.Status = Reference_Returned
        and then Result.Ref = Ref and then Integer_Data (Snapshot (A), 1) = 2,
        "result slot reusable after rejected export"); Drop;
   end;
   declare
      package Tiny is new AML_Namespace (8, AML_Delays.Unavailable_Provider, Max_Retained_Roots => 1, Max_Retained_Incarnation => 1);
      T : Tiny.Owned.Arena;
      TRoot : Tiny.Owned.Retained_Root;
      TStatus : Tiny.Owned.Invocation_Retention_Status;
      Loaded : Tiny.Load_Status;
      use type Tiny.Load_Status;
      use type Tiny.State;
      use type Tiny.Owned.Invocation_Retention_Status;
      use type Tiny.Owned.Retained_Root;
   begin
      Tiny.Owned.Reset (T, OK); Check (OK, "tiny reset");
      Tiny.Owned.Load (T, [16#14#,8,84,69,83,84,0,16#A4#,1], Bits_64, Loaded);
      Check (Loaded = Tiny.Loaded, "tiny fixture");
      Tiny.Owned.Invoke_Retained (T, Input, 1, Zero_Args, 0, 100, Result, TRoot, TStatus);
      Check (TStatus = Tiny.Owned.No_Root_Required and then Result.Status = Returned, "consume last pin incarnation");
      declare Before : constant Tiny.State := Tiny.Owned.Snapshot (T); begin
         Tiny.Owned.Invoke_Retained (T, Input, 1, Zero_Args, 0, 100, Result, TRoot, TStatus);
         Check (TStatus = Tiny.Owned.Result_Identity_Exhausted and then TRoot = Tiny.Owned.No_Retained_Root
           and then Result.Status = Value_Limit and then Result.Charged = 0
           and then Tiny.Owned.Snapshot (T) = Before, "pin issuer exhaustion denied before execution");
      end;
   end;
   Ada.Text_IO.Put_Line ("RETAINED INVOCATION" & Checks'Image);
end Retained_Invoke_Tests;
