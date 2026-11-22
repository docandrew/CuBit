pragma Ada_2022;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Delays;
with AML_Execute;
with AML_Namespace;
with AML_Table_Backing;
procedure Source_Buffer_GC_Tests is
   package N is new AML_Namespace (64, AML_Delays.Unavailable_Provider);
   package V is new N.Owned.Collecting;
   use type AML_Decode.Bytes;
   use type AML_Decode.Integer_Value;
   use type N.Load_Status;
   use type N.Lookup_Status;
   use type V.Access_Status;
   use type V.Collection_Count;
   use type V.Value_Kind;
   use type AML_Execute.Execution_Status;
   Checks : Natural := 0;
   Pressure_Bytes : constant := 16_384;
   Pressure_Allocations : constant := 5;
   Repetitions : constant := 2;
   procedure Check (Good : Boolean; Message : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Message; end if;
   end Check;
   function Enc (Text : String) return Bytes is
      Data : Bytes (1 .. Text'Length);
   begin
      for I in Data'Range loop Data (I) := Character'Pos (Text (Text'First + (I - 1))); end loop;
      return Data;
   end Enc;
   function Block (Opcode : Byte; Payload : Bytes) return Bytes is
      Size : Natural := Payload'Length + 1;
   begin
      if Size <= 63 then return Bytes'[Opcode, Byte (Size)] & Payload; end if;
      Size := Payload'Length + 2;
      if Size > 4095 then raise Program_Error with "fixture package bound"; end if;
      return Bytes'[Opcode, Byte (16#40# + Size mod 16), Byte (Size / 16)] & Payload;
   end Block;
   function Method (Name : String; Code : Bytes) return Bytes is
     (Block (16#14#, Enc (Name) & [0] & Code));
   function Pressure return Bytes is
      Allocate : constant Bytes := Bytes'[16#70#] &
        Block (16#11#, Bytes'[16#0B#, Byte (Pressure_Bytes mod 256), Byte (Pressure_Bytes / 256)]) & [16#60#];
      Result : Bytes (1 .. Allocate'Length * Pressure_Allocations);
   begin
      for I in 0 .. Pressure_Allocations - 1 loop
         Result (I * Allocate'Length + 1 .. (I + 1) * Allocate'Length) := Allocate;
      end loop;
      return Result;
   end Pressure;
   function Field_Code (Index, Count : Bytes; Name : String) return Bytes is
     (Bytes'[16#5B#,16#13#] & Enc ("BUF0") & Index & Count & Enc (Name));
   Original : constant Bytes := [16#11#,16#22#,16#33#];
   Replacement : constant Bytes := [16#44#,16#55#,16#66#];
   function Buffer_Code (Data : Bytes) return Bytes is
     (Block (16#11#, Bytes'[16#0A#,3] & Data));
   Code : constant Bytes :=
     Bytes'[16#08#] & Enc ("BUF0") & Buffer_Code (Original) &
     Method ("RST0", Bytes'[16#9D#] & Buffer_Code (Original) & Enc ("BUF0") & [16#A4#,0]) &
     Method ("READ", Bytes'[16#A4#,16#99#] & Enc ("BUF0") & [0]) &
     Method ("COPY", Bytes'[16#9D#] & Buffer_Code (Replacement) & Enc ("BUF0") & Pressure & [16#A4#,0]) &
     Method ("STOR", Bytes'[16#70#] & Buffer_Code (Replacement) & Enc ("BUF0") & Pressure & [16#A4#,0]) &
     Method ("HOLD", Field_Code (Enc ("COPY"), Bytes'[16#0A#,24], "FLD0") & [16#A4#] & Enc ("FLD0")) &
     Method ("LIVE", Field_Code (Enc ("STOR"), Bytes'[16#0A#,24], "FLD0") & [16#A4#] & Enc ("FLD0"));

begin
   for Width in Integer_Width loop
      declare
         A : V.Arena;
         Input : aliased AML_Table_Backing.State (1, 1);
         Access_Result : V.Access_Status;
         Loaded : N.Load_Status;
         Init : N.Initialization_Report;
         Before, After : V.Collection_Statistics;
         procedure Invoke (Name : String; Expected : Integer_Value;
            Expected_Status : AML_Execute.Execution_Status := AML_Execute.Returned) is
            Found : N.Lookup_Result;
            Result : V.Result;
         begin
            V.Find_Object (A, N.Root, Name, Found, Access_Result);
            Check (Access_Result = V.Available and then Found.Status = N.Found, "find " & Name);
            V.Invoke (A, Input, Found.Node, [others => (V.Immediate_Argument, 0)],
              0, 20_000, Result, Access_Result);
            Check (Access_Result = V.Available and then Result.Status = Expected_Status, "status " & Name);
            if Expected_Status = AML_Execute.Returned then Check (Result.Number = Expected, "value " & Name); end if;
            Check (V.Retained_Count (A) = 0, "pins " & Name);
            -- Method-boundary owned roots, not an assertion about all transport slots.
            pragma Assert (V.Module_Transient_Free (A));
         end Invoke;
         procedure Invoke_Buffer (Name : String; Expected : Bytes) is
            Found : N.Lookup_Result;
            Result : V.Result;
            Description : V.Value_Description;
            Data : Bytes (11 .. 10 + Expected'Length);
            Copied : Natural;
         begin
            V.Find_Object (A, N.Root, Name, Found, Access_Result);
            Check (Access_Result = V.Available and then Found.Status = N.Found, "find buffer method");
            V.Invoke (A, Input, Found.Node, [others => (V.Immediate_Argument, 0)],
              0, 20_000, Result, Access_Result);
            Check (Access_Result = V.Available and then Result.Status = AML_Execute.Object_Returned, "buffer result");
            V.Describe (A, Result.Handle, Description, Access_Result);
            Check (Access_Result = V.Available and then Description.Kind = V.Buffer_Description
              and then Description.Length = Expected'Length, "full field shape");
            V.Read_Bytes (A, Result.Handle, 0, Data, Copied, Access_Result);
            Check (Access_Result = V.Available and then Copied = Expected'Length and then Data = Expected,
              "every field byte");
            V.Release (A, Result.Handle, Access_Result);
            Check (Access_Result = V.Available and then V.Retained_Count (A) = 0, "release field");
            pragma Assert (V.Module_Transient_Free (A));
         end Invoke_Buffer;
         procedure Absent (Parent, Name : String) is
            Found : N.Lookup_Result;
            Child : N.Node_ID;
         begin
            V.Find_Object (A, N.Root, Parent, Found, Access_Result);
            Check (Access_Result = V.Available and then Found.Status = N.Found, "parent");
            V.Find_Child (A, Found.Node, Name, Child, Access_Result);
            Check (Access_Result = V.Available and then Child = N.Root, "method field removed " & Parent);
         end Absent;
         procedure Collected is
         begin
            After := V.Reclamation_Metrics (A);
            Check (After.Completed > Before.Completed, "actual collection");
            Check (After.Freed_Objects > Before.Freed_Objects and then After.Freed_Bytes > Before.Freed_Bytes,
              "actual reclaimed objects/bytes");
            Check (After.Rejected = Before.Rejected, "no rejected collection");
         end Collected;
      begin
         V.Initialize (A, Access_Result); Check (Access_Result = V.Available, "initialize");
         V.Load (A, Code, Width, Loaded, Access_Result);
         Check (Access_Result = V.Available and then Loaded = N.Loaded, "load");
         V.Seal (A, Init, Access_Result); Check (Access_Result = V.Available, "seal");
         for Pass in 1 .. Repetitions loop
            Invoke ("RST0", 0);
            Before := V.Reclamation_Metrics (A);
            Invoke_Buffer ("HOLD", Original); Collected;
            Invoke ("READ", 16#665544#); Absent ("HOLD", "FLD0");
            Invoke ("RST0", 0);
            Before := V.Reclamation_Metrics (A);
            Invoke_Buffer ("LIVE", Replacement); Collected;
            Invoke ("READ", 16#665544#); Absent ("LIVE", "FLD0");
         end loop;
         V.Reset (A, Access_Result);
         Check (Access_Result = V.Available and then V.Retained_Count (A) = 0, "reset");
      end;
   end loop;
   Ada.Text_IO.Put_Line ("source buffer GC checks" & Checks'Image);
end Source_Buffer_GC_Tests;
