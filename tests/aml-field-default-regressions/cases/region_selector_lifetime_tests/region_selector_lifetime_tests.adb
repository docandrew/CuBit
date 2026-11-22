pragma Ada_2022;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Delays;
with AML_Execute;
with AML_Namespace;
with AML_Table_Backing;
procedure Region_Selector_Lifetime_Tests is
   package N is new AML_Namespace (64, AML_Delays.Unavailable_Provider);
   package V is new N.Owned.Collecting;
   use type AML_Decode.Bytes;
   use type AML_Decode.Integer_Value;
   use type N.Load_Status;
   use type N.Lookup_Status;
   use type V.Access_Status;
   use type V.Collection_Count;
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
   function String_Code (Text : String) return Bytes is
     (Bytes'[16#0D#] & Enc (Text) & [0]);
   function Number (Value : Integer_Value; Width : Integer_Width) return Bytes is
      Data : Bytes (1 .. (if Width = Bits_32 then 5 else 9));
      Remaining : Integer_Value := Value;
   begin
      Data (1) := (if Width = Bits_32 then 16#0C# else 16#0E#);
      for I in 2 .. Data'Last loop
         Data (I) := Byte (Remaining mod 256); Remaining := Remaining / 256;
      end loop;
      return Data;
   end Number;
   function Region (Signature, OEM : Bytes) return Bytes is
     (Bytes'[16#5B#,16#88#] & Enc ("REGN") & Signature & OEM & String_Code (""));
   function Read_Region return Bytes is
     (Bytes'[16#5B#] & Block (16#81#, Enc ("REGN") & [1,0,16#40#,4] & Enc ("FLD0") & [8])
      & [16#A4#] & Enc ("FLD0"));
   function Program (Width : Integer_Width) return Bytes is
      Initial : constant Integer_Value := (if Width = Bits_32 then 16#1111_0000# else 16#1111_0000_0000_0000#);
      Updated : constant Integer_Value := (if Width = Bits_32 then 16#2222_0000# else 16#2222_0000_0000_0000#);
      Nested : constant Bytes := Bytes'[16#70#] & Enc ("INNR") & Enc ("NEST");
   begin
      return Bytes'[16#08#] & Enc ("SRC0") & Number (Initial, Width) &
        [16#08#] & Enc ("NEST") & [0] &
        Method ("RST0", Bytes'[16#9D#] & Number (Initial, Width) & Enc ("SRC0") & [16#70#,0] & Enc ("NEST") & [16#A4#,0]) &
        Method ("READ", Bytes'[16#A4#] & Enc ("SRC0")) &
        Method ("NRED", Bytes'[16#A4#] & Enc ("NEST")) &
        Method ("INNR", Region (String_Code ("2222"), String_Code ("")) & Read_Region) &
        Method ("COPY", Bytes'[16#9D#] & Number (Updated, Width) & Enc ("SRC0") & Nested & Pressure & [16#A4#] & String_Code ("")) &
        Method ("STOR", Bytes'[16#70#] & Number (Updated, Width) & Enc ("SRC0") & Nested & Pressure & [16#A4#] & String_Code ("")) &
        Method ("BOOM", Bytes'[16#70#] & Number (Updated, Width) & Enc ("SRC0") & [16#A4#,16#78#,1,0,0,0]) &
        Method ("HOLD", Region (Bytes'[16#83#,16#71#] & Enc ("SRC0"), Enc ("COPY")) & Read_Region) &
        Method ("LIVE", Region (Bytes'[16#83#,16#71#] & Enc ("SRC0"), Enc ("STOR")) & Read_Region) &
        Method ("FAIL", Region (Bytes'[16#83#,16#71#] & Enc ("SRC0"), Enc ("BOOM")) & Read_Region);
   end Program;
   Target_1 : constant Bytes := [16#32#, 16#32#, 16#32#, 16#32#, 16#2A#, 16#00#, 16#00#, 16#00#, 16#01#, 16#9D#, 16#43#, 16#55#, 16#42#, 16#49#, 16#54#, 16#00#, 16#53#, 16#45#, 16#4C#, 16#43#, 16#54#, 16#52#, 16#4C#, 16#20#, 16#01#, 16#00#, 16#00#, 16#00#, 16#49#, 16#4E#, 16#54#, 16#4C#, 16#08#, 16#04#, 16#26#, 16#20#, 16#08#, 16#54#, 16#41#, 16#52#, 16#47#, 16#00#];
   Target_2 : constant Bytes := [16#32#, 16#32#, 16#32#, 16#32#, 16#2A#, 16#00#, 16#00#, 16#00#, 16#02#, 16#9C#, 16#43#, 16#55#, 16#42#, 16#49#, 16#54#, 16#00#, 16#53#, 16#45#, 16#4C#, 16#43#, 16#54#, 16#52#, 16#4C#, 16#20#, 16#01#, 16#00#, 16#00#, 16#00#, 16#49#, 16#4E#, 16#54#, 16#4C#, 16#08#, 16#04#, 16#26#, 16#20#, 16#08#, 16#54#, 16#41#, 16#52#, 16#47#, 16#00#];
begin
   for Width in Integer_Width loop
      declare
         A : V.Arena;
         Input : aliased AML_Table_Backing.State (1, 256);
         Target : constant Bytes := (if Width = Bits_32 then Target_1 else Target_2);
         Revision : constant Integer_Value := (if Width = Bits_32 then 1 else 2);
         Updated : constant Integer_Value := (if Width = Bits_32 then 16#2222_0000# else 16#2222_0000_0000_0000#);
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
         Input.Count := 1; Input.Tables (1) := (Offset => 0, Extent => Target'Length);
         Input.Data.Bytes := [others => 0]; Input.Data.Bytes (1 .. Target'Length) := Target;
         Check (AML_Table_Backing.Valid_Span (Input, 1), "full owned target span");
         V.Load (A, Program (Width), Width, Loaded, Access_Result);
         Check (Access_Result = V.Available and then Loaded = N.Loaded, "load");
         V.Seal (A, Init, Access_Result); Check (Access_Result = V.Available, "seal");
         for Pass in 1 .. Repetitions loop
            Invoke ("RST0", 0);
            Before := V.Reclamation_Metrics (A);
            Invoke ("HOLD", 0, AML_Execute.Unknown_Name); Collected;
            Invoke ("READ", Updated); Invoke ("NRED", Revision);
            Absent ("HOLD", "REGN"); Absent ("HOLD", "FLD0");
            Absent ("INNR", "REGN"); Absent ("INNR", "FLD0");
            Invoke ("RST0", 0);
            Before := V.Reclamation_Metrics (A);
            Invoke ("LIVE", Revision); Collected;
            Invoke ("READ", Updated); Invoke ("NRED", Revision);
            Absent ("LIVE", "REGN"); Absent ("LIVE", "FLD0");
            Absent ("INNR", "REGN"); Absent ("INNR", "FLD0");
            Invoke ("RST0", 0);
            Invoke ("FAIL", 0, AML_Execute.Division_By_Zero);
            Invoke ("READ", Updated);
            Absent ("FAIL", "REGN"); Absent ("FAIL", "FLD0");
            Invoke ("LIVE", Revision);
            Absent ("LIVE", "REGN"); Absent ("LIVE", "FLD0");
         end loop;
         V.Reset (A, Access_Result);
         Check (Access_Result = V.Available and then V.Retained_Count (A) = 0, "reset");
      end;
   end loop;
   Ada.Text_IO.Put_Line ("region selector lifetime checks" & Checks'Image);
end Region_Selector_Lifetime_Tests;
