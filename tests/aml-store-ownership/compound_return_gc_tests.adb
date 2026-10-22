pragma Ada_2022;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Delays;
with AML_Execute;
with AML_Namespace;
with AML_Table_Backing;
procedure Compound_Return_GC_Tests is
   package N is new AML_Namespace (64, AML_Delays.Unavailable_Provider);
   package V is new N.Owned.Collecting;
   use type AML_Decode.Bytes;
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
      for I in Data'Range loop Data (I) := Character'Pos (Text (Text'First + I - 1)); end loop;
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
   function Method (Name : String; Args : Byte; Code : Bytes) return Bytes is
     (Block (16#14#, Enc (Name) & [Args] & Code));
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
   Expected : constant Bytes := Enc ("nested method-local return survives");
   type Shape is (Buffer_Result, String_Result);
   function Fixture (Kind : Shape) return Bytes is
      Value : constant Bytes :=
        (case Kind is
           when Buffer_Result => Block (16#11#, Bytes'[16#0A#, Expected'Length] & Expected),
           when String_Result => Bytes'[16#0D#] & Expected & [0]);
      Empty_Value : constant Bytes :=
        (case Kind is
           when Buffer_Result => Block (16#11#, Bytes'[0]),
           when String_Result => Bytes'[16#0D#,0]);
      Local_Body : constant Bytes := Bytes'[16#08#] & Enc ("TEMP") & Value & [16#A4#] & Enc ("TEMP");
      -- Device forces the early Return path to unwind a lexical container.
      Container : constant Bytes := Bytes'[16#5B#] & Block (16#82#, Enc ("BOX0") & Local_Body);
   begin
      return Method ("MAKE", 0, Container) &
        Method ("PASS", 2, Bytes'[16#A4#,16#68#]) &
        Method ("PRES", 0, Pressure & [16#A4#,0]) &
        Method ("CALL", 0, Bytes'[16#A4#] & Enc ("PASS") & Enc ("MAKE") & Enc ("PRES")) &
        Method ("CELL", 0, Bytes'[16#70#] & Enc ("MAKE") & [16#60#,16#A4#] & Enc ("PASS") & [16#60#] & Enc ("PRES")) &
        -- Lexical compound Store must share the exact object into its result
        -- owner while preserving the source cell. Later pressure tests both.
        Method ("COPY", 0, Bytes'[16#70#] & Enc ("MAKE") &
          [16#60#,16#70#,16#60#,16#61#,16#A4#] & Enc ("PASS") & [16#61#] & Enc ("PRES")) &
        -- Independently read the original source cell after the same copy.
        Method ("KEEP", 0, Bytes'[16#70#] & Enc ("MAKE") &
          [16#60#,16#70#,16#60#,16#61#,16#A4#] & Enc ("PASS") & [16#60#] & Enc ("PRES")) &
        -- Return an Index carrier, then resolve its compound member only at
        -- Concatenate consumption. Pressure runs while the parent is retained.
        Method ("PACK", 0, Bytes'[16#08#] & Enc ("PKG0") &
          Block (16#12#, Bytes'[1] & Value) &
          [16#A4#,16#88#] & Enc ("PKG0") & [0,0]) &
        Method ("ZERO", 0, Pressure & [16#A4#] & Empty_Value) &
        Method ("JOIN", 0, Bytes'[16#A4#,16#73#] & Enc ("PACK") & Enc ("ZERO") & [0]);
   end Fixture;
begin
   for Width in Integer_Width loop
      for Kind in Shape loop
         declare
            A : V.Arena;
            Input : aliased AML_Table_Backing.State (1, 1);
            Access_Result : V.Access_Status;
            Loaded : N.Load_Status;
            Init : N.Initialization_Report;
            procedure Exercise (Name : String) is
               Found : N.Lookup_Result;
               Result : V.Result;
               Description : V.Value_Description;
               Data : Bytes (11 .. 10 + Expected'Length);
               Copied : Natural;
               Before : constant V.Collection_Statistics := V.Reclamation_Metrics (A);
               After : V.Collection_Statistics;
               Child : N.Node_ID;
            begin
               V.Find_Object (A, N.Root, Name, Found, Access_Result);
               Check (Access_Result = V.Available and then Found.Status = N.Found, "find " & Name);
               V.Invoke (A, Input, Found.Node, [others => (V.Immediate_Argument, 0)],
                 0, 20_000, Result, Access_Result);
               Check (Access_Result = V.Available and then Result.Status = AML_Execute.Object_Returned, "return " & Name);
               V.Describe (A, Result.Handle, Description, Access_Result);
               Check (Access_Result = V.Available and then
                 Description.Kind = (case Kind is when Buffer_Result => V.Buffer_Description, when String_Result => V.String_Description)
                 and then Description.Length = Expected'Length, "returned shape");
               V.Read_Bytes (A, Result.Handle, 0, Data, Copied, Access_Result);
               Check (Access_Result = V.Available and then Copied = Expected'Length and then Data = Expected, "full retained bytes");
               After := V.Reclamation_Metrics (A);
               Check (After.Completed > Before.Completed, "actual collection");
               Check (After.Freed_Objects > Before.Freed_Objects and then After.Freed_Bytes > Before.Freed_Bytes, "reclaimed storage");
               Check (After.Rejected = Before.Rejected, "no rejected collection");
               V.Release (A, Result.Handle, Access_Result);
               Check (Access_Result = V.Available and then V.Retained_Count (A) = 0, "release result");
               V.Find_Object (A, N.Root, "MAKE", Found, Access_Result);
               Check (Access_Result = V.Available and then Found.Status = N.Found, "maker persists");
               V.Find_Child (A, Found.Node, "BOX0", Child, Access_Result);
               Check (Access_Result = V.Available and then Child = N.Root, "temporary container retired");
               pragma Assert (V.Module_Transient_Free (A));
            end Exercise;
         begin
            V.Initialize (A, Access_Result); Check (Access_Result = V.Available, "initialize");
            V.Load (A, Fixture (Kind), Width, Loaded, Access_Result);
            Check (Access_Result = V.Available and then Loaded = N.Loaded, "load");
            V.Seal (A, Init, Access_Result); Check (Access_Result = V.Available, "seal");
            for Pass in 1 .. Repetitions loop Exercise ("CALL"); Exercise ("CELL"); Exercise ("COPY"); Exercise ("JOIN"); Exercise ("KEEP"); end loop;
            V.Reset (A, Access_Result); Check (Access_Result = V.Available, "reset");
         end;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("compound return GC checks" & Checks'Image);
end Compound_Return_GC_Tests;
