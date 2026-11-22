pragma Ada_2022;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Delays;
with AML_Execute;
with AML_Namespace;
with AML_Table_Backing;
procedure Owned_GC_Tests is
   package N is new AML_Namespace (32, AML_Delays.Unavailable_Provider);
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
   Repetitions : constant := 3;
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
   Target : constant Bytes := Bytes'[16#88#] & Enc ("OUTB") & Enc ("TARG") & [0];
   Read_Output : constant Bytes := Bytes'[16#A4#,16#83#,16#88#] & Enc ("OUTB") & [0,0];
   Code : constant Bytes :=
     Bytes'[16#08#] & Enc ("AAAA") & [16#0A#,16#11#] &
     [16#08#] & Enc ("OUTB") & Block (16#11#, Bytes'[1]) &
     Method ("RST0", Bytes'[16#9D#,16#0A#,16#11#] & Enc ("AAAA") &
       [16#70#,0,16#88#] & Enc ("OUTB") & [0,0,16#A4#,0]) &
     Method ("TARG", Bytes'[16#9D#,16#0A#,16#22#] & Enc ("AAAA") & Pressure & [16#A4#,0]) &
     Method ("GET0", Bytes'[16#08#] & Enc ("TEMP") & [16#0A#,16#11#,16#A4#,16#83#,16#71#] & Enc ("TEMP")) &
     Method ("HOLD", Bytes'[16#70#,16#83#,16#71#] & Enc ("AAAA") & Target & Read_Output) &
     Method ("NEST", Bytes'[16#70#] & Enc ("GET0") & Target & Read_Output) &
     Method ("READ", Bytes'[16#A4#] & Enc ("AAAA"));
begin
   for Width in Integer_Width loop
      declare
         A : V.Arena;
         Input : aliased AML_Table_Backing.State (1, 1);
         Access_Result : V.Access_Status;
         Load_Result : N.Load_Status;
         Init : N.Initialization_Report;
         Result : V.Result;
         Before, After : V.Collection_Statistics;
         Method_Node : N.Lookup_Result;
         Child : N.Node_ID;
         procedure Invoke (Name : String; Expected : Integer_Value) is
            Found : N.Lookup_Result;
         begin
            V.Find_Object (A, N.Root, Name, Found, Access_Result);
            Check (Access_Result = V.Available and then Found.Status = N.Found, "find " & Name);
            V.Invoke (A, Input, Found.Node, [others => (V.Immediate_Argument, 0)],
              0, 20_000, Result, Access_Result);
            Check (Access_Result = V.Available and then Result.Status = AML_Execute.Returned,
              "invoke " & Name);
            Check (Result.Number = Expected, "value " & Name);
            Check (V.Retained_Count (A) = 0, "scalar result must not retain pin " & Name);
            -- Checked profile additionally observes exact transient/frame cleanup.
            pragma Assert (V.Module_Transient_Free (A));
         end Invoke;
      begin
         V.Initialize (A, Access_Result); Check (Access_Result = V.Available, "initialize");
         V.Load (A, Code, Width, Load_Result, Access_Result);
         Check (Access_Result = V.Available and then Load_Result = N.Loaded, "load");
         V.Seal (A, Init, Access_Result); Check (Access_Result = V.Available, "seal");
         for Pass in 1 .. Repetitions loop
            for Nested in Boolean loop
               Invoke ("RST0", 0);
               Invoke ("READ", 16#11#);
               Before := V.Reclamation_Metrics (A);
               if Nested then Invoke ("NEST", 16#11#); else Invoke ("HOLD", 16#11#); end if;
               After := V.Reclamation_Metrics (A);
               Check (After.Completed > Before.Completed, "actual collection required");
               Check (After.Freed_Objects > Before.Freed_Objects and then After.Freed_Bytes > Before.Freed_Bytes,
                 "actual objects and bytes reclaimed");
               Check (After.Rejected = Before.Rejected, "no rejected collection");
               Invoke ("READ", 16#22#);
               V.Find_Object (A, N.Root, "GET0", Method_Node, Access_Result);
               Check (Access_Result = V.Available and then Method_Node.Status = N.Found, "find helper");
               V.Find_Child (A, Method_Node.Node, "TEMP", Child, Access_Result);
               Check (Access_Result = V.Available and then Child = N.Root, "callee TEMP removed");
               pragma Assert (V.Module_Transient_Free (A));
            end loop;
         end loop;
         V.Reset (A, Access_Result); Check (Access_Result = V.Available and then V.Retained_Count (A) = 0, "final reset");
         pragma Assert (V.Module_Transient_Free (A));
      end;
   end loop;
   Ada.Text_IO.Put_Line ("owned GC checks" & Checks'Image);
end Owned_GC_Tests;
