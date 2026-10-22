pragma Ada_2022;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Delays;
with AML_Execute;
with AML_Namespace;
with AML_Table_Backing;
with AML_References;
procedure Name_Capture_GC_Tests is
   package N is new AML_Namespace (32, AML_Delays.Unavailable_Provider);
   package V is new N.Owned.Collecting;
   use type AML_Decode.Bytes;
   use type AML_Decode.Integer_Value;
   use type N.Load_Status;
   use type N.Lookup_Status;
   use type V.Access_Status;
   use type V.Collection_Count;
   use type AML_Execute.Execution_Status;
   use type V.Value_Kind;
   use type AML_References.Reference_Kind;
   use type V.Value_Handle;
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
   Code : constant Bytes :=
     Method ("MAKE", Bytes'[16#08#] & Enc ("TEMP") &
       Block (16#12#, Bytes'[2] & Enc ("TEMP") & [16#0A#,42]) &
       [16#A4#] & Enc ("TEMP")) &
     Method ("PRES", Pressure & [16#A4#,0]);
begin
   for Width in Integer_Width loop
      declare
         A : V.Arena;
         Input : aliased AML_Table_Backing.State (1, 1);
         Access_Result : V.Access_Status;
         Load_Result : N.Load_Status;
         Init : N.Initialization_Report;
         Result : V.Result;
         Package_Handle, Name_Handle, Rejected : V.Value_Handle;
         Description : V.Value_Description;
         Before, After : V.Collection_Statistics;
         Maker, Pressure_Node : N.Lookup_Result;
         Child : N.Node_ID;
      begin
         V.Initialize (A, Access_Result); Check (Access_Result = V.Available, "initialize");
         V.Load (A, Code, Width, Load_Result, Access_Result);
         Check (Access_Result = V.Available and then Load_Result = N.Loaded, "load");
         V.Seal (A, Init, Access_Result); Check (Access_Result = V.Available, "seal");
         V.Find_Object (A, N.Root, "MAKE", Maker, Access_Result);
         Check (Access_Result = V.Available and then Maker.Status = N.Found, "find maker");
         V.Find_Object (A, N.Root, "PRES", Pressure_Node, Access_Result);
         Check (Access_Result = V.Available and then Pressure_Node.Status = N.Found, "find pressure");
         for Pass in 1 .. Repetitions loop
            V.Invoke (A, Input, Maker.Node, [others => (V.Immediate_Argument, 0)],
              0, 20_000, Result, Access_Result);
            Check (Access_Result = V.Available and then Result.Status = AML_Execute.Object_Returned,
              "return package");
            Package_Handle := Result.Handle;
            V.Read_Element (A, Package_Handle, 0, Name_Handle, Access_Result);
            Check (Access_Result = V.Available, "read actual NAME member");
            V.Describe (A, Name_Handle, Description, Access_Result);
            Check (Access_Result = V.Available and then Description.Kind = V.Reference_Description
              and then Description.Ref_Kind = AML_References.Name_Member, "NAME kind");
            V.Release (A, Package_Handle, Access_Result); Check (Access_Result = V.Available, "release parent");
            Check (V.Retained_Count (A) = 1, "only standalone NAME remains");
            V.Find_Child (A, Maker.Node, "TEMP", Child, Access_Result);
            Check (Access_Result = V.Available and then Child = N.Root, "node expired");
            V.Dereference (A, Name_Handle, Rejected, Access_Result);
            Check (Access_Result = V.Invalid_Value and then Rejected = V.No_Value,
              "expired NAME read rejected before collection");
            Check (V.Retained_Count (A) = 1, "failed read retains no extra pin");
            Before := V.Reclamation_Metrics (A);
            V.Invoke (A, Input, Pressure_Node.Node, [others => (V.Immediate_Argument, 0)],
              0, 20_000, Result, Access_Result);
            Check (Access_Result = V.Available and then Result.Status = AML_Execute.Returned
              and then Result.Number = 0, "pressure invocation");
            After := V.Reclamation_Metrics (A);
            Check (After.Completed > Before.Completed, "actual collection");
            Check (After.Freed_Objects > Before.Freed_Objects and then After.Freed_Bytes > Before.Freed_Bytes,
              "actual objects and bytes reclaimed");
            -- No other package exists in this fixture. These two slots belong
            -- to MAKE's released package; retaining NAME must not keep it alive.
            Check (After.Freed_Elements >= Before.Freed_Elements + 2, "old package elements reclaimed");
            Check (After.Rejected = Before.Rejected, "no rejected collection");
            V.Describe (A, Name_Handle, Description, Access_Result);
            Check (Access_Result = V.Available and then Description.Kind = V.Reference_Description
              and then Description.Ref_Kind = AML_References.Name_Member, "retained descriptor remains valid data");
            V.Dereference (A, Name_Handle, Rejected, Access_Result);
            Check (Access_Result = V.Invalid_Value and then Rejected = V.No_Value,
              "expired NAME read rejected after collection");
            Check (V.Retained_Count (A) = 1, "no package or Index root");
            V.Release (A, Name_Handle, Access_Result); Check (Access_Result = V.Available, "release NAME");
            Check (V.Retained_Count (A) = 0, "all pins released");
            pragma Assert (V.Module_Transient_Free (A));
         end loop;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("optional NAME GC checks" & Checks'Image);
end Name_Capture_GC_Tests;
