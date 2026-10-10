with AML_Delays;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_Objects;
with AML_Table_Backing;
procedure Slot_Copy_Tests is
   procedure Capture (Value : out Integer_Value; Available : out Boolean);
   package NS is new AML_Namespace (16, AML_Delays.Unavailable_Provider, Capture);
   use NS; use NS.Owned;
   use type AML_Objects.Usage;
   use type Integer_Value;
   use type AML_Objects.State;
   use type AML_Objects.Allocation_Status;
   A : Arena;
   Input : aliased AML_Table_Backing.State (1, 1);
   Result : Execution_Result;
   Loaded_Status : Load_Status;
   OK : Boolean;
   Checks : Natural := 0;
   Buffer_A : constant Bytes := [16#11#,3,1,65];
   Package_A : constant Bytes := [16#12#,6,1,16#11#,3,1,65];
   procedure Check (Value : Boolean) is
   begin
      Checks := Checks + 1;
      if not Value then raise Program_Error with Checks'Image & Result.Status'Image; end if;
   end Check;
   procedure Run (Code : Bytes; Width : Integer_Width) is
   begin
      Reset (A, OK); Check (OK);
      Load (A, Bytes'(16#14#, Byte (6 + Code'Length),84,69,83,84,0) & Code,
            Width, Loaded_Status);
      Check (Loaded_Status = Loaded);
      Invoke (A, Input, 1, [others => <>], 0, 300, Result);
   end Run;
   procedure Expect (Code : Bytes; Width : Integer_Width; Value : Integer_Value) is
   begin
      Run (Code, Width);
      Check (Result.Status = Returned and then Result.Value = Value);
   end Expect;
   Prior : AML_Objects.Usage;
   type Pool_Mode is (Object_Pool, Byte_Pool);
   Pool : Pool_Mode := Object_Pool;
   Saved_Values : AML_Objects.State;
   procedure Capture (Value : out Integer_Value; Available : out Boolean) is
      ID : AML_Objects.Object_ID;
      Status : AML_Objects.Allocation_Status;
   begin
      Value := 0; Available := True;
      case Pool is
      when Object_Pool =>
         while Values_Used (A).Objects < AML_Objects.Max_Objects loop
            Append (A, Bytes'(1 .. 0 => 0), ID, Status);
            Check (Status = AML_Objects.Allocated);
         end loop;
      when Byte_Pool =>
         Append (A, Bytes'(1 .. AML_Objects.Max_Bytes - Values_Used (A).Bytes => 0), ID, Status);
         Check (Status = AML_Objects.Allocated);
      end case;
      Saved_Values := Value_Store (Snapshot (A));
   end Capture;
begin
   for Width in Integer_Width loop
      -- Local-to-Local Buffer capture: mutating the destination preserves source.
      Expect (Bytes'(1 => 16#70#) & Buffer_A & Bytes'(16#60#,16#70#,16#60#,16#61#,
        16#70#,16#0A#,9,16#88#,16#61#,0,0,
        16#A4#,16#83#,16#88#,16#60#,0,0), Width,65);
      -- Direct Arg capture follows the same policy even with no initial args.
      Expect (Bytes'(1 => 16#70#) & Buffer_A & Bytes'(16#60#,16#70#,16#60#,16#68#,
        16#70#,16#0A#,9,16#88#,16#68#,0,0,
        16#A4#,16#83#,16#88#,16#60#,0,0), Width,65);
      -- A reference to a frame slot is an independently copied destination.
      Expect (Bytes'(1 => 16#70#) & Buffer_A & Bytes'(16#60#,
        16#70#,0,16#61#,16#70#,16#60#,16#71#,16#61#,
        16#70#,16#0A#,9,16#88#,16#61#,0,0,
        16#A4#,16#83#,16#88#,16#60#,0,0), Width,65);
      -- Nested package Buffer is independently copied, not merely package header.
      Expect (Bytes'(1 => 16#70#) & Package_A & Bytes'(16#60#,16#70#,16#60#,16#61#,
        16#70#,16#0A#,9,16#88#,16#83#,16#88#,16#61#,0,0,0,0,
        16#A4#,16#83#,16#88#,16#83#,16#88#,16#60#,0,0,0,0), Width,65);
      -- Arg RefOf frame slot writes indirectly; Local reference is overwritten.
      Expect (Bytes'(1 => 16#70#) & Buffer_A & Bytes'(16#60#,
        16#70#,0,16#61#,16#70#,16#71#,16#61#,16#68#,16#70#,16#60#,16#68#,
        16#70#,16#0A#,9,16#88#,16#61#,0,0,
        16#A4#,16#83#,16#88#,16#60#,0,0), Width,65);
      -- Reference leaves inside a copied package retain their frame identity.
      Expect ([16#70#,16#0A#,5,16#64#,
        16#70#,16#12#,3,1,0,16#60#,
        16#70#,16#71#,16#64#,16#88#,16#60#,0,0,
        16#70#,16#60#,16#61#,16#70#,16#0A#,9,16#64#,
        16#A4#,16#83#,16#83#,16#88#,16#61#,0,0], Width,9);
      -- Named Arg RefOf destination uses the existing owner-side clone once.
      Expect (Bytes'(16#08#,83,82,67,66) & Buffer_A &
        Bytes'(16#08#,68,83,84,66,0,
          16#70#,16#71#,68,83,84,66,16#68#,
          16#70#,83,82,67,66,16#68#,
          16#70#,16#0A#,9,16#88#,68,83,84,66,0,0,
          16#A4#,16#83#,16#88#,83,82,67,66,0,0), Width,65);
      for Fill in Pool_Mode loop
         Pool := Fill;
         -- Timer fills the allocator after source capture. A failed capture
         -- preserves its post-expression store exactly; no issuer rewind claim.
         Run (Bytes'(1 => 16#70#) & Package_A & Bytes'(16#60#,16#70#,16#5B#,16#33#,16#67#,
           16#70#,16#60#,16#61#,16#A4#,1), Width);
         Check (Result.Status = Value_Limit);
         Check (Value_Store (Snapshot (A)) = Saved_Values);
         Run (Bytes'(1 => 16#70#) & Package_A & Bytes'(16#60#,16#70#,16#5B#,16#33#,16#67#,
           16#70#,16#60#,16#60#,16#A4#,1), Width);
         Check (Result.Status = Returned and then Result.Value = 1);
         Check (Value_Store (Snapshot (A)) = Saved_Values);
      end loop;
      -- Self Store allocates nothing for Buffer and Package objects.
      for Is_Package in Boolean loop
         declare Item : constant Bytes := (if Is_Package then Package_A else Buffer_A); begin
            Run (Bytes'(1 => 16#70#) & Item & Bytes'(16#60#,16#A4#,1), Width);
            Check (Result.Status = Returned); Prior := Values_Used (A);
            Run (Bytes'(1 => 16#70#) & Item & Bytes'(16#60#,16#70#,16#60#,16#60#,16#A4#,1), Width);
            Check (Result.Status = Returned); Check (Values_Used (A) = Prior);
         end;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("SLOT COPY: PASS" & Checks'Image);
end Slot_Copy_Tests;
