with AML_Delays;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Objects;
with AML_Objects.Package_References;
with AML_Frame_Handles;
with AML_Table_Backing;
with AML_Namespace;
with AML_Names;
procedure Copy_Boundary_Extended_Tests is
   package NS is new AML_Namespace (64, AML_Delays.Unavailable_Provider);
   package Owner renames NS.Owned;
   use type NS.State;
   use type NS.Load_Status;
   use type AML_Objects.Usage;
   use type AML_Objects.State;
   use type AML_Objects.Allocation_Status;
   use type AML_Objects.Package_References.Result_Status;
   use type AML_Frame_Handles.Invocation_Serial;
   use type Integer_Value;
   Checks : Natural := 0;
   Input : aliased AML_Table_Backing.State (1, 1);
   Args : constant Value_Arguments := [others => (Integer_Datum, 0, AML_Decode.Ordinary_Integer)];
   DST : constant Bytes := [16#44#,16#53#,16#54#,16#30#];
   BUF : constant Bytes := [16#42#,16#55#,16#46#,16#30#];
   procedure Check (C : Boolean) is
   begin
      Checks := Checks + 1;
      if not C then raise Program_Error with Checks'Image; end if;
   end Check;
   procedure Load (A : in out Owner.Arena; B : Bytes; W : Integer_Width) is
      S : NS.Load_Status;
   begin
      Owner.Load (A, B, W, S); Check (S = NS.Loaded);
   end Load;
   procedure Reset (A : in out Owner.Arena) is
      OK : Boolean;
   begin Owner.Reset (A, OK); Check (OK); end Reset;
   procedure Fill_Objects (A : in out Owner.Arena; Remaining : Natural) is
      ID : AML_Objects.Object_ID;
      S : AML_Objects.Allocation_Status;
   begin
      while Owner.Values_Used (A).Objects < AML_Objects.Max_Objects - Remaining loop
         Owner.Append (A, Bytes'(1 .. 0 => 0), ID, S);
         Check (S = AML_Objects.Allocated);
      end loop;
   end Fill_Objects;
begin
   for W in Integer_Width loop
      -- Nested source consumes two package elements per clone. Fill to8190.
      declare
         A : Owner.Arena;
         R : Execution_Result;
         Serial : AML_Frame_Handles.Invocation_Serial;
      begin
         Reset (A);
         Load (A, Bytes'(1 => 16#08#) & BUF &
           Bytes'(16#12#,7,1,16#12#,4,1,16#0A#,7) &
           Bytes'(1 => 16#08#) & DST & Bytes'(16#0A#,42) &
           Bytes'(16#14#,17,16#54#,16#45#,16#53#,16#54#,0,16#9D#) &
           BUF & DST & Bytes'(16#A4#,1), W);
         -- 32*255+28 filler entries plus source2 =8190.
         for I in 0 .. 32 loop
            Load (A, Bytes'(16#08#,16#50#,16#41#,
              Byte (Character'Pos ('A') + I / 26),
              Byte (Character'Pos ('A') + I mod 26),
              16#12#,2,(if I < 32 then 255 else 28)), W);
         end loop;
         Check (Owner.Values_Used (A).Elements = AML_Objects.Max_Elements - 2);
         declare
            Before : constant NS.State := Owner.Snapshot (A);
         begin
            Serial := Owner.Invocation_Count (A);
            Owner.Invoke (A, Input, 3, Args, 0, 100, R);
            Check (R.Status = Value_Limit);
            Check (Owner.Invocation_Count (A) = Serial + 1);
            Check (NS.Value_Store (Owner.Snapshot (A)) = NS.Value_Store (Before));
            Check (NS.Cleanup_Frame (Owner.Snapshot (A), Before));
         end;
      end;
      -- Source method effect belongs before transaction: preserve Store(One,EFFT).
      declare
         A, Control : Owner.Arena;
         R, Source_Result : Execution_Result;
         Fixture : constant Bytes :=
           Bytes'(1 => 16#08#) & BUF & Bytes'(16#11#,4,16#0A#,1,7) &
           Bytes'(1 => 16#08#) & DST & Bytes'(16#0A#,42) &
           Bytes'(16#08#,16#45#,16#46#,16#46#,16#54#,0) &
           -- Method(SRC0,0){Store(One,EFFT);Return(BUF0)}
           Bytes'(16#14#,17,16#53#,16#52#,16#43#,16#30#,0,
             16#70#,1,16#45#,16#46#,16#46#,16#54#,16#A4#) & BUF &
           -- Method(TEST,0){CopyObject(SRC0(),DST0);Return(One)}
           Bytes'(16#14#,17,16#54#,16#45#,16#53#,16#54#,0,
             16#9D#,16#53#,16#52#,16#43#,16#30#) & DST & Bytes'(16#A4#,1);
      begin
         Reset (A); Reset (Control);
         Load (A, Fixture, W); Load (Control, Fixture, W);
         Fill_Objects (A, 1); Fill_Objects (Control, 1);
         -- Execute only the source on identically populated control arena.
         Owner.Invoke (Control, Input, 4, Args, 0, 100, Source_Result);
         Check (Source_Result.Status = Object_Returned);
         Check (NS.Integer_Data (Owner.Snapshot (Control), 3) = 1);
         declare
            Serial : constant AML_Frame_Handles.Invocation_Serial := Owner.Invocation_Count (A);
         begin
            Owner.Invoke (A, Input, 5, Args, 0, 100, R);
            Check (R.Status = Value_Limit);
            Check (Owner.Invocation_Count (A) = Serial + 1);
            Check (NS.Integer_Data (Owner.Snapshot (A), 3) = 1);
            Check (NS.Data_Object (Owner.Snapshot (A), 2) = NS.Data_Object (Owner.Snapshot (Control), 2));
            Check (Owner.Values_Used (A) = Owner.Values_Used (Control));
            Check (NS.Value_Store (Owner.Snapshot (A)) = NS.Value_Store (Owner.Snapshot (Control)));
         end;
      end;
      -- Empty package element is a valid source reference with no value.
      declare
         A : Owner.Arena;
         Ref : Owner.Reference;
         Made : AML_Objects.Package_References.Result_Status;
         Copy : Datum := (Integer_Datum, 99, AML_Decode.Ordinary_Integer);
         S : Execution_Status;
      begin
         Reset (A);
         Load (A, Bytes'(16#08#,16#50#,16#4B#,16#47#,16#30#,16#12#,2,1,
                        16#08#) & DST & Bytes'(16#0A#,42), W);
         Owner.Make_Element (A, NS.Data_Object (Owner.Snapshot (A), 1), 0, Ref, Made);
         Check (Made = AML_Objects.Package_References.Ready);
         declare
            Before : constant NS.State := Owner.Snapshot (A);
            Serial : constant AML_Frame_Handles.Invocation_Serial := Owner.Invocation_Count (A);
         begin
            Owner.Copy_And_Attach (A,
              (Named_Destination, 0, AML_Names.Read_Name (DST)), W,
              (Reference_Datum, Ref), Copy, S);
            Check (S = Uninitialized);
            Check (Owner.Snapshot (A) = Before);
            Check (Owner.Invocation_Count (A) = Serial);
            Check (Copy.Value_Kind = Integer_Datum and then Copy.Number = 0);
         end;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("COPY BOUNDARY EXTENDED" & Checks'Image);
end Copy_Boundary_Extended_Tests;
