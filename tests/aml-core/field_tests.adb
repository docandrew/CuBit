with Ada.Text_IO; use Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Fields; use AML_Fields;
procedure Field_Tests is
   use type Byte;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Checks'Image; end if;
   end Check;
   procedure Frame (Data : Bytes; Kind : Entry_Kind; Consumed : Positive) is
      R : constant Entry_Result := Read_Entry (Data);
   begin
      Check (R.Status = Accepted and then R.Kind = Kind and then R.Consumed = Consumed);
      for N in 0 .. Consumed - 1 loop
         Check (Read_Entry (Data (Data'First .. Data'First + N - 1)).Status = Truncated);
      end loop;
   end Frame;
   R : Entry_Result;
begin
   Frame ([0, 0], Reserved_Field, 2);
   Frame ([16#41#, 16#42#, 16#43#, 16#44#, 0], Named_Field, 5);
   Frame ([1, 0, 0, 16#FF#], Access_Field, 3);
   Frame ([3, 5, 16#0E#, 32, 16#FF#], Extended_Access_Field, 4);
   Frame ([2, 16#41#, 16#42#, 16#43#, 16#44#], Name_Connection, 5);
   Frame ([2, 16#5C#, 16#41#, 16#42#, 16#43#, 16#44#], Name_Connection, 6);
   Frame ([2, 16#11#, 2, 0, 16#FF#], Buffer_Connection, 4);
   Frame ([2, 16#11#, 16#44#, 0, 16#0A#, 0], Buffer_Connection, 6);
   Check (Read_Entry ([2, 16#11#, 1]).Status = Malformed);
   Check (Read_Entry ([2, 16#11#, 0]).Status = Malformed);
   Check (Read_Entry ([2, 16#FF#]).Status = Malformed);
   -- Raw attributes are preserved, not mistaken for I/O authorization.
   for A in Byte loop
      for B in Byte loop
         R := Read_Entry ([1, A, B]);
         Check (R.Status = Accepted and then R.Access_Type = A and then R.Attribute = B
                and then R.Access_Length = 0);
         R := Read_Entry ([3, A, B, A]);
         Check (R.Status = Accepted and then R.Access_Type = A and then R.Attribute = B
                and then R.Access_Length = A);
      end loop;
      R := Read_Entry ([16#41#, A, 16#43#, 16#44#, 8]);
      if A = 16#5F# or else A in 16#41# .. 16#5A# or else A in 16#30# .. 16#39# then
         Check (R.Status = Accepted and then Character'Pos (R.Name (2)) = Natural (A));
      else Check (R.Status = Malformed);
      end if;
   end loop;
   -- Named and reserved entries share exact bit counts, not package extents.
   for A in 0 .. 63 loop
      R := Read_Entry ([16#41#, 16#42#, 16#43#, 16#44#, Byte (A), 16#FF#]);
      Check (R.Status = Accepted and then R.Name = "ABCD" and then R.Bits = A
             and then R.Consumed = 5);
      R := Read_Entry ([0, Byte (A), 16#FF#]);
      Check (R.Status = Accepted and then R.Bits = A and then R.Consumed = 2);
   end loop;
   R := Read_Entry ([0, 16#CF#, 16#FF#, 16#FF#, 16#FF#]);
   Check (R.Status = Accepted and then R.Bits = Field_Bit_Length'Last);
   Frame ([0, 16#C0#, 0, 0, 0], Reserved_Field, 5);
   Check (Read_Entry ([0, 16#70#, 0]).Status = Malformed);
   R := Read_Entry ([Positive'Last - 4 => 16#41#, Positive'Last - 3 => 16#42#,
                    Positive'Last - 2 => 16#43#, Positive'Last - 1 => 16#44#,
                    Positive'Last => 32]);
   Check (R.Status = Accepted and then R.Bits = 32 and then R.Name = "ABCD");
   R := Read_Entry ([Positive'Last - 3 => 2, Positive'Last - 2 => 16#11#,
                    Positive'Last - 1 => 2, Positive'Last => 0]);
   Check (R.Status = Accepted and then R.Kind = Buffer_Connection and then R.Consumed = 4);
   R := Read_Entry ([Positive'Last => 0]);
   Check (R.Status = Truncated);
   -- Walk a mixed list; connection and access entries do not become bit lengths.
   declare
      Data : constant Bytes := [16#41#, 16#42#, 16#43#, 16#44#, 32,
        0, 8, 1, 1, 0, 2, 16#11#, 2, 0, 3, 5, 16#0E#, 32,
        16#45#, 16#46#, 16#47#, 16#48#, 16#48#, 4];
      Position : Positive := Data'First;
      Total, Entries : Natural := 0;
   begin
      while Position <= Data'Last loop
         R := Read_Entry (Data (Position .. Data'Last));
         Check (R.Status = Accepted);
         Total := Total + R.Bits;
         Entries := Entries + 1;
         Position := Position + R.Consumed;
      end loop;
      Check (Entries = 6 and Total = 112 and Position = Data'Last + 1);
   end;
   Put_Line ("AML-FIELD-CHECK: PASS" & Checks'Image);
end Field_Tests;
