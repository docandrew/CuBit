with Ada.Text_IO; use Ada.Text_IO;
with AML_Decode;
with AML_Names; use AML_Names;
procedure Name_Tests is
   use type AML_Decode.Byte;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then
         raise Program_Error with Checks'Image;
      end if;
   end Check;
   Data : AML_Decode.Bytes (7 .. 1286) := [others => 16#41#];
   R : Name_Result;
   Size : Natural;
begin
   for N in 1 .. 255 loop
      Data := [others => 16#41#];
      Data (7) := 16#2F#;
      Data (8) := AML_Decode.Byte (N);
      Size := 2 + N * 4;
      R := Read_Name (Data (7 .. 6 + Size));
      Check (R.Kind = Accepted and then R.Count = N
             and then R.Consumed = Size and then not R.Rooted
             and then R.Parents = 0);
      for I in 1 .. N loop
         Check (R.Parts (I) = "AAAA");
      end loop;
      for Trunc in 0 .. Size - 1 loop
         Check (Read_Name (Data (7 .. 6 + Trunc)).Kind = Truncated);
      end loop;
   end loop;
   --  Every possible byte in every segment position, after DualNamePrefix,
   --  so invalid leading bytes cannot be reinterpreted as other name forms.
   for J in 1 .. 8 loop
      for B in AML_Decode.Byte loop
         Data := [others => 16#41#];
         Data (7) := 16#2E#;
         Data (7 + J) := B;
         R := Read_Name (Data (7 .. 15));
         declare
            Valid : constant Boolean :=
              (B in 16#41# .. 16#5A# or else B = 16#5F# or else
               (J not in 1 | 5 and then B in 16#30# .. 16#39#));
         begin
            Check ((if Valid then R.Kind = Accepted else R.Kind = Malformed));
         end;
      end loop;
   end loop;
   for P in 0 .. 256 loop
      Data := [others => 16#5E#];
      Data (7 + P) := 0;
      R := Read_Name (Data (7 .. 7 + P));
      Check ((if P > 255 then R.Kind = Limit_Exceeded else
              R.Kind = Accepted and then R.Parents = P and then R.Count = 0
              and then R.Consumed = P + 1));
   end loop;
   Check (Read_Name ([16#2F#, 0]).Kind = Malformed);
   Check (Read_Name ([16#5C#, 16#5E#, 0]).Kind = Malformed);
   Check (Read_Name ([16#5E#, 16#5C#, 0]).Kind = Malformed);
   Check (Read_Name ([16#5C#]).Kind = Truncated);
   Check (Read_Name ([16#5E#]).Kind = Truncated);
   R := Read_Name ([Positive'Last - 4 => 16#5C#,
                   Positive'Last - 3 => 16#5F#, Positive'Last - 2 => 16#53#,
                   Positive'Last - 1 => 16#42#, Positive'Last => 16#5F#]);
   Check (R.Kind = Accepted and then R.Rooted and then R.Parents = 0
          and then R.Count = 1 and then R.Parts (1) = "_SB_"
          and then R.Consumed = 5);
   R := Read_Name ([Positive'Last => 0]);
   Check (R.Kind = Accepted and then R.Count = 0 and then R.Consumed = 1);
   Put_Line ("AML-NAME-CHECK: PASS" & Checks'Image);
end Name_Tests;
