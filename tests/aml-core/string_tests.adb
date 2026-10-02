with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with Namespace_Instance;
procedure String_Tests is
   use type Byte;
   package NS renames Namespace_Instance;
   use type NS.Load_Status;
   use type NS.Object_Kind;
   use type NS.State;
   Checks : Natural := 0;
   Data : Bytes (11 .. 268) := [others => 65];
   R : String_Result;
   Tree : NS.State := NS.Empty;
   Before : NS.State;
   Loaded : NS.Load_Status;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Checks'Image; end if;
   end Check;
begin
   for N in 0 .. 255 loop
      Data := [others => 65];
      Data (11) := 16#0D#;
      Data (12 + N) := 0;
      R := Read_String (Data);
      Check (R.Kind = Accepted and then R.Length = N and then R.Consumed = N + 2);
      for I in 1 .. N loop
         Check (R.Text (I) = 'A');
      end loop;
      for Cut in 0 .. N + 1 loop
         Check (Read_String (Data (11 .. 10 + Cut)).Kind = Truncated);
      end loop;
   end loop;
   for B in Byte loop
      R := Read_String ([16#0D#, B, 0]);
      Check ((if B = 0 then R.Kind = Accepted and then R.Length = 0
              elsif B <= 127 then R.Kind = Accepted and then R.Length = 1
                and then R.Text (1) = Character'Val (B)
              else R.Kind = Malformed));
   end loop;
   Data := [others => 65];
   Data (11) := 16#0D#;
   Check (Read_String (Data).Kind = Limit_Exceeded);
   Check (Read_String ([1,0]).Kind = Unsupported);
   R := Read_String ([Positive'Last - 1 => 16#0D#, Positive'Last => 0]);
   Check (R.Kind = Accepted and then R.Length = 0);
   NS.Load_Names (Tree,
     [8,16#5F#,16#48#,16#49#,16#44#,16#0D#,
      16#50#,16#4E#,16#50#,16#30#,16#43#,16#30#,16#44#,0], Bits_64, Loaded);
   Check (Loaded = NS.Loaded and then NS.Kind (Tree, 1) = NS.String_Object
          and then NS.String_Data (Tree, 1) = "PNP0C0D");
   Before := Tree;
   NS.Load_Names (Tree,
     [8,16#54#,16#45#,16#53#,16#54#,16#0D#,128,0], Bits_64, Loaded);
   Check (Loaded = NS.Bad_String and then Tree = Before);
   NS.Load_Names (Tree,
     [8,16#54#,16#45#,16#53#,16#54#] & Data, Bits_64, Loaded);
   Check (Loaded = NS.Value_Limit and then Tree = Before);
   Ada.Text_IO.Put_Line ("AML-STRING-CHECK: PASS" & Checks'Image);
end String_Tests;
