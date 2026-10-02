with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with Namespace_Instance;
procedure Buffer_Tests is
   use type Byte;
   package NS renames Namespace_Instance;
   use type NS.Load_Status;
   use type NS.Object_Kind;
   use type NS.State;
   Data : Bytes (7 .. 1100) := [others => 0];
   R : Buffer_Result;
   Tree : NS.State := NS.Empty;
   Before : NS.State;
   Loaded : NS.Load_Status;
   Checks : Natural := 0;
   Extent : Natural;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Checks'Image; end if;
   end Check;
begin
   for Declared in 0 .. 32 loop
      for Initial in 0 .. 32 loop
         Data := [others => 0];
         Data (7) := 16#11#;
         Extent := Initial + 4;
         Data (8) := Byte (16#40# + Extent mod 16);
         Data (9) := Byte (Extent / 16);
         Data (10) := 16#0A#;
         Data (11) := Byte (Declared);
         for I in 1 .. Initial loop Data (11 + I) := Byte (I); end loop;
         R := Read_Buffer (Data, Bits_64);
         Check (R.Kind = Accepted and then R.Length = Natural'Max (Declared, Initial)
                and then R.Consumed = Initial + 5);
         for I in 1 .. R.Length loop
            Check (R.Content (I) = (if I <= Initial then Byte (I) else 0));
         end loop;
         for Cut in 0 .. Initial + 4 loop
            Check (Read_Buffer (Data (7 .. 6 + Cut), Bits_64).Kind /= Accepted);
         end loop;
      end loop;
   end loop;
   R := Read_Buffer ([16#11#,4,16#0B#,0,4], Bits_64);
   Check (R.Kind = Accepted and then R.Length = 1024);
   for I in 1 .. R.Length loop Check (R.Content (I) = 0); end loop;
   Check (Read_Buffer ([16#11#,4,16#0B#,1,4], Bits_64).Kind = Limit_Exceeded);
   Check (Read_Buffer ([16#11#,1], Bits_64).Kind = Malformed);
   Check (Read_Buffer ([16#11#,2,16#60#], Bits_64).Kind = Unsupported);
   --  QWord size truncates to DSDT integer width before budget admission.
   R := Read_Buffer ([16#11#,10,16#0E#,0,0,0,0,1,0,0,0], Bits_32);
   Check (R.Kind = Accepted and then R.Length = 0);
   Check (Read_Buffer ([16#11#,10,16#0E#,0,0,0,0,1,0,0,0], Bits_64).Kind = Limit_Exceeded);
   R := Read_Buffer ([Positive'Last - 2 => 16#11#,
                     Positive'Last - 1 => 2, Positive'Last => 0], Bits_64);
   Check (R.Kind = Accepted and then R.Length = 0);
   NS.Load_Names (Tree, [8,16#5F#,16#43#,16#52#,16#53#,
                        16#11#,5,16#0A#,2,16#79#,0], Bits_64, Loaded);
   Check (Loaded = NS.Loaded and then NS.Kind (Tree, 1) = NS.Buffer_Object
          and then NS.Buffer_Data (Tree, 1) = Bytes'[16#79#,0]);
   Before := Tree;
   NS.Load_Names (Tree, [8,16#42#,16#41#,16#44#,16#5F#,16#11#,1], Bits_64, Loaded);
   Check (Loaded = NS.Bad_Buffer and then Tree = Before);
   Ada.Text_IO.Put_Line ("AML-BUFFER-CHECK: PASS" & Checks'Image);
end Buffer_Tests;
