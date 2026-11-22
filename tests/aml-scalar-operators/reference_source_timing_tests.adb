with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Delays;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_Table_Backing;
procedure Reference_Source_Timing_Tests is
   use type Integer_Value;
   package N is new AML_Namespace (32, AML_Delays.Unavailable_Provider);
   use N; use N.Owned;
   A : Arena;
   Input : aliased AML_Table_Backing.State (1, 32);
   Checks : Natural := 0;
   Case_1 : constant Bytes :=
     [16#08#, 16#53#, 16#52#, 16#43#, 16#30#, 16#0A#, 16#11#, 16#08#, 16#42#, 16#55#, 16#46#, 16#30#,
      16#11#, 16#05#, 16#0A#, 16#02#, 16#00#, 16#00#, 16#14#, 16#0F#, 16#4D#, 16#55#, 16#54#, 16#30#,
      16#00#, 16#70#, 16#0A#, 16#22#, 16#53#, 16#52#, 16#43#, 16#30#, 16#A4#, 16#01#, 16#14#, 16#3A#,
      16#54#, 16#45#, 16#53#, 16#54#, 16#00#, 16#9D#, 16#0A#, 16#11#, 16#53#, 16#52#, 16#43#, 16#30#,
      16#70#, 16#00#, 16#88#, 16#42#, 16#55#, 16#46#, 16#30#, 16#01#, 16#00#, 16#70#, 16#83#, 16#71#,
      16#53#, 16#52#, 16#43#, 16#30#, 16#88#, 16#42#, 16#55#, 16#46#, 16#30#, 16#4D#, 16#55#, 16#54#,
      16#30#, 16#00#, 16#A4#, 16#7D#, 16#79#, 16#53#, 16#52#, 16#43#, 16#30#, 16#0A#, 16#08#, 16#00#,
      16#83#, 16#88#, 16#42#, 16#55#, 16#46#, 16#30#, 16#01#, 16#00#, 16#00#];
   Case_2 : constant Bytes :=
     [16#08#, 16#53#, 16#52#, 16#43#, 16#30#, 16#0A#, 16#11#, 16#08#, 16#42#, 16#55#, 16#46#, 16#30#,
      16#11#, 16#05#, 16#0A#, 16#02#, 16#00#, 16#00#, 16#14#, 16#0F#, 16#4D#, 16#55#, 16#54#, 16#30#,
      16#00#, 16#9D#, 16#0A#, 16#22#, 16#53#, 16#52#, 16#43#, 16#30#, 16#A4#, 16#01#, 16#14#, 16#3A#,
      16#54#, 16#45#, 16#53#, 16#54#, 16#00#, 16#9D#, 16#0A#, 16#11#, 16#53#, 16#52#, 16#43#, 16#30#,
      16#70#, 16#00#, 16#88#, 16#42#, 16#55#, 16#46#, 16#30#, 16#01#, 16#00#, 16#70#, 16#83#, 16#71#,
      16#53#, 16#52#, 16#43#, 16#30#, 16#88#, 16#42#, 16#55#, 16#46#, 16#30#, 16#4D#, 16#55#, 16#54#,
      16#30#, 16#00#, 16#A4#, 16#7D#, 16#79#, 16#53#, 16#52#, 16#43#, 16#30#, 16#0A#, 16#08#, 16#00#,
      16#83#, 16#88#, 16#42#, 16#55#, 16#46#, 16#30#, 16#01#, 16#00#, 16#00#];
   Case_3 : constant Bytes :=
     [16#08#, 16#53#, 16#52#, 16#43#, 16#30#, 16#0A#, 16#11#, 16#08#, 16#42#, 16#55#, 16#46#, 16#30#,
      16#11#, 16#05#, 16#0A#, 16#02#, 16#00#, 16#00#, 16#14#, 16#0F#, 16#4D#, 16#55#, 16#54#, 16#30#,
      16#00#, 16#70#, 16#0A#, 16#22#, 16#53#, 16#52#, 16#43#, 16#30#, 16#A4#, 16#01#, 16#14#, 16#3A#,
      16#54#, 16#45#, 16#53#, 16#54#, 16#00#, 16#9D#, 16#0A#, 16#11#, 16#53#, 16#52#, 16#43#, 16#30#,
      16#70#, 16#00#, 16#88#, 16#42#, 16#55#, 16#46#, 16#30#, 16#01#, 16#00#, 16#99#, 16#83#, 16#71#,
      16#53#, 16#52#, 16#43#, 16#30#, 16#88#, 16#42#, 16#55#, 16#46#, 16#30#, 16#4D#, 16#55#, 16#54#,
      16#30#, 16#00#, 16#A4#, 16#7D#, 16#79#, 16#53#, 16#52#, 16#43#, 16#30#, 16#0A#, 16#08#, 16#00#,
      16#83#, 16#88#, 16#42#, 16#55#, 16#46#, 16#30#, 16#01#, 16#00#, 16#00#];
   Case_4 : constant Bytes :=
     [16#08#, 16#53#, 16#52#, 16#43#, 16#30#, 16#0A#, 16#11#, 16#08#, 16#42#, 16#55#, 16#46#, 16#30#,
      16#11#, 16#05#, 16#0A#, 16#02#, 16#00#, 16#00#, 16#14#, 16#0F#, 16#4D#, 16#55#, 16#54#, 16#30#,
      16#00#, 16#9D#, 16#0A#, 16#22#, 16#53#, 16#52#, 16#43#, 16#30#, 16#A4#, 16#01#, 16#14#, 16#3A#,
      16#54#, 16#45#, 16#53#, 16#54#, 16#00#, 16#9D#, 16#0A#, 16#11#, 16#53#, 16#52#, 16#43#, 16#30#,
      16#70#, 16#00#, 16#88#, 16#42#, 16#55#, 16#46#, 16#30#, 16#01#, 16#00#, 16#99#, 16#83#, 16#71#,
      16#53#, 16#52#, 16#43#, 16#30#, 16#88#, 16#42#, 16#55#, 16#46#, 16#30#, 16#4D#, 16#55#, 16#54#,
      16#30#, 16#00#, 16#A4#, 16#7D#, 16#79#, 16#53#, 16#52#, 16#43#, 16#30#, 16#0A#, 16#08#, 16#00#,
      16#83#, 16#88#, 16#42#, 16#55#, 16#46#, 16#30#, 16#01#, 16#00#, 16#00#];
   procedure Check (Good : Boolean; Detail : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Detail; end if;
   end Check;
   procedure Exercise (Data : Bytes; Width : Integer_Width; Case_ID : Positive) is
      OK : Boolean;
      Loaded_Status : Load_Status;
      Test_Node : Node_ID;
      Result : Execution_Result;
      Expected : constant Integer_Value :=
        (if Case_ID in 1 | 3 then 16#2222# else 16#2211#);
   begin
      Reset (A, OK); Check (OK, "isolated reference timing reset");
      Load (A, Data, Width, Loaded_Status);
      Check (Loaded_Status = Loaded, "load exact oracle AML payload");
      Test_Node := Child (Snapshot (A), Root, "TEST");
      Check (Test_Node /= Root, "find TEST");
      -- TEST resets SRC0 and BUF1 itself; the second call reuses this arena.
      -- Store mutates the referenced object (0x2222); CopyObject replaces the
      -- name binding while the old referenced object stays 0x11 (0x2211).
      for Repetition in 1 .. 2 loop
         Invoke (A, Input, Test_Node,
           [others => (Integer_Datum, 0, Ordinary_Integer)], 0, 1000, Result);
         Check (Result.Status = Returned,
           Case_ID'Image & Width'Image & Repetition'Image & Result.Status'Image);
         Check (Result.Status = Returned and then Result.Value = Expected,
           "source/target timing case" & Case_ID'Image & Width'Image & Repetition'Image &
           " actual" & (if Result.Status = Returned then Result.Value'Image else " unavailable"));
         Check (Frame_Root_Count (A) = 0, "reference timing frame cleanup");
      end loop;
   end Exercise;
begin
   for Width in Integer_Width loop
      Exercise (Case_1, Width, 1);
      Exercise (Case_2, Width, 2);
      Exercise (Case_3, Width, 3);
      Exercise (Case_4, Width, 4);
   end loop;
   Ada.Text_IO.Put_Line ("AML-REFERENCE-SOURCE-TIMING: PASS" & Checks'Image);
end Reference_Source_Timing_Tests;
