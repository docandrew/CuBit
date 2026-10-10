with Ada.Text_IO;
with AML_Delays;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_Objects;
with AML_Table_Backing;
procedure Format_Capture_Tests is
   package N is new AML_Namespace (8, Perform_Delay => AML_Delays.Unavailable_Provider);
   use N; use N.Owned;
   use type AML_Decode.Integer_Value;
   A : Arena;
   Input : aliased AML_Table_Backing.State (1, 1);
   Checks : Natural := 0;
   OK : Boolean;
   Loaded_Status : Load_Status;
   S : Execution_Status;
   R : Execution_Result;
   Result_Value, Cell_Value : Datum;
   procedure Check (Good : Boolean) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Checks'Image; end if;
   end Check;
   function Definitions (Code : Bytes) return Bytes is
     ([16#08#,83,84,82,48,16#0D#,65,66,67,0,
       16#14#,Byte (6 + Code'Length),84,69,83,84,0] & Code);
   procedure Prepare (Code : Bytes; Width : Integer_Width) is
   begin
      Reset (A, OK); Check (OK);
      Load (A, Definitions (Code), Width, Loaded_Status);
      Check (Loaded_Status = Loaded);
   end Prepare;
   procedure Fill (Width : Integer_Width) is
   begin
      -- One slot remains for the initial Store of STR0 into Local0.
      while Values_Used (A).Objects < AML_Objects.Max_Objects - 1 loop
         To_Buffer_And_Attach (A, Width, (Integer_Datum, 1, Ordinary_Integer),
           (Kind => Detached_Result), Result_Value, Cell_Value, S);
         Check (S = Returned);
      end loop;
   end Fill;
begin
   for Op in Byte range 16#97# .. 16#98# loop
      for Width in Integer_Width loop
         for Frame_Target in Boolean loop
            declare
               Target : constant Bytes :=
                 (if Frame_Target then Bytes'(16#71#,16#60#) else Bytes'(1 => 16#60#));
               Code : constant Bytes :=
                 [16#70#,83,84,82,48,16#60#,Op,16#60#] & Target & [16#A4#,16#87#,16#60#];
            begin
               Prepare (Code, Width); Fill (Width);
               Invoke (A, Input, 2, [others => <>], 0, 100, R);
               Check (R.Status = Returned and then R.Value = 3);
               Check (Values_Used (A).Objects = AML_Objects.Max_Objects);
            end;
         end loop;
         -- A distinct Local destination must copy a borrowed String, so the
         -- full pool rejects it rather than silently sharing the source.
         Prepare ([16#70#,83,84,82,48,16#60#,Op,16#60#,16#61#,16#A4#,0], Width);
         Fill (Width);
         Invoke (A, Input, 2, [others => <>], 0, 100, R);
         Check (R.Status = Value_Limit);
         Check (Values_Used (A).Objects = AML_Objects.Max_Objects);
         -- Direct mutation of the returned conversion value must leave a
         -- borrowed String's Local capture independent, but mutate fresh capture.
         for Fresh in Boolean loop
            declare
               Source : constant Bytes :=
                 (if Fresh then Bytes'(16#0A#,65) else Bytes'(83,84,82,48));
               Code : constant Bytes := [16#70#,16#0A#,16#5A#,16#88#,Op] & Source &
                 [16#60#,0,0,16#A4#,16#83#,16#88#,16#60#,0,0];
            begin
               Prepare (Code, Width);
               Invoke (A, Input, 2, [others => <>], 0, 100, R);
               Check (R.Status = Returned and then R.Value = (if Fresh then 16#5A# else 65));
            end;
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Formatting capture checks" & Checks'Image);
end Format_Capture_Tests;
