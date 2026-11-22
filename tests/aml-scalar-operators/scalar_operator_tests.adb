with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Delays;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_Table_Backing;
procedure Scalar_Operator_Tests is
   use type Bytes;
   use type Integer_Value;
   package N is new AML_Namespace (32, AML_Delays.Unavailable_Provider);
   use N; use N.Owned;
   A : Arena;
   Input : aliased AML_Table_Backing.State (1, 32);
   Checks : Natural := 0;
   function Text (Value : String) return Bytes is
      Result : Bytes (1 .. Value'Length);
   begin
      for I in Result'Range loop Result (I) := Character'Pos (Value (Value'First + I - 1)); end loop;
      return Result;
   end Text;
   function Method (Name : String; Args : Byte; Code : Bytes) return Bytes is
     ([16#14#, Byte (Code'Length + 6)] & Text (Name) & [Args] & Code);
   Data : constant Bytes :=
     Method ("IDEN", 1, [16#A4#, 16#68#]) &
     Method ("STEX", 0, [16#A4#, 16#70#] & Text ("IDEN") & [16#0A#, 9, 16#60#]) &
     Method ("NOOP", 0, [16#A4#, 16#99#] & Text ("IDEN") & [16#0A#, 9, 16#60#]) &
     Method ("COPY", 0, [16#A4#, 16#9D#] & Text ("IDEN") & [16#0A#, 9, 16#60#]) &
     Method ("REAL", 0, [16#A4#, 16#99#, 16#0D#, 16#39#, 0, 16#60#]) &
     Method ("REFS", 0, [16#70#, 0, 16#60#, 16#A4#, 16#70#] & Text ("IDEN") &
       [16#0A#, 9, 16#71#, 16#60#]) &
     Method ("NEST", 0, [16#A4#, 16#72#, 16#99#] & Text ("IDEN") &
       [16#0A#, 9, 16#60#, 16#9D#] & Text ("IDEN") & [16#0A#, 7, 16#61#, 0]) &
     Method ("TARG", 0, [16#70#, 16#11#, 4, 1, 0, 0, 16#60#,
       16#A4#, 16#99#] & Text ("IDEN") & [16#0A#, 9, 16#88#, 16#60#] & Text ("IDEN") & [0, 0]) &
     Method ("NLOC", 0, [16#70#, 16#0A#, 9, 16#60#, 16#A4#, 16#99#, 16#60#, 16#61#]) &
     Method ("NARG", 1, [16#A4#, 16#99#, 16#68#, 16#60#]) &
     Method ("MSTA", 0, [16#70#, 0, 16#60#, 16#70#, 16#60#, 16#88#] & Text ("BUF0") &
       [16#70#, 1, 16#60#, 0, 16#A4#, 16#83#, 16#88#] & Text ("BUF0") & [1, 0]) &
     Method ("MCON", 0, [16#70#, 0, 16#60#, 16#99#, 16#60#, 16#88#] & Text ("BUF0") &
       [16#70#, 1, 16#60#, 0, 16#A4#, 16#83#, 16#88#] & Text ("BUF0") & [1, 0]) &
     Method ("MADD", 0, [16#70#, 0, 16#60#, 16#A4#, 16#72#, 16#60#, 16#70#, 1, 16#60#, 0]) &
     [16#08#] & Text ("BUF0") & [16#11#, 5, 16#0A#, 2, 0, 0];
   procedure Check (Good : Boolean; Detail : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Detail; end if;
   end Check;
begin
   for Width in Integer_Width loop
      declare OK : Boolean; Loaded_Status : Load_Status; R, Short : Execution_Result; begin
         Reset (A, OK); Check (OK, "reset");
         Load (A, Data, Width, Loaded_Status); Check (Loaded_Status = Loaded, "load");
         for ID in Node_ID range 2 .. 13 loop
            if ID >= 11 then
               Reset (A, OK); Check (OK, "isolated timing reset");
               Load (A, Data, Width, Loaded_Status); Check (Loaded_Status = Loaded, "isolated timing load");
            end if;
            Invoke (A, Input, ID, [0 => (Integer_Datum, 9, Ordinary_Integer), others => (Integer_Datum, 0, Ordinary_Integer)],
              (if ID = 10 then 1 else 0), 1000, R);
            Check (R.Status = Returned and then R.Value = (if ID = 7 then 16 elsif ID in 11 .. 12 then 1 elsif ID = 13 then 2 else 9),
              Width'Image & ID'Image & R.Status'Image);
            Check (Frame_Root_Count (A) = 0, "frame cleanup");
            if ID >= 11 then
               Invoke (A, Input, ID, [others => (Integer_Datum, 0, Ordinary_Integer)], 0, 1000, Short);
               Check (Short.Status = Returned and then Short.Value = (if ID = 13 then 2 else 1), "timing repeated");
               Check (Frame_Root_Count (A) = 0, "repeated cleanup");
            end if;
            if R.Status = Returned then
               Invoke (A, Input, ID, [0 => (Integer_Datum, 9, Ordinary_Integer), others => (Integer_Datum, 0, Ordinary_Integer)],
              (if ID = 10 then 1 else 0), R.Charged - 1, Short);
               Check (Short.Status = Budget_Exceeded, "short fuel");
               Check (Frame_Root_Count (A) = 0, "failure cleanup");
            end if;
         end loop;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("AML-SCALAR-OPERATOR-CHECK: PASS" & Checks'Image);
end Scalar_Operator_Tests;
