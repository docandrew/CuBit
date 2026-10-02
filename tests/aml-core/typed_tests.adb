with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Objects;
with Namespace_Instance;
procedure Typed_Tests is
   package NS renames Namespace_Instance;
   use type NS.Load_Status;
   use type NS.State;
   use type Byte;
   use type Integer_Value;
   use type AML_Objects.Object_Kind;
   Checks : Natural := 0;
   function Enc (Text : String) return Bytes is
      Result : Bytes (1 .. Text'Length);
   begin
      for I in Result'Range loop Result (I) := Character'Pos (Text (Text'First + (I - 1))); end loop;
      return Result;
   end Enc;
   function Method_Data (Name : String; Body_Data : Bytes; Count : Byte := 0) return Bytes is
     ([16#14#, Byte (6 + Body_Data'Length)] & Enc (Name) & [Count] & Body_Data);
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Checks'Image; end if;
   end Check;
   procedure Test (Object_Name : String; Body_Data : Bytes;
                   Expected : Execution_Status := Object_Returned; Number : Integer_Value := 0;
                   Pick : Byte := 16#68#) is
      Definitions : constant Bytes :=
        [8] & Enc ("STR0") & [16#0D#,49,70,0] &
        [8] & Enc ("BUF0") & [16#11#,5,16#0A#,2,16#34#,16#12#] &
        [8] & Enc ("EMP0") & [16#11#,2,0] &
        [8] & Enc ("PKG0") & [16#12#,4,2,1,0];
      Tree, Before : NS.State;
      Loaded : NS.Load_Status;
      R : Execution_Result;
      ID : AML_Objects.Object_ID;
      Data : constant Bytes := Definitions &
        Method_Data ("CALI", [16#A4#,16#68#], 1) &
        Method_Data ("CALL", [16#70#,16#68#,16#60#,16#A4#,16#60#], 1) &
        Method_Data ("TYPE", [16#A4#,16#8E#,16#68#], 1) &
        Method_Data ("SIZE", [16#A4#,16#87#,16#68#], 1) &
        Method_Data ("MATH", [16#A4#,16#72#,16#68#,1,0], 1) &
        Method_Data ("PICK", [16#A4#,Pick], 7) &
        Method_Data ("REPL", [16#70#,1,16#68#,16#A4#,16#68#], 1) &
        Method_Data ("READ", Body_Data);
   begin
      for W in Integer_Width loop
         Tree := NS.Empty;
         NS.Load_Names (Tree, Data, W, Loaded);
         Check (Loaded = NS.Loaded);
         Before := Tree;
         ID := NS.Data_Object (Tree, NS.Child (Tree, 0, Object_Name));
         R := NS.Invoke (Tree, NS.Child (Tree, 0, "READ"), [others => 0], 0, 100);
         Check (R.Status = Expected);
         if R.Status = Object_Returned then
            Check (R.Object.ID = ID);
            Check (R.Object.Size = AML_Objects.Length (NS.Value_Store (Tree), ID));
            Check (R.Object.Type_Code = (case AML_Objects.Kind (NS.Value_Store (Tree), ID) is
              when AML_Objects.String_Object => 2, when AML_Objects.Buffer_Object => 3,
              when AML_Objects.Package_Object => 4, when others => 1));
         elsif R.Status = Returned then
            Check (R.Value = Number);
         end if;
         Check (Tree = Before);
         -- Every insufficient budget must fail without publishing an object.
         declare
            Used : constant Natural := R.Charged;
         begin
            for Fuel in 0 .. Used - 1 loop
               R := NS.Invoke (Tree, NS.Child (Tree, 0, "READ"), [others => 0], 0, Fuel);
               Check (R.Status = Budget_Exceeded and R.Charged <= Fuel);
               Check (Tree = Before);
            end loop;
         end;
      end loop;
   end Test;
begin
   for Kind in 1 .. 4 loop
      declare
         Name : constant String := (case Kind is
           when 1 => "STR0", when 2 => "BUF0", when 3 => "PKG0", when others => "EMP0");
         Type_Code : constant Integer_Value := (case Kind is when 1 => 2, when 3 => 4, when others => 3);
         Size : constant Integer_Value := (if Kind = 4 then 0 else 2);
      begin
         Test (Name, [16#A4#] & Enc (Name));
         Test (Name, [16#A4#,16#70#] & Enc (Name) & [16#68#]);
         Test (Name, [16#70#] & Enc (Name) & [16#60#] & Enc ("REPL") & [16#60#,16#A4#,16#60#]);
         Test (Name, [16#A4#] & Enc ("CALI") & Enc (Name));
         Test (Name, [16#A4#] & Enc ("CALLCALI") & Enc (Name));
         Test (Name, [16#A4#] & Enc ("TYPE") & Enc (Name), Returned, Type_Code);
         Test (Name, [16#A4#] & Enc ("SIZE") & Enc (Name), Returned, Size);
         for Argument in 0 .. 6 loop
            declare
               Actuals : Bytes (1 .. 10);
               Used : Natural := 0;
            begin
               for I in 0 .. 6 loop
                  if I = Argument then
                     Actuals (Used + 1 .. Used + 4) := Enc (Name);
                     Used := Used + 4;
                  else
                     Used := Used + 1;
                     Actuals (Used) := 1;
                  end if;
               end loop;
               Test (Name, [16#A4#] & Enc ("PICK") & Actuals,
                     Pick => Byte (16#68# + Argument));
            end;
         end loop;
         for Local in Byte range 16#60# .. 16#6E# loop
            Test (Name, [16#70#] & Enc (Name) & [Local,16#A4#,Local]);
            Test (Name, [16#70#] & Enc (Name) & [Local,16#A4#,16#8E#,Local], Returned, Type_Code);
            Test (Name, [16#70#] & Enc (Name) & [Local,16#A4#,16#87#,Local], Returned, Size);
            Test (Name, [16#70#] & Enc (Name) & [Local,16#70#,1,Local,16#A4#,Local], Returned, 1);
            Test (Name, [16#70#,1,Local,16#70#] & Enc (Name) & [Local,16#A4#,Local]);
         end loop;
      end;
   end loop;
   Test ("STR0", [16#72#,16#70#] & Enc ("STR0") & [16#68#,1,0,16#A4#,16#8E#,16#68#], Returned, 2);
   Test ("STR0", [16#A4#,16#72#] & Enc ("CALLSTR0") & [1,0], Returned, 32);
   Test ("BUF0", [16#A4#] & Enc ("MATHBUF0"), Returned, 16#1235#);
   Test ("EMP0", [16#A4#] & Enc ("MATHEMP0"), Empty_Buffer);
   Test ("PKG0", [16#A4#] & Enc ("MATHPKG0"), Unsupported_Value);
   Test ("PKG0", [16#70#] & Enc ("PKG0") & [16#60#] & Enc ("CALLBUF0") & [16#A4#,16#60#]);
   Test ("PKG0", [16#A4#,16#5C#,0], Unsupported_Value);
   Ada.Text_IO.Put_Line ("AML-TYPED-CHECK: PASS" & Checks'Image);
end Typed_Tests;
