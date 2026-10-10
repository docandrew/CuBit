with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Objects;
with AML_References;
with AML_Table_Backing;
with Namespace_Instance;
procedure Typed_Tests is
   package NS renames Namespace_Instance;
   use type NS.Load_Status;
   use type NS.State;
   use type NS.Object_Kind;
   use type Byte;
   use type Integer_Value;
   use type AML_Objects.Object_Kind;
   use type AML_References.Reference;
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
                   Pick : Byte := 16#68#; Copies_Compound : Boolean := False) is
      Definitions : constant Bytes :=
        [8] & Enc ("STR0") & [16#0D#,49,70,0] &
        [8] & Enc ("BUF0") & [16#11#,5,16#0A#,2,16#34#,16#12#] &
        [8] & Enc ("EMP0") & [16#11#,2,0] &
        [8] & Enc ("PKG0") & [16#12#,4,2,1,0];
      A : NS.Owned.Arena;
      Input : aliased AML_Table_Backing.State (1, 1);
      Tree, Before : NS.State;
      Ready : Boolean;
      Report : NS.Initialization_Report;
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
      procedure Prepare (W : Integer_Width) is
      begin
         NS.Owned.Reset (A, Ready); Check (Ready);
         NS.Owned.Load (A, Data, W, Loaded); Check (Loaded = NS.Loaded);
         NS.Owned.Initialize_Members (A, Report);
         Check (Report.Missing = 0 and Report.Unsupported = 0);
         Tree := NS.Owned.Snapshot (A); Before := Tree;
         ID := NS.Data_Object (Tree, NS.Child (Tree, 0, Object_Name));
      end Prepare;
      procedure Invoke (Fuel : Natural) is
      begin
         NS.Owned.Invoke (A, Input, NS.Child (Before, 0, "READ"),
           [others => (Integer_Datum, 0, AML_Decode.Ordinary_Integer)], 0, Fuel, R);
         Tree := NS.Owned.Snapshot (A);
      end Invoke;
      function Sources_Preserved return Boolean is
         use AML_Objects;
         Current : constant AML_Objects.State := NS.Value_Store (Tree);
         Prior : constant AML_Objects.State := NS.Value_Store (Before);
         function Same_Contents (Original, Copied : Object_ID; Remaining : Natural) return Boolean is
         begin
            if Original = 0 or else Copied = 0 then return Original = Copied; end if;
            if Remaining = 0 or else not Is_Live (Prior, Original)
              or else not Is_Live (Current, Copied) or else Kind (Current, Copied) /= Kind (Prior, Original)
              or else Length (Current, Copied) /= Length (Prior, Original)
            then return False; end if;
            case Kind (Prior, Original) is
               when String_Object | Buffer_Object =>
                  return Byte_Data (Current, Copied) = Byte_Data (Prior, Original);
               when Integer_Object =>
                  return Integer_Data (Current, Copied) = Integer_Data (Prior, Original)
                    and then Origin_Of (Current, Copied) = Origin_Of (Prior, Original);
               when Reference_Object =>
                  return Reference_Data (Current, Copied) = Reference_Data (Prior, Original);
               when Package_Object =>
                  for E in 0 .. Length (Prior, Original) - 1 loop
                     if not Same_Contents (Element (Prior, Original, E),
                       Element (Current, Copied, E), Remaining - 1)
                     then return False; end if;
                  end loop;
                  return True;
            end case;
         end Same_Contents;
      begin
         if Live_Count (Current) = Live_Count (Prior) then
            return Tree = Before;
         end if;
         -- Capturing a compound value consumes new storage, even if a later
         -- expression exhausts fuel. Every original node/object must survive.
         if Live_Count (Current) < Live_Count (Prior) or else NS.Count (Tree) /= NS.Count (Before)
           or else NS.Method_Usage (Tree) /= NS.Method_Usage (Before)
           or else Byte_Count (Current) < Byte_Count (Prior)
           or else Element_Count (Current) < Element_Count (Prior)
         then return False; end if;
         for N in 1 .. NS.Count (Before) loop
            if NS.Present (Tree, N) /= NS.Present (Before, N)
              or else NS.Name (Tree, N) /= NS.Name (Before, N)
              or else NS.Parent (Tree, N) /= NS.Parent (Before, N)
              or else NS.Kind (Tree, N) /= NS.Kind (Before, N)
            then return False; end if;
            if N <= 4 then
               if NS.Data_Object (Tree, N) /= NS.Data_Object (Before, N) then return False; end if;
            elsif NS.Method_Data (Tree, N) /= NS.Method_Data (Before, N) then return False;
            end if;
         end loop;
         for O in 1 .. Slot_Bound (Prior) loop
            if Is_Live (Prior, O) then
            if not Is_Live (Current, O) or else Kind (Current, O) /= Kind (Prior, O) or else Length (Current, O) /= Length (Prior, O)
            then return False; end if;
            case Kind (Prior, O) is
               when String_Object | Buffer_Object =>
                  if Byte_Data (Current, O) /= Byte_Data (Prior, O) then return False; end if;
               when Integer_Object =>
                  if Integer_Data (Current, O) /= Integer_Data (Prior, O)
                    or else Origin_Of (Current, O) /= Origin_Of (Prior, O) then return False; end if;
               when Reference_Object =>
                  if Reference_Data (Current, O) /= Reference_Data (Prior, O) then return False; end if;
               when Package_Object =>
                  for E in 0 .. Length (Prior, O) - 1 loop
                     if Element (Current, O, E) /= Element (Prior, O, E) then return False; end if;
                  end loop;
            end case;
            end if;
         end loop;
         for O in 1 .. Slot_Bound (Current) loop
            if Is_Live (Current, O) and then not Is_Live (Prior, O) then
            -- Every allocation must be a content-preserving copy of an
            -- original object; package leaves are compared recursively.
            if not (for some Source in 1 .. Slot_Bound (Prior) =>
              Is_Live (Prior, Source) and then Same_Contents (Source, O, Live_Count (Prior))) then return False; end if;
            end if;
         end loop;
         return True;
      end Sources_Preserved;
   begin
      for W in Integer_Width loop
         Prepare (W);
         Invoke (100);
         Check (R.Status = Expected);
         if R.Status = Object_Returned then
            Check (if Copies_Compound then R.Object.ID /= ID else R.Object.ID = ID);
            if Copies_Compound and Object_Name /= "PKG0" then
               Check (AML_Objects.Byte_Data (NS.Value_Store (Tree), R.Object.ID) =
                      AML_Objects.Byte_Data (NS.Value_Store (Before), ID));
            end if;
            Check (R.Object.Size = AML_Objects.Length (NS.Value_Store (Tree), ID));
            Check (R.Object.Type_Code = (case AML_Objects.Kind (NS.Value_Store (Tree), ID) is
              when AML_Objects.String_Object => 2, when AML_Objects.Buffer_Object => 3,
              when AML_Objects.Package_Object => 4, when others => 1));
         elsif R.Status = Returned then
            Check (R.Value = Number);
         end if;
         Check (Sources_Preserved);
         -- Every insufficient budget must fail without publishing an object.
         declare
            Used : constant Natural := R.Charged;
         begin
            for Fuel in 0 .. Used - 1 loop
               Prepare (W);
               Invoke (Fuel);
               Check (R.Status = Budget_Exceeded and R.Charged <= Fuel);
               Check (Sources_Preserved);
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
         Test (Name, [16#70#] & Enc (Name) & [16#60#] & Enc ("REPL") & [16#60#,16#A4#,16#60#], Copies_Compound => True);
         Test (Name, [16#A4#] & Enc ("CALI") & Enc (Name));
         Test (Name, [16#A4#] & Enc ("CALLCALI") & Enc (Name), Copies_Compound => True);
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
            Test (Name, [16#70#] & Enc (Name) & [Local,16#A4#,Local], Copies_Compound => True);
            Test (Name, [16#70#] & Enc (Name) & [Local,16#A4#,16#8E#,Local], Returned, Type_Code);
            Test (Name, [16#70#] & Enc (Name) & [Local,16#A4#,16#87#,Local], Returned, Size);
            Test (Name, [16#70#] & Enc (Name) & [Local,16#70#,1,Local,16#A4#,Local], Returned, 1);
            Test (Name, [16#70#,1,Local,16#70#] & Enc (Name) & [Local,16#A4#,Local], Copies_Compound => True);
         end loop;
      end;
   end loop;
   Test ("STR0", [16#72#,16#70#] & Enc ("STR0") & [16#68#,1,0,16#A4#,16#8E#,16#68#], Returned, 2);
   Test ("STR0", [16#A4#,16#72#] & Enc ("CALLSTR0") & [1,0], Returned, 32);
   Test ("BUF0", [16#A4#] & Enc ("MATHBUF0"), Returned, 16#1235#);
   Test ("EMP0", [16#A4#] & Enc ("MATHEMP0"), Empty_Buffer);
   Test ("PKG0", [16#A4#] & Enc ("MATHPKG0"), Unsupported_Value);
   Test ("PKG0", [16#70#] & Enc ("PKG0") & [16#60#] & Enc ("CALLBUF0") & [16#A4#,16#60#], Copies_Compound => True);
   Test ("PKG0", [16#A4#,16#5C#,0], Unsupported_Value);
   Ada.Text_IO.Put_Line ("AML-TYPED-CHECK: PASS" & Checks'Image);
end Typed_Tests;
