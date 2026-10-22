pragma Ada_2022;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Delays;
with AML_Execute;
with AML_Namespace;
with AML_Table_Backing;
procedure Source_Buffer_Replay is
   package N is new AML_Namespace (64, AML_Delays.Unavailable_Provider);
   package V is new N.Owned.Collecting;
   use type AML_Decode.Integer_Value;
   use type N.Load_Status;
   use type N.Lookup_Status;
   use type V.Access_Status;
   use type V.Value_Kind;
   use type AML_Execute.Execution_Status;
   type Expected_Kind is (Scalar, Text, Overflow, Type_Failure, Package_Failure, Buffer_Failure);
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Message : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Message; end if;
   end Check;
   procedure Run_Case (Case_ID : Positive; Code : Bytes; Width : Integer_Width) is
      A : V.Arena;
      Input : aliased AML_Table_Backing.State (1, 1);
      Access_Result : V.Access_Status;
      Loaded : N.Load_Status;
      Init : N.Initialization_Report;
      procedure Invoke (Name : String; Kind : Expected_Kind;
         Number : Integer_Value := 0; Expected_Text : String := "") is
         Found : N.Lookup_Result;
         Result : V.Result;
         Description : V.Value_Description;
         Actual : Bytes (7 .. 6 + Expected_Text'Length);
         Copied : Natural;
         Label_Text : constant String := Case_ID'Image & " " & Name;
      begin
         V.Find_Object (A, N.Root, Name, Found, Access_Result);
         Check (Access_Result = V.Available and then Found.Status = N.Found, "find" & Label_Text);
         V.Invoke (A, Input, Found.Node, [others => (V.Immediate_Argument, 0)],
           0, 20_000, Result, Access_Result);
         Check (Access_Result = V.Available, "access" & Label_Text);
         case Kind is
            when Scalar =>
               Check (Result.Status = AML_Execute.Returned, "scalar status" & Label_Text);
               Check (Result.Number = Number, "scalar value" & Label_Text);
            when Overflow =>
               Check (Result.Status = AML_Execute.Numeric_Overflow, "overflow" & Label_Text);
            when Type_Failure =>
               Check (Result.Status = AML_Execute.Unsupported_Value, "operand type" & Label_Text);
            when Package_Failure =>
               Check (Result.Status = AML_Execute.Package_Limit, "package limit" & Label_Text);
            when Buffer_Failure =>
               Check (Result.Status = AML_Execute.Buffer_Field_Limit, "buffer limit" & Label_Text);
            when Text =>
               Check (Result.Status = AML_Execute.Object_Returned, "string status" & Label_Text);
               V.Describe (A, Result.Handle, Description, Access_Result);
               Check (Access_Result = V.Available and then Description.Kind = V.String_Description
                 and then Description.Length = Expected_Text'Length, "string shape" & Label_Text);
               V.Read_Bytes (A, Result.Handle, 0, Actual, Copied, Access_Result);
               Check (Access_Result = V.Available and then Copied = Expected_Text'Length,
                 "complete string" & Label_Text);
               for I in Expected_Text'Range loop
                  Check (Natural (Actual (Actual'First + (I - Expected_Text'First))) =
                    Character'Pos (Expected_Text (I)), "string byte" & Label_Text);
               end loop;
               V.Release (A, Result.Handle, Access_Result);
               Check (Access_Result = V.Available, "release" & Label_Text);
         end case;
         Check (V.Retained_Count (A) = 0, "pins" & Label_Text);
         pragma Assert (V.Module_Transient_Free (A));
      end Invoke;
   begin
      V.Initialize (A, Access_Result); Check (Access_Result = V.Available, "initialize");
      V.Load (A, Code, Width, Loaded, Access_Result);
      Check (Access_Result = V.Available and then Loaded = N.Loaded, "load" & Case_ID'Image);
      V.Seal (A, Init, Access_Result); Check (Access_Result = V.Available, "seal");
      case Case_ID is
         when 1 =>
            Invoke ("PRE0", Scalar, 16#0000000000332211#);
            Invoke ("TEST", Scalar, 16#0000000000000044#);
            Invoke ("POST", Scalar, 16#0000000000665544#);
            Invoke ("FLAG", Scalar, 16#0000000000000001#);
         when 2 =>
            Invoke ("PRE0", Scalar, 16#0000000000332211#);
            Invoke ("TEST", Scalar, 16#0000000000000011#);
            Invoke ("POST", Scalar, 16#0000000000665544#);
            Invoke ("FLAG", Scalar, 16#0000000000000001#);
         when 3 =>
            Invoke ("PRE0", Scalar, 16#0000000000332211#);
            Invoke ("TEST", Scalar, 16#0000000000000011#);
            Invoke ("POST", Scalar, 16#0000000000332211#);
            Invoke ("FLAG", Scalar, 16#0000000000000001#);
         when 4 =>
            Invoke ("PRE0", Scalar, 16#0000000000332211#);
            Invoke ("TEST", Scalar, 16#0000000000000044#);
            Invoke ("POST", Scalar, 16#0000000000665544#);
            Invoke ("FLAG", Scalar, 16#0000000000000001#);
         when 5 =>
            Invoke ("PRE0", Scalar, 16#0000000000332211#);
            Invoke ("TEST", Scalar, 16#0000000000000011#);
            Invoke ("POST", Scalar, 16#0000000000665544#);
            Invoke ("FLAG", Scalar, 16#0000000000000001#);
         when 6 =>
            Invoke ("PRE0", Scalar, 16#0000000000332211#);
            Invoke ("TEST", Scalar, 16#0000000000000011#);
            Invoke ("POST", Scalar, 16#0000000000332211#);
            Invoke ("FLAG", Scalar, 16#0000000000000001#);
         when others => raise Program_Error with "unregistered oracle case";
      end case;
      V.Reset (A, Access_Result); Check (Access_Result = V.Available, "reset");
   end Run_Case;
begin
   -- r1-store
   Run_Case (1, [16#08#, 16#46#, 16#4C#, 16#47#, 16#30#, 16#00#, 16#08#, 16#42#, 16#55#, 16#46#, 16#30#, 16#11#, 16#06#, 16#0A#, 16#03#, 16#11#, 16#22#, 16#33#, 16#14#, 16#1F#, 16#49#, 16#44#, 16#58#, 16#48#, 16#00#, 16#70#, 16#01#, 16#46#, 16#4C#, 16#47#, 16#30#, 16#70#, 16#0A#, 16#A1#, 16#5B#, 16#31#, 16#70#, 16#11#, 16#06#, 16#0A#, 16#03#, 16#44#, 16#55#, 16#66#, 16#42#, 16#55#, 16#46#, 16#30#, 16#A4#, 16#00#, 16#14#, 16#0D#, 16#50#, 16#52#, 16#45#, 16#30#, 16#00#, 16#A4#, 16#99#, 16#42#, 16#55#, 16#46#, 16#30#, 16#00#, 16#14#, 16#1D#, 16#54#, 16#45#, 16#53#, 16#54#, 16#00#, 16#5B#, 16#13#, 16#42#, 16#55#, 16#46#, 16#30#, 16#49#, 16#44#, 16#58#, 16#48#, 16#0A#, 16#08#, 16#46#, 16#4C#, 16#44#, 16#30#, 16#A4#, 16#99#, 16#46#, 16#4C#, 16#44#, 16#30#, 16#00#, 16#14#, 16#0D#, 16#50#, 16#4F#, 16#53#, 16#54#, 16#00#, 16#A4#, 16#99#, 16#42#, 16#55#, 16#46#, 16#30#, 16#00#, 16#14#, 16#0B#, 16#46#, 16#4C#, 16#41#, 16#47#, 16#00#, 16#A4#, 16#46#, 16#4C#, 16#47#, 16#30#], Bits_32);
   -- r1-copyobject
   Run_Case (2, [16#08#, 16#46#, 16#4C#, 16#47#, 16#30#, 16#00#, 16#08#, 16#42#, 16#55#, 16#46#, 16#30#, 16#11#, 16#06#, 16#0A#, 16#03#, 16#11#, 16#22#, 16#33#, 16#14#, 16#1F#, 16#49#, 16#44#, 16#58#, 16#48#, 16#00#, 16#70#, 16#01#, 16#46#, 16#4C#, 16#47#, 16#30#, 16#70#, 16#0A#, 16#A1#, 16#5B#, 16#31#, 16#9D#, 16#11#, 16#06#, 16#0A#, 16#03#, 16#44#, 16#55#, 16#66#, 16#42#, 16#55#, 16#46#, 16#30#, 16#A4#, 16#00#, 16#14#, 16#0D#, 16#50#, 16#52#, 16#45#, 16#30#, 16#00#, 16#A4#, 16#99#, 16#42#, 16#55#, 16#46#, 16#30#, 16#00#, 16#14#, 16#1D#, 16#54#, 16#45#, 16#53#, 16#54#, 16#00#, 16#5B#, 16#13#, 16#42#, 16#55#, 16#46#, 16#30#, 16#49#, 16#44#, 16#58#, 16#48#, 16#0A#, 16#08#, 16#46#, 16#4C#, 16#44#, 16#30#, 16#A4#, 16#99#, 16#46#, 16#4C#, 16#44#, 16#30#, 16#00#, 16#14#, 16#0D#, 16#50#, 16#4F#, 16#53#, 16#54#, 16#00#, 16#A4#, 16#99#, 16#42#, 16#55#, 16#46#, 16#30#, 16#00#, 16#14#, 16#0B#, 16#46#, 16#4C#, 16#41#, 16#47#, 16#00#, 16#A4#, 16#46#, 16#4C#, 16#47#, 16#30#], Bits_32);
   -- r1-control
   Run_Case (3, [16#08#, 16#46#, 16#4C#, 16#47#, 16#30#, 16#00#, 16#08#, 16#42#, 16#55#, 16#46#, 16#30#, 16#11#, 16#06#, 16#0A#, 16#03#, 16#11#, 16#22#, 16#33#, 16#14#, 16#13#, 16#49#, 16#44#, 16#58#, 16#48#, 16#00#, 16#70#, 16#01#, 16#46#, 16#4C#, 16#47#, 16#30#, 16#70#, 16#0A#, 16#A1#, 16#5B#, 16#31#, 16#A4#, 16#00#, 16#14#, 16#0D#, 16#50#, 16#52#, 16#45#, 16#30#, 16#00#, 16#A4#, 16#99#, 16#42#, 16#55#, 16#46#, 16#30#, 16#00#, 16#14#, 16#1D#, 16#54#, 16#45#, 16#53#, 16#54#, 16#00#, 16#5B#, 16#13#, 16#42#, 16#55#, 16#46#, 16#30#, 16#49#, 16#44#, 16#58#, 16#48#, 16#0A#, 16#08#, 16#46#, 16#4C#, 16#44#, 16#30#, 16#A4#, 16#99#, 16#46#, 16#4C#, 16#44#, 16#30#, 16#00#, 16#14#, 16#0D#, 16#50#, 16#4F#, 16#53#, 16#54#, 16#00#, 16#A4#, 16#99#, 16#42#, 16#55#, 16#46#, 16#30#, 16#00#, 16#14#, 16#0B#, 16#46#, 16#4C#, 16#41#, 16#47#, 16#00#, 16#A4#, 16#46#, 16#4C#, 16#47#, 16#30#], Bits_32);
   -- r2-store
   Run_Case (4, [16#08#, 16#46#, 16#4C#, 16#47#, 16#30#, 16#00#, 16#08#, 16#42#, 16#55#, 16#46#, 16#30#, 16#11#, 16#06#, 16#0A#, 16#03#, 16#11#, 16#22#, 16#33#, 16#14#, 16#1F#, 16#49#, 16#44#, 16#58#, 16#48#, 16#00#, 16#70#, 16#01#, 16#46#, 16#4C#, 16#47#, 16#30#, 16#70#, 16#0A#, 16#A1#, 16#5B#, 16#31#, 16#70#, 16#11#, 16#06#, 16#0A#, 16#03#, 16#44#, 16#55#, 16#66#, 16#42#, 16#55#, 16#46#, 16#30#, 16#A4#, 16#00#, 16#14#, 16#0D#, 16#50#, 16#52#, 16#45#, 16#30#, 16#00#, 16#A4#, 16#99#, 16#42#, 16#55#, 16#46#, 16#30#, 16#00#, 16#14#, 16#1D#, 16#54#, 16#45#, 16#53#, 16#54#, 16#00#, 16#5B#, 16#13#, 16#42#, 16#55#, 16#46#, 16#30#, 16#49#, 16#44#, 16#58#, 16#48#, 16#0A#, 16#08#, 16#46#, 16#4C#, 16#44#, 16#30#, 16#A4#, 16#99#, 16#46#, 16#4C#, 16#44#, 16#30#, 16#00#, 16#14#, 16#0D#, 16#50#, 16#4F#, 16#53#, 16#54#, 16#00#, 16#A4#, 16#99#, 16#42#, 16#55#, 16#46#, 16#30#, 16#00#, 16#14#, 16#0B#, 16#46#, 16#4C#, 16#41#, 16#47#, 16#00#, 16#A4#, 16#46#, 16#4C#, 16#47#, 16#30#], Bits_64);
   -- r2-copyobject
   Run_Case (5, [16#08#, 16#46#, 16#4C#, 16#47#, 16#30#, 16#00#, 16#08#, 16#42#, 16#55#, 16#46#, 16#30#, 16#11#, 16#06#, 16#0A#, 16#03#, 16#11#, 16#22#, 16#33#, 16#14#, 16#1F#, 16#49#, 16#44#, 16#58#, 16#48#, 16#00#, 16#70#, 16#01#, 16#46#, 16#4C#, 16#47#, 16#30#, 16#70#, 16#0A#, 16#A1#, 16#5B#, 16#31#, 16#9D#, 16#11#, 16#06#, 16#0A#, 16#03#, 16#44#, 16#55#, 16#66#, 16#42#, 16#55#, 16#46#, 16#30#, 16#A4#, 16#00#, 16#14#, 16#0D#, 16#50#, 16#52#, 16#45#, 16#30#, 16#00#, 16#A4#, 16#99#, 16#42#, 16#55#, 16#46#, 16#30#, 16#00#, 16#14#, 16#1D#, 16#54#, 16#45#, 16#53#, 16#54#, 16#00#, 16#5B#, 16#13#, 16#42#, 16#55#, 16#46#, 16#30#, 16#49#, 16#44#, 16#58#, 16#48#, 16#0A#, 16#08#, 16#46#, 16#4C#, 16#44#, 16#30#, 16#A4#, 16#99#, 16#46#, 16#4C#, 16#44#, 16#30#, 16#00#, 16#14#, 16#0D#, 16#50#, 16#4F#, 16#53#, 16#54#, 16#00#, 16#A4#, 16#99#, 16#42#, 16#55#, 16#46#, 16#30#, 16#00#, 16#14#, 16#0B#, 16#46#, 16#4C#, 16#41#, 16#47#, 16#00#, 16#A4#, 16#46#, 16#4C#, 16#47#, 16#30#], Bits_64);
   -- r2-control
   Run_Case (6, [16#08#, 16#46#, 16#4C#, 16#47#, 16#30#, 16#00#, 16#08#, 16#42#, 16#55#, 16#46#, 16#30#, 16#11#, 16#06#, 16#0A#, 16#03#, 16#11#, 16#22#, 16#33#, 16#14#, 16#13#, 16#49#, 16#44#, 16#58#, 16#48#, 16#00#, 16#70#, 16#01#, 16#46#, 16#4C#, 16#47#, 16#30#, 16#70#, 16#0A#, 16#A1#, 16#5B#, 16#31#, 16#A4#, 16#00#, 16#14#, 16#0D#, 16#50#, 16#52#, 16#45#, 16#30#, 16#00#, 16#A4#, 16#99#, 16#42#, 16#55#, 16#46#, 16#30#, 16#00#, 16#14#, 16#1D#, 16#54#, 16#45#, 16#53#, 16#54#, 16#00#, 16#5B#, 16#13#, 16#42#, 16#55#, 16#46#, 16#30#, 16#49#, 16#44#, 16#58#, 16#48#, 16#0A#, 16#08#, 16#46#, 16#4C#, 16#44#, 16#30#, 16#A4#, 16#99#, 16#46#, 16#4C#, 16#44#, 16#30#, 16#00#, 16#14#, 16#0D#, 16#50#, 16#4F#, 16#53#, 16#54#, 16#00#, 16#A4#, 16#99#, 16#42#, 16#55#, 16#46#, 16#30#, 16#00#, 16#14#, 16#0B#, 16#46#, 16#4C#, 16#41#, 16#47#, 16#00#, 16#A4#, 16#46#, 16#4C#, 16#47#, 16#30#], Bits_64);
   Ada.Text_IO.Put_Line ("source buffer replay checks" & Checks'Image);
end Source_Buffer_Replay;
