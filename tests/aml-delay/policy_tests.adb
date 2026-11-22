with Ada.Text_IO;
with AML_Decode;
with AML_Delays; use AML_Delays;
procedure Policy_Tests is
   use type AML_Decode.Integer_Value;
   Checks : Natural := 0;
   type Case_Data is record
      Input : AML_Decode.Integer_Value;
      Sleep_32, Sleep_64 : Sleep_Milliseconds;
      Stall_OK : Boolean;
      Stall_Value : Stall_Microseconds;
   end record;
   Cases : constant array (Positive range <>) of Case_Data :=
     [(0,0,0,True,0), (1,1,1,True,1), (100,100,100,True,100),
      (255,255,255,True,255), (256,256,256,False,0),
      (1_999,1_999,1_999,False,0), (2_000,2_000,2_000,False,0),
      (2_001,2_000,2_000,False,0),
      (16#FFFF_FFFF#,2_000,2_000,False,0),
      (16#1_0000_0000#,0,2_000,True,0),
      (16#1_0000_0064#,100,2_000,True,100),
      (16#FFFF_FFFF_0000_00FF#,255,2_000,True,255),
      (16#FFFF_FFFF_0000_0100#,256,2_000,False,0),
      (AML_Decode.Integer_Value'Last,2_000,2_000,False,0)];
   procedure Check (Good : Boolean) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Checks'Image; end if; end Check;
begin
   for Width in AML_Decode.Integer_Width loop
      for Test of Cases loop
         declare
            S : constant Normalization_Result := Normalize (Sleep_Delay, Width, Test.Input);
            T : constant Normalization_Result := Normalize (Stall_Delay, Width, Test.Input);
            R : Outcome := Completed;
         begin
            Check (S.Status = Accepted and then S.Item.Kind = Sleep_Delay);
            Check (S.Item.Milliseconds = (case Width is
              when AML_Decode.Bits_32 => Test.Sleep_32, when AML_Decode.Bits_64 => Test.Sleep_64));
            Unavailable_Provider (S.Item, R); Check (R = Unavailable);
            Check ((T.Status = Accepted) = Test.Stall_OK);
            if Test.Stall_OK then
               Check (T.Item.Kind = Stall_Delay and then T.Item.Microseconds = Test.Stall_Value);
               Unavailable_Provider (T.Item, R); Check (R = Unavailable);
            end if;
         end;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Delay policy checks" & Checks'Image);
end Policy_Tests;
