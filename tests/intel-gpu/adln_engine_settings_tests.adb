with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_Inventory; use Intel_GPU_ADLN_Inventory;
with Intel_GPU_ADLN_Engine_Settings; use Intel_GPU_ADLN_Engine_Settings;
with Intel_GPU_Nonpriv_Registers;
procedure ADLN_Engine_Settings_Tests is
   Description : Inventory := Decode (16#8086#, 16#46D2#, 0);
   Value : Settings_Plan;
   Expected : constant Settings_Array :=
     [(16#20C4#, 16#3FFF#, 0, True, False),
      (16#B004#, 16#80#, 0, False, False),
      (16#E18C#, 16#8001#, 16#8001#, True, True),
      (16#20EC#, 2, 2, True, False),
      (16#E4F4#, 16#4100#, 16#4100#, True, True),
      (16#20A0#, 16#80000#, 16#80000#, False, False),
      (16#E48C#, 16#200#, 16#200#, True, True),
      (16#2050#, 16#1080#, 16#1080#, True, False),
      (16#20E0#, 16#4000#, 16#4000#, True, False), others => <>];
begin
   Value := Build (Description, Render, 0);
   pragma Assert (Value.Entries (1 .. 9) = Expected (1 .. 9));
   declare
      package NP renames Intel_GPU_Nonpriv_Registers;
      use type NP.Decision;
      Raw : constant array (0 .. 11) of Unsigned_32 :=
        [16#10002348#, 16#1000234C#, 16#10002350#, 16#10002354#,
         16#7010#, 16#7018#, 16#7304#, others => 16#2094#];
      Permissions : NP.Register_List (1 .. 12);
   begin
      for Slot in Raw'Range loop
         pragma Assert (Value.Entries (10 + Slot) =
           Setting'(16#24D0# + Unsigned_32 (Slot * 4), Unsigned_32'Last,
                    Raw (Slot), False, False));
         Permissions (1 + Slot) := NP.Decode (Value.Entries (10 + Slot).Value);
      end loop;
      for I in 0 .. 3 loop
         pragma Assert (NP.Evaluate (Permissions, 16#2348# + Unsigned_32 (I * 4),
                                    NP.Read_Register) = NP.Allow);
         pragma Assert (NP.Evaluate (Permissions, 16#2348# + Unsigned_32 (I * 4),
                                    NP.Write_Register) = NP.Unspecified);
      end loop;
      pragma Assert (NP.Evaluate (Permissions, 16#2340#, NP.Read_Register) = NP.Unspecified);
      pragma Assert (NP.Evaluate (Permissions, 16#2358#, NP.Read_Register) = NP.Unspecified);
      pragma Assert (NP.Evaluate (Permissions, 16#2080#, NP.Write_Register) = NP.Unspecified);
      pragma Assert (NP.Evaluate (Permissions, 16#2270#, NP.Write_Register) = NP.Unspecified);
   end;
   for E in Engine loop
      for Index in MOCS_Index loop
         Value := Build (Description, E, Index);
         pragma Assert (Value.Count = (if E = Render then 21 else 1));
         pragma Assert (Value.Entries (1).Offset = Engine_Base (E) + 16#C4#);
         pragma Assert (Write_Value (Value.Entries (1), 0) =
                        16#3FFF0000# + Unsigned_32 (Index * 258));
         for I in 1 .. Value.Count loop
            declare
               S : constant Setting := Value.Entries (I);
            begin
               pragma Assert ((S.Value and not S.Mask) = 0);
               if S.Masked_Write then
                  pragma Assert (Write_Value (S, 0) = Write_Value (S, 16#12345678#));
               else
                  pragma Assert ((Write_Value (S, 16#12345678#) and not S.Mask) =
                                   (16#12345678# and not S.Mask));
                  pragma Assert ((Write_Value (S, 16#12345678#) and S.Mask) = S.Value);
               end if;
            end;
         end loop;
      end loop;
      Description.Engines (E) := False;
      pragma Assert (Build (Description, E, 0).Count = 0);
      Description.Engines (E) := True;
   end loop;
   Description.Valid := False;
   pragma Assert (Build (Description, Render, 0).Count = 0);
   Ada.Text_IO.Put_Line ("ADL-N engine settings: 320 engine/MOCS plans and write encodings PASS");
end ADLN_Engine_Settings_Tests;
