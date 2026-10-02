with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_Inventory; use Intel_GPU_ADLN_Inventory;
with Intel_GPU_ADLN_Steering;
with Intel_GPU_ADLN_Regset; use Intel_GPU_ADLN_Regset;
with Intel_GPU_ADS_Regset; use Intel_GPU_ADS_Regset;
with Intel_GPU_ADLN_Engine_Settings;
procedure ADLN_Regset_Tests is
   Description : Inventory := Decode (16#8086#, 16#46D2#, 0);
   Steering : Intel_GPU_ADLN_Steering.Topology :=
     Intel_GPU_ADLN_Steering.Decode (1, 4, 0);
   Value, Common : Register_Set;
   Base : Unsigned_32;
   Seen_Ring, Seen_Whitelist, Seen_MOCS, Seen_Perf : Natural;
begin
   for E in Engine loop
      Value := Build_Common (Description, E, Steering, 16#200000#);
      pragma Assert (Value.Ready and Value.Registers.Count = 54);
      Base := Engine_Base (E);
      Seen_Ring := 0; Seen_Whitelist := 0; Seen_MOCS := 0; Seen_Perf := 0;
      for I in 1 .. Value.Registers.Count loop
         declare
            R : constant Register_Entry := Value.Registers.Entries (I);
         begin
            if I > 1 then
               pragma Assert (Value.Registers.Entries (I - 1).Offset < R.Offset);
            end if;
            if R.Offset = Base + 16#80# or R.Offset = Base + 16#A8# or
              R.Offset = Base + 16#29C#
            then
               Seen_Ring := Seen_Ring + 1;
               pragma Assert (R.Masked = (R.Offset = Base + 16#29C#));
               pragma Assert (not R.Steered);
            elsif R.Offset in Base + 16#4D0# .. Base + 16#4FC# then
               Seen_Whitelist := Seen_Whitelist + 1;
               pragma Assert (not R.Masked and not R.Steered);
            elsif R.Offset in 16#B020# .. 16#B09C# then
               Seen_MOCS := Seen_MOCS + 1;
               pragma Assert (not R.Masked and not R.Steered);
            else
               pragma Assert (R.Offset in 16#E458# | 16#E45C# | 16#E558# |
                 16#E55C# | 16#E658# | 16#E65C# | 16#E758#);
               Seen_Perf := Seen_Perf + 1;
               pragma Assert (R.Steered and not R.Masked and R.Group_ID = 0 and R.Instance_ID = 2);
            end if;
         end;
      end loop;
      pragma Assert (Seen_Ring = 3 and Seen_Whitelist = 12 and Seen_MOCS = 32 and Seen_Perf = 7);
      Description.Engines (E) := False;
      pragma Assert (not Build_Common (Description, E, Steering, 16#200000#).Ready);
      Description.Engines (E) := True;
      Value := Build_Common (Description, E, Steering, 16#1000#);
      pragma Assert (not Value.Ready and Value.Registers.Count = 0);
   end loop;
   Steering.Valid := False;
   pragma Assert (not Build_Common (Description, Render, Steering, 16#200000#).Ready);
   pragma Assert (not Build (Description, Render, Steering, 16#200000#).Ready);
   for DSS in Unsigned_32 range 1 .. 63 loop
      Steering := Intel_GPU_ADLN_Steering.Decode (1, DSS, 0);
      for E in Engine loop
         Common := Build_Common (Description, E, Steering, 16#200000#);
         Value := Build (Description, E, Steering, 16#200000#);
         pragma Assert (Value.Ready and Value.Registers.Count = (if E = Render then 63 else 55));
         for I in 2 .. Value.Registers.Count loop
            pragma Assert (Value.Registers.Entries (I - 1).Offset < Value.Registers.Entries (I).Offset);
         end loop;
         for I in 1 .. Common.Registers.Count loop
            pragma Assert ((for some J in 1 .. Value.Registers.Count =>
              Value.Registers.Entries (J) = Common.Registers.Entries (I)));
         end loop;
         declare
            Settings : constant Intel_GPU_ADLN_Engine_Settings.Settings_Plan :=
              Intel_GPU_ADLN_Engine_Settings.Build (Description, E, 63);
         begin
            for S of Settings.Entries (1 .. Settings.Count) loop
               pragma Assert ((for some J in 1 .. Value.Registers.Count =>
                 Value.Registers.Entries (J) = Register_Entry'
                   (S.Offset, S.Masked_Write,
                    S.Offset not in 16#24D0# .. 16#24FC#, 0,
                    (if S.Offset in 16#24D0# .. 16#24FC# then 0
                     else Steering.Default_Instance))));
            end loop;
         end;
         Description.Engines (E) := False;
         Value := Build (Description, E, Steering, 16#200000#);
         pragma Assert (not Value.Ready and Value.Registers.Count = 0);
         Description.Engines (E) := True;
         Value := Build (Description, E, Steering, 0);
         pragma Assert (not Value.Ready and Value.Registers.Count = 0);
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("ADL-N common regset: all five engines, 54 entries each and rejection PASS");
   Ada.Text_IO.Put_Line ("ADL-N merged regset: 315 engine/topology plans, all entries and flags PASS");
end ADLN_Regset_Tests;
