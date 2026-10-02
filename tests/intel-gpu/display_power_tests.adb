with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Display_Topology; use Intel_GPU_Display_Topology;
with Intel_GPU_Display_Power;
procedure Display_Power_Tests is
   procedure Run (P : Pipe; Inherited : Boolean; Fail_Hold, Fail_Drop : Natural) is
      Control : Unsigned_32 := 16#0100_0000#;
      Calls : Natural := 0;
      Seen : Unsigned_64 := 0;
      DC_On : Boolean := Inherited;
      Current_Pipe : Pipe := P;
      Selection : Unsigned_64 := Required (P);
      function Number (W : Well) return Natural is (Well'Pos (W) + 1);
      function Selected (N : Natural) return Boolean is
        (N /= 0 and then (Selection and Shift_Left (Unsigned_64'(1), N - 1)) /= 0);
      function Read (Offset : Unsigned_32) return Unsigned_32 is
      begin
         Calls := Calls + 1;
         return (case Offset is when 16#45404# => Control,
           when 16#42000# => 16#0FFF_FFFF#, when 16#46430# => 0,
           when others => raise Program_Error);
      end Read;
      procedure Write (Offset, Value : Unsigned_32; Success : out Boolean) is
      begin
         Calls := Calls + 1;
         if Offset = 16#45404# then
            pragma Assert ((Value and 16#0100_0000#) /= 0);
            pragma Assert (not Inherited);
            Control := Value;
            for W in Well loop
               if W /= DC_Off and then (Control and Request_Mask (W)) /= 0 then
                  Control := Control or State_Mask (W);
               end if;
            end loop;
         else pragma Assert (Offset = 16#46430# and Value = 16#8000#); end if;
         Success := True;
      end Write;
      function Now return Unsigned_64 is (0);
      procedure Pause is null;
      procedure Post (W : Request_Well; Success : out Boolean) is
      begin
         Calls := Calls + 1;
         pragma Assert ((Selection and Bit (W)) /= 0);
         pragma Assert ((Seen and Ancestors (W)) = Ancestors (W));
         if W in PWC | PWD then pragma Assert (DC_On); end if;
         Seen := Seen or Bit (W);
         Success := Number (W) /= Fail_Hold;
      end Post;
      procedure Pre (W : Request_Well; Success : out Boolean) is
      begin
         Calls := Calls + 1;
         pragma Assert (not Inherited);
         for Child in Well loop
            if (Ancestors (Child) and Bit (W)) /= 0 then
               pragma Assert ((Seen and Bit (Child)) = 0);
            end if;
         end loop;
         Success := Number (W) /= Fail_Drop;
         if Success then Seen := Seen and not Bit (W); end if;
      end Pre;
      procedure Hold_DC (Added, Success : out Boolean) is
      begin
         Calls := Calls + 1;
         pragma Assert ((Seen and Bit (PW1)) /= 0);
         pragma Assert (Current_Pipe in C | D);
         Added := not Inherited; DC_On := True;
         Seen := Seen or Bit (DC_Off);
         Success := Number (DC_Off) /= Fail_Hold;
      end Hold_DC;
      procedure Drop_DC (Added : Boolean; Success : out Boolean) is
      begin
         Calls := Calls + 1;
         pragma Assert (Added /= Inherited);
         if not Inherited then
            pragma Assert ((Seen and (Bit (PWC) or Bit (PWD))) = 0);
         end if;
         Success := Number (DC_Off) /= Fail_Drop;
         if Success then
            Seen := Seen and not Bit (DC_Off);
            if Added then DC_On := False; end if;
         end if;
      end Drop_DC;
      package Power is new Intel_GPU_Display_Power
        (Read, Write, Now, Pause, Post, Pre, Hold_DC, Drop_DC);
      use type Power.Ownership_State;
      OK : Boolean;
      Before : Natural;
      Failing : Natural := 0;
   begin
      if Inherited then
         for W in Well loop
            if W /= DC_Off then Control := Control or Request_Mask (W) or State_Mask (W); end if;
         end loop;
      end if;
      Power.Acquire (P, False, 3, OK);
      pragma Assert (not OK and Calls = 0 and Power.State = Power.Idle);
      Power.Acquire (P, True, 3, OK);
      pragma Assert (OK = not Selected (Fail_Hold));
      if not OK then Failing := Fail_Hold;
      else
         pragma Assert (Power.State = Power.Held and Seen = Selection);
         Before := Calls;
         Power.Acquire (P, True, 1, OK);
         pragma Assert (not OK and Calls = Before);
         Power.Release (OK);
         pragma Assert (OK = (not Selected (Fail_Drop) or else
           (Inherited and Fail_Drop /= Number (DC_Off))));
         if not OK then Failing := Fail_Drop;
         else
            pragma Assert (Power.State = Power.Idle and Power.Retained = 0);
         end if;
      end if;
      if Failing /= 0 then
         pragma Assert (Power.State = Power.Faulted);
         pragma Assert (Power.Uncertain = Shift_Left (Unsigned_64'(1), Failing - 1));
         pragma Assert (Power.Retained = (Selection and (Shift_Left (Unsigned_64'(1), Failing) - 1)));
         Before := Calls;
         Power.Release (OK); pragma Assert (not OK);
         Power.Acquire (P, True, 3, OK);
         pragma Assert (not OK and Calls = Before);
      elsif Fail_Hold = 0 and Fail_Drop = 0 then
         for Next_Pipe in Pipe loop
            -- Switch dependencies on the same manager, including crossing
            -- into and out of DC-off and sharing PW1/PW2 across selections.
            Current_Pipe := Next_Pipe;
            Selection := Required (Next_Pipe);
            Seen := 0;
            Power.Acquire (Next_Pipe, True, 3, OK); pragma Assert (OK);
            Power.Release (OK); pragma Assert (OK);
         end loop;
      end if;
   end Run;
begin
   for P in Pipe loop
      for Inherited in Boolean loop
         for H in 0 .. 7 loop
            for D in 0 .. 7 loop Run (P, Inherited, H, D); end loop;
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Display power PASS: 512 complete-pipe fault combinations and reuse cycles");
end Display_Power_Tests;
