--  Hosted test of the KERN-014 ownership model: a long pseudo-random run of
--  every operation whose precondition holds, with contracts checked
--  (-gnata), plus Unreachable on every frame that holds process data.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Kernel_Map_Ownership; use Kernel_Map_Ownership;

procedure Main is
   Seed : Unsigned_64 := 16#9E37_79B9_7F4A_7C15#;
   function Next return Unsigned_64 is
   begin
      Seed := Seed xor Shift_Left (Seed, 13);
      Seed := Seed xor Shift_Right (Seed, 7);
      Seed := Seed xor Shift_Left (Seed, 17);
      return Seed;
   end Next;
   type Op is (A_Kernel, F_Kernel, K_Zero, A_User, Write, F_User, P_Pin, P_Unpin,
               W_Map, W_Zero, W_Copy, W_Unmap, Round, B_Unmap, F_Unmap, M_Back,
               V_Map, V_Free, V_Unmap, M_Clean);
   Done : array (Op) of Natural := [others => 0];
   Steps : constant := 3_000_000;
   Checked : Natural := 0;
   function Region_Free (R : Region_Index; Allow_Data : Boolean) return Boolean is
     (for all F in Frame_Index =>
        (if Region_Of (F) = R then Owners (F) = Free
           and then (Allow_Data or else Contents (F) /= Process_Data)));
   function No_Window_In (R : Region_Index) return Boolean is
     (for all C in CPU_Index =>
        (if Windows (C).Mapped then Region_Of (Windows (C).Frame) /= R));
begin
   for Step in 1 .. Steps loop
      declare
         O : constant Op := Op'Val (Natural (Next mod Unsigned_64 (Op'Pos (Op'Last) + 1)));
         --  Bias frames toward one region so regions do empty and move.
         F : constant Frame_Index := Frame_Index (Next mod Unsigned_64 (Frame_Index'Last + 1));
         R : constant Region_Index := Region_Of (F);
         C : constant CPU_Index := CPU_Index (Next mod Unsigned_64 (CPU_Count));
         S : constant Map_State := Regions (R).State;
         
      begin
         case O is
            when A_Kernel =>
               if Owners (F) = Free and S = Kernel_Mapped then Alloc_Kernel (F); Done (O) := Done (O) + 1; end if;
            when F_Kernel =>
               if Owners (F) = Kernel then Free_Kernel (F); Done (O) := Done (O) + 1; end if;
            when K_Zero =>
               if Owners (F) = Free and S = Kernel_Mapped then Kernel_Zero (F); Done (O) := Done (O) + 1; end if;
            when A_User =>
               if Owners (F) = Free and S = Unmapped and Clean (F) and not In_Window (F)
               then
                  Alloc_User (F, (if Next mod 4 = 0 then Device else Process)); Done (O) := Done (O) + 1;
               end if;
            when Write =>
               if Owners (F) in Process_Owner then Process_Writes (F); Done (O) := Done (O) + 1; end if;
            when F_User =>
               if Owners (F) in Process_Owner and not Pins (F) and Viewers (F) = 0 then
                  Free_User (F); Done (O) := Done (O) + 1;
               end if;
            when P_Pin =>
               if Owners (F) in Process_Owner and not Pins (F) then Pin (F); Done (O) := Done (O) + 1; end if;
            when P_Unpin =>
               if Pins (F) and not In_Window (F) then Unpin (F); Done (O) := Done (O) + 1; end if;
            when W_Map =>
               if not Windows (C).Mapped and S = Unmapped and (Owners (F) = Free or Pins (F)) then
                  Window_Map (C, F); Done (O) := Done (O) + 1;
               end if;
            when W_Zero =>
               if Windows (C).Mapped and then Owners (Windows (C).Frame) = Free then
                  Window_Zero (C); Done (O) := Done (O) + 1;
               end if;
            when W_Copy =>
               if Windows (C).Mapped and then Owners (Windows (C).Frame) in Process_Owner then
                  Window_Copy (C); Done (O) := Done (O) + 1;
               end if;
            when W_Unmap =>
               if Windows (C).Mapped then Window_Unmap (C); Done (O) := Done (O) + 1; end if;
            when Round =>
               Complete_Round; Done (O) := Done (O) + 1;
            when B_Unmap =>
               if S = Kernel_Mapped and then Region_Free (R, True) then Begin_Unmap (R); Done (O) := Done (O) + 1; end if;
            when F_Unmap =>
               if S = Stale and then Rounds > Regions (R).Stamp then Finish_Unmap (R); Done (O) := Done (O) + 1; end if;
            when V_Map =>
               if Owners (F) in Process_Owner and Viewers (F) < Max_Views then
                  View_Map (F); Done (O) := Done (O) + 1;
               end if;
            when V_Free =>
               if Owners (F) in Process_Owner and not Pins (F) and Viewers (F) > 0 then
                  Free_Viewed (F); Done (O) := Done (O) + 1;
               end if;
            when V_Unmap =>
               if Viewers (F) > 0 then View_Unmap (F); Done (O) := Done (O) + 1; end if;
            when M_Clean =>
               if Owners (F) = Free and then Contents (F) = Zero and then Rounds > Residue (F) then
                  Mark_Clean (F); Done (O) := Done (O) + 1;
               end if;
            when M_Back =>
               if S = Unmapped and then Region_Free (R, False) and then No_Window_In (R) then
                  Map_Back (R); Done (O) := Done (O) + 1;
               end if;
         end case;
      end;
      if Step mod 1000 = 0 then
         pragma Assert (Isolated and then Laundered);
         for F in Frame_Index loop
            if Viewers (F) > 0 then
               pragma Assert (Owners (F) not in Free | Kernel);
            end if;
            if Contents (F) = Process_Data and then not In_Window (F) then
               pragma Assert (not Kernel_Reachable (F));
               Checked := Checked + 1;
            end if;
         end loop;
      end if;
   end loop;
   for O in Op loop
      Put_Line ("kernel-map-ownership: " & O'Image & Done (O)'Image);
   end loop;
   Put_Line ("kernel-map-ownership: unreachable checks" & Checked'Image);
   Put_Line ("kernel-map-ownership: PASS");
end Main;
