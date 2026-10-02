with Ada.Text_IO; with Interfaces; with Intel_GPU_Combo_PHY;
procedure Combo_PHY_Tests is
   use Interfaces; use Intel_GPU_Combo_PHY;
   S, Restored : Snapshot;
   P : Plan;
   Valid : Boolean;
   Damaged : Snapshot;
   Expected_9, Expected_10, Expected_1 : Unsigned_32;
   A_Read : constant Snapshot :=
     (16#64C00#, 16#1628A0#, 16#162804#, 16#162104#, 16#162124#,
      16#162128#, 16#162120#, 16#162100#, 16#162014#, 16#16210C#);
   B_Read : constant Snapshot :=
     (16#64C04#, 16#6C8A0#, 16#6C804#, 16#6C104#, 16#6C124#,
      16#6C128#, 16#6C120#, 16#6C100#, 16#6C014#, 16#6C10C#);
   Order : constant array (Positive range 1 .. 9) of Field :=
     (Misc, TX_8, PCS_1, Comp_1, Comp_9, Comp_10, Comp_8, Comp_0, CL_5);
   -- Independent policy oracle, kept as masks in the test only.
   Relevant : constant array (Field) of Unsigned_32 :=
     (Misc => 16#00800000#, TX_8 => 16#E0000000#, PCS_1 => 16#00300000#,
      Comp_1 => 16#00FF00FF#, Comp_9 | Comp_10 => 16#FFFFFFFF#,
      Comp_8 => 16#01000000#, Comp_0 => 16#80000000#,
      CL_5 => 16#10#, Comp_3 => 16#1F000000#);
begin
   for F in Field loop
      for Bit in 0 .. 31 loop
         declare
            Toggle : constant Unsigned_32 := Shift_Left (Unsigned_32'(1), Bit);
         begin
            pragma Assert (Same_Configuration (F, 0, Toggle) =
              ((Relevant (F) and Toggle) = 0));
            pragma Assert (Same_Configuration (F, 16#AB25AB25#, 16#AB25AB25# xor Toggle) =
              ((Relevant (F) and Toggle) = 0));
         end;
      end loop;
   end loop;
   pragma Assert (Same_Configuration (Comp_0, 16#80005F24#, 16#80005F23#));
   pragma Assert (Read_Offset (A, TX_8) = 16#1628A0#);
   pragma Assert (Write_Offset (A, TX_8) = 16#1626A0#);
   pragma Assert (Read_Offset (B, PCS_1) = 16#6C804#);
   pragma Assert (Write_Offset (B, PCS_1) = 16#6C604#);
   pragma Assert (Read_Offset (A, Misc) = 16#64C00#);
   pragma Assert (Read_Offset (B, Misc) = 16#64C04#);
   for Port in PHY loop
      for F in Field loop
         pragma Assert (Read_Offset (Port, F) =
           (if Port = A then A_Read (F) else B_Read (F)));
         if F /= Comp_3 and F /= TX_8 and F /= PCS_1 then
            pragma Assert (Write_Offset (Port, F) = Read_Offset (Port, F));
         end if;
      end loop;
      for Selector in Unsigned_32 range 0 .. 31 loop
         S := (others => 16#12555555#);
         S (Comp_3) := Shift_Left (Selector, 24);
         Valid := Selector = 0 or Selector = 1 or Selector = 5 or
           Selector = 2 or Selector = 6;
         P := Prepare (Port, S);
         if not Valid then
            pragma Assert (P.Status = Unknown_Process and P.Count = 0);
         else
            pragma Assert (P.Status = Restore);
            Expected_1 := 0;
            case Selector is
               when 0 => Expected_9 := 16#62AB67BB#; Expected_10 := 16#51914F96#;
               when 1 => Expected_9 := 16#86E172C7#; Expected_10 := 16#77CA5EAB#;
               when 5 => Expected_9 := 16#93F87FE1#; Expected_10 := 16#8AE871C5#;
               when 2 => Expected_9 := 16#98FA82DD#; Expected_10 := 16#89E46DC1#;
               when others =>
                  Expected_1 := 16#00440000#;
                  Expected_9 := 16#9A00AB25#; Expected_10 := 16#8AE38FF1#;
            end case;
            pragma Assert (P.Writes (4).Value = ((S (Comp_1) and 16#FF00FF00#) or Expected_1));
            pragma Assert (P.Writes (5).Value = Expected_9);
            pragma Assert (P.Writes (6).Value = Expected_10);
            Restored := S;
            for I in 1 .. P.Count loop
               pragma Assert (P.Writes (I).Register =
                 Order (if Port = B and I >= 7 then I + 1 else I));
               Restored (P.Writes (I).Register) := P.Writes (I).Value;
            end loop;
            pragma Assert (Prepare (Port, Restored).Status = Already_Ready);
            -- Each checked state bit must invalidate an otherwise ready PHY.
            for F in Misc .. CL_5 loop
               if F /= Comp_8 or Port = A then
                  Damaged := Restored;
                  Damaged (F) := Damaged (F) xor
                    (case F is
                       when Misc => 16#00800000#, when TX_8 => 16#80000000#,
                       when PCS_1 => 16#00100000#, when Comp_8 => 16#01000000#,
                       when Comp_0 => 16#80000000#, when CL_5 => 16#10#,
                       when others => 1);
                  pragma Assert (Prepare (Port, Damaged).Status = Restore);
               end if;
            end loop;
            pragma Assert ((Restored (Misc) xor S (Misc)) in 0 | 16#00800000#);
            pragma Assert ((Restored (TX_8) and 16#1FFFFFFF#) = (S (TX_8) and 16#1FFFFFFF#));
            pragma Assert ((Restored (PCS_1) and not 16#00300000#) = (S (PCS_1) and not 16#00300000#));
            pragma Assert ((Restored (Comp_1) and 16#FF00FF00#) = (S (Comp_1) and 16#FF00FF00#));
            pragma Assert (Restored (Comp_3) = S (Comp_3));
            if Port = B then
               pragma Assert (Restored (Comp_8) = S (Comp_8));
            end if;
         end if;
      end loop;
      for F in Field loop
         S := (others => 0); S (F) := Unsigned_32'Last;
         P := Prepare (Port, S);
         pragma Assert (P.Status = Invalid_Read and P.Count = 0);
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("combo PHY plans: selectors, ordering, preservation, sentinel rejection PASS");
end Combo_PHY_Tests;
