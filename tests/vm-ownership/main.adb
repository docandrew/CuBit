--  Hosted test of the VMM-001 ownership model: a long pseudo-random run of
--  every operation whose precondition holds, with contracts checked
--  (-gnata, so Invariant is evaluated before and after each one), and the
--  theorems checked on the live state.
pragma Ada_2022;
with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with VM_Ownership; use VM_Ownership;

procedure Main is
   Seed : Unsigned_64 := 16#2545_F491_4F6C_DD1D#;
   function Next return Unsigned_64 is
   begin
      Seed := Seed xor Shift_Left (Seed, 13);
      Seed := Seed xor Shift_Right (Seed, 7);
      Seed := Seed xor Shift_Left (Seed, 17);
      return Seed;
   end Next;
   function Pick (N : Positive) return Natural is (Natural (Next mod Unsigned_64 (N)));

   type Op is (A_Kernel, A_Process, M_Device, C_VM, A_Table, U_Table, F_Table,
               A_Guest, F_Guest, G_Device, R_Device, P_Map, P_Unmap, P_Round,
               P_Retire, D_Map, D_Unmap, V_Map, V_Unmap, C_VCPU, D_VCPU,
               P_Load, P_Clear, P_Enter, P_Leave, G_Touch, T_Walk, S_Arch, F_Tag, P_Destroy,
               P_Finish);
   Done : array (Op) of Natural := [others => 0];
   Steps : constant := 2_000_000;
   Theorems : Natural := 0;
   Finished : Natural := 0;
begin
   for Step in 1 .. Steps loop
      declare
         O : constant Op := Op'Val (Pick (Op'Pos (Op'Last) + 1));
         F : constant Frame_Index := Pick (Frame_Count);
         V : constant VM_Index := 1 + Pick (VM_Count);
         G : constant Page_Index := Pick (Page_Count);
         X : constant VCPU_Index := 1 + Pick (VCPU_Count);
         C : constant CPU_Index := 1 + Pick (CPU_Count);
         P : constant Process_Index := 1 + Pick (Process_Count);
         procedure Count is begin Done (O) := Done (O) + 1; end Count;
      begin
         case O is
            when A_Kernel =>
               if Owners (F) = Free and Kernel_Mapped (F) and Done (O) < 3 then
                  Alloc_Kernel (F); Count;
               end if;
            when A_Process =>
               if Owners (F) = Free and not Kernel_Mapped (F) and Done (O) < 3 then
                  Alloc_Process (F); Count;
               end if;
            when M_Device =>
               if Owners (F) = Free and not Kernel_Mapped (F) and Done (O) < 3 then
                  Make_Device (F); Count;
               end if;
            when C_VM =>
               if Phase (V) = Gone and Owners (F) = Free and Kernel_Mapped (F) then
                  Create_VM (V, F, P); Count;
               end if;
            when A_Table =>
               if Phase (V) /= Gone and Owners (F) = Free and Kernel_Mapped (F) then
                  Add_Table (V, F); Count;
               end if;
            when U_Table =>
               if Table_Of (F) /= No_VM and then Table_Linked (F) then Unlink_Table (F); Count; end if;
            when F_Table =>
               if Table_Of (F) /= No_VM and then not Table_Linked (F)
                 and then Table_Stamp (F) < Round (Table_Of (F))
               then
                  Free_Table (F); Count;
               end if;
            when A_Guest =>
               if Phase (V) = Live and Owners (F) = Free and not Kernel_Mapped (F) then
                  Alloc_Guest (V, F); Count;
               end if;
            when F_Guest =>
               if Owners (F) = Guest and Use_VM (F) = No_VM and View_By (F) = No_Process then
                  Free_Guest (F); Count;
               end if;
            when G_Device =>
               if Phase (V) = Live and Owners (F) = Device and Device_Of (F) = No_VM then
                  Grant_Device (V, F); Count;
               end if;
            when R_Device =>
               if Device_Of (F) /= No_VM and Use_VM (F) = No_VM then Revoke_Device (F); Count; end if;
            when P_Map =>
               if Phase (V) = Live and then Kind (V) (G) = Absent and then Use_VM (F) = No_VM
                 and then ((Owners (F) = Guest and then Guest_Of (F) = V)
                           or else (Owners (F) = Device and then Device_Of (F) = V))
               then
                  Map (V, G, F); Count;
               end if;
            when P_Unmap =>
               if Kind (V) (G) = Mapped and not Dom (V) (G) then Unmap (V, G); Count; end if;
            when P_Round =>
               if Phase (V) /= Gone and Round (V) < Round_Count'Last then
                  Complete_Round (V); Count;
               end if;
            when P_Retire =>
               if Kind (V) (G) = Stale and then Stamp (V) (G) < Round (V) then
                  Retire (V, G); Count;
               end if;
            when D_Map =>
               if Kind (V) (G) = Mapped then Map_Domain (V, G); Count; end if;
            when D_Unmap =>
               if Dom (V) (G) then Unmap_Domain (V, G); Count; end if;
            when V_Map =>
               if Phase (V) /= Gone and then Monitor (V) /= No_Process
                 and then Kind (V) (G) = Mapped
                 and then Owners (Target (V) (G)) = Guest
                 and then View_By (Target (V) (G)) = No_Process
               then
                  Map_View (Monitor (V), V, G); Count;
               end if;
            when V_Unmap =>
               if View_By (F) /= No_Process then Unmap_View (F); Count; end if;
            when C_VCPU =>
               if Phase (V) = Live and VM_Of (X) = No_VM and Owners (F) = Free
                 and Kernel_Mapped (F)
               then
                  Create_VCPU (V, X, F); Count;
               end if;
            when D_VCPU =>
               if VM_Of (X) /= No_VM and Active_On (X) = No_CPU then
                  for E in Frame_Index loop
                     if Control_Of (E) = X then
                        if (for all D in Frame_Index => (if D /= E then Control_Of (D) /= X)) then
                           Destroy_VCPU (X, E); Count;
                        end if;
                        exit;
                     end if;
                  end loop;
               end if;
            when P_Load =>
               if VM_Of (X) /= No_VM and then Phase (VM_Of (X)) = Live
                 and then Active_On (X) = No_CPU and then Current (C) = No_VCPU
               then
                  Load (X, C); Count;
               end if;
            when P_Clear =>
               if Active_On (X) /= No_CPU and then not Running (Active_On (X)) then
                  Clear (X); Count;
               end if;
            when P_Enter =>
               if Current (C) /= No_VCPU and then Phase (VM_Of (Current (C))) = Live
                 and then not Tag_Stale (Current (C))
               then
                  Enter (C); Count;
               end if;
            when P_Leave =>
               if Running (C) then Leave (C); Count; end if;
            when G_Touch =>
               if Running (C) and then Kind (VM_Of (Current (C))) (G) = Mapped then
                  Guest_Touch (C, G); Count;
               end if;
            when T_Walk =>
               if Running (C) and then Table_Of (F) = VM_Of (Current (C))
                 and then Table_Linked (F)
               then
                  Table_Walk (C, F); Count;
               end if;
            when S_Arch =>
               if VM_Of (X) /= No_VM
                 and then (Active_On (X) = No_CPU or else not Running (Active_On (X)))
               then
                  Set_Arch_State (X, Pick (2) = 0); Count;
               end if;
            when F_Tag =>
               if Active_On (X) /= No_CPU then Flush_Tag (X); Count; end if;
            when P_Destroy =>
               if Phase (V) = Live and Pick (256) = 0 then Destroy (V); Count; end if;
            when P_Finish =>
               if Phase (V) = Closing
                 and then (for all Y in VCPU_Index => VM_Of (Y) /= V)
                 and then (for all H in Page_Index => Kind (V) (H) = Absent)
                 and then (for all E in Frame_Index =>
                             Guest_Of (E) /= V and then Device_Of (E) /= V
                             and then Table_Of (E) /= V)
                 and then (for all D in CPU_Index => not Ran_On (V) (D))
               then
                  Finish_Destroy (V); Count; Finished := Finished + 1;
               end if;
         end case;
      end;
      if Step mod 997 = 0 then
         for V in VM_Index loop
            for G in Page_Index loop
               if Kind (V) (G) /= Absent then Leaf_Owned (V, G); Theorems := Theorems + 1; end if;
            end loop;
            if Phase (V) = Gone then Reclaimed (V); Theorems := Theorems + 1; end if;
         end loop;
         for F in Frame_Index loop
            if Table_Of (F) /= No_VM or else Control_Of (F) /= No_VCPU then
               Tables_Private (F); Theorems := Theorems + 1;
            end if;
         end loop;
         for C in CPU_Index loop
            for V in VM_Index loop
               for G in Page_Index loop
                  if Cached (C) (V) (G) then Cached_Owned (C, V, G); Theorems := Theorems + 1; end if;
               end loop;
            end loop;
         end loop;
      end if;
   end loop;
   for O in Op loop
      Put_Line ("vm-ownership: " & O'Image & Done (O)'Image);
   end loop;
   Put_Line ("vm-ownership: VMs torn down to Gone" & Finished'Image);
   Put_Line ("vm-ownership: theorem checks" & Theorems'Image);
   for O in Op loop
      if Done (O) = 0 then
         Put_Line ("vm-ownership: FAIL, never ran " & O'Image);
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
         return;
      end if;
   end loop;
   Put_Line ("vm-ownership: PASS");
end Main;
