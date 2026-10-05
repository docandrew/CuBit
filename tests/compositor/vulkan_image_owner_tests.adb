with Ada.Text_IO; with Interfaces; with System; with Vulkan_Image_Owner;
procedure Vulkan_Image_Owner_Tests is
   package O renames Vulkan_Image_Owner;
   package A renames O.Accounting;
   use type O.Phase, O.State, A.State, A.Phase, Interfaces.Unsigned_32, A.Ticket;
   subtype U32 is Interfaces.Unsigned_32;
   subtype U64 is Interfaces.Unsigned_64;
   procedure Mock (Bytes : U64; Mask, P, B, R : U32)
     with Import, Convention => C, External_Name => "image_mock";
   function Binds return U32 with Import, Convention => C, External_Name => "image_mock_binds";
   function Releases return U32 with Import, Convention => C, External_Name => "image_mock_releases";
   function Selected return U32 with Import, Convention => C, External_Name => "image_mock_selected";
begin
   for Bit in 0 .. 31 loop
      declare
         S : O.State; Budget : A.State := A.Open (8192);
         Mask : constant U32 := Interfaces.Shift_Left (1, Bit);
      begin
         Mock (4096, Mask, 0, 0, 0);
         O.Prepare (S, System.Null_Address);
         O.Allocate (S, Budget, Mask);
         pragma Assert (O.Status (S) = O.Live and A.Charged (Budget) = 4096);
         pragma Assert (Binds = 1 and Selected = U32 (Bit));
         O.Release (S, Budget, False);
         pragma Assert (O.Status (S) = O.Live and Releases = 0);
         O.Release (S, Budget, True);
         pragma Assert (O.Status (S) = O.Closed and A.Charged (Budget) = 0 and Releases = 1);
      end;
   end loop;
   for Case_No in 0 .. 8 loop
      declare S : O.State; Budget : A.State := A.Open (4096); begin
         case Case_No is
            when 0 => Mock (4097, 1, 0, 0, 0); -- budget
            when 1 => Mock (4096, 2, 0, 0, 0); -- incompatible type
            when 2 => Mock (4096, 1, 0, 1, 0); -- clean allocation failure
            when 3 => Mock (4096, 1, 0, 2, 0); -- uncertain allocation
            when 4 => Mock (4096, 1, 0, 0, 2); -- uncertain release
            when 5 => Mock (4096, 1, 1, 0, 0); -- prepare rejected
            when 6 => Mock (4096, 1, 2, 0, 0); -- prepare uncertain
            when 7 => Mock (U64'Last, 1, 0, 0, 0); -- size overflow
            when others => Mock (0, 1, 0, 0, 0);
         end case;
         O.Prepare (S, System.Null_Address);
         if O.Status (S) = O.Prepared then O.Allocate (S, Budget, 1); end if;
         if O.Status (S) = O.Live then O.Release (S, Budget, True); end if;
         if Case_No = 3 or Case_No = 4 then
            pragma Assert (O.Status (S) = O.Quarantined and A.Charged (Budget) = 4096);
            pragma Assert (A.Status (Budget, O.Lease (S)) = A.Quarantined);
         elsif Case_No = 6 then
            pragma Assert (O.Status (S) = O.Quarantined and A.Charged (Budget) = 0);
         else pragma Assert (O.Status (S) = O.Closed and A.Charged (Budget) = 0);
         end if;
         if Case_No = 0 or Case_No = 1 or Case_No >= 5 then pragma Assert (Binds = 0); end if;
      end;
   end loop;
   declare
      Capacity : constant := Natural (A.Slot'Last);
      Owners : array (1 .. Capacity + 2) of O.State;
      Budget : A.State := A.Open ((Capacity + 2) * 4096);
      Old : A.Ticket;
   begin
      Mock (4096, 1, 0, 0, 0);
      for I in 1 .. Capacity + 1 loop
         O.Prepare (Owners (I), System.Null_Address); O.Allocate (Owners (I), Budget, 1);
      end loop;
      pragma Assert (Binds = U32 (Capacity) and O.Status (Owners (Capacity + 1)) = O.Closed and A.Charged (Budget) = Capacity * 4096);
      Old := O.Lease (Owners (1)); O.Release (Owners (1), Budget, True);
      O.Prepare (Owners (Capacity + 2), System.Null_Address); O.Allocate (Owners (Capacity + 2), Budget, 1);
      pragma Assert (O.Lease (Owners (Capacity + 2)) /= Old and not A.Current (Budget, Old));
      pragma Assert (A.Charged (Budget) = Capacity * 4096);
   end;
   -- Reuse one owner without resetting allocation identity without resetting the ledger.
   -- Keep another allocation live throughout: reuse must not refund its bytes.
   declare
      S, Other : O.State;
      Budget : A.State := A.Open (8192);
      Old : A.Ticket := A.No_Ticket;
      Accepted : Boolean;
   begin
      Mock (4096, 1, 0, 0, 0);
      O.Prepare (Other, System.Null_Address); O.Allocate (Other, Budget, 1);
      for Cycle in 1 .. 32 loop
         O.Rearm (S, Budget, Accepted);
         pragma Assert (Accepted = (Cycle > 1));
         pragma Assert (O.Status (S) = O.Fresh);
         O.Prepare (S, System.Null_Address);
         declare Before : constant O.State := S; begin
            O.Rearm (S, Budget, Accepted);
            pragma Assert (not Accepted and S = Before);
         end;
         O.Allocate (S, Budget, 1);
         pragma Assert (O.Status (S) = O.Live and A.Charged (Budget) = 8192);
         pragma Assert (A.Identity (O.Lease (S)) = Cycle + 1);
         pragma Assert (O.Lease (S) /= Old and not A.Current (Budget, Old));
         Old := O.Lease (S);
         O.Release (S, Budget, False);
         declare Before : constant O.State := S; begin
            O.Rearm (S, Budget, Accepted);
            pragma Assert (not Accepted and S = Before);
         end;
         O.Release (S, Budget, True);
         pragma Assert (A.Charged (Budget) = 4096 and O.Can_Release (Other, Budget));
         pragma Assert (not A.Current (Budget, Old));
      end loop;
      pragma Assert (A.Issued (Budget) = 33 and Binds = 33 and Releases = 32);
      O.Release (Other, Budget, True);
      pragma Assert (A.Charged (Budget) = 0);
   end;
   -- Neither uncharged preparation uncertainty nor charged bind/release
   -- uncertainty can be erased by the reuse operation.
   for Failure in 0 .. 2 loop
      declare
         S : O.State; Budget : A.State := A.Open (4096);
         Accepted : Boolean;
      begin
         case Failure is
            when 0 => Mock (4096, 1, 2, 0, 0);
            when 1 => Mock (4096, 1, 0, 2, 0);
            when others => Mock (4096, 1, 0, 0, 2);
         end case;
         O.Prepare (S, System.Null_Address);
         if O.Status (S) = O.Prepared then O.Allocate (S, Budget, 1); end if;
         if O.Status (S) = O.Live then O.Release (S, Budget, True); end if;
         declare
            Before : constant O.State := S;
            Before_Budget : constant A.State := Budget;
            Before_Releases : constant U32 := Releases;
         begin
            O.Rearm (S, Budget, Accepted);
            pragma Assert (not Accepted and S = Before and Budget = Before_Budget);
            pragma Assert (O.Status (S) = O.Quarantined and Releases = Before_Releases);
         end;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("GPU image owner: 32 memory types, 9 failures, held readers, slot exhaustion and stale identity PASS");
end Vulkan_Image_Owner_Tests;
