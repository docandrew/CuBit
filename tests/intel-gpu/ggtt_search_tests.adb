with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_GGTT_Search;
with Intel_GPU_GGTT_Reservations;
procedure GGTT_Search_Tests is
   Entries : array (Unsigned_64 range 0 .. 31) of Unsigned_64 := [others => 0];
   Reads : Natural := 0;
   Last : Unsigned_64 := 0;
   Fail : Boolean := False;
   Ledger : Intel_GPU_GGTT_Reservations.Ledger;
   Admitted : Boolean;
   function Page_Available (Address : Unsigned_64) return Boolean is
     (Intel_GPU_GGTT_Reservations.Space_Free (Ledger, Address, 4096));
   procedure Read_PTE (Index : Unsigned_64; Value : out Unsigned_64; Success : out Boolean) is
   begin
      pragma Assert (Index in 3 .. 14);
      pragma Assert (Page_Available (Index * 4096));
      pragma Assert (Reads = 0 or else Index > Last);
      Last := Index; Reads := Reads + 1;
      Value := Entries (Index); Success := not Fail;
   end Read_PTE;
   package Search is new Intel_GPU_GGTT_Search (Page_Available, Read_PTE);
   use Search;
   Selected, Expected : Unsigned_64;
   Status : Result;
begin
   Intel_GPU_GGTT_Reservations.Admit (Ledger, 8_388_608, 3 * 4096, 12 * 4096, Admitted);
   pragma Assert (Admitted);
   -- Exhaustive 12-page occupancy patterns, compared with a simple oracle.
   for Mask in Unsigned_64 range 0 .. 4095 loop
      for Page in Unsigned_64 range 3 .. 14 loop
         Entries (Page) := (if (Shift_Right (Mask, Natural (Page - 3)) and 1) /= 0 then 1 else 0);
      end loop;
      for Pages in Unsigned_64 range 1 .. 4 loop
         for Alignment in Natural range 0 .. 2 loop
            Expected := 0;
            for Page in Unsigned_64 range 3 .. 15 - Pages loop
               if Page mod (2 ** Alignment) = 0 and then
                 (for all I in Page .. Page + Pages - 1 => Entries (I) = 0)
               then Expected := Page * 4096; exit; end if;
            end loop;
            Reads := 0;
            Find (True, 8_388_608, 3 * 4096, 12 * 4096,
              Pages * 4096, 4096 * 2 ** Alignment, Selected, Status);
            pragma Assert (Selected = Expected);
            pragma Assert (Status = (if Expected = 0 then Exhausted else Found));
            pragma Assert (Reads <= 12);
            pragma Assert (Last_Evidence.Outcome = Status and Last_Evidence.Reads = Unsigned_64 (Reads));
            pragma Assert (Last_Evidence.Blocked = 0 and Last_Evidence.Nonzero <= Last_Evidence.Reads);
         end loop;
      end loop;
   end loop;
   Entries := [others => 16#7C80_0001#]; Reads := 0;
   Find (True, 8_388_608, 3 * 4096, 12 * 4096, 4096, 4096, Selected, Status);
   pragma Assert (Status = Exhausted and Last_Evidence.Reads = 12 and
     Last_Evidence.Nonzero = 12 and Last_Evidence.Blocked = 0 and
     Last_Evidence.First_Nonzero_Index = 3 and
     Last_Evidence.First_Nonzero_Value = 16#7C80_0001#);
   Entries := [others => 0]; Fail := True; Reads := 0;
   Find (True, 8_388_608, 3 * 4096, 12 * 4096, 4096, 4096, Selected, Status);
   pragma Assert (Status = Read_Failed and Selected = 0 and Reads = 1);
   pragma Assert (Last_Evidence.Outcome = Read_Failed and Last_Evidence.Reads = 1 and
     Last_Evidence.Nonzero = 0 and Last_Evidence.First_Nonzero_Value = 0);
   Fail := False; Entries (3) := Unsigned_64'Last; Reads := 0;
   Find (True, 8_388_608, 3 * 4096, 12 * 4096, 4096, 4096, Selected, Status);
   pragma Assert (Status = Read_Failed and Selected = 0 and Reads = 1);
   Reads := 0;
   Find (False, 8_388_608, 3 * 4096, 12 * 4096, 4096, 4096, Selected, Status);
   pragma Assert (Status = Rejected and Selected = 0 and Reads = 0);
   pragma Assert (Last_Evidence = (Rejected, 0, 0, 0, 0, 0));
   Find (True, 8_388_608, Unsigned_64'Last, 4096, 4096, 4096, Selected, Status);
   pragma Assert (Status = Rejected and Selected = 0 and Reads = 0);
   Find (True, 8_388_608, 4096, Unsigned_64'Last, 4096, 4096, Selected, Status);
   pragma Assert (Status = Rejected and Selected = 0 and Reads = 0);
   Find (True, 8_388_608, 4096, 4096, 4096, 12288, Selected, Status);
   pragma Assert (Status = Rejected and Selected = 0 and Reads = 0);
   declare
      Claim_Status : Intel_GPU_GGTT_Reservations.Result;
      use type Intel_GPU_GGTT_Reservations.Result;
   begin
      -- Includes retained, never-published claims whose PTEs remain zero.
      Intel_GPU_GGTT_Reservations.Reserve (Ledger, 3 * 4096, 4096, Claim_Status);
      pragma Assert (Claim_Status = Intel_GPU_GGTT_Reservations.Reserved);
      Intel_GPU_GGTT_Reservations.Reserve (Ledger, 6 * 4096, 2 * 4096, Claim_Status);
      pragma Assert (Claim_Status = Intel_GPU_GGTT_Reservations.Reserved);
      Intel_GPU_GGTT_Reservations.Reserve (Ledger, 12 * 4096, 4096, Claim_Status);
      pragma Assert (Claim_Status = Intel_GPU_GGTT_Reservations.Reserved);
      for Mask in Unsigned_64 range 0 .. 4095 loop
         for Page in Unsigned_64 range 3 .. 14 loop
            Entries (Page) := (if (Shift_Right (Mask, Natural (Page - 3)) and 1) /= 0 then 1 else 0);
         end loop;
         for Pages in Unsigned_64 range 1 .. 4 loop
            for Alignment in Natural range 0 .. 2 loop
               Expected := 0;
               for Page in Unsigned_64 range 3 .. 15 - Pages loop
                  if Page mod (2 ** Alignment) = 0 and then
                    (for all I in Page .. Page + Pages - 1 =>
                       Entries (I) = 0 and I not in 3 | 6 | 7 | 12)
                  then Expected := Page * 4096; exit; end if;
               end loop;
               Reads := 0;
               Find (True, 8_388_608, 3 * 4096, 12 * 4096,
                 Pages * 4096, 4096 * 2 ** Alignment, Selected, Status);
               pragma Assert (Selected = Expected);
               pragma Assert (Status = (if Expected = 0 then Exhausted else Found));
               pragma Assert (Reads <= 8 and Intel_GPU_GGTT_Reservations.Count (Ledger) = 3);
               pragma Assert (Last_Evidence.Outcome = Status and
                 Last_Evidence.Reads = Unsigned_64 (Reads) and
                 Last_Evidence.Blocked <= 4 and Last_Evidence.Nonzero <= Last_Evidence.Reads);
            end loop;
         end loop;
      end loop;
   end;
   Ada.Text_IO.Put_Line ("GGTT search PASS: 98304 hardware/ledger combinations plus fail-closed cases");
end GGTT_Search_Tests;
