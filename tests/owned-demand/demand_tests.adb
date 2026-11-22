with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Owned_Demand_Policy; use Owned_Demand_Policy;

procedure Demand_Tests is
   Count : Natural := 0;
   type Addresses is array (Positive range <>) of Unsigned_64;
   Points : constant Addresses :=
     [0, 4095, 4096, 4097, 8191, 8192, User_Limit, Unsigned_64'Last];
   procedure Expect (Actual, Expected : Decision) is
   begin
      Count := Count + 1;
      if Actual /= Expected then
         raise Program_Error with "case" & Count'Image & ": " &
           Actual'Image & " /= " & Expected'Image;
      end if;
   end Expect;
   Expected : Decision;
begin
   -- Cross product includes faults outside the half-open reservation,
   -- all access/backing states and resource exhaustion boundaries.
   for Address of Points loop
      for Mode in Access_Mode loop
         for State in Backing_State loop
            for Write in Boolean loop
               for Execute in Boolean loop
                  for Protection in Boolean loop
                     for Used in 0 .. 3 loop
                        for Capacity in 0 .. 3 loop
                           for Quota in 0 .. 3 loop
                              if Address < 4096 or Address >= 8192
                                or Mode = Guard or Execute or Protection
                                or (Write and Mode = Read_Only)
                                or State = Retiring or State = Quarantined
                              then
                                 Expected := Denied;
                              elsif State = Resident then
                                 Expected := Retry_Resident;
                              elsif Used >= Capacity then
                                 Expected := Tracking_Full;
                              elsif Quota > 0 and Used >= Quota then
                                 Expected := Quota_Full;
                              else
                                 Expected := Needs_Backing;
                              end if;
                              Expect
                                (Check (4096, 8192, Address, Mode, State,
                                        Write, Execute, Protection,
                                        Used, Capacity, Quota), Expected);
                           end loop;
                        end loop;
                     end loop;
                  end loop;
               end loop;
            end loop;
         end loop;
      end loop;
   end loop;
   -- Invalid ranges must fail even with a claimed resident page.
   for First of Points loop
      for Limit of Points loop
         if First = 0 or First >= Limit or Limit > User_Limit
           or First mod 4096 /= 0 or Limit mod 4096 /= 0
         then
            Expect (Check (First, Limit, First, Read_Write, Resident,
                           False, False, False, 0, 1, 0), Denied);
         end if;
      end loop;
   end loop;
   Expect (Check (User_Limit - 4096, User_Limit, User_Limit - 1,
                  Read_Write, Absent, True, False, False,
                  Natural'Last - 1, Natural'Last, 0), Needs_Backing);
   Expect (Check (4096, 8192, 4096, Read_Write, Absent,
                  False, False, False,
                  Natural'Last, Natural'Last, 0), Tracking_Full);
   Expect (Check (4096, 8192, 4096, Read_Only, Resident,
                  False, False, False,
                  Natural'Last, 0, 1), Retry_Resident);
   Put_Line ("PASS owned demand admission" & Count'Image & " cases");
end Demand_Tests;
