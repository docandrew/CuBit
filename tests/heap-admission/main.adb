with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Heap_Admission; use Heap_Admission;
procedure Main is
   Base : constant Unsigned_64 := 16#40_0000#;
   procedure Check (Growth, Used, Capacity, Quota : Unsigned_64; Expected : Status) is
      P : constant Growth_Plan := Plan (Base, Base, Base + 16 * Page_Size,
                                       Growth, Used, Capacity, Quota);
   begin
      pragma Assert (P.Result = Expected);
   end Check;
begin
   Check (0, 8192, 8192, 0, Ready);
   Check (Page_Size, 8192, 8192, 0, Tracking_Limit);
   Check (2 * Page_Size, 8191, 8192, 0, Tracking_Limit);
   Check (Page_Size, 8191, 8192, 0, Ready);
   Check (2 * Page_Size, 99, 8192, 100, Quota_Limit);
   Check (Page_Size, 99, 8192, 100, Ready);
   Check (Page_Size, 101, 8192, 100, Quota_Limit);
   Check (Page_Size, 8193, 8192, 0, Tracking_Limit);
   Check (Unsigned_64'Last, 0, 8192, 0, Address_Limit);
   for Offset in Unsigned_64 range 0 .. Page_Size - 1 loop
      for Growth in Unsigned_64 range 0 .. 2 * Page_Size loop
         declare
            Old : constant Unsigned_64 := Base + Offset;
            P : constant Growth_Plan :=
              Plan (Base, Old, Base + 4 * Page_Size, Growth, 0, 8, 0);
            Count : Unsigned_64 := 0;
         begin
            for Index in Unsigned_64 range 0 .. 3 loop
               if Base + Index * Page_Size >= Old and then
                 Base + Index * Page_Size < Old + Growth
               then Count := Count + 1; end if;
            end loop;
            pragma Assert (P.Result = Ready);
            pragma Assert (P.Page_Count = Count);
            pragma Assert (P.Old_Break = Old and then P.New_Break = Old + Growth);
            pragma Assert (P.First_Page mod Page_Size = 0 and then P.First_Page >= Old);
         end;
      end loop;
   end loop;
   pragma Assert (Plan (Base, Base - 1, User_Limit, 1, 0, 8, 0).Result = Invalid_Range);
   pragma Assert (Plan (Base, Base, User_Limit + Page_Size, 0, 0, 8, 0).Result = Invalid_Range);
   pragma Assert (Plan (Base, Base, Base + 1, 0, 0, 8, 0).Result = Invalid_Range);
   declare
      P : constant Growth_Plan := Plan (Base, User_Limit - 1, User_Limit, 1, 8, 8, 8);
   begin
      pragma Assert (P.Result = Ready and then P.Page_Count = 0 and then P.New_Break = User_Limit);
   end;
   Put_Line ("HEAP ADMISSION PASS: bounds, whole-request capacity/quota and exhaustive page offsets");
end Main;
