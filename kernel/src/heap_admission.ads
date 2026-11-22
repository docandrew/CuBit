with Interfaces; use Interfaces;

--  Pure admission only: no allocation, address overlays or kernel locks.
package Heap_Admission with SPARK_Mode, Pure is
   Page_Size : constant Unsigned_64 := 4096;
   User_Limit : constant Unsigned_64 := 16#0000_8000_0000_0000#;
   type Status is (Ready, Invalid_Range, Address_Limit,
                   Tracking_Limit, Quota_Limit);
   -- Preserve all existing tracking headroom (including demand-mapped stack
   -- pages). Zero means the expanded capacity is not representable.
   function Expanded_Capacity
     (Current : Positive; Additional : Natural) return Natural
     with Post =>
       (if Additional <= Natural'Last - Current then
          Expanded_Capacity'Result = Current + Additional
        else Expanded_Capacity'Result = 0);
   type Growth_Plan (Result : Status := Invalid_Range) is record
      case Result is
         when Ready =>
            Old_Break, New_Break, First_Page, Page_Count : Unsigned_64;
         when others => null;
      end case;
   end record;

   function Plan
     (Heap_Start, Current_Break, Exclusive_Limit, Increment : Unsigned_64;
      Used_Frames, Tracking_Capacity, Quota : Unsigned_64) return Growth_Plan
     with Post =>
       (if Plan'Result.Result = Ready then
          Plan'Result.Old_Break = Current_Break and then
          Plan'Result.New_Break >= Current_Break and then
          Plan'Result.New_Break <= Exclusive_Limit and then
          Plan'Result.New_Break - Current_Break = Increment and then
          Plan'Result.First_Page =
            ((Current_Break + Page_Size - 1) / Page_Size) * Page_Size and then
          Plan'Result.Page_Count =
            (((Plan'Result.New_Break + Page_Size - 1) / Page_Size) * Page_Size -
               Plan'Result.First_Page) / Page_Size and then
          (if Plan'Result.Page_Count > 0 then
             Used_Frames <= Tracking_Capacity and then
             Plan'Result.Page_Count <= Tracking_Capacity - Used_Frames and then
             (Quota = 0 or else
                (Used_Frames <= Quota and then
                 Plan'Result.Page_Count <= Quota - Used_Frames))));
   --  Zero quota means no policy quota, not unlimited tracking storage.
   --  Limits must be page aligned. Failed admission has no partial plan.
end Heap_Admission;
