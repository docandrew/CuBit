package body Heap_Admission with SPARK_Mode is
   function Plan
     (Heap_Start, Current_Break, Exclusive_Limit, Increment : Unsigned_64;
      Used_Frames, Tracking_Capacity, Quota : Unsigned_64) return Growth_Plan
   is
      New_Break, First_Page, Last_Page, Count : Unsigned_64;
   begin
      if Exclusive_Limit > User_Limit or else
        Exclusive_Limit mod Page_Size /= 0 or else
        Heap_Start mod Page_Size /= 0 or else
        Heap_Start > Current_Break or else Current_Break > Exclusive_Limit
      then return (Result => Invalid_Range); end if;

      --  Check by subtraction before adding a userspace-supplied increment.
      --  In particular, all-ones is not treated as signed shrinking of heap.
      if Increment > Exclusive_Limit - Current_Break then
         return (Result => Address_Limit);
      end if;
      New_Break := Current_Break + Increment;
      First_Page := ((Current_Break + Page_Size - 1) / Page_Size) * Page_Size;
      Last_Page := ((New_Break + Page_Size - 1) / Page_Size) * Page_Size;
      Count := (Last_Page - First_Page) / Page_Size;
      if Count > 0 then
         if Used_Frames > Tracking_Capacity or else
           Count > Tracking_Capacity - Used_Frames
         then return (Result => Tracking_Limit); end if;
         if Quota /= 0 and then
           (Used_Frames > Quota or else Count > Quota - Used_Frames)
         then return (Result => Quota_Limit); end if;
      end if;
      return (Result => Ready, Old_Break => Current_Break,
              New_Break => New_Break, First_Page => First_Page, Page_Count => Count);
   end Plan;
end Heap_Admission;
