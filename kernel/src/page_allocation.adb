package body Page_Allocation with SPARK_Mode is
   procedure Acquire (Frame : out Frame_Address; Outcome : out Result) is
      Candidate : Frame_Address;
      Accepted : Boolean;
   begin
      Frame := 0;
      Allocate (Candidate);
      if Candidate = 0 then
         Outcome := Physical_Memory_Exhausted;
         return;
      end if;
      Track (Candidate, Accepted);
      if not Accepted then
         Release_Frame (Candidate);
         Outcome := Tracking_Storage_Exhausted;
         return;
      end if;
      Claim (Candidate, Accepted);
      if not Accepted then
         Forget;
         Release_Frame (Candidate);
         Outcome := Frame_Ownership_Failed;
         return;
      end if;
      Map (Candidate, Accepted);
      if not Accepted then
         Forget;
         Release_Frame (Candidate);
         Outcome := Page_Table_Exhausted;
         return;
      end if;
      Frame := Candidate;
      Outcome := Page_Added;
   end Acquire;
end Page_Allocation;
