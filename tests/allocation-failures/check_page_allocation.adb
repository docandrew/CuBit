with Ada.Text_IO;
with Interfaces; use Interfaces;
with Page_Allocation; use Page_Allocation;

-- Instantiate the SAME acquisition sequence as Process.tryAddPage, replacing
-- hardware/ownership operations with an independently checked resource model.
procedure Check_Page_Allocation is
   type Failure_Point is (No_Failure, Physical, Tracking, Ownership, Page_Table);
   Fail_At : Failure_Point;
   Allocated, Tracked, Claimed, Published : Boolean := False;
   Releases : Natural := 0;
   Forgotten : Natural := 0;
   Frame : Unsigned_64;
   Outcome : Result;

   procedure Allocate (Frame : out Unsigned_64) is
   begin
      pragma Assert (not Allocated and then not Tracked and then not Published);
      Frame := (if Fail_At = Physical then 0 else 4096);
      Allocated := Frame /= 0;
   end Allocate;
   procedure Release_Frame (Frame : Unsigned_64) is
   begin
      pragma Assert (Frame = 4096 and then Allocated and then not Tracked and then not Published);
      Allocated := False;
      Claimed := False;
      Releases := Releases + 1;
   end Release_Frame;
   procedure Track (Frame : Unsigned_64; Accepted : out Boolean) is
   begin
      pragma Assert (Frame = 4096 and then Allocated and then not Tracked);
      Accepted := Fail_At /= Tracking;
      Tracked := Accepted;
   end Track;
   procedure Forget is
   begin
      pragma Assert (Tracked and then Allocated and then not Published);
      Tracked := False;
      Forgotten := Forgotten + 1;
   end Forget;
   procedure Claim (Frame : Unsigned_64; Accepted : out Boolean) is
   begin
      pragma Assert (Frame = 4096 and then Tracked and then not Claimed);
      Accepted := Fail_At /= Ownership;
      Claimed := Accepted;
   end Claim;
   procedure Map (Frame : Unsigned_64; Accepted : out Boolean) is
   begin
      pragma Assert (Frame = 4096 and then Tracked and then Claimed and then not Published);
      Accepted := Fail_At /= Page_Table;
      Published := Accepted;
   end Map;
   procedure Acquire is new Page_Allocation.Acquire
     (Unsigned_64, Allocate, Release_Frame, Track, Forget, Claim, Map);

   procedure Successful_Retry is
   begin
      Fail_At := No_Failure;
      Acquire (Frame, Outcome);
      pragma Assert (Outcome = Page_Added and then Frame = 4096);
      pragma Assert (Allocated and then Tracked and then Claimed and then Published);
      -- Model a completed address-space retirement, not a live-page rollback.
      Published := False;
      Forget;
      Release_Frame (Frame);
   end Successful_Retry;
begin
   for Point in Physical .. Page_Table loop
      for Attempt in 1 .. 100 loop
         Fail_At := Point;
         Releases := 0;
         Forgotten := 0;
         Acquire (Frame, Outcome);
         pragma Assert (Frame = 0 and then not Allocated and then not Tracked);
         pragma Assert (not Claimed and then not Published);
         pragma Assert (Releases = (if Point = Physical then 0 else 1));
         pragma Assert (Forgotten = (if Point in Ownership | Page_Table then 1 else 0));
         pragma Assert (Outcome = (case Point is
            when Physical => Physical_Memory_Exhausted,
            when Tracking => Tracking_Storage_Exhausted,
            when Ownership => Frame_Ownership_Failed,
            when Page_Table => Page_Table_Exhausted));
         Successful_Retry;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("PASS actual page-acquisition sequence: each failure stage, cleanup, successful retries");
end Check_Page_Allocation;
