package body Observatory_Trace_View with SPARK_Mode is
   procedure Start (S : out State; Header : A.Chunk; Wanted : Page_Number) is
   begin
      S := (Selected => Wanted, others => <>);
      F.Start (S.Archive, Header);
   end Start;
   procedure Feed (S : in out State; Data : A.Chunk) is
      Accepted : Boolean;
      D : A.Decoded;
      First : constant Natural := S.Selected * Capacity;
   begin
      S.EOF_Seen := False;
      F.Feed (S.Archive, Data, Accepted);
      if Accepted then
         D := A.Decode (Data);
         if D.Success then
            A.Lemma_Valid (Data);
            S.Saw_Loss := S.Saw_Loss or D.Value.Producer_Dropped /= 0 or D.Value.Batch_Gaps /= 0;
            if F.Events (S.Archive) > First and F.Events (S.Archive) <= First + Capacity and S.Used < Capacity then
               S.Rows (S.Used + 1) := D.Value;
               S.Used := S.Used + 1;
            end if;
         end if;
      elsif F.Status (S.Archive) = F.Footer_Seen then
         S.Hit_Budget := Data (6) = 1;
         S.Saw_Failure := Data (6) = 2;
         S.Saw_Loss := S.Saw_Loss or (Data (8) or Data (9) or Data (10) or Data (12)) /= 0;
      end if;
   end Feed;
   procedure Finish (S : in out State; Trailing_Bytes : Natural) is
   begin
      F.End_Of_File (S.Archive, Trailing_Bytes);
      S.EOF_Seen := True;
   end Finish;
end Observatory_Trace_View;
