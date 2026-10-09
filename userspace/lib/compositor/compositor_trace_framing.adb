package body Compositor_Trace_Framing with SPARK_Mode is
   use type A.Content;
   function Seal (Data : A.Chunk) return A.Chunk with
     Post => A.Prefix (Seal'Result) = A.Prefix (Data) and then
       Seal'Result (31) = A.Checksum (A.Prefix (Data));
   function Seal (Data : A.Chunk) return A.Chunk is
      Result : A.Chunk := Data;
   begin
      Result (31) := A.Checksum (A.Prefix (Result));
      return Result;
   end Seal;
   function Header (M : Metadata) return A.Chunk is
     (Seal ((0 => Header_Magic, 1 => 256, 2 => M.Capture_ID,
       3 => M.Endpoint, 4 => M.Started_Us, 5 => Word (M.Budget),
       6 => 1, others => 0)));
   procedure Start (S : out State; Data : A.Chunk) is
   begin
      S := (Current => Rejected, others => <>);
      if Header_Valid (Data) then
         S := (Reading, (Data (2), Data (3), Data (4), Count (Data (5))), 0, Data (31));
      end if;
   end Start;
   function Event_Chunk (S : State; Value : A.S.Capture) return A.Chunk is
     (A.Encode (Word (S.Used) + 1, Value));
   function Footer (S : State; Ended_Us : Word; Reason : Stop_Reason;
      Statistics : A.S.Statistics) return A.Chunk is
     (Seal ((0 => Footer_Magic, 1 => 256, 2 => S.Meta.Capture_ID,
       3 => S.Meta.Endpoint, 4 => Word (S.Used), 5 => Ended_Us,
       6 => Word (Stop_Reason'Pos (Reason)), 7 => S.Chain,
       8 => Statistics.Skipped_Rows, 9 => Statistics.Rejected_Rows,
       10 => Statistics.Abandoned_Events, 11 => Statistics.Emitted_Events,
       12 => Statistics.Endpoint_Mismatches, others => 0)));
   function Footer_Matches (S : State; Data : A.Chunk) return Boolean is
     (Data (0) = Footer_Magic and then Data (1) = 256 and then
      Data (2) = S.Meta.Capture_ID and then Data (3) = S.Meta.Endpoint and then
      Data (4) = Word (S.Used) and then Data (5) /= Word'Last and then
      Data (5) >= S.Meta.Started_Us and then Data (6) <= 2 and then
      (if Data (6) = 1 then S.Used = S.Meta.Budget) and then
      Data (7) = S.Chain and then Data (11) >= Word (S.Used) and then
      (for all I in 13 .. 30 => Data (I) = 0) and then
      Data (31) = A.Checksum (A.Prefix (Data)));
   procedure Feed (S : in out State; Data : A.Chunk; Accepted_Event : out Boolean) is
   begin
      Accepted_Event := False;
      if S.Current /= Reading then S.Current := Rejected; return; end if;
      if Data (0) = Footer_Magic then
         S.Current := (if Footer_Matches (S, Data) then Footer_Seen else Rejected);
      else
         declare D : constant A.Decoded := A.Decode (Data); begin
            if D.Success and then S.Used < S.Meta.Budget and then
              D.Sequence = Word (S.Used) + 1 and then D.Value.Incarnation = S.Meta.Endpoint
            then
               S.Used := S.Used + 1;
               S.Chain := (S.Chain xor Data (31)) * 16#100_0000_01B3#;
               Accepted_Event := True;
            else S.Current := Rejected;
            end if;
         end;
      end if;
   end Feed;
   procedure End_Of_File (S : in out State; Trailing_Bytes : Natural) is
   begin
      if S.Current = Footer_Seen and Trailing_Bytes = 0 then
         S.Current := Complete;
      elsif S.Current = Reading then
         S.Current := Incomplete;
      else
         S.Current := Rejected;
      end if;
   end End_Of_File;
end Compositor_Trace_Framing;
