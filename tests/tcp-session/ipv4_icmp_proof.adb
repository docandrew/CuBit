package body IPv4_ICMP_Proof with SPARK_Mode is
   procedure Count (N : in out Natural) is
   begin
      if N < Natural'Last then
         N := N + 1;
      end if;
   end Count;
   procedure Send (Frame : IPv4_Header.Bytes) is
   begin
      Count (Frames);
   end Send;
   procedure Echo_Answered (From : IPv4_Header.Address; Sequence : Unsigned_16) is
      pragma Unreferenced (From, Sequence);
   begin
      Count (Answers);
   end Echo_Answered;
   procedure Error_Arrived (Message : IPv4_Header.Bytes) is
      pragma Unreferenced (Message);
   begin
      Count (Errors);
   end Error_Arrived;
end IPv4_ICMP_Proof;
