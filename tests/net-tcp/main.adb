--  Executable checks for userspace/net/src (proofs: gnatprove on this
--  project; see README.md).
with Ada.Text_IO;    use Ada.Text_IO;
with Interfaces;     use Interfaces;
with TCP_Sequence;   use TCP_Sequence;
with TCP_Acceptance; use TCP_Acceptance;
with TCP_RTO;

procedure Main is
   Failures : Natural := 0;

   procedure Check (OK : Boolean; Name : String) is
   begin
      if not OK then
         Put_Line ("FAIL " & Name);
         Failures := Failures + 1;
      end if;
   end Check;

   First : Seq;
   Skip, Count : Segment_Length;
begin
   --  Sequence order, including wraparound.
   Check (Lt (1, 2) and then not Lt (2, 1), "order");
   Check (Lt (Seq'Last, 0) and then Lt (Seq'Last - 5, 3), "order across wrap");
   Check (not Lt (0, Half) and then not Lt (Half, 0), "half-space is unordered");
   Check (Le (7, 7) and then not Lt (7, 7), "reflexive Le, irreflexive Lt");
   Check (In_Window (Seq'Last, Seq'Last - 1, 4) and then In_Window (1, Seq'Last - 1, 4)
          and then not In_Window (2, Seq'Last - 1, 4), "window across wrap");

   --  RFC 9293 3.10.7.4, the four cases.
   Check (Acceptable (100, 0, 100, 0) and then not Acceptable (101, 0, 100, 0),
          "empty segment, zero window");
   Check (Acceptable (150, 0, 100, 100) and then not Acceptable (200, 0, 100, 100),
          "empty segment, open window");
   Check (not Acceptable (100, 10, 100, 0), "data into a zero window");
   Check (Acceptable (95, 10, 100, 100) and then Acceptable (195, 10, 100, 100)
          and then not Acceptable (80, 10, 100, 100) and then not Acceptable (200, 10, 100, 100),
          "data overlapping either window edge");

   --  Trimming.
   Trim (95, 10, 100, 100, First, Skip, Count);
   Check (First = 100 and then Skip = 5 and then Count = 5, "drop the old prefix");
   Trim (195, 10, 100, 100, First, Skip, Count);
   Check (First = 195 and then Skip = 0 and then Count = 5, "cut at the right edge");
   Trim (Seq'Last - 2, 10, 2, 100, First, Skip, Count);
   Check (First = 2 and then Skip = 5 and then Count = 5, "trim across wrap");

   --  RFC 6298.
   declare
      E : TCP_RTO.Estimator;
   begin
      Check (E.RTO = 1_000, "initial RTO");
      TCP_RTO.Update (E, 100);   --  SRTT 100, RTTVAR 50, RTO 300
      Check (E.SRTT = 100 and then E.RTTVAR = 50 and then E.RTO = 300, "first sample");
      TCP_RTO.Update (E, 100);   --  RTTVAR 37, SRTT 100, RTO 248
      Check (E.SRTT = 100 and then E.RTTVAR = 37 and then E.RTO = 248, "second sample");
      TCP_RTO.Update (E, 1);     --  small RTT: clamped at the minimum
      for I in 1 .. 20 loop TCP_RTO.Update (E, 1); end loop;
      Check (E.RTO = TCP_RTO.Minimum_RTO, "minimum RTO");
      for I in 1 .. 20 loop TCP_RTO.Back_Off (E); end loop;
      Check (E.RTO = TCP_RTO.Maximum_RTO, "backoff saturates");
   end;

   Put_Line (if Failures = 0 then "NET-TCP: PASS" else "NET-TCP: FAIL");
end Main;
