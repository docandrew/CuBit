------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  acpi-one-service: the one acpi.svc answers ACPI_Requests.Read_Metrics
--  on its observer endpoint once bring-up reaches registration: phase
--  Complete, every expected table installed, and a revision that does not
--  move between two reads.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with Startup;

procedure Main is
   use ASCII;

   --  ACPI_Requests.Read_Metrics and ACPI_Endpoint.Reply_OK.
   READ_METRICS : constant Unsigned_32 := 0;
   REPLY_OK     : constant Unsigned_32 := 16#F000#;
   --  ACPI_Requests phase representation once registration is done.
   PHASE_COMPLETE : constant Unsigned_64 := 2;
   --  q35's 7 tables plus the ACPI-ONE-001 regression SSDT
   --  (tests/headless acpi-one-service, -acpitable).
   Q35_MINIMUM_TABLES : constant Unsigned_64 := 8;

   Call_Deadline_Ms : constant := 2_000;
   --  acpi.svc runs AML startup before it registers; each attempt waits
   --  Call_Deadline_Ms, so this bounds the wait at one minute.
   Attempts : constant := 30;

   Failures : Natural := 0;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if Condition then
         debugPrint ("acpi-metrics-check: " & Name & " PASS" & LF);
      else
         Failures := Failures + 1;
         debugPrint ("acpi-metrics-check: " & Name & " FAIL" & LF);
      end if;
   end Check;

   function Read return Message is
      Request : Message := NULL_MESSAGE;
   begin
      Request.tag.label := READ_METRICS;
      Request.tag.length := 4;
      Request.tag := capCall (Startup.Acpi, Request, Deadline_After (Call_Deadline_Ms));
      return Request;
   end Read;

   function Complete (Reply : Message) return Boolean is
     (Reply.tag.label = REPLY_OK and then Reply.tag.length = 4
      and then Reply.words (1) = PHASE_COMPLETE);

   First : Message := NULL_MESSAGE;
begin
   debugPrint ("acpi-metrics-check: starting" & LF);
   for Attempt in 1 .. Attempts loop
      First := Read;
      exit when Complete (First);
   end loop;
   Check (First.tag.label = REPLY_OK and then First.tag.length = 4
          and then First.tag.protocol = 0 and then First.tag.reserved = 0,
          "metrics reply well formed");
   Check (Complete (First), "phase complete");
   -- The harness checks the exact census (8 normally,9 with CMOS SSDT).
   debugPrint ("acpi-metrics-check: tables captured=" & First.words (2)'Image &
     " installed=" & First.words (3)'Image & LF);
   Check (First.words (2) >= Q35_MINIMUM_TABLES and then First.words (3) = First.words (2),
          "every expected table installed");
   declare
      Second : constant Message := Read;
   begin
      Check (Complete (Second) and then Second.words (0) = First.words (0),
             "revision stable across reads");
   end;
   if Failures = 0 then
      debugPrint ("TEST: PASS acpi-metrics" & LF);
   else
      debugPrint ("TEST: FAIL acpi-metrics" & Natural'Image (Failures) & LF);
   end if;
end Main;
