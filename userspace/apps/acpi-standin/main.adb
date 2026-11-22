------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Test-only stand-in for acpi.svc's catalog service "acpi": registers the
--  role the way acpi.svc does and answers a probe with the kernel-stamped
--  badge of the request, so the headless firmware-snapshot test sees that a
--  declared client's endpoint carries ACPI_Protocol.Observer_Tag.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with ACPI_Protocol;
with Startup;

procedure Main is
   use ASCII;
   --  The test's probe (not an ACPI protocol operation): the reply's first
   --  word is the request's authority tag.
   Probe_Label : constant Unsigned_32 := 16#AC01#;
   REPLY_OK      : constant Unsigned_32 := 16#F000#;
   REPLY_UNKNOWN : constant Unsigned_32 := 16#F001#;
   From : Process_ID;
   Request, Answer : Message;
   Ignore : Unsigned_64;
begin
   if registerDriver (DRIVER_ACPI, Startup.Registration) = Unsigned_64'Last then
      debugPrint ("acpi-standin: registration failed" & LF);
      return;
   end if;
   debugPrint ("acpi-standin: registered" & LF);
   loop
      receive (From, Request);
      Answer := NULL_MESSAGE;
      if Request.tag.label = Probe_Label then
         debugPrint ("acpi-standin: probe observer=" &
                     (if ACPI_Protocol.Is_Observer (ACPI_Protocol.Role_Tag (Request.authorityTag))
                      then "yes" else "no") & LF);
         Answer.tag := (label => REPLY_OK, length => 1, protocol => 0, ipc => <>, reserved => 0);
         Answer.words (0) := Request.authorityTag;
      else
         Answer.tag := (label => REPLY_UNKNOWN, length => 1, protocol => 0, ipc => <>, reserved => 0);
         Answer.words (0) := Unsigned_64 (Request.tag.label);
      end if;
      Ignore := reply (From, Answer);
   end loop;
end Main;
