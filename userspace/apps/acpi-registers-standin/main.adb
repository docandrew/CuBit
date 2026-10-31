------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Test-only stand-in for acpi-backend.svc's receive endpoint (role 35,
--  "acpi-registers"): registers the role the way the backend will and
--  answers a probe with the kernel-stamped badge of the request, so the
--  headless acpi-hardware test sees which scope a client's endpoint carries.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with ACPI_Register_Roles;
with Startup;

procedure Main is
   use ASCII;
   --  The test's probe (not a register operation): the reply's first word
   --  is the request's authority tag.
   Probe_Label   : constant Unsigned_32 := 16#AC35#;
   REPLY_OK      : constant Unsigned_32 := 16#F000#;
   REPLY_UNKNOWN : constant Unsigned_32 := 16#F001#;
   From : Process_ID;
   Request, Answer : Message;
   Ignore : Unsigned_64;
begin
   if registerDriver (DRIVER_ACPI_REGISTERS, Startup.Registration) = Unsigned_64'Last then
      debugPrint ("acpi-registers-standin: registration failed" & LF);
      return;
   end if;
   debugPrint ("acpi-registers-standin: registered" & LF);
   loop
      receive (From, Request);
      Answer := NULL_MESSAGE;
      if Request.tag.label = Probe_Label then
         debugPrint ("acpi-registers-standin: probe frontend=" &
                     (if ACPI_Register_Roles.Is_Frontend (ACPI_Register_Roles.Role_Tag (Request.authorityTag))
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
