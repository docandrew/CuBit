------------------------------------------------------------------------------
--  Hosted tests of SPAWN's admission rules (kernel/src/spawn_admission.ads):
--  every rights set, kind and object of a spawn authority, against an
--  independent statement of the three rules, and against the rule the
--  kernel had before CAP-012 (any CAP_PROCESS with Execute spawned, the
--  handles always carried RWXG, and any live process could be named the
--  parent), which these cases show admits what the new rules refuse.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Capabilities; use Capabilities;
with Spawn_Admission; use Spawn_Admission;

procedure Main is
   Failures : Natural := 0;
   Checks   : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   Caller_Ref : constant Unsigned_64 := 7;
   Child_Ref  : constant Unsigned_64 := 9;
   Other_Ref  : constant Unsigned_64 := 11;
   Live_Gen   : constant Generation := 3;

   function Process_Cap (Rights : CapabilityRights; Ref : Unsigned_64;
                         Gen : Generation := Live_Gen) return Capability is
     ((capType => CAP_PROCESS, rights => Rights,
       authorityTag => NO_AUTHORITY_TAG, object => (ref => Ref, param => 0),
       gen => Gen));

   function Rights_Of (Mask : Unsigned_8) return CapabilityRights is
     ((RIGHT_READ    => (Mask and 1) /= 0,
       RIGHT_WRITE   => (Mask and 2) /= 0,
       RIGHT_EXECUTE => (Mask and 4) /= 0,
       RIGHT_GRANT   => (Mask and 8) /= 0,
       RIGHT_REVOKE  => (Mask and 16) /= 0));

   RWXG : constant CapabilityRights := Control_Ceiling;
   RX   : constant CapabilityRights :=
     (RIGHT_READ | RIGHT_EXECUTE => True, others => False);
   XG   : constant CapabilityRights :=
     (RIGHT_EXECUTE | RIGHT_GRANT => True, others => False);

   --  The kernel's rule before CAP-012.
   function Old_Admits (A : Capability) return Boolean is
     (A.capType = CAP_PROCESS and then A.rights (RIGHT_EXECUTE));
   Old_Control : constant CapabilityRights := RWXG;

   Kinds : constant array (1 .. 4) of CapabilityType :=
     [CAP_PROCESS, CAP_ENDPOINT, CAP_CSPACE, CAP_NULL];
   Refs  : constant array (1 .. 3) of Unsigned_64 := [Every_Process, Child_Ref, Other_Ref];
begin
   --  Every authority, exhaustively: rule 1 is exactly "a CAP_PROCESS with
   --  Execute naming every process"; rule 2's handles are within it.
   for Kind of Kinds loop
      for Ref of Refs loop
         for Mask in Unsigned_8 range 0 .. 31 loop
            declare
               A : constant Capability :=
                 (capType => Kind, rights => Rights_Of (Mask),
                  authorityTag => NO_AUTHORITY_TAG,
                  object => (ref => Ref, param => 0), gen => Live_Gen);
               C : constant CapabilityRights := Control_Rights (A);
               E : constant CapabilityRights := Endpoint_Rights (A);
            begin
               Check (Is_Spawn_Authority (A) =
                        (Kind = CAP_PROCESS and then (Mask and 4) /= 0 and then Ref = 0),
                      "rule 1 for mask" & Mask'Image);
               Check (isSubsetOf (C, A.rights) and then not C (RIGHT_REVOKE),
                      "control within the authority, mask" & Mask'Image);
               Check (C = (A.rights and RWXG), "control keeps the authority's rights");
               Check (Gives_Mailbox (A) = ((Mask and 8) /= 0), "mailbox only with Grant");
               Check (isSubsetOf (E, A.rights) and then not E (RIGHT_EXECUTE),
                      "endpoint within the authority");
               Check (Within_Authority (A), "within the authority");
            end;
         end loop;
      end loop;
   end loop;

   --  Rule 1: a child's Control, with every right, does not spawn; the
   --  pre-CAP-012 rule let it.
   declare
      Child_Control : constant Capability := Process_Cap (RWXG, Child_Ref);
   begin
      Check (not Is_Spawn_Authority (Child_Control), "a child's Control never spawns");
      Check (Old_Admits (Child_Control), "the old rule admitted a child's Control");
      Check (Is_Spawn_Authority (Process_Cap (RWXG, Every_Process)), "the wildcard spawns");
      Check (Is_Spawn_Authority (Process_Cap (RX, Every_Process)),
             "an Execute wildcard without Grant spawns");
      Check (not Is_Spawn_Authority (Process_Cap (XG and not Rights_Of (4), Every_Process)),
             "a wildcard without Execute does not");
   end;

   --  Rule 2: an Execute-only authority gets no Write, no Grant, no mailbox.
   declare
      A : constant Capability := Process_Cap (RX, Every_Process);
   begin
      Check (Control_Rights (A) = RX, "RX authority: RX control");
      Check (not Gives_Mailbox (A), "RX authority: no endpoint, no CSPACE");
      Check (not isSubsetOf (Old_Control, A.rights),
             "the old rule handed an RX authority RWXG control");
      Check (Control_Rights (Process_Cap (XG, Every_Process)) = XG
             and then Gives_Mailbox (Process_Cap (XG, Every_Process))
             and then Endpoint_Rights (Process_Cap (XG, Every_Process)) = Cspace_Rights,
             "XG authority: XG control, a Grant-only endpoint");
      Check (Control_Rights (Process_Cap (ALL_RIGHTS, Every_Process)) = RWXG,
             "Revoke is never passed to the control handle");
   end;

   --  Rule 3: the caller is always admitted as parent; another process
   --  only under Grant over it (the wildcard, or a current handle to it).
   declare
      Wild_RWXG : constant Capability := Process_Cap (RWXG, Every_Process);
      Wild_RX   : constant Capability := Process_Cap (RX, Every_Process);
   begin
      Check (Parent_Admitted (Wild_RX, True, Caller_Ref, Live_Gen), "the caller as parent");
      Check (not Parent_Admitted (Wild_RX, False, Other_Ref, Live_Gen),
             "no Grant: another parent refused");
      Check (Parent_Admitted (Wild_RWXG, False, Other_Ref, Live_Gen),
             "wildcard Grant: any parent");
      Check (Holds_Grant_Over (Process_Cap (XG, Other_Ref), Other_Ref, Live_Gen),
             "Grant over exactly that process");
      Check (not Holds_Grant_Over (Process_Cap (XG, Other_Ref), Other_Ref, Live_Gen + 1),
             "a stale handle is no Grant over the process's later life");
      Check (not Holds_Grant_Over (Process_Cap (XG, Child_Ref), Other_Ref, Live_Gen),
             "Grant over one process is not Grant over another");
      Check (not Holds_Grant_Over
               ((capType => CAP_ENDPOINT, rights => RWXG, authorityTag => NO_AUTHORITY_TAG,
                 object => (ref => Every_Process, param => 0), gen => Live_Gen),
                Other_Ref, Live_Gen),
             "an endpoint is no process authority");
   end;

   Put_Line ("spawn-admission:" & Checks'Image & " checks," & Failures'Image & " failed");
   if Failures > 0 then
      raise Program_Error;
   end if;
end Main;
