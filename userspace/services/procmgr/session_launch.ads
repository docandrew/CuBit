------------------------------------------------------------------------------
--  CuBit procmgr
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Launch-into-S (CAP-012, docs/authority-model.md section 7.5): what a
--  program launched into a session supervisor gets, and who may launch.
--
--  @description
--  A session supervisor S is a node in procmgr, not a process. A requester
--  (the Desktop, the shell, a console, a CCL script, ccl-remote) holds a
--  Launch badge on procmgr's endpoint naming S. procmgr starts the program
--  into S and grants it its install approval within S's envelope; the
--  requester gets the child's identity (observe, stop, its exit), never the
--  child's authority, and adds only what it introduces from its own
--  holdings (its Launch badge, delegated places, outlet rings).
--
--  Stand-ins until install approvals (SYS-003, SIGN-001) and security
--  contexts (CTX-001) exist, stated here so they are not mistaken for the
--  design:
--  * the install approval is the program's manifest in the image (its
--    builder approved it by shipping it) plus the startup plan's rules;
--  * there is one S, the user session; its envelope is User_Session below:
--    ordinary services, the observation kinds a viewer needs, an offerable
--    self endpoint, render, outbound TCP connections, and the declared
--    filesystem, config, TLS and metrics scopes (no context narrows them
--    yet). Never startup-only control (audio, clock, disks, installer,
--    network configuration, display capture, power, ACPI), notifications,
--    the framebuffer, hardware, kernel objects or publications.
--
--  A request the envelope withholds: optional, the child starts without it;
--  required, the launch is refused with Request_Withheld naming it.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Launch_Attenuation;

package Session_Launch with SPARK_Mode, Pure is

   package LAtt renames Launch_Attenuation;

   --  The Launch badges' roles (userspace/ccl/catalogs/native-runtime-
   --  services.ccl) and the tags procmgr stamps on them.
   Launch_Role        : constant := 45;
   Remote_Launch_Role : constant := 46;
   Session_Launch_Tag : constant Unsigned_64 := 16#4C41_554E_4348_0001#;
   Remote_Launch_Tag  : constant Unsigned_64 := 16#4C41_554E_4348_0002#;

   type Badge is (No_Badge, Session_Badge, Remote_Badge);

   function Badge_Of (Tag : Unsigned_64) return Badge is
     (if Tag = Session_Launch_Tag then Session_Badge
      elsif Tag = Remote_Launch_Tag then Remote_Badge
      else No_Badge);

   function Tag_Of (Role : Unsigned_64) return Unsigned_64 is
     (if Role = Remote_Launch_Role then Remote_Launch_Tag else Session_Launch_Tag)
   with Pre => Role in Launch_Role | Remote_Launch_Role;

   --  The typed hook for launches a remote requester asks for (7.5 item 3):
   --  whether one must be confirmed on the device first. None must yet.
   function Needs_Device_Confirmation (B : Badge) return Boolean is (False);

   ---------------------------------------------------------------------------
   --  Envelopes
   ---------------------------------------------------------------------------
   --  A declared request, as procmgr classifies it.
   type Request_Kind is
     (Service,                --  an ordinary service role
      Observation,            --  log, metric or process observation, log control
      Startup_Only_Control,   --  audio, clock, disk, installer, network
                              --  configuration, display capture, power, ACPI
      Launch_Badge,           --  a Launch badge: only by introduction
      Offerable_Self,
      Render,
      Outbound_Network,       --  TCP connections within Broad_Outbound_TCP
      Other_Network,          --  listening, UDP, anything narrower in action
      Framebuffer,
      Notification,
      Kernel_Object,          --  firmware snapshot, trace, test console, power,
                              --  VM creation
      Hardware,               --  devices, ports, interrupts, DMA, ACPI windows
      Publication);

   type Kind_Set is array (Request_Kind) of Boolean;

   type Envelope is record
      Allows         : Kind_Set := [others => False];
      --  The most rights a service endpoint in it may carry.
      Service_Rights : LAtt.Rights_Mask := 0;
   end record;

   User_Session : constant Envelope :=
     (Allows =>
        [Service | Observation | Offerable_Self | Render | Outbound_Network => True,
         others => False],
      Service_Rights => LAtt.Read_Right + LAtt.Write_Right);

   --  The request is inside the envelope: its kind, and for a service, its
   --  rights.
   function Within (E : Envelope; K : Request_Kind; Asked : LAtt.Rights_Mask) return Boolean is
     (E.Allows (K)
      and then (if K in Service | Observation then LAtt.Covers (E.Service_Rights, Asked)));

   --  Launch_Gains_Nothing's first half, in this unit's terms: nothing a
   --  launch grants is a Launch badge unless the requester introduces it.
   procedure Lemma_No_Badge_From_Envelope (E : Envelope; Asked : LAtt.Rights_Mask)
   with Ghost, Global => null,
        Pre  => not E.Allows (Launch_Badge),
        Post => not Within (E, Launch_Badge, Asked);

   --  The user session never grants control, hardware or kernel objects.
   procedure Lemma_User_Session_Bounded (K : Request_Kind; Asked : LAtt.Rights_Mask)
   with Ghost, Global => null,
        Pre  => K in Startup_Only_Control | Launch_Badge | Other_Network | Framebuffer
                     | Notification | Kernel_Object | Hardware | Publication,
        Post => not Within (User_Session, K, Asked);

end Session_Launch;
