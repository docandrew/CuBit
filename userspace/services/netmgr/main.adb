------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The network manager (netmgr.svc): configuration policy for every
--  interface netstack has, however many (docs/net-link.md section 7).
--
--  - Interfaces are named by stable identity (CuBit.Net_Link.Name_Of) and
--    configured from `net.interface.<name>` (Net_Interface_Settings):
--    Automatic (DHCP for IPv4, also what an absent setting means; SLAAC is
--    netstack's), Static, or Disabled.
--  - Events come from netstack without netstack ever calling netmgr: one
--    Watch submitted and parked there, answered with each interface that
--    changed after the sequence netmgr has handled (CuBit.Net_Management).
--  - On a link that comes Up, DHCP re-validates its lease (INIT-REBOOT) or
--    discovers one; a static interface has its settings applied again.
--  - DHCP runs per interface (DHCP_Client), its messages in a page lent to
--    netstack (DHCP_Open, DHCP_Send, a parked DHCP_Await).
--
--  The interface table lives in the heap (CuAlloc) and grows with the
--  interfaces: running out of memory leaves that interface unmanaged
--  (logged), never stops netmgr.
--
--  Startup capabilities (its manifest; devmgr answers them at launch):
--    netstack     -> netstack, badged as the network manager
--    config       -> config.svc (net.*)
--    registration -> DRIVER_NETMGR registration
--    ready        -> devmgr (ready signal)
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with System.Storage_Elements; use System.Storage_Elements;
with System.Address_To_Access_Conversions;
with System.Machine_Code;

with CuBit.Messages; use CuBit.Messages;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with CuBit.Memory_Grants;
with CuBit.Grant_References;
with CuBit.Net_Address;
with CuBit.Granted_Logs;
with CuBit.Net_Link;
with CuBit.Net_Management;
with CuBit.Settings;
with CuBit.Settings.Readable;
with CuBit.Handles;
with DHCP_Client;
with Net_Interface_Settings;
with Network_Context;
with Startup;

procedure main is
   use ASCII;
   package KA renames CuBit.Kernel_ABI;
   package KC renames CuBit.Kernel_Calls;
   package MG renames CuBit.Memory_Grants;
   package NL renames CuBit.Net_Link;
   package NM renames CuBit.Net_Management;
   package NS renames Net_Interface_Settings;
   package D renames DHCP_Client;
   use type NL.Link_Condition, NL.Interface_Handle, NL.Link_Kind, NL.Generation, NS.Policy;
   use all type CuBit.Settings.Outcome;

   --  devmgr's ready signal (no words); HW-002's CuBit.Driver_Readiness
   --  names it Ready_Label.
   Ready_Label : constant Unsigned_32 := 16#FF00#;
   --  How long a configuration call to netstack may take: netstack answers
   --  these at once, so a missed deadline means it is gone or stuck.
   Call_Deadline_MS : constant Unsigned_64 := 2_000;
   Page_Bytes : constant := NM.DHCP_Page_Bytes;
   Page_Count : constant := 1;
   --  The Watch's completion token; an Await's is its interface's handle
   --  word, whose epoch is never zero, so the two never collide.
   Watch_Token : constant Unsigned_64 := 1;
   --  The metric of a DHCP lease's routes: above a static setting's
   --  default (100), so a statically configured link is preferred.
   Automatic_Metric : constant NM.Route_Metric := 200;

   function Netstack return CuBit.Handles.Endpoint is
     (if Startup.Netstack.Present then Startup.Netstack.Value else CuBit.Handles.No_Endpoint);

   function Image (V : Unsigned_64) return String is
      S : constant String := Unsigned_64'Image (V);
   begin
      return S (S'First + 1 .. S'Last);
   end Image;

   Octet : constant := 16#FF#;
   function IPv4_Image (W : NM.IPv4_Word) return String is
     (Image (Unsigned_64 (W and Octet)) & "." & Image (Unsigned_64 (Shift_Right (W, 8) and Octet)) & "." &
      Image (Unsigned_64 (Shift_Right (W, 16) and Octet)) & "." & Image (Unsigned_64 (Shift_Right (W, 24))));

   ---------------------------------------------------------------------------
   --  The interface table (CuAlloc).
   ---------------------------------------------------------------------------
   function Allocate_Zeroed (Bytes, Alignment : Unsigned_64) return Unsigned_64
     with Import, Convention => C, External_Name => "cualloc_allocate_zeroed";
   function Reallocate (Item, Bytes : Unsigned_64) return Unsigned_64
     with Import, Convention => C, External_Name => "cualloc_reallocate";
   Heap_Alignment : constant := 16;

   type Slot_State is (Free, Live, Retiring);

   type Interface_Record is record
      State     : Slot_State := Free;
      Handle    : NL.Interface_Handle;
      Name      : NL.Interface_Name;
      Setting   : NS.Interface_Setting;
      Condition : NL.Link_Condition := NL.Down;
      --  How many times netstack has seen its link come Up (a change
      --  while Up both before and after is a bounce we did not see).
      Ups       : NM.Up_Count := 0;
      Ups_Valid : Boolean := False;   --  Ups came from an event
      --  For the log lines: its kind and the address it attached with.
      Kind      : NL.Link_Kind := NL.Ethernet;
      MAC       : NL.Hardware_Address := NL.No_Address;
      Client    : D.Client;
      --  The DHCP page lent to netstack (Automatic only).
      Page      : Unsigned_64 := 0;
      Grant     : MG.Grant_Reference;
      Opened    : Boolean := False;
      Awaiting  : Boolean := False;   --  a DHCP_Await is parked
   end record;
   type Record_Access is access all Interface_Record;
   package Records is new System.Address_To_Access_Conversions (Interface_Record);

   Record_Bytes : constant Unsigned_64 :=
     Unsigned_64 ((Interface_Record'Size + System.Storage_Unit - 1) / System.Storage_Unit);
   Table_Base : Unsigned_64 := 0;
   Capacity   : Natural := 0;

   function Item (Index : Positive) return Record_Access is
     (Record_Access (Records.To_Pointer
        (To_Address (Integer_Address (Table_Base + Unsigned_64 (Index - 1) * Record_Bytes)))))
   with Pre => Index <= Capacity;

   --  A free slot (growing the table when none is), or 0 when out of memory.
   function Claim return Natural is
   begin
      for I in 1 .. Capacity loop
         if Item (I).State = Free then
            Item (I).all := (others => <>);
            return I;
         end if;
      end loop;
      declare
         Wanted : constant Natural := Natural'Max (1, Capacity * 2);
         Bytes : constant Unsigned_64 := Unsigned_64 (Wanted) * Record_Bytes;
         Grown : constant Unsigned_64 :=
           (if Table_Base = 0 then Allocate_Zeroed (Bytes, Heap_Alignment)
            else Reallocate (Table_Base, Bytes));
         First : constant Positive := Capacity + 1;
      begin
         if Grown = 0 then
            return 0;
         end if;
         Table_Base := Grown;
         Capacity := Wanted;
         for I in First .. Capacity loop
            Item (I).all := (others => <>);
         end loop;
         return First;
      end;
   end Claim;

   function Find (H : NL.Interface_Handle) return Natural is
   begin
      for I in 1 .. Capacity loop
         if Item (I).State = Live and then Item (I).Handle = H then
            return I;
         end if;
      end loop;
      return 0;
   end Find;

   function Name_Of (R : Interface_Record) return String is (R.Name.Text (1 .. R.Name.Length));

   ---------------------------------------------------------------------------
   --  Calls to netstack (synchronous, down the layering, with a deadline).
   ---------------------------------------------------------------------------
   function Condition_Image (C : NL.Link_Condition) return String is
     (case C is when NL.Up => "Up", when NL.Down => "Down", when NL.Dormant => "Dormant");

   function Reason_Image (R : NL.Down_Reason) return String is
     (case R is
         when NL.Not_Reported          => "Not_Reported",
         when NL.No_Carrier            => "No_Carrier",
         when NL.Administratively_Down => "Administratively_Down",
         when NL.Not_Associated        => "Not_Associated",
         when NL.Authentication_Failed => "Authentication_Failed",
         when NL.Hardware_Fault        => "Hardware_Fault");

   function MAC_Image (M : NL.Hardware_Address) return String is
      Hex  : constant String := "0123456789abcdef";
      Text : String (1 .. 3 * NL.Octets - 1) := [others => ':'];
   begin
      for I in NL.Octet_Index loop
         Text (3 * I + 1) := Hex (Natural (Shift_Right (M (I), 4)) + 1);
         Text (3 * I + 2) := Hex (Natural (M (I) and 16#F#) + 1);
      end loop;
      return Text;
   end MAC_Image;

   --  One line on serial and, once devmgr grants the publisher, in logstore
   --  (coordination/release-stack.md, "bring-up logging convention").
   procedure Say (Text : String; Warn : Boolean := False) is
   begin
      CuBit.Granted_Logs.Line
        (Text, (if Warn then CuBit.Granted_Logs.Warning else CuBit.Granted_Logs.Information));
   end Say;

   function Request (Op : NM.Operation; Words : NM.Words4 := [others => 0]) return Message is
     ((tag => (label => NM.Label (Op), length => Unsigned_8 (NM.Request_Words (Op)),
               protocol => 0, ipc => <>, reserved => 0),
       authorityTag => 0, words => (Words (0), Words (1), Words (2), Words (3))));

   function Call (M : in out Message) return Boolean is
     (capCall (Netstack, M, Deadline_After (Call_Deadline_MS)).label = KA.Reply_OK);

   procedure Configure (R : Interface_Record; C : NM.Configuration) is
      M : Message := Request (NM.Configure, NM.Encode (R.Handle, C));
   begin
      if Call (M) then
         if C.Address = NM.No_IPv4 then
            Say ("netmgr: " & Name_Of (R) & " unconfigured");
         else
            Say ("netmgr: " & Name_Of (R) & " configured " & IPv4_Image (C.Address) & "/" &
                        Image (Unsigned_64 (C.Prefix)));
            --  What it applied, on one line for a screen photo.
            Say ("netmgr: " & Name_Of (R) & " applied method=" &
                 (case R.Setting.Kind is when NS.Static => "static",
                                         when NS.Automatic => "dhcp",
                                         when NS.Disabled => "none") &
                 " address=" & IPv4_Image (C.Address) & "/" & Image (Unsigned_64 (C.Prefix)) &
                 " gateway=" & (if C.Gateway = NM.No_IPv4 then "none" else IPv4_Image (C.Gateway)) &
                 " metric=" & Image (Unsigned_64 (C.Metric)) &
                 " routes=connected" & (if C.Gateway = NM.No_IPv4 then "" else ",default") &
                 " link=" & Condition_Image (R.Condition));
         end if;
      else
         Say ("netmgr: " & Name_Of (R) & " configure refused");
      end if;
   end Configure;

   --  The DNS servers from the settings (No_IPv4: none set).
   Settings_DNS_1, Settings_DNS_2 : NM.IPv4_Word := NM.No_IPv4;

   procedure Set_DNS (Primary, Secondary : NM.IPv4_Word) is
      M : Message := Request (NM.Set_DNS, [Unsigned_64 (Primary), Unsigned_64 (Secondary), 0, 0]);
   begin
      if Call (M) then
         Say ("netmgr: DNS " & IPv4_Image (Primary));
      end if;
   end Set_DNS;

   ---------------------------------------------------------------------------
   --  DHCP
   ---------------------------------------------------------------------------
   --  A transaction identifier: unique per exchange, not secret (RFC 2131
   --  4.1 asks only that clients choose them to avoid collisions).
   function Random32 return Unsigned_32 is
      Low, High : Unsigned_32;
      Golden : constant Unsigned_32 := 16#9E37_79B9#;
   begin
      System.Machine_Code.Asm ("rdtsc",
        Outputs => [Unsigned_32'Asm_Output ("=a", Low), Unsigned_32'Asm_Output ("=d", High)],
        Volatile => True);
      return Low xor (High * Golden) xor Unsigned_32 (syscall (SYSCALL_GETTIME) and 16#FFFF_FFFF#);
   end Random32;

   function Outgoing (R : Interface_Record) return System.Address is
     (To_Address (Integer_Address (R.Page + NM.Outgoing_At)));
   function Incoming (R : Interface_Record) return System.Address is
     (To_Address (Integer_Address (R.Page + NM.Incoming_At)));

   --  Lend netstack the interface's DHCP page.
   procedure Open_DHCP (R : in out Interface_Record) is
      Base : constant Unsigned_64 := KC.Call (KA.Allocate_Owned_Memory, Page_Bytes);
      Created : Boolean;
   begin
      if Base = KA.Failed then
         Say ("netmgr: " & Name_Of (R) & ": no memory for DHCP");
         return;
      end if;
      MG.Create_Via_Capability (Netstack, To_Address (Integer_Address (Base)), Page_Count, True,
                                R.Grant, Created);
      if not Created then
         declare
            Ignore : constant Unsigned_64 := KC.Call (KA.Release_Owned_Memory, Base, Page_Bytes);
         begin
            Say ("netmgr: " & Name_Of (R) & ": DHCP page not lent");
            return;
         end;
      end if;
      R.Page := Base;
      declare
         M : Message := Request
           (NM.DHCP_Open, [NL.Handle_Word (R.Handle), CuBit.Grant_References.Encode (R.Grant), 0, 0]);
      begin
         R.Opened := Call (M);
         if not R.Opened then
            Say ("netmgr: " & Name_Of (R) & ": DHCP channel refused");
         end if;
      end;
   end Open_DHCP;

   procedure Park_Await (R : in out Interface_Record) is
   begin
      if R.Opened and then not R.Awaiting then
         R.Awaiting := capSubmit
           (Netstack, Request (NM.DHCP_Await, [NL.Handle_Word (R.Handle), 0, 0, 0]),
            NL.Handle_Word (R.Handle));
      end if;
   end Park_Await;

   --  Carry out what the client asked for.
   procedure Act (R : in out Interface_Record; A : D.Action) is
      use type D.Phase;
   begin
      if A.Deconfigure then
         Configure (R, (others => <>));
      end if;
      if A.Configure then
         Say ("netmgr: " & Name_Of (R) & " DHCP bound " & IPv4_Image (A.Configured.Address) & "/" &
                     Image (Unsigned_64 (A.Configured.Prefix)) & " gw " & IPv4_Image (A.Configured.Router));
         Configure (R, (Address => A.Configured.Address, Prefix => A.Configured.Prefix,
                        Gateway => A.Configured.Router, Metric => Automatic_Metric));
         if Settings_DNS_1 = NM.No_IPv4 and then A.Configured.DNS_1 /= NM.No_IPv4 then
            Set_DNS (A.Configured.DNS_1, A.Configured.DNS_2);
         end if;
      end if;
      if A.Send and then R.Opened then
         if R.Client.Current = D.Selecting then
            Say ("netmgr: " & Name_Of (R) & " DHCP DISCOVER");
         elsif R.Client.Current = D.Rebooting then
            Say ("netmgr: " & Name_Of (R) & " DHCP INIT-REBOOT");
         end if;
         declare
            M : Message := Request
              (NM.DHCP_Send, [NL.Handle_Word (R.Handle), Unsigned_64 (A.Length), 0, 0]);
         begin
            if not Call (M) then
               null;   --  the link went down meanwhile: the next Up starts again
            end if;
         end;
      end if;
      Park_Await (R);
   end Act;

   ---------------------------------------------------------------------------
   --  Interfaces coming, changing, going
   ---------------------------------------------------------------------------

   --  The link's condition is now Now_Condition: act on a transition (only
   --  on one: Configure itself makes netstack report the interface again).
   --  Ups (when Ups_Known): netstack's count of the link coming Up; Up
   --  before and after with another count is a bounce (the link went down
   --  and came back, or joined another network, between two events).
   procedure Link_Changed (Index : Positive; Now_Condition : NL.Link_Condition;
                           Ups : NM.Up_Count := 0; Ups_Known : Boolean := False;
                           Reason : NL.Down_Reason := NL.Not_Reported;
                           Generation : NL.Generation := NL.Never_Reported) is
      R : Interface_Record renames Item (Index).all;
      Bounced : constant Boolean :=
        Ups_Known and then R.Ups_Valid and then R.Condition = NL.Up and then
        Now_Condition = NL.Up and then Ups /= R.Ups;
      Was_Up : constant Boolean := R.Condition = NL.Up and then not Bounced;
      Is_Up  : constant Boolean := Now_Condition = NL.Up;
      Old    : constant NL.Link_Condition := R.Condition;
   begin
      if Ups_Known then
         R.Ups := Ups;
         R.Ups_Valid := True;
      end if;
      if R.Condition = Now_Condition and then not Bounced then
         return;
      end if;
      R.Condition := Now_Condition;
      --  One line per transition, readable from a screen photo
      --  (coordination/release-stack.md): name, kind, MAC, new and old
      --  state, the link report's generation and the reason.
      Say ("netmgr: " & Name_Of (R) & " link " &
           (if Bounced then "bounced" else Condition_Image (Now_Condition)) &
           " reason=" & (if Now_Condition = NL.Down then Reason_Image (Reason)
                         elsif Bounced then "Bounced" else "Reported") &
           " was=" & Condition_Image (Old) &
           " kind=" & (if R.Kind = NL.Ethernet then "Ethernet" else "Wireless") &
           " mac=" & MAC_Image (R.MAC) &
           " gen=" & Image (Unsigned_64 (Generation)),
           Warn => Now_Condition /= NL.Up);
      if Bounced and then R.Setting.Kind = NS.Automatic then
         D.Link_Down (R.Client);
      end if;
      Network_Context.Changed ((Name => R.Name, Handle => R.Handle, Condition => Now_Condition));
      if Is_Up and then not Was_Up then
         case R.Setting.Kind is
            when NS.Static =>
               Say ("netmgr: " & Name_Of (R) & " static settings applied");
               Configure (R, R.Setting.Settings);
               if Settings_DNS_1 /= NM.No_IPv4 then
                  Set_DNS (Settings_DNS_1, Settings_DNS_2);
               end if;
            when NS.Automatic =>
               if R.Opened then
                  declare
                     A : D.Action;
                     Out_Bytes : D.Bytes with Import, Address => Outgoing (R);
                  begin
                     D.Link_Up (R.Client, syscall (SYSCALL_GETTIME), Random32, Out_Bytes, A);
                     Act (R, A);
                  end;
               end if;
            when NS.Disabled =>
               null;
         end case;
      elsif Was_Up and then not Is_Up then
         if R.Setting.Kind = NS.Automatic then
            D.Link_Down (R.Client);
         end if;
      end if;
   end Link_Changed;

   --  A new interface: name it, read its setting, take it on. Its link
   --  starts Down here; the caller passes on its condition.
   procedure Attached (H : NL.Interface_Handle; Condition : out NL.Link_Condition; Taken : out Boolean) is
      M : Message := Request (NM.Interface_Detail, [NL.Handle_Word (H), 0, 0, 0]);
      Index : Natural;
   begin
      Condition := NL.Down;
      Taken := False;
      if Find (H) /= 0 or else not Call (M) then
         return;
      end if;
      declare
         S : constant NM.Decoded_Summary :=
           NM.Decode_Summary ((M.words (0), M.words (1), M.words (2), M.words (3)));
      begin
         if not S.Valid then
            return;
         end if;
         Index := Claim;
         if Index = 0 then
            Say ("netmgr: out of memory: an interface is left unmanaged");
            return;
         end if;
         Condition := S.Item.Condition;
         Taken := True;
         declare
            R : Interface_Record renames Item (Index).all;
         begin
            R.State := Live;
            R.Handle := H;
            --  netstack reports the address the interface attached with, so
            --  a factory-address name never changes with Set_Address.
            R.Name := NL.Name_Of (S.Item.Identity, S.Item.Address);
            R.Kind := S.Item.Kind;
            R.MAC := S.Item.Address;
            declare
               Stored : constant Startup.Settings.Net_Interface.Reading :=
                 Startup.Settings.Net_Interface.Read (Name_Of (R));
            begin
               case Stored.Kind is
                  when Present => R.Setting := Stored.Item;
                  when Absent | Unavailable => R.Setting := NS.Default;
                  when others =>
                     --  A setting that is there but unusable: leave the
                     --  interface alone rather than guess.
                     Say ("netmgr: net.interface." & Name_Of (R) & " " &
                                 CuBit.Settings.Outcome'Image (Stored.Kind));
                     R.Setting := (Kind => NS.Disabled);
               end case;
            end;
            Say ("netmgr: " & Name_Of (R) & " attached");
            Say ("netmgr: " & Name_Of (R) & " policy " &
                        (case R.Setting.Kind is when NS.Automatic => "automatic (DHCP, SLAAC)",
                                                when NS.Static => "static",
                                                when NS.Disabled => "disabled"));
            case R.Setting.Kind is
               when NS.Static =>
                  Say ("netmgr: " & Name_Of (R) & " static settings applied");
                  --  Applied now: netstack keeps the routes unusable while
                  --  the link is not Up.
                  Configure (R, R.Setting.Settings);
                  if Settings_DNS_1 /= NM.No_IPv4 then
                     Set_DNS (Settings_DNS_1, Settings_DNS_2);
                  end if;
               when NS.Automatic =>
                  R.Client := D.Create (S.Item.Address);
                  Open_DHCP (R);
               when NS.Disabled =>
                  null;
            end case;
         end;
      end;
   end Attached;

   --  Take on H if new, then act on its link's condition.
   procedure Reported (H : NL.Interface_Handle; Condition : NL.Link_Condition;
                       Ups : NM.Up_Count := 0; Ups_Known : Boolean := False;
                       Reason : NL.Down_Reason := NL.Not_Reported;
                       Generation : NL.Generation := NL.Never_Reported) is
      Initial : NL.Link_Condition;
      Taken : Boolean;
   begin
      if Find (H) = 0 then
         Attached (H, Initial, Taken);
         if not Taken then
            return;
         end if;
         --  A static interface attached Up is configured already: its first
         --  Up only needs saying.
         if Ups_Known then
            Item (Find (H)).Ups := Ups;
            Item (Find (H)).Ups_Valid := True;
         end if;
         if Item (Find (H)).Setting.Kind = NS.Static and then Condition = NL.Up then
            Item (Find (H)).Condition := NL.Up;
            declare
               R : Interface_Record renames Item (Find (H)).all;
            begin
               Say ("netmgr: " & Name_Of (R) & " link Up reason=Reported was=Down" &
                    " kind=" & (if R.Kind = NL.Ethernet then "Ethernet" else "Wireless") &
                    " mac=" & MAC_Image (R.MAC) &
                    (if Generation = NL.Never_Reported then ""   --  found by enumeration
                     else " gen=" & Image (Unsigned_64 (Generation))));
            end;
            Network_Context.Changed ((Name => Item (Find (H)).Name, Handle => H, Condition => NL.Up));
            return;
         end if;
      end if;
      Link_Changed (Find (H), Condition, Ups, Ups_Known, Reason, Generation);
   end Reported;

   procedure Detached (H : NL.Interface_Handle) is
      Index : constant Natural := Find (H);
   begin
      if Index = 0 then
         return;
      end if;
      declare
         R : Interface_Record renames Item (Index).all;
         Ignore : Boolean;
      begin
         Say ("netmgr: " & Name_Of (R) & " detached");
         Network_Context.Changed ((Name => R.Name, Handle => R.Handle, Condition => NL.Down));
         if R.Page /= 0 then
            --  The page is netstack's to map until the kernel confirms the
            --  grant retired; then it is released (Retire_Pages).
            MG.Revoke (R.Grant, Ignore);
            R.State := Retiring;
         else
            R.State := Free;
         end if;
      end;
   end Detached;

   procedure Retire_Pages is
   begin
      for I in 1 .. Capacity loop
         if Item (I).State = Retiring and then MG.Retirement_Confirmed (Item (I).Grant) then
            declare
               Ignore : constant Unsigned_64 := KC.Call (KA.Release_Owned_Memory, Item (I).Page, Page_Bytes);
            begin
               Item (I).all := (others => <>);
            end;
         end if;
      end loop;
   end Retire_Pages;

   ---------------------------------------------------------------------------
   --  The event loop
   ---------------------------------------------------------------------------
   Seen : Unsigned_32 := 0;
   Watching : Boolean := False;
   --  netstack refused the watch (or went away): asking again would only
   --  be refused again.
   Watch_Ended : Boolean := False;

   procedure Watch is
   begin
      if not Watching and then not Watch_Ended and then Startup.Netstack.Present then
         Watching := capSubmit (Netstack, Request (NM.Watch, [Unsigned_64 (Seen), 0, 0, 0]), Watch_Token);
      end if;
   end Watch;

   procedure Completed (C : CompletionEntry) is
      W : constant NM.Words4 := (C.msg.words (0), C.msg.words (1), C.msg.words (2), C.msg.words (3));
   begin
      if C.token = Watch_Token then
         Watching := False;
         if C.status /= COMPLETION_OK or else C.msg.tag.label /= KA.Reply_OK then
            Say ("netmgr: interface watch refused");
            Watch_Ended := True;
            return;
         end if;
         declare
            E : constant NM.Decoded_Event := NM.Decode_Event (W);
         begin
            if not E.Valid then
               return;
            end if;
            case E.Item.Event is
               when NM.Attached | NM.Changed =>
                  Reported (E.Item.Handle, E.Item.Condition, E.Item.Ups, Ups_Known => True,
                            Reason => E.Item.Reason, Generation => E.Item.Generation);
               when NM.Detached =>
                  Detached (E.Item.Handle);
            end case;
            Seen := E.Item.Sequence;
         end;
      elsif NL.Valid_Handle_Word (C.token) then
         declare
            Index : constant Natural := Find (NL.Handle_Of (C.token));
         begin
            if Index = 0 then
               return;
            end if;
            declare
               R : Interface_Record renames Item (Index).all;
               A : D.Action;
               In_Bytes : D.Bytes with Import, Address => Incoming (R);
               Out_Bytes : D.Bytes with Import, Address => Outgoing (R);
               use type D.Phase;
            begin
               R.Awaiting := False;
               if C.status = COMPLETION_OK and then C.msg.tag.label = KA.Reply_OK and then
                 C.msg.words (0) <= Unsigned_64 (NM.Maximum_Message)
               then
                  declare
                     Before : constant D.Phase := R.Client.Current;
                  begin
                     D.Receive (R.Client, syscall (SYSCALL_GETTIME), In_Bytes, Natural (C.msg.words (0)),
                                Out_Bytes, A);
                     --  Only a NAK sends a client that was asking back to
                     --  DISCOVER from a received message.
                     if Before in D.Requesting | D.Rebooting | D.Renewing | D.Rebinding
                       and then R.Client.Current = D.Selecting
                     then
                        Say ("netmgr: " & Name_Of (R) & " DHCP NAK");
                     end if;
                  end;
                  Act (R, A);
               elsif C.status = COMPLETION_OK and then R.Condition = NL.Up then
                  Park_Await (R);
               end if;
            end;
         end;
      end if;
   end Completed;

   Log_Retry_MS : constant := 1_000;

   --  The earliest DHCP timer.
   function Next_Deadline return Unsigned_64 is
      Earliest : Unsigned_64 := Unsigned_64'Last;
   begin
      for I in 1 .. Capacity loop
         if Item (I).State = Live and then Item (I).Setting.Kind = NS.Automatic then
            Earliest := Unsigned_64'Min (Earliest, D.Next_Deadline (Item (I).Client));
         end if;
      end loop;
      --  Until logstore runs and devmgr grants the log publisher, look
      --  again now and then (Request asks once logstore has registered).
      if not CuBit.Granted_Logs.Settled and then not CuBit.Granted_Logs.Waiting then
         Earliest := Unsigned_64'Min (Earliest, Deadline_After (Log_Retry_MS));
      end if;
      return Earliest;
   end Next_Deadline;

   procedure Run_Timers is
      Now : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
   begin
      for I in 1 .. Capacity loop
         if Item (I).State = Live and then Item (I).Setting.Kind = NS.Automatic
           and then Item (I).Opened and then D.Next_Deadline (Item (I).Client) <= Now
         then
            declare
               R : Interface_Record renames Item (I).all;
               A : D.Action;
               Out_Bytes : D.Bytes with Import, Address => Outgoing (R);
            begin
               D.Tick (R.Client, Now, Random32, Out_Bytes, A);
               Act (R, A);
            end;
         end if;
      end loop;
   end Run_Timers;

   --  The interfaces netstack has now.
   procedure Enumerate is
      Previous : Unsigned_64 := 0;
   begin
      loop
         declare
            M : Message := Request (NM.Interface_Next, [Previous, 0, 0, 0]);
         begin
            exit when not Call (M) or else not NL.Valid_Handle_Word (M.words (0))
              or else M.words (0) = Previous;
            Previous := M.words (0);
            declare
               D_M : Message := Request (NM.Interface_Detail, [Previous, 0, 0, 0]);
            begin
               if Call (D_M) then
                  declare
                     S : constant NM.Decoded_Summary :=
                       NM.Decode_Summary ((D_M.words (0), D_M.words (1), D_M.words (2), D_M.words (3)));
                  begin
                     if S.Valid then
                        Reported (NL.Handle_Of (Previous), S.Item.Condition);
                     end if;
                  end;
               end if;
            end;
         end;
      end loop;
   end Enumerate;

   --  A DNS setting as an IPv4 word (No_IPv4 when absent, refused or not IPv4).
   generic
      with package Setting is new CuBit.Settings.Readable (<>);
      with function Address_Of (Item : Setting.Value) return CuBit.Net_Address.Address;
   function DNS_Word return NM.IPv4_Word;
   function DNS_Word return NM.IPv4_Word is
      Stored : constant Setting.Reading := Setting.Read;
   begin
      if Stored.Kind /= Present then
         if Stored.Kind /= Absent then
            Say ("netmgr: " & Setting.Setting_Key & " " & CuBit.Settings.Outcome'Image (Stored.Kind));
         end if;
         return NM.No_IPv4;
      end if;
      if not CuBit.Net_Address.Is_Mapped (Address_Of (Stored.Item)) then
         Say ("netmgr: " & Setting.Setting_Key & " is not an IPv4 address; IPv6 DNS is not configured yet");
         return NM.No_IPv4;
      end if;
      return NM.Word_Of (Address_Of (Stored.Item));
   end DNS_Word;
   function Same (Item : CuBit.Net_Address.Address) return CuBit.Net_Address.Address is (Item);
   function Primary_DNS is new DNS_Word (Startup.Settings.Net_Dns_Primary, Same);
   function Secondary_DNS is new DNS_Word (Startup.Settings.Net_Dns_Secondary, Same);

   Ignore : Unsigned_64;
begin
   Say ("netmgr: starting");
   Ignore := registerDriver (DRIVER_NETMGR, Startup.Registration);

   Settings_DNS_1 := Primary_DNS;
   Settings_DNS_2 := Secondary_DNS;

   declare
      Ready : Message := (tag => (label => Ready_Label, length => 0, protocol => 0, ipc => <>, reserved => 0),
                          authorityTag => 0, words => (others => 0));
   begin
      Ignore := Unsigned_64 (capCall (Startup.Ready, Ready, Wait_Forever).label);
      Say ("netmgr: signaled ready");
   end;

   if Startup.Netstack.Present then
      if Settings_DNS_1 /= NM.No_IPv4 then
         Set_DNS (Settings_DNS_1, Settings_DNS_2);
      end if;
      Enumerate;
      Watch;
   else
      Say ("netmgr: no netstack");
   end if;

   loop
      declare
         Activity : constant Activity_Result := Wait_For_Activity_Until (Next_Deadline);
         Entry_Found : CompletionEntry;
         Sender : Process_ID;
         Request_Message : Message;
         Found : Boolean;
      begin
         exit when Activity = Unavailable;
         while Poll_Completion (Entry_Found'Address) = 1 loop
            if not CuBit.Granted_Logs.Take (Entry_Found) then
               Completed (Entry_Found);
            end if;
         end loop;
         CuBit.Granted_Logs.Request (Startup.Ready);
         loop
            Poll_Service_Request (Sender, Request_Message, Found);
            exit when not Found;
            Ignore := reply (Sender, (tag => (label => KA.Reply_Error, length => 0, protocol => 0,
                                              ipc => <>, reserved => 0),
                                      authorityTag => 0, words => (others => 0)));
         end loop;
         Run_Timers;
         Retire_Pages;
         Watch;
      end;
   end loop;
   Say ("netmgr: activity wait unavailable");
end main;
