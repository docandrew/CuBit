------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The typed requests a holder of netmgr's "netmgr" endpoint (Desktop's
--  network items, UI-027) sends netmgr.svc: watch every interface's state
--  and apply a per-interface setting by stable name.
--
--  @description
--  - Watch (submit: word 0 a grant reference, CuBit.Grant_References wire
--    form, of one writable page the caller owns; word 1 the last report
--    generation it has seen) -> parked until netmgr's report generation
--    differs, then REPLY_OK with word 0 the generation, after netmgr wrote
--    the report (CuBit.Net_Manager_Report) into the page. An interface
--    attaching, detaching or changing (link, address, lease, setting)
--    advances it. At most Maximum_Watchers park (REPLY_ERR Busy past it).
--  - Apply (submit: word 0 a grant reference of one readable page holding
--    the interface's stable name then the setting's canonical CCL source,
--    a CCL Net_Interface such as Net_Interface.Automatic; word 1 the
--    lengths word) -> REPLY_OK once netmgr decoded the setting with the
--    same codec it reads net.interface.<name> with and applied it, or
--    REPLY_ERR with a Refusal. The setting holds until netmgr restarts:
--    it is the runtime value of net.interface.<name>; the declaration in
--    the System keeps a setting across boots (the panel shows its CCL).
--  - Set_DNS (submit: words 0 and 1 the primary and secondary DNS servers
--    as IPv4 words (CuBit.Net_Management), word 2 a DNS_Choice) -> REPLY_OK.
--    netstack has one DNS pair for every interface; Use_Settings returns
--    to net.dns.* and the leases.
--
--  The IPC graph stays a DAG: netmgr never calls its client; the watch is
--  the client's own parked request.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Net_Link;

package CuBit.Net_Manager_Control with Pure, SPARK_Mode is
   package NL renames CuBit.Net_Link;

   --  netmgr's role (DRIVER_NETMGR): it registers it, clients bind to it.
   Service_Role : constant Unsigned_64 := 12;

   type Operation is (Watch, Apply, Set_DNS);
   for Operation use (Watch => 16#04B0#, Apply => 16#04B1#, Set_DNS => 16#04B2#);

   function Label (Op : Operation) return Unsigned_32 is (Operation'Enum_Rep (Op));
   function Is_Operation (Value : Unsigned_32) return Boolean is
     (for some Op in Operation => Value = Label (Op));
   function Operation_Of (Value : Unsigned_32) return Operation is
     (if Value = Label (Watch) then Watch elsif Value = Label (Apply) then Apply else Set_DNS)
   with Pre => Is_Operation (Value), Post => Label (Operation_Of'Result) = Value;

   function Request_Words (Op : Operation) return Natural is
     (case Op is when Watch | Apply => 2, when Set_DNS => 3);

   Maximum_Watchers : constant := 4;
   Page_Bytes : constant := 4_096;

   ---------------------------------------------------------------------------
   --  Apply's lengths word: name length [7:0], source length [23:8]. The
   --  page: the name, then the source.
   ---------------------------------------------------------------------------
   Maximum_Source : constant := Page_Bytes - NL.Name_Characters;
   subtype Source_Length is Natural range 1 .. Maximum_Source;
   Source_Shift : constant := 8;
   Name_Mask : constant Unsigned_64 := 16#FF#;
   Source_Mask : constant Unsigned_64 := 16#FFFF#;

   function Lengths_Word (Name : NL.Name_Length; Source : Source_Length) return Unsigned_64 is
     (Unsigned_64 (Name) or Shift_Left (Unsigned_64 (Source), Source_Shift));

   type Decoded_Lengths (Valid : Boolean := False) is record
      case Valid is
         when True =>
            Name   : NL.Name_Length;
            Source : Source_Length;
         when False => null;
      end case;
   end record;

   function Decode_Lengths (W : Unsigned_64) return Decoded_Lengths is
     (if (W and Name_Mask) in 1 .. NL.Name_Characters
        and then (Shift_Right (W, Source_Shift) and Source_Mask) in 1 .. Maximum_Source
        and then Shift_Right (W, Source_Shift) <= Source_Mask
        and then Lengths_Word (Natural (W and Name_Mask), Natural (Shift_Right (W, Source_Shift) and Source_Mask)) = W
      then (Valid => True, Name => Natural (W and Name_Mask),
            Source => Natural (Shift_Right (W, Source_Shift) and Source_Mask))
      else (Valid => False))
   with Post => (if Decode_Lengths'Result.Valid then
                   Lengths_Word (Decode_Lengths'Result.Name, Decode_Lengths'Result.Source) = W);

   type DNS_Choice is (Use_Settings, Use_These);
   for DNS_Choice use (Use_Settings => 0, Use_These => 1);

   ---------------------------------------------------------------------------
   --  Refusals (REPLY_ERR word 0).
   ---------------------------------------------------------------------------
   type Refusal is (Malformed, Unknown_Interface, Invalid_Setting, Busy);
   for Refusal use (Malformed => 1, Unknown_Interface => 2, Invalid_Setting => 3, Busy => 4);
end CuBit.Net_Manager_Control;
