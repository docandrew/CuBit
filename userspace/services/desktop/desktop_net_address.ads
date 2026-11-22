------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  What the Deskbar's addressing editor (UI-027) reads from its fields and
--  hands netmgr: IPv4 addresses, prefix lengths and DNS servers typed as
--  text, checked before anything is applied, and the setting's CCL (the
--  canonical Net_Interface source netmgr decodes, which the panel shows).
--
--  @description
--  Addresses are the IPv4 words of CuBit.Net_Management (a | b << 8 |
--  c << 16 | d << 24); the CCL carries them IPv4-mapped (::ffff:a.b.c.d,
--  the one 16-byte address type), with the prefix over all 128 bits.
--  Parsing is total: anything not exactly a dotted quad of decimal octets
--  (no signs, no leading zeros, no blanks inside) is refused.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CCL.Interfaces.Net_Interface;

package Desktop_Net_Address with SPARK_Mode, Pure is
   subtype IPv4_Word is Unsigned_32;
   No_Address : constant IPv4_Word := 0;
   subtype Prefix_Length is Natural range 0 .. 32;

   --  The longest text a field takes: a dotted quad with its prefix, or
   --  two DNS servers and a separator (a named ceiling for typed input).
   Maximum_Field : constant := 40;
   subtype Field_Length is Natural range 0 .. Maximum_Field;

   --  The octet A of a word (0 is the first written).
   function Octet (W : IPv4_Word; A : Natural) return Natural is
     (Natural (Shift_Right (W, 8 * A) and 16#FF#))
   with Pre => A <= 3, Post => Octet'Result <= 255;

   function Word_Of (A, B, C, D : Natural) return IPv4_Word is
     (Unsigned_32 (A) or Shift_Left (Unsigned_32 (B), 8) or Shift_Left (Unsigned_32 (C), 16)
      or Shift_Left (Unsigned_32 (D), 24))
   with Pre => A <= 255 and then B <= 255 and then C <= 255 and then D <= 255;

   --  "a.b.c.d".
   procedure Parse_Address (Text : String; Value : out IPv4_Word; OK : out Boolean)
   with Pre => Text'Last < Positive'Last;

   --  "a.b.c.d/n" (the prefix is required).
   procedure Parse_Address_Prefix
     (Text : String; Value : out IPv4_Word; Length : out Prefix_Length; OK : out Boolean)
   with Pre => Text'Last < Positive'Last;

   --  Up to two servers, separated by a comma and/or blanks; empty text is
   --  no server (both No_Address).
   procedure Parse_DNS (Text : String; Primary, Secondary : out IPv4_Word; OK : out Boolean)
   with Pre => Text'Last < Positive'Last;

   --  "a.b.c.d".
   function Image (W : IPv4_Word) return String
   with Post => Image'Result'First = 1 and then Image'Result'Length in 7 .. 15;

   ---------------------------------------------------------------------------
   --  A static configuration and what is wrong with it.
   ---------------------------------------------------------------------------
   type Static_Choice is record
      Address : IPv4_Word := No_Address;
      Length  : Prefix_Length := 0;
      Gateway : IPv4_Word := No_Address;   --  No_Address: none
   end record;

   type Problem is
     (None, Address_Malformed, Address_Reserved, Prefix_Too_Short, Address_Is_Network,
      Address_Is_Broadcast, Gateway_Malformed, Gateway_Outside, Gateway_Is_Address, DNS_Malformed);

   --  Shortest prefix accepted (a /0 would claim every address).
   Shortest_Prefix : constant := 1;
   --  Prefixes up to this one have distinct network and broadcast
   --  addresses (/31 and /32 are point-to-point and host routes).
   Longest_Subnet  : constant := 30;

   function Network_Of (W : IPv4_Word; Length : Prefix_Length) return IPv4_Word;
   function Same_Network (A, B : IPv4_Word; Length : Prefix_Length) return Boolean is
     (Network_Of (A, Length) = Network_Of (B, Length));

   --  The first problem with S, or None.
   function Check (S : Static_Choice) return Problem;

   function Problem_Text (P : Problem) return String is
     (case P is
         when None                 => "",
         when Address_Malformed    => "Address: write a.b.c.d/prefix",
         when Address_Reserved     => "Address: not a host address",
         when Prefix_Too_Short     => "Prefix: 1 to 32",
         when Address_Is_Network   => "Address is the network itself",
         when Address_Is_Broadcast => "Address is the broadcast address",
         when Gateway_Malformed    => "Gateway: write a.b.c.d or leave empty",
         when Gateway_Outside      => "Gateway is outside the network",
         when Gateway_Is_Address   => "Gateway is this address",
         when DNS_Malformed        => "DNS: up to two a.b.c.d");

   ---------------------------------------------------------------------------
   --  The setting's CCL: net.interface.<name> as a Net_Interface.
   ---------------------------------------------------------------------------
   Automatic_Source : constant String := "Net_Interface.Automatic";
   Disabled_Source  : constant String := "Net_Interface.Disabled";
   Mapped_Prefix    : constant := 96;   --  an IPv4 prefix plus 96 over 128 bits
   Default_Metric   : constant := CCL.Interfaces.Net_Interface.DEFAULT_METRIC;

   --  "::ffff:a.b.c.d".
   function Mapped_Image (W : IPv4_Word) return String is ("::ffff:" & Image (W));

   --  (Net_Interface.Static (Static_Interface address => "::ffff:a.b.c.d"
   --   prefix => N gateway => G metric => M)), exactly as netmgr's codec
   --  reads it.
   function Static_Source (S : Static_Choice) return String is
     ("(Net_Interface.Static (Static_Interface address => """ & Mapped_Image (S.Address) &
      """ prefix =>" & Natural'Image (S.Length + Mapped_Prefix) & " gateway => " &
      (if S.Gateway = No_Address then "Gateway.None"
       else "(Gateway.Via """ & Mapped_Image (S.Gateway) & """)") &
      " metric =>" & Natural'Image (Default_Metric) & "))");

   --  The declaration that keeps it across boots, as the System writes it.
   function Setting_Line (Name, Source : String) return String is
     ("(setting ""net.interface." & Name & """ " & Source & ")")
   with Pre => Name'Length <= 64 and then Source'Length <= 512;
end Desktop_Net_Address;
