------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The Key Data field of EAPOL-Key messages 3 and group 1, after AES key
--  unwrap (IEEE 802.11-2020 12.7.2): the AP's RSN element (checked
--  against its beacon's) and the GTK KDE (12.7.2, Table 12-10).
--
--  @description
--  A sequence of elements (ID, Length, data) and KDEs (0xDD, Length, OUI
--  00-0F-AC, Data Type, data); padding is 0xDD followed by zero octets.
--  GTK KDE (Data Type 1): Key ID [1:0], Tx [2], one reserved octet, the
--  GTK. Decoding is total and proved free of run-time errors.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Wifi_Key_Data with Pure, SPARK_Mode is

   Maximum_Octets : constant := 4096;
   subtype Octet_Index is Natural range 0 .. Maximum_Octets;
   type Octets is array (Octet_Index range <>) of Unsigned_8;

   KDE_ID        : constant Unsigned_8 := 16#DD#;
   RSN_ID        : constant Unsigned_8 := 48;
   KDE_Type_GTK  : constant Unsigned_8 := 1;
   KDE_Header    : constant := 6;     --  ID, Length, OUI (3), Data Type
   GTK_Prefix    : constant := 2;     --  Key ID/Tx octet, reserved octet
   Key_ID_Mask   : constant Unsigned_8 := 16#03#;
   Maximum_GTK   : constant := 32;
   subtype GTK_Length is Natural range 0 .. Maximum_GTK;
   subtype Key_ID is Natural range 0 .. 3;
   type GTK_Bytes is array (1 .. Maximum_GTK) of Unsigned_8;

   type Place is record
      First  : Natural := 0;
      Length : Natural := 0;    --  0: absent
   end record;

   type Contents is record
      RSN_Element : Place;      --  the whole element (header included)
      Has_GTK     : Boolean := False;
      GTK_Key_ID  : Key_ID := 0;
      GTK         : GTK_Bytes := [others => 0];
      GTK_Bytes_Count : GTK_Length := 0;
   end record;

   type Status is (Decoded, Overrun, GTK_Malformed);

   procedure Decode (Data : Octets; Item : out Contents; Result : out Status)
   with Pre  => Data'First = 0 and then Data'Length <= 4096,
        Post => (if Result = Decoded then
                   Item.RSN_Element.First + Item.RSN_Element.Length <= Data'Length);

end Wifi_Key_Data;
