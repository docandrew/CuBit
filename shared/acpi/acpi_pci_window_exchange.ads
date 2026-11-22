pragma Ada_2022;
with Interfaces;
with System_Publications;
with Capability_ABI;
-- devmgr's PCI-window broker. A request names a bus, never an address.
-- Wire facts are not authority; the receiver must validate the kernel badge,
-- peer, MCFG, physical classification and actual carried handle separately.
package ACPI_PCI_Window_Exchange with Pure, SPARK_Mode is
   subtype Word is Interfaces.Unsigned_64;
   use type Word, Interfaces.Unsigned_32, Interfaces.Unsigned_8;
   Service_Role : constant System_Publications.Service_Role :=
     System_Publications.ACPI_PCI_Windows_Role;
   Backend_Tag : constant Word := 16#5043_4952_4F42_5553#; -- PCIROBUS
   Request_Label : constant Interfaces.Unsigned_32 := 16#06C0#;
   Reply_Label : constant Interfaces.Unsigned_32 := 16#06C1#;
   function Accepting_Flags return Word is (Word
     (Capability_ABI.Flag_Byte (0, Capability_ABI.Accepting)));
   function Transferred_Flags return Word is (Word (Capability_ABI.Flag_Byte
     (0, (Capability_Transferred => True, others => False))));
   Message_Words : constant := 4;
   type Request_ID is new Word;
   type Record_ID is new Word;
   No_Request : constant Request_ID := 0;
   No_Record : constant Record_ID := 0;
   type Segment_Number is new Interfaces.Unsigned_16;
   type Bus_Number is new Interfaces.Unsigned_8;
   type Bus_Key is record
      Segment : Segment_Number := 0;
      Bus : Bus_Number := 0;
   end record;
   type Access_Mode is (Read_Only, Read_Write);
   for Access_Mode use (Read_Only => 0, Read_Write => 1);
   type Request is record
      ID : Request_ID := No_Request;
      Key : Bus_Key;
      Mode : Access_Mode := Read_Only;
   end record;
   type Disposition is (Granted, Not_Approved, Outside_MCFG, Write_Refused,
      Malformed, Request_Changed, No_Root, Protected_Range, Capacity_Refused,
      Delivery_Uncertain, Clock_Unavailable, Expired, Unavailable);
   for Disposition use (Granted => 0, Not_Approved => 1, Outside_MCFG => 2,
      Write_Refused => 3, Malformed => 4, Request_Changed => 5, No_Root => 6,
      Protected_Range => 7, Capacity_Refused => 8, Delivery_Uncertain => 9,
      Clock_Unavailable => 10, Expired => 11, Unavailable => 12);
   type Word_Array is array (Natural range 0 .. Message_Words-1) of Word;
   type Packet is record
      Label : Interfaces.Unsigned_32 := 0;
      Length : Interfaces.Unsigned_8 := 0;
      Flags, Protocol, Reserved : Word := 0; -- actual received IPC metadata
      Words : Word_Array := [others => 0];
   end record;
   function Valid (Item : Request) return Boolean is (Item.ID /= No_Request);
   function Encode (Item : Request) return Packet with Pre => Valid (Item);
   type Decoded_Request (Valid : Boolean := False) is record
      case Valid is when True => Item : Request; when False => null; end case;
   end record;
   function Decode (Item : Packet; Has_Capability : Boolean) return Decoded_Request;
   Page_Bytes : constant := 4096;
   Devices_Per_Bus : constant := 32;
   Functions_Per_Device : constant := 8;
   Bus_Bytes : constant := Page_Bytes * Devices_Per_Bus * Functions_Per_Device;
   type Reply is record
      ID : Request_ID := No_Request;
      Status : Disposition := Malformed;
      Record_Number : Record_ID := No_Record;
      First : Word := 0; -- diagnostic extent metadata, checked against INSPECT
   end record;
   function Valid (Item : Reply) return Boolean is
     (Item.ID /= No_Request and then
      (if Item.Status = Granted then Item.Record_Number /= No_Record
       and then Item.First mod Page_Bytes = 0
       and then Bus_Bytes-1 <= Word'Last-Item.First
       else Item.Record_Number = No_Record and then Item.First = 0));
   -- Server-side encoding contains no kernel-only IPC flags. Reply_With
   -- stamps capability transfer metadata; Decode checks that received stamp.
   function Encode (Item : Reply) return Packet with Pre => Valid (Item);
   type Decoded_Reply (Valid : Boolean := False) is record
      case Valid is when True => Item : Reply; when False => null; end case;
   end record;
   -- Expected_ID and capability presence come from the saved call and kernel
   -- receive metadata, not from the reply. Even a refused packet may carry a
   -- handle which the caller must dispose of independently of this decoder.
   function Decode (Item : Packet; Expected_ID : Request_ID;
      Has_Capability : Boolean) return Decoded_Reply;
end ACPI_PCI_Window_Exchange;
