with Interfaces;
with CCL.Objects;
with CuBit.Messages;
with CuBit.Grant_References;
with CCL.Objects.Schemas;

--  Public native-object Config IPC. No CBOR on this local boundary. Kernel
--  endpoint/reply authority authenticates transport; names/digests do not.
package Config_Object_Messages with SPARK_Mode is
   use Interfaces;
   type Operation is (Open_Collection, Get_Object, Set_Object, Close_Collection, Create_Collection);
   for Operation use
     (Open_Collection => 16#0611#, Get_Object => 16#0612#,
      Set_Object => 16#0613#, Close_Collection => 16#0614#, Create_Collection => 16#0618#);
   type Status is
     (Success, Invalid_Request, Denied, Missing, Stale, Busy, Unavailable,
      Schema_Mismatch, Conflict, Rejected, Capacity_Exceeded, Uncertain);
   for Status use
     (Success => 16#F000#, Invalid_Request => 16#F001#, Denied => 16#F007#,
      Missing => 16#F070#, Stale => 16#F071#, Busy => 16#F072#,
      Unavailable => 16#F073#, Schema_Mismatch => 16#F074#,
      Conflict => 16#F075#, Rejected => 16#F076#, Capacity_Exceeded => 16#F077#,
      Uncertain => 16#F078#);
   type Access_Mode is (Read_Only, Write_Only, Read_Write);
   for Access_Mode use (Read_Only => 1, Write_Only => 2, Read_Write => 3);
   Version : constant := 1;
   Control_Bytes : constant := 4096;
   Value_Offset : constant := Control_Bytes;
   Frame_Bytes : constant := Control_Bytes + CCL.Objects.Native_Image_Bytes;
   Maximum_Name : constant := 128;
   Maximum_Revision : constant Unsigned_64 := 2 ** 63 - 1;
   type Padding_Array is array (1 .. Control_Bytes - 184) of Unsigned_8 with Component_Size => 8;
   --  Only raw modular fields/string/bytes cross an untrusted mapping. No
   --  untrusted enums, Boolean representations, pointers or local type IDs.
   type Open_Descriptor is record
      Format : Unsigned_32 := Version;
      Access_Rights : Unsigned_32 := 0;
      Context : Unsigned_64 := 0;
      Schema : CCL.Objects.Schema_Key := CCL.Objects.No_Schema;
      Name_Length, Reserved : Unsigned_32 := 0;
      Name : String (1 .. Maximum_Name) := [others => Character'Val (0)];
      Padding : Padding_Array := [others => 0];
   end record with Size => Control_Bytes * 8, Alignment => 4096;
   for Open_Descriptor use record
      Format at 0 range 0 .. 31;
      Access_Rights at 4 range 0 .. 31;
      Context at 8 range 0 .. 63;
      Schema at 16 range 0 .. 255;
      Name_Length at 48 range 0 .. 31;
      Reserved at 52 range 0 .. 31;
      Name at 56 range 0 .. Maximum_Name * 8 - 1;
      Padding at 184 range 0 .. (Control_Bytes - 184) * 8 - 1;
   end record;

   Creation_Bytes : constant := Control_Bytes + CCL.Objects.Schemas.Native_Schema_Bytes;
   type Creation_Frame is record
      Control : Open_Descriptor;
      Metadata : CCL.Objects.Schemas.Image;
   end record with Size => Creation_Bytes * 8, Alignment => 4096;
   for Creation_Frame use record
      Control at 0 range 0 .. Control_Bytes * 8 - 1;
      Metadata at Control_Bytes range 0 .. CCL.Objects.Schemas.Native_Schema_Bytes * 8 - 1;
   end record;
   type Frame is record
      Control : Open_Descriptor;
      Value : CCL.Objects.Image;
   end record with Size => Frame_Bytes * 8, Alignment => 4096;
   for Frame use record
      Control at 0 range 0 .. Control_Bytes * 8 - 1;
      Value at Value_Offset range 0 .. CCL.Objects.Native_Image_Bytes * 8 - 1;
   end record;

   procedure Describe
     (Name : String; Access_Rights : Access_Mode; Context : Unsigned_64;
      Schema : CCL.Objects.Schema_Key; Item : out Open_Descriptor; Valid : out Boolean);
   function Valid_Descriptor (Item : Open_Descriptor) return Boolean
     with Post => (if Valid_Descriptor'Result then Item.Name_Length in 1 .. Maximum_Name);
   --  Open: (packed grant). Get: (handle, packed grant). Set additionally
   --  carries expected revision. Close: (handle). Unused words must be zero.
   function Request
     (Action : Operation; Grant : CuBit.Grant_References.Reference;
      Handle : Unsigned_64 := 0; Revision : Unsigned_64 := 0) return CuBit.Messages.Message;
   function Valid_Request (Item : CuBit.Messages.Message; Action : Operation) return Boolean;
   function Reply (Code : Status; Value : Unsigned_64 := 0) return CuBit.Messages.Message is
     (tag => (label => Status'Enum_Rep (Code),
              length => (if Code in Success | Stale then 1 else 0), flags => 0, reserved => 0),
      authorityTag => 0,
      words => [0 => (if Code in Success | Stale then Value else 0), others => 0]);
   --  Success carries opened handle/read revision/committed revision; Close
   --  carries zero. Stale is read-only. Every error exposes no payload.
   -- Uncertain is a pending Set/Create whose effect cannot be determined. It
   -- never means rollback and does not authorize retry. Unavailable on these
   -- operations denotes failure before staging their persistent work.
   -- Create success includes an opened handle, not just a saved definition;
   -- failure to grant that handle does not imply absence of the definition.
   function Valid_Reply
     (Item : CuBit.Messages.Message; Action : Operation;
      Expected_Revision : Unsigned_64 := 0) return Boolean;
end Config_Object_Messages;
