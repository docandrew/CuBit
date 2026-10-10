with Interfaces; use Interfaces;
with System;
with ISO_Records;
package ISO9660 is
   function Media_Failed return Boolean;
   --  Immutable /apps payload of the native USB optical volume. Authority
   --  checks belong to filesystem.svc and precede these operations.
   procedure Find (Path : String; Item : out ISO_Records.File_Record;
                   Found : out Boolean);
   --  The volume's 2048-byte blocks (Queue_Describe_Volume); Mounted
   --  False: no optical volume.
   procedure Describe (Blocks : out Unsigned_64; Mounted_OK : out Boolean);
   procedure Read
     (Item : ISO_Records.File_Record; Offset, Count : Unsigned_64;
      Destination : System.Address; Bytes : out Unsigned_64;
      Success : out Boolean);
end ISO9660;
