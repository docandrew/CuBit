with Interfaces; use Interfaces;
package CuBit.Messages is
   type MessageTag is record
      label : Unsigned_32;
      length, flags : Unsigned_8;
      reserved : Unsigned_16;
   end record;
   type MessageWords is array (0 .. 3) of Unsigned_64;
   type Message is record
      tag : MessageTag;
      authorityTag : Unsigned_64 := 0;
      words : MessageWords;
   end record;
   NULL_MESSAGE : constant Message :=
     ((0, 0, 0, 0), 0, [others => 0]);
   function capCall
     (slot : Unsigned_64; msg : in out Message) return MessageTag;
   procedure debugPrint (value : String) is null;

   --  Test-only transport: real Ext2 code, simulated sector device.
   type Bytes is array (Natural range <>) of Unsigned_8;
   Disk : Bytes (0 .. 65_535) := [others => 0];
   Durable : Bytes (Disk'Range) := [others => 0];
   Grant_Buffer : Bytes (0 .. 4095) := [others => 0];
   type Failure_Mode is (Before_IO, Partial_Transfer, After_IO);
   Fail_At : Natural := 0;
   Mode : Failure_Mode := Before_IO;
   type Failure_Reply is
     (Error_Label, Short_Transfer, Bad_Length, Bad_Flags, Bad_Reserved);
   Reply_Style : Failure_Reply := Error_Label;
   Calls, Writes, Barriers, Reclaims : Natural := 0;
   Publication_Attempted : Boolean := False;
   Failed : Boolean := False;
   Check_Reclamation : Boolean := True;
   Inode_Write_Call, Pointer_Write_Call : Natural := 0;
   Check_Creation : Boolean := False;
   Override_Description : Boolean := False;
   Description_Reply : Message := NULL_MESSAGE;
   Creation_Block : Natural := 20;
   procedure Reset;
end CuBit.Messages;
