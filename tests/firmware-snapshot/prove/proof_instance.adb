pragma Ada_2022;
package body Proof_Instance with SPARK_Mode is
   procedure Copy_Part
     (Index, Offset : Natural; Destination : out Firmware_Tables.Bytes;
      Result : out S.Transfers.Transfer_Result) is
   begin
      S.Transfers.Copy_Part (Snapshot, Index, Offset, Destination, Result);
   end Copy_Part;

   procedure Write_User
     (Destination : Unsigned_64; Source : Firmware_Tables.Bytes; Success : out Boolean) is
   begin
      Success := Destination /= 0;
      if Success and then Written < Unsigned_64'Last then
         Written := Written + 1;
      end if;
      pragma Unreferenced (Source);
   end Write_User;

   procedure Write_Reply (Item : Firmware_Snapshot_ABI.Reply; Success : out Boolean) is
   begin
      Success := Item (1) /= Unsigned_64'Last;
      if Written < Unsigned_64'Last then
         Written := Written + 1;
      end if;
   end Write_Reply;
end Proof_Instance;
