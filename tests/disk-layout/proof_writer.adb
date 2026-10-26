package body Proof_Writer with SPARK_Mode is
   procedure Put (Offset : Byte_Offset; Data : Bytes; OK : out Boolean) is
      pragma Unreferenced (Offset, Data);
   begin
      OK := True;
   end Put;

   procedure Zero (Offset : Byte_Offset; Length : Unsigned_64; OK : out Boolean) is
      pragma Unreferenced (Offset, Length);
   begin
      OK := True;
   end Zero;

   procedure Read
     (Volume : Install_Volume; File : Install_Tree.Entry_Index; Offset : Unsigned_64;
      Data : out Bytes; OK : out Boolean)
   is
      pragma Unreferenced (Volume, File, Offset);
   begin
      Data := [others => 0];
      OK := True;
   end Read;
end Proof_Writer;
