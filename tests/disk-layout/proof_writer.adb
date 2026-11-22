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

   procedure Get (Offset : Byte_Offset; Data : out Bytes; OK : out Boolean) is
      pragma Unreferenced (Offset);
   begin
      Data := [others => 0];
      OK := True;
   end Get;

   procedure Read
     (Volume : Install_Volume; File : Install_Tree.Entry_Index; Offset : Unsigned_64;
      Data : out Bytes; OK : out Boolean)
   is
      pragma Unreferenced (Volume, File, Offset);
   begin
      Data := [others => 0];
      OK := True;
   end Read;

   procedure Check
     (Volume : Install_Volume; File : Install_Tree.Entry_Index; Offset : Unsigned_64;
      Data : Bytes; Final : Boolean; Matches : out Boolean)
   is
      pragma Unreferenced (Volume, File, Offset, Data, Final);
   begin
      Matches := True;
   end Check;

   procedure Flush (OK : out Boolean) is
   begin
      OK := True;
   end Flush;

   procedure Enter (Step : Install_Steps.Step) is
      pragma Unreferenced (Step);
   begin
      null;
   end Enter;
end Proof_Writer;
