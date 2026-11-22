with Windowed_Reads;

package body Read_Proof with SPARK_Mode is
   procedure Check
     (Length, Reply_Length : Natural; Reply_Success : Boolean;
      Result : out Boolean)
   is
      procedure Transfer
        (Offset : Natural; Count : Positive;
         Transferred : out Natural; Success : out Boolean)
      is
         pragma Unreferenced (Offset, Count);
      begin
         Transferred := Reply_Length;
         Success := Reply_Success;
      end Transfer;
      package Reader is new Windowed_Reads (1024 * 1024, Transfer);
   begin
      Result := Reader.Read_All (Length);
   end Check;
end Read_Proof;
