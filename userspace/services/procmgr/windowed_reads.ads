--  Exact regular-file transfer sequencing, independent of grants and IPC.
--  A short/error reply never becomes a successful executable image.
generic
   Window_Bytes : Positive;
   with procedure Transfer
     (Offset : Natural; Count : Positive;
      Transferred : out Natural; Success : out Boolean);
package Windowed_Reads with SPARK_Mode is
   function Read_All (Length : Natural) return Boolean;
end Windowed_Reads;
