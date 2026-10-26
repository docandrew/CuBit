--  A SPARK instance of Layout_Writer over a null sink, so gnatprove
--  analyses the generic writer's body (absence of run-time errors).
pragma Ada_2022;
with Interfaces; use Interfaces;
with Disk_Layout; use Disk_Layout;
with Layout_Writer;

package Proof_Writer with SPARK_Mode is
   procedure Put (Offset : Byte_Offset; Data : Bytes; OK : out Boolean)
     with Global => null;
   procedure Zero (Offset : Byte_Offset; Length : Unsigned_64; OK : out Boolean)
     with Global => null;
   package Writer is new Layout_Writer (Put, Zero);
end Proof_Writer;
