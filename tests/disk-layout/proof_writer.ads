--  A SPARK instance of Layout_Writer over a null sink and source, so
--  gnatprove analyses the generic writer's body (absence of run-time errors).
pragma Ada_2022;
with Interfaces; use Interfaces;
with Disk_Layout; use Disk_Layout;
with Install_Steps;
with Install_Tree;
with Layout_Writer;

package Proof_Writer with SPARK_Mode is
   procedure Put (Offset : Byte_Offset; Data : Bytes; OK : out Boolean)
     with Global => null;
   procedure Zero (Offset : Byte_Offset; Length : Unsigned_64; OK : out Boolean)
     with Global => null;
   procedure Get (Offset : Byte_Offset; Data : out Bytes; OK : out Boolean)
     with Global => null;
   procedure Read
     (Volume : Install_Volume; File : Install_Tree.Entry_Index; Offset : Unsigned_64;
      Data : out Bytes; OK : out Boolean)
     with Global => null;
   procedure Check
     (Volume : Install_Volume; File : Install_Tree.Entry_Index; Offset : Unsigned_64;
      Data : Bytes; Final : Boolean; Matches : out Boolean)
     with Global => null;
   procedure Flush (OK : out Boolean)
     with Global => null;
   procedure Enter (Step : Install_Steps.Step)
     with Global => null;
   package Writer is new Layout_Writer (Put, Zero, Get, Read, Check, Flush, Enter);
end Proof_Writer;
