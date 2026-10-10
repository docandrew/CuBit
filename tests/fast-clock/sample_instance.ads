--  An instance of the generic reader, so GNATprove analyzes its body.
pragma Ada_2022;
with Interfaces; use Interfaces;
with Clock_Publication; use Clock_Publication;
with Clock_Publication.Sample;

package Sample_Instance with SPARK_Mode is
   Published : constant Parameters := Initial (3_000_000_000, 10, 20);
   function Load_Sequence return Unsigned_64 is (2);
   function Load_Fields return Parameters is (Published);
   function Load_Counter return Unsigned_64 is (1_000);
   procedure Read is new Clock_Publication.Sample
     (Load_Sequence, Load_Fields, Load_Counter);
end Sample_Instance;
