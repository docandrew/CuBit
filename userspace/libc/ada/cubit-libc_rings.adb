------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Libc_Rings with SPARK_Mode is

   procedure Put (R : in out Ring; Source : Bytes; Done : out Ring_Count) is
      Old : constant Ring := R with Ghost;
   begin
      Done := Natural'Min (Source'Length, Room (R));
      for K in 0 .. Done - 1 loop
         R.Data ((R.Head + R.Length + K) mod Ring_Bytes) := Source (Source'First + K);
         pragma Loop_Invariant (R.Head = Old.Head and then R.Length = Old.Length
                                and then R.Readers = Old.Readers
                                and then R.Writers = Old.Writers);
         pragma Loop_Invariant
           (for all J in 0 .. Old.Length - 1 => Waiting (R, J) = Waiting (Old, J));
         pragma Loop_Invariant
           (for all J in 0 .. K =>
              Waiting (R, Old.Length + J) = Source (Source'First + J));
      end loop;
      R.Length := R.Length + Done;
   end Put;

   procedure Take (R : in out Ring; Target : out Bytes; Count : out Ring_Count) is
      Old : constant Ring := R with Ghost;
   begin
      Target := [others => 0];
      Count := Natural'Min (Target'Length, R.Length);
      for K in 0 .. Count - 1 loop
         Target (Target'First + K) := Waiting (R, K);
         pragma Loop_Invariant (R = Old);
         pragma Loop_Invariant
           (for all J in 0 .. K => Target (Target'First + J) = Waiting (Old, J));
      end loop;
      R.Head := (R.Head + Count) mod Ring_Bytes;
      R.Length := R.Length - Count;
   end Take;

end CuBit.Libc_Rings;
