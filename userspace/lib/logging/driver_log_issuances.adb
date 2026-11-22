pragma Ada_2022;

package body Driver_Log_Issuances with SPARK_Mode is
   procedure Allocate (A : in out Allocator; Value : out Unsigned_64; Granted : out Boolean) is
   begin
      Value := 0;
      Granted := A.Next <= Issuance'Last;
      if Granted then
         Value := A.Next;
         A.Next := A.Next + 1;
      end if;
   end Allocate;
end Driver_Log_Issuances;
