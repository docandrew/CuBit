pragma Ada_2022;
package body Hosted_Byte_Memory with SPARK_Mode is
   procedure Read
     (Memory : State; Address : Number; Width : Natural;
      Value : out Number; Accepted : out Boolean)
   is
      Factor : Number := 1;
   begin
      Value := 0;
      Accepted := Can_Access (Address, Width);
      if not Accepted then return; end if;
      for Offset in 0 .. Width - 1 loop
         Value := Value + Number (Memory.Data (Cell_Index (Address) + Offset)) * Factor;
         Factor := Factor * 256;
      end loop;
   end Read;
   procedure Write
     (Memory : in out State; Address : Number; Width : Natural;
      Value : Number; Accepted : out Boolean)
   is
      Remaining : Number := Value;
   begin
      Accepted := Can_Access (Address, Width);
      if not Accepted then return; end if;
      for Offset in 0 .. Width - 1 loop
         Memory.Data (Cell_Index (Address) + Offset) := Octet (Remaining mod 256);
         Remaining := Remaining / 256;
      end loop;
   end Write;
end Hosted_Byte_Memory;
