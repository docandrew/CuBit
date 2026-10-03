with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
package body Intel_GPU_Metadata_Initialize is
   function Clear (Address, Bytes : Unsigned_64) return Boolean is
   begin
      if Address = 0 or else Address mod 4096 /= 0 or else Bytes = 0 or else
        Bytes mod 4096 /= 0 or else Bytes > 65536 or else
        Address > Unsigned_64'Last - Bytes then return False; end if;
      declare
         type Words is array (Natural range <>) of Unsigned_64;
         Memory : Words (0 .. Natural (Bytes / 8) - 1)
           with Import, Volatile, Address => To_Address (Integer_Address (Address));
      begin
         for I in Memory'Range loop Memory (I) := 0; end loop;
      end;
      return True;
   end Clear;
end Intel_GPU_Metadata_Initialize;
