with System.Storage_Elements; use System.Storage_Elements;
package body Intel_GPU_Record_Store is
   Stride : constant Unsigned_64 := Element'Object_Size / 8;
   function Capacity (Object : Store) return Positive is (Object.Available);
   function Get (Object : Store; Index : Positive) return Element is
   begin
      if Index <= Bootstrap_Count then return Object.Inline (Index); end if;
      if Index > Object.Available or else Object.Base = 0 then return Empty_Element; end if;
      declare
         Value : Element with Import, Address => To_Address (Integer_Address
           (Object.Base + Unsigned_64 (Index - Bootstrap_Count - 1) * Stride));
      begin return Value; end;
   end Get;
   procedure Put (Object : in out Store; Index : Positive; Value : Element) is
   begin
      if Index <= Bootstrap_Count then Object.Inline (Index) := Value; return; end if;
      if Index > Object.Available or else Object.Base = 0 then return; end if;
      declare
         Target : Element with Import, Address => To_Address (Integer_Address
           (Object.Base + Unsigned_64 (Index - Bootstrap_Count - 1) * Stride));
      begin Target := Value; end;
   end Put;
   procedure Extend
     (Object : in out Store; Base, Bytes : Unsigned_64; Accepted : out Boolean) is
      Records : Unsigned_64;
   begin
      Accepted := False;
      if Stride = 0 or else Element'Object_Size mod 8 /= 0 or else
        Stride mod Unsigned_64 (Element'Alignment) /= 0 or else
        Base = 0 or else Base mod 4096 /= 0 or else
        Base mod Unsigned_64 (Element'Alignment) /= 0 or else
        Bytes = 0 or else Bytes mod 4096 /= 0 or else Base > Unsigned_64'Last - Bytes or else
        Bytes <= Object.Bytes or else Bytes - Object.Bytes > 65536 or else
        (Object.Base /= 0 and then Object.Base /= Base) then return; end if;
      Records := Bytes / Stride;
      if Records >= Unsigned_64 (Positive'Last - Bootstrap_Count) then return; end if;
      for I in Object.Available + 1 .. Bootstrap_Count + Natural (Records) loop
         declare
            Target : Element with Import, Address => To_Address (Integer_Address
              (Base + Unsigned_64 (I - Bootstrap_Count - 1) * Stride));
         begin Target := Empty_Element; end;
      end loop;
      Object.Base := Base; Object.Bytes := Bytes;
      Object.Available := Bootstrap_Count + Natural (Records);
      Accepted := True;
   end Extend;
end Intel_GPU_Record_Store;
