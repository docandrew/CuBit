pragma Ada_2022;
with Bridge_Trace; use Bridge_Trace;
with System.Storage_Elements;
package body CuAlloc_Native is
 function Allocate_Zeroed(Bytes,Alignment : Unsigned_64) return Unsigned_64 is
 begin
  if Bytes not in 4096 | 8192 or else Alignment/=4096 then raise Program_Error; end if;
  Allocated_Bytes:=Bytes;
  Note(Allocation); Buffer:=[others=>0];
  return Unsigned_64(System.Storage_Elements.To_Integer(Buffer'Address));
 end Allocate_Zeroed;
 function Usable_Size(Item : Unsigned_64) return Unsigned_64 is
 begin
  if Item/=Unsigned_64(System.Storage_Elements.To_Integer(Buffer'Address)) then raise Program_Error; end if;
  return Buffer'Length;
 end Usable_Size;
end CuAlloc_Native;
