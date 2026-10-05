with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit; use CuBit;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Capability_Grants;
procedure Main is
   type Words is array (0 .. 5) of Unsigned_64;
   Data : aliased Words := [others => 0];
   Result : Unsigned_64;
   Self : constant CuBit.Capability_Grants.Recipient :=
     CuBit.Capability_Grants.Capture (0);
begin
   Result := syscall (SYSCALL_INSPECT_CAPABILITY, syscall (SYSCALL_GETPID), 25,
     Unsigned_64 (To_Integer (Data'Address)));
   if Result /= 1 or else Data (0) /= 0 or else
     not CuBit.Capability_Grants.Valid (Self)
   then
      debugPrint ("TEST: FAIL render-startup software capability" & ASCII.LF);
      return;
   end if;
   debugPrint ("RENDER-STARTUP: software child incarnation=" &
     Unsigned_64'Image (CuBit.Capability_Grants.Incarnation (Self)) & ASCII.LF);
end Main;
