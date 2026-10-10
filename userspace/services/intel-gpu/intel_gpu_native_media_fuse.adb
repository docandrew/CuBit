with System.Storage_Elements; use System.Storage_Elements;
package body Intel_GPU_Native_Media_Fuse is
   use Interfaces;
   package Media renames Intel_GPU_Media_Engines;
   Page_Bytes : constant Unsigned_64 := 4096;
   Saved_Raw : Media.Fuse_Word := Media.Unreadable;
   function Last_Raw return Media.Fuse_Word is (Saved_Raw);

   function Read_Fuse return Media.Fuse_Word is
      Base : constant Unsigned_64 := Fuse_Page_Base;
      Within : constant Unsigned_64 :=
        Unsigned_64 (Media.Fuse_Register) mod Page_Bytes;
      Value : Unsigned_32;
   begin
      if Base = 0 or else Base mod Page_Bytes /= 0 or else
        Base > Unsigned_64'Last - Within or else not Owner_Ready
      then return Media.Unreadable; end if;
      declare
         Register : Unsigned_32 with Import, Volatile_Full_Access,
           Address => To_Address (Integer_Address (Base + Within));
      begin
         Value := Register;
      end;
      return Value;
   end Read_Fuse;

   function Sample (Vendor, Device : Unsigned_16) return Media.Engines is
      First, Second : Media.Fuse_Word;
      Result : Media.Engines;
   begin
      First := Read_Fuse;
      Second := Read_Fuse;
      Result := Media.Decode_Stable (Vendor, Device, First, Second);
      if not Owner_Ready then
         Saved_Raw := Media.Unreadable;
         return (others => <>);
      end if;
      Saved_Raw := (if First = Second then First else Media.Unreadable);
      return Result;
   end Sample;
end Intel_GPU_Native_Media_Fuse;
