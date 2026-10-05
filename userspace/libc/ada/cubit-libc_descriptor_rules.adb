------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Libc_Descriptor_Rules with SPARK_Mode is

   procedure Open_Options (Flags : long; Options : out Unsigned_64; Valid : out Boolean) is
      Mode : constant Unsigned_64 := Access_Mode (Flags);
   begin
      Options := OPEN_READ_ONLY;
      Valid := Mode in Bits (O_RDONLY) | Bits (O_WRONLY) | Bits (O_RDWR);
      if not Valid then
         return;
      end if;
      Options := (if Mode = Bits (O_WRONLY) then OPEN_WRITE_ONLY
                  elsif Mode = Bits (O_RDWR) then OPEN_READ_WRITE
                  else OPEN_READ_ONLY);
      if Has (Flags, O_CREAT) then
         Options := Options or OPEN_CREATE;
         if Has (Flags, O_EXCL) then
            Options := Options or OPEN_EXCLUSIVE;
         end if;
      end if;
      if Has (Flags, O_TRUNC) then
         Options := Options or OPEN_TRUNCATE;
      end if;
   end Open_Options;

   procedure Seek
     (Whence : int; Offset : Integer_64; Current, Size : Unsigned_64;
      Result : out Unsigned_64; Valid : out Boolean)
   is
      Limit : constant Unsigned_64 := Unsigned_64 (Integer_64'Last);
      Base : Unsigned_64;
   begin
      Result := 0;
      Valid := False;
      if Whence = SEEK_SET then
         Base := 0;
      elsif Whence = SEEK_CUR then
         Base := Current;
      elsif Whence = SEEK_END then
         Base := Size;
      else
         return;
      end if;
      if Base > Limit then
         return;
      end if;
      if Offset >= 0 then
         if Unsigned_64 (Offset) > Limit - Base then
            return;
         end if;
         Result := Base + Unsigned_64 (Offset);
      else
         --  -Offset without overflowing for Integer_64'First.
         declare
            Back : constant Unsigned_64 := Unsigned_64 (-(Offset + 1)) + 1;
         begin
            if Back > Base then
               return;
            end if;
            Result := Base - Back;
         end;
      end if;
      Valid := True;
   end Seek;

end CuBit.Libc_Descriptor_Rules;
