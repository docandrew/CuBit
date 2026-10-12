------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body Launch_Configuration with SPARK_Mode is

   function Header (Kind : Configuration_Kind) return Encoded
   with Post => Header'Result.Length = Header_Words + Field_Count (Kind)
                and then Header'Result.Words (1) = Magic
                and then Header'Result.Words (2) = Configuration_Kind'Enum_Rep (Kind)
                and then Header'Result.Words (3) = Unsigned_64 (Field_Count (Kind));

   function To_Word (Flag : Boolean) return Unsigned_64;
   function Is_Address (Word : Unsigned_64) return Boolean;
   function Is_Bytes (Word : Unsigned_64) return Boolean;
   function Is_Offset (Word : Unsigned_64) return Boolean;
   function Is_Flag (Word : Unsigned_64) return Boolean;

   function Header (Kind : Configuration_Kind) return Encoded
   is
      Result : Encoded;
   begin
      Result.Words (1) := Magic;
      Result.Words (2) := Configuration_Kind'Enum_Rep (Kind);
      Result.Words (3) := Unsigned_64 (Field_Count (Kind));
      Result.Length := Header_Words + Field_Count (Kind);
      return Result;
   end Header;

   function To_Word (Flag : Boolean) return Unsigned_64 is
     (if Flag then 1 else 0);

   function Encode (Value : Device_Manager_Configuration) return Encoded is
      Result : Encoded := Header (Device_Manager);
   begin
      Result.Words (4) := Value.Boot_Image;
      Result.Words (5) := Value.Framebuffer.First;
      Result.Words (6) := Value.Framebuffer.Bytes;
      return Result;
   end Encode;

   function Encode (Value : Process_Manager_Configuration) return Encoded is
      Result : Encoded := Header (Process_Manager);
   begin
      Result.Words (4) := Value.Framebuffer.First;
      Result.Words (5) := Value.Framebuffer.Bytes;
      return Result;
   end Encode;

   function Encode (Value : Boot_Image_Configuration) return Encoded is
      Result : Encoded := Header (Boot_Image_Reader);
   begin
      Result.Words (4) := Value.Boot_Image;
      return Result;
   end Encode;

   function Encode
     (Value : Controller_Configuration; Kind : Configuration_Kind) return Encoded
   is
      Result : Encoded := Header (Kind);
   begin
      Result.Words (4) := Value.Registers;
      Result.Words (5) := Value.DMA;
      return Result;
   end Encode;

   function Encode
     (Value : Queue_Configuration; Kind : Configuration_Kind) return Encoded
   is
      Result : Encoded := Header (Kind);
   begin
      Result.Words (4) := Value.DMA;
      return Result;
   end Encode;

   function Encode (Value : Virtio_GPU_Configuration) return Encoded is
      Result : Encoded := Header (Virtio_GPU);
   begin
      Result.Words (4)  := Value.Registers;
      Result.Words (5)  := Value.DMA;
      Result.Words (6)  := Value.Second_DMA;
      Result.Words (7)  := Value.DMA_Bytes;
      Result.Words (8)  := Value.Common_Offset;
      Result.Words (9)  := Value.Notify_Offset;
      Result.Words (10) := Value.ISR_Offset;
      Result.Words (11) := Value.Device_Offset;
      Result.Words (12) := Value.Notify_Multiplier;
      Result.Words (13) := To_Word (Value.Boot_Display);
      return Result;
   end Encode;

   function Is_Address (Word : Unsigned_64) return Boolean is
     (Word in Physical_Address);
   function Is_Bytes (Word : Unsigned_64) return Boolean is
     (Word in Byte_Count);
   function Is_Offset (Word : Unsigned_64) return Boolean is
     (Word in Register_Offset);
   function Is_Flag (Word : Unsigned_64) return Boolean is
     (Word <= 1);

   procedure Decode
     (Words : Word_Array; Value : out Device_Manager_Configuration; Valid : out Boolean)
   is
   begin
      Value := (others => <>);
      Valid := Header_Valid (Words, Device_Manager)
        and then Is_Bytes (Words (4)) and then Is_Address (Words (5))
        and then Is_Bytes (Words (6));
      if Valid then
         Value := (Boot_Image  => Words (4),
                   Framebuffer => (First => Words (5), Bytes => Words (6)));
      end if;
   end Decode;

   procedure Decode
     (Words : Word_Array; Value : out Process_Manager_Configuration; Valid : out Boolean)
   is
   begin
      Value := (others => <>);
      Valid := Header_Valid (Words, Process_Manager)
        and then Is_Address (Words (4)) and then Is_Bytes (Words (5));
      if Valid then
         Value := (Framebuffer => (First => Words (4), Bytes => Words (5)));
      end if;
   end Decode;

   procedure Decode
     (Words : Word_Array; Value : out Boot_Image_Configuration; Valid : out Boolean)
   is
   begin
      Value := (others => <>);
      Valid := Header_Valid (Words, Boot_Image_Reader) and then Is_Bytes (Words (4));
      if Valid then
         Value := (Boot_Image => Words (4));
      end if;
   end Decode;

   procedure Decode
     (Words : Word_Array; Kind : Configuration_Kind;
      Value : out Controller_Configuration; Valid : out Boolean)
   is
   begin
      Value := (others => <>);
      Valid := Header_Valid (Words, Kind)
        and then Is_Address (Words (4)) and then Is_Address (Words (5));
      if Valid then
         Value := (Registers => Words (4), DMA => Words (5));
      end if;
   end Decode;

   procedure Decode
     (Words : Word_Array; Kind : Configuration_Kind;
      Value : out Queue_Configuration; Valid : out Boolean)
   is
   begin
      Value := (others => <>);
      Valid := Header_Valid (Words, Kind) and then Is_Address (Words (4));
      if Valid then
         Value := (DMA => Words (4));
      end if;
   end Decode;

   procedure Decode
     (Words : Word_Array; Value : out Virtio_GPU_Configuration; Valid : out Boolean)
   is
   begin
      Value := (others => <>);
      Valid := Header_Valid (Words, Virtio_GPU)
        and then Is_Address (Words (4)) and then Is_Address (Words (5))
        and then Is_Address (Words (6)) and then Is_Bytes (Words (7))
        and then Is_Offset (Words (8)) and then Is_Offset (Words (9))
        and then Is_Offset (Words (10)) and then Is_Offset (Words (11))
        and then Is_Offset (Words (12)) and then Is_Flag (Words (13));
      if Valid then
         Value := (Registers         => Words (4),
                   DMA               => Words (5),
                   Second_DMA        => Words (6),
                   DMA_Bytes         => Words (7),
                   Common_Offset     => Words (8),
                   Notify_Offset     => Words (9),
                   ISR_Offset        => Words (10),
                   Device_Offset     => Words (11),
                   Notify_Multiplier => Words (12),
                   Boot_Display      => Words (13) = 1);
      end if;
   end Decode;

end Launch_Configuration;
