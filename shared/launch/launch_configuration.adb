------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body Launch_Configuration with SPARK_Mode is

   function Header (Kind : Configuration_Kind) return Encoded
   with Pre  => Kind /= Device_Manager,
        Post => Header'Result.Length = Header_Words + Field_Count (Kind)
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

   function Location_Image (Location : Routing_ID) return Location_Text is
      Hex_Digits : constant String (1 .. 16) := "0123456789ABCDEF";
      Bus_Shift    : constant := 8;
      Device_Shift : constant := 3;
      Device_Mask  : constant := 16#1F#;
      Function_Mask : constant := 16#7#;
      Nibble_Mask  : constant := 16#F#;
      Nibble_Bits  : constant := 4;
      Bus    : constant Unsigned_64 := Shift_Right (Location, Bus_Shift);
      Device : constant Unsigned_64 := Shift_Right (Location, Device_Shift) and Device_Mask;
      Func   : constant Unsigned_64 := Location and Function_Mask;
      function Digit (Value : Unsigned_64) return Character is
        (Hex_Digits (Natural (Value and Nibble_Mask) + 1));
   begin
      return [1 => Digit (Shift_Right (Bus, Nibble_Bits)), 2 => Digit (Bus), 3 => ':',
              4 => Digit (Shift_Right (Device, Nibble_Bits)), 5 => Digit (Device), 6 => '.',
              7 => Digit (Func)];
   end Location_Image;

   function Encode_Prefix (Value : Device_Manager_Configuration) return Encoded is
      Result : Encoded;
   begin
      Result.Words (1) := Magic;
      Result.Words (2) := Configuration_Kind'Enum_Rep (Device_Manager);
      Result.Words (3) := Device_Manager_Fields (Value.Device_Roots, Value.Firmware_Roots);
      Result.Words (Device_Roots_Word) := Value.Device_Roots;
      Result.Words (Firmware_Roots_Word) := Value.Firmware_Roots;
      Result.Words (Boot_Image_Word) := Value.Boot_Image;
      Result.Words (Framebuffer_First_Word) := Value.Framebuffer.First;
      Result.Words (Framebuffer_Bytes_Word) := Value.Framebuffer.Bytes;
      Result.Words (First_Handle_Word .. Device_Manager_Prefix_Words) :=
        Handle_Words (Value.Handles);
      Result.Length := Device_Manager_Prefix_Words;
      return Result;
   end Encode_Prefix;

   function Encode (Value : Process_Manager_Configuration) return Encoded is
      Result : Encoded := Header (Process_Manager);
   begin
      Result.Words (4) := Value.Framebuffer.First;
      Result.Words (5) := Value.Framebuffer.Bytes;
      Result.Words (6 .. 21) := Process_Handle_Words (Value.Handles);
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
      Result.Words (6) := Value.Location;
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

   function Encode (Value : Network_Device_Configuration) return Encoded is
      Result : Encoded := Header (Network_Device);
   begin
      Result.Words (4) := Value.DMA;
      Result.Words (5) := Value.Location;
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
      Valid := Device_Manager_Valid (Words);
      if Valid then
         Value := (Device_Roots   => Words (Device_Roots_Word),
                   Firmware_Roots => Words (Firmware_Roots_Word),
                   Boot_Image     => Words (Boot_Image_Word),
                   Framebuffer    => (First => Words (Framebuffer_First_Word),
                                      Bytes => Words (Framebuffer_Bytes_Word)),
                   Handles        =>
                     (Self              => Words (First_Handle_Word),
                      Self_Process      => Words (First_Handle_Word + 1),
                      Self_Grantable    => Words (First_Handle_Word + 2),
                      Process_Root      => Words (First_Handle_Word + 3),
                      PCI_Ports         => Words (First_Handle_Word + 4),
                      Registration      => Words (First_Handle_Word + 5),
                      Vector_Authority  => Words (First_Handle_Word + 6),
                      IO_Ports          => Words (First_Handle_Word + 7),
                      Legacy_Interrupts => Words (First_Handle_Word + 8),
                      Notifications     => Words (First_Handle_Word + 9),
                      Scheduling        => Words (First_Handle_Word + 10),
                      Firmware_Snapshot => Words (First_Handle_Word + 11)));
      end if;
   end Decode;

   procedure Decode
     (Words : Word_Array; Value : out Process_Manager_Configuration; Valid : out Boolean)
   is
   begin
      Value := (others => <>);
      Valid := Header_Valid (Words, Process_Manager)
        and then Is_Address (Words (4)) and then Is_Bytes (Words (5));
      if not Valid then
         return;
      end if;
      Value.Handles :=
        (Self => Words (6), Self_Process => Words (7),
         Self_Grantable => Words (8), Process_Root => Words (9),
         Registration => Words (10), Notifications => Words (11),
         Filesystem => Words (12), Config => Words (13),
         Netstack => Words (14), Network_Policy => Words (15),
         Mixer => Words (16), Device_Manager => Words (17),
         Ready => Words (18), Framebuffer => Words (19),
         Device_Inspection => Words (20), Firmware_Snapshot => Words (21));
      Valid := Process_Handles_Well_Formed (Value.Handles);
      if Valid then
         Value.Framebuffer := (First => Words (4), Bytes => Words (5));
      else
         Value := (others => <>);
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
        and then Is_Address (Words (4)) and then Is_Address (Words (5))
        and then Words (6) in Routing_ID;
      if Valid then
         Value := (Registers => Words (4), DMA => Words (5), Location => Words (6));
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
     (Words : Word_Array; Value : out Network_Device_Configuration; Valid : out Boolean)
   is
   begin
      Value := (others => <>);
      Valid := Header_Valid (Words, Network_Device)
        and then Is_Address (Words (4)) and then Words (5) in Routing_ID;
      if Valid then
         Value := (DMA => Words (4), Location => Words (5));
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

   --  Generic_PCI words: 4 location, 5 vendor, 6 device, 7 revision,
   --  8 class code, 9 window count, then (first, bytes) per window, then
   --  DMA first, DMA bytes, DMA virtual, delivery and vectors.
   GENERIC_WINDOW_COUNT : constant := 9;
   GENERIC_WINDOWS      : constant := 10;
   GENERIC_DMA          : constant := GENERIC_WINDOWS + 2 * Maximum_Windows;
   GENERIC_DELIVERY     : constant := GENERIC_DMA + 3;

   function Encode (Value : Generic_PCI_Configuration) return Encoded is
      Result : Encoded := Header (Generic_PCI);
   begin
      Result.Words (4) := Value.Location;
      Result.Words (5) := Value.Vendor;
      Result.Words (6) := Value.Device;
      Result.Words (7) := Value.Revision;
      Result.Words (8) := Value.Class_Code;
      Result.Words (GENERIC_WINDOW_COUNT) := Unsigned_64 (Value.Window_Total);
      for K in Window_Index loop
         Result.Words (GENERIC_WINDOWS + 2 * (K - 1)) := Value.Windows (K).First;
         Result.Words (GENERIC_WINDOWS + 2 * (K - 1) + 1) := Value.Windows (K).Bytes;
      end loop;
      Result.Words (GENERIC_DMA) := Value.DMA.First;
      Result.Words (GENERIC_DMA + 1) := Value.DMA.Bytes;
      Result.Words (GENERIC_DMA + 2) := Value.DMA_Virtual;
      Result.Words (GENERIC_DELIVERY) := Interrupt_Delivery'Enum_Rep (Value.Delivery);
      Result.Words (GENERIC_DELIVERY + 1) := Value.Vectors;
      return Result;
   end Encode;

   procedure Decode
     (Words : Word_Array; Value : out Generic_PCI_Configuration; Valid : out Boolean)
   is
      Delivery : Interrupt_Delivery := Polled;
      Known : Boolean := False;
   begin
      Value := (others => <>);
      Valid := False;
      if not Header_Valid (Words, Generic_PCI)
        or else Words (4) not in Routing_ID or else Words (5) not in PCI_ID
        or else Words (6) not in PCI_ID or else Words (7) not in PCI_Revision
        or else Words (8) not in PCI_Class_Code
        or else Words (GENERIC_WINDOW_COUNT) > Maximum_Windows
        or else Words (GENERIC_DELIVERY + 1) > Maximum_Vectors
        or else Words (GENERIC_DMA + 2) not in Virtual_Address
      then
         return;
      end if;
      for D in Interrupt_Delivery loop
         if Interrupt_Delivery'Enum_Rep (D) = Words (GENERIC_DELIVERY) then
            Delivery := D;
            Known := True;
         end if;
      end loop;
      if not Known then
         return;
      end if;
      for K in Window_Index loop
         if Words (GENERIC_WINDOWS + 2 * (K - 1)) not in Physical_Address
           or else Words (GENERIC_WINDOWS + 2 * (K - 1) + 1) not in Byte_Count
         then
            return;
         end if;
         Value.Windows (K) := (First => Words (GENERIC_WINDOWS + 2 * (K - 1)),
                               Bytes => Words (GENERIC_WINDOWS + 2 * (K - 1) + 1));
      end loop;
      if Words (GENERIC_DMA) not in Physical_Address
        or else Words (GENERIC_DMA + 1) not in Byte_Count
      then
         return;
      end if;
      Value.Location := Words (4);
      Value.Vendor := Words (5);
      Value.Device := Words (6);
      Value.Revision := Words (7);
      Value.Class_Code := Words (8);
      Value.Window_Total := Window_Count (Words (GENERIC_WINDOW_COUNT));
      Value.DMA := (First => Words (GENERIC_DMA), Bytes => Words (GENERIC_DMA + 1));
      Value.DMA_Virtual := Words (GENERIC_DMA + 2);
      Value.Delivery := Delivery;
      Value.Vectors := Words (GENERIC_DELIVERY + 1);
      Valid := Valid_Generic (Value);
      if not Valid then
         Value := (others => <>);
      end if;
   end Decode;

end Launch_Configuration;
