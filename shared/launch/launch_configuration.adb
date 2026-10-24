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
      Result.Words (7 .. 26) := Handle_Words (Value.Handles);
      return Result;
   end Encode;

   function Encode (Value : Process_Manager_Configuration) return Encoded is
      Result : Encoded := Header (Process_Manager);
   begin
      Result.Words (4) := Value.Framebuffer.First;
      Result.Words (5) := Value.Framebuffer.Bytes;
      Result.Words (6 .. 20) := Process_Handle_Words (Value.Handles);
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
        and then Is_Bytes (Words (6))
        and then Handles_Valid (Words (7 .. 26));
      if Valid then
         Value := (Boot_Image  => Words (4),
                   Framebuffer => (First => Words (5), Bytes => Words (6)),
                   Handles     =>
                     (Self              => Words (7),
                      Self_Process      => Words (8),
                      Self_Grantable    => Words (9),
                      Process_Root      => Words (10),
                      PCI_Ports         => Words (11),
                      Registration      => Words (12),
                      Vector_Authority  => Words (13),
                      IO_Ports          => Words (14),
                      Legacy_Interrupts => Words (15),
                      Notifications     => Words (16),
                      Scheduling        => Words (17),
                      Root_Count        => Device_Root_Count (Words (18)),
                      Device_Roots      => [for R in Device_Root_Index => Words (18 + R)]));
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
         Device_Inspection => Words (20));
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
