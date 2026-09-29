------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Virtio_Net_Control;

package body Virtio.Modern is

   --  Common configuration (virtio 1.0 4.1.4.3), by offset.
   DEVICE_FEATURE_SELECT : constant := 16#00#;
   DEVICE_FEATURE        : constant := 16#04#;
   DRIVER_FEATURE_SELECT : constant := 16#08#;
   DRIVER_FEATURE        : constant := 16#0C#;
   MSIX_CONFIG           : constant := 16#10#;
   DEVICE_STATUS         : constant := 16#14#;
   QUEUE_SELECT          : constant := 16#16#;
   QUEUE_SIZE_REG        : constant := 16#18#;
   QUEUE_MSIX_VECTOR     : constant := 16#1A#;
   QUEUE_ENABLE          : constant := 16#1C#;
   QUEUE_NOTIFY_OFF      : constant := 16#1E#;
   QUEUE_DESC            : constant := 16#20#;
   QUEUE_DRIVER          : constant := 16#28#;
   QUEUE_DEVICE          : constant := 16#30#;
   COMMON_SIZE           : constant := 16#38#;
   NET_CONFIG_SIZE       : constant := 6;       --  the MAC
   NOTIFY_WRITE          : constant := 2;       --  a 16-bit queue index
   MAXIMUM_NOTIFY_OFF    : constant := 16#FFFF#;
   RESET_POLLS           : constant := 1_000_000;

   Base : constant Integer_Address :=
     Integer_Address (CuBit.Virtio_Net_Control.Modern_Virtual_Address);
   Common_At, Device_At, Notify_At : Integer_Address := 0;
   Notify_Multiplier, Mapped_Bytes : Unsigned_64 := 0;
   Notify_Address : array (Unsigned_16 range 0 .. 1) of Integer_Address := [others => 0];
   Notify_Ready   : array (Unsigned_16 range 0 .. 1) of Boolean := [others => False];

   function A (Offset : Integer_Address) return Address is (To_Address (Base + Offset));

   function R8 (Offset : Integer_Address) return Unsigned_8 is
      V : Unsigned_8 with Volatile, Import, Address => A (Offset);
   begin
      return V;
   end R8;
   procedure W8 (Offset : Integer_Address; Value : Unsigned_8) is
      V : Unsigned_8 with Volatile, Import, Address => A (Offset);
   begin
      V := Value;
   end W8;
   function R16 (Offset : Integer_Address) return Unsigned_16 is
      V : Unsigned_16 with Volatile, Import, Address => A (Offset);
   begin
      return V;
   end R16;
   procedure W16 (Offset : Integer_Address; Value : Unsigned_16) is
      V : Unsigned_16 with Volatile, Import, Address => A (Offset);
   begin
      V := Value;
   end W16;
   function R32 (Offset : Integer_Address) return Unsigned_32 is
      V : Unsigned_32 with Volatile, Import, Address => A (Offset);
   begin
      return V;
   end R32;
   procedure W32 (Offset : Integer_Address; Value : Unsigned_32) is
      V : Unsigned_32 with Volatile, Import, Address => A (Offset);
   begin
      V := Value;
   end W32;
   --  64-bit fields as two 32-bit writes, low first (virtio 1.0 4.1.3.1).
   procedure W64 (Offset : Integer_Address; Value : Unsigned_64) is
   begin
      W32 (Offset, Unsigned_32 (Value and 16#FFFF_FFFF#));
      W32 (Offset + 4, Unsigned_32 (Shift_Right (Value, 32)));
   end W64;

   function Fits (Offset, Length : Unsigned_64) return Boolean is
     (Offset <= Mapped_Bytes and then Length <= Mapped_Bytes - Offset);

   procedure Bind
     (Common, Device, Notify, Multiplier, Mapped : Unsigned_64; OK : out Boolean) is
   begin
      Mapped_Bytes := Mapped;
      OK := Mapped <= CuBit.Virtio_Net_Control.Maximum_Modern_Bytes and then
            Fits (Common, COMMON_SIZE) and then Fits (Device, NET_CONFIG_SIZE) and then
            Fits (Notify, NOTIFY_WRITE) and then
            Common mod 4 = 0 and then Notify mod 2 = 0 and then Multiplier mod 2 = 0 and then
            Multiplier <= Mapped;
      if OK then
         Common_At := Integer_Address (Common);
         Device_At := Integer_Address (Device);
         Notify_At := Integer_Address (Notify);
         Notify_Multiplier := Multiplier;
      end if;
   end Bind;

   procedure Reset is
   begin
      W8 (Common_At + DEVICE_STATUS, STATUS_RESET);
      --  The reset completes when the status reads zero (4.1.4.3.2).
      for Poll in 1 .. RESET_POLLS loop
         exit when R8 (Common_At + DEVICE_STATUS) = STATUS_RESET;
      end loop;
   end Reset;

   procedure Negotiate (Wanted : Unsigned_64; Agreed : out Unsigned_64; OK : out Boolean) is
      Offered : Unsigned_64;
      Status  : Unsigned_8 := STATUS_ACKNOWLEDGE;
   begin
      Reset;
      W8 (Common_At + DEVICE_STATUS, Status);
      Status := Status or STATUS_DRIVER;
      W8 (Common_At + DEVICE_STATUS, Status);
      W32 (Common_At + DEVICE_FEATURE_SELECT, 0);
      Offered := Unsigned_64 (R32 (Common_At + DEVICE_FEATURE));
      W32 (Common_At + DEVICE_FEATURE_SELECT, 1);
      Offered := Offered or Shift_Left (Unsigned_64 (R32 (Common_At + DEVICE_FEATURE)), 32);
      Agreed := Offered and Wanted;
      if (Agreed and F_VERSION_1) = 0 then
         OK := False;
         return;
      end if;
      W32 (Common_At + DRIVER_FEATURE_SELECT, 0);
      W32 (Common_At + DRIVER_FEATURE, Unsigned_32 (Agreed and 16#FFFF_FFFF#));
      W32 (Common_At + DRIVER_FEATURE_SELECT, 1);
      W32 (Common_At + DRIVER_FEATURE, Unsigned_32 (Shift_Right (Agreed, 32)));
      Status := Status or STATUS_FEATURES_OK;
      W8 (Common_At + DEVICE_STATUS, Status);
      OK := (R8 (Common_At + DEVICE_STATUS) and STATUS_FEATURES_OK) /= 0;
   end Negotiate;

   procedure Setup_Queue
     (Index : Unsigned_16; Size : Unsigned_16; Desc, Driver, Device : Unsigned_64;
      Vector : Unsigned_16; OK : out Boolean)
   is
      Offset : Unsigned_64;
   begin
      OK := False;
      if Index > Notify_Address'Last then
         return;
      end if;
      W16 (Common_At + QUEUE_SELECT, Index);
      if R16 (Common_At + QUEUE_SIZE_REG) < Size then
         return;   --  the device cannot hold our ring
      end if;
      W16 (Common_At + QUEUE_SIZE_REG, Size);
      W16 (Common_At + QUEUE_MSIX_VECTOR, Vector);
      if R16 (Common_At + QUEUE_MSIX_VECTOR) /= Vector then
         return;
      end if;
      W64 (Common_At + QUEUE_DESC, Desc);
      W64 (Common_At + QUEUE_DRIVER, Driver);
      W64 (Common_At + QUEUE_DEVICE, Device);
      --  This queue's doorbell: notify_off times the multiplier into the
      --  notification structure, which must lie inside what was mapped.
      Offset := Unsigned_64 (Notify_At) +
        Unsigned_64 (R16 (Common_At + QUEUE_NOTIFY_OFF)) * Notify_Multiplier;
      if Unsigned_64 (R16 (Common_At + QUEUE_NOTIFY_OFF)) > MAXIMUM_NOTIFY_OFF or else
        not Fits (Offset, NOTIFY_WRITE)
      then
         return;
      end if;
      Notify_Address (Index) := Integer_Address (Offset);
      Notify_Ready (Index) := True;
      W16 (Common_At + QUEUE_ENABLE, 1);
      OK := True;
   end Setup_Queue;

   procedure Set_Config_Vector (Vector : Unsigned_16; OK : out Boolean) is
   begin
      W16 (Common_At + MSIX_CONFIG, Vector);
      OK := R16 (Common_At + MSIX_CONFIG) = Vector;
   end Set_Config_Vector;

   procedure Start is
   begin
      W8 (Common_At + DEVICE_STATUS, R8 (Common_At + DEVICE_STATUS) or STATUS_DRIVER_OK);
   end Start;

   procedure Notify (Index : Unsigned_16) is
   begin
      if Index <= Notify_Address'Last and then Notify_Ready (Index) then
         W16 (Notify_Address (Index), Index);
      end if;
   end Notify;

   function Device_Byte (I : Natural) return Unsigned_8 is
     (if Unsigned_64 (I) < NET_CONFIG_SIZE then R8 (Device_At + Integer_Address (I)) else 0);

end Virtio.Modern;
