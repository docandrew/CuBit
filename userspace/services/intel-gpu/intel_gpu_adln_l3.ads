with Interfaces; use Interfaces;
with System;
package Intel_GPU_ADLN_L3 with SPARK_Mode is
   -- Intel TGL Vol2c1265-1266,1273; Vol7 configuration 7.
   -- This describes the TGL allocation layout, not every gfx12 variant.
   -- In particular bit 9 is reserved here, NOT FullWayAllocationEnable.
   Allocation_Offset : constant Unsigned_32 := 16#B134#;
   Parameters_Offset : constant Unsigned_32 := 16#B164#;
   type B1 is mod 2 ** 1 with Size => 1;
   type B2 is mod 2 ** 2 with Size => 2;
   type B3 is mod 2 ** 3 with Size => 3;
   type B7 is mod 2 ** 7 with Size => 7;
   type B8 is mod 2 ** 8 with Size => 8;
   type B16 is mod 2 ** 16 with Size => 16;
   type Allocation is record
      Error_Status : B1 := 0;
      URB_Ways : B7 := 0;
      Reserved_8 : B3 := 0;
      Read_Only_Ways : B7 := 0;
      Data_Ways : B7 := 0;
      All_Client_Ways : B7 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Allocation use record
      Error_Status at 0 range 0 .. 0;
      URB_Ways at 0 range 1 .. 7;
      Reserved_8 at 0 range 8 .. 10;
      Read_Only_Ways at 0 range 11 .. 17;
      Data_Ways at 0 range 18 .. 24;
      All_Client_Ways at 0 range 25 .. 31;
   end record;
   function Encode (V : Allocation) return Unsigned_32 is
     (Unsigned_32 (V.Error_Status) or
      Shift_Left (Unsigned_32 (V.URB_Ways), 1) or
      Shift_Left (Unsigned_32 (V.Reserved_8), 8) or
      Shift_Left (Unsigned_32 (V.Read_Only_Ways), 11) or
      Shift_Left (Unsigned_32 (V.Data_Ways), 18) or
      Shift_Left (Unsigned_32 (V.All_Client_Ways), 25));
   function Decode_Allocation (V : Unsigned_32) return Allocation is
     ((Error_Status => B1 (V and 1),
       URB_Ways => B7 (Shift_Right (V, 1) and 127),
       Reserved_8 => B3 (Shift_Right (V, 8) and 7),
       Read_Only_Ways => B7 (Shift_Right (V, 11) and 127),
       Data_Ways => B7 (Shift_Right (V, 18) and 127),
       All_Client_Ways => B7 (Shift_Right (V, 25) and 127)));
   -- Field-level allocation check only: not topology/capacity admission.
   function Render_Allocation_Matches (V : Allocation) return Boolean is
     (V.Error_Status = 0 and V.URB_Ways = 32 and
      V.Read_Only_Ways = 0 and V.Data_Ways = 0 and V.All_Client_Ways = 88);
   type Parameters is record
      Tagged_Ways : B8 := 0;
      Untagged_Ways : B8 := 0;
      Reserved_16 : B16 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Parameters use record
      Tagged_Ways at 0 range 0 .. 7;
      Untagged_Ways at 0 range 8 .. 15;
      Reserved_16 at 0 range 16 .. 31;
   end record;
   function Encode (V : Parameters) return Unsigned_32 is
     (Unsigned_32 (V.Tagged_Ways) or
      Shift_Left (Unsigned_32 (V.Untagged_Ways), 8) or
      Shift_Left (Unsigned_32 (V.Reserved_16), 16));
   function Decode_Parameters (V : Unsigned_32) return Parameters is
     ((Tagged_Ways => B8 (V and 16#FF#),
       Untagged_Ways => B8 (Shift_Right (V, 8) and 16#FF#),
       Reserved_16 => B16 (Shift_Right (V, 16))));
   -- TGL Vol2c part2 pp84-85: each low bit disables one bank.
   type Fuse_Control is record
      Disabled_Banks : B8 := 0;
      Reserved_8 : B16 := 0;
      WGBox_Configuration : B2 := 0;
      Reserved_26 : B2 := 0;
      Hash_Mode : B1 := 0;
      Reserved_29 : B3 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Fuse_Control use record
      Disabled_Banks at 0 range 0 .. 7;
      Reserved_8 at 0 range 8 .. 23;
      WGBox_Configuration at 0 range 24 .. 25;
      Reserved_26 at 0 range 26 .. 27;
      Hash_Mode at 0 range 28 .. 28;
      Reserved_29 at 0 range 29 .. 31;
   end record;
   function Encode (V : Fuse_Control) return Unsigned_32 is
     (Unsigned_32 (V.Disabled_Banks) or
      Shift_Left (Unsigned_32 (V.Reserved_8), 8) or
      Shift_Left (Unsigned_32 (V.WGBox_Configuration), 24) or
      Shift_Left (Unsigned_32 (V.Reserved_26), 26) or
      Shift_Left (Unsigned_32 (V.Hash_Mode), 28) or
      Shift_Left (Unsigned_32 (V.Reserved_29), 29));
   function Decode_Fuse (V : Unsigned_32) return Fuse_Control is
     ((Disabled_Banks => B8 (V and 255),
       Reserved_8 => B16 (Shift_Right (V, 8) and 65535),
       WGBox_Configuration => B2 (Shift_Right (V, 24) and 3),
       Reserved_26 => B2 (Shift_Right (V, 26) and 3),
       Hash_Mode => B1 (Shift_Right (V, 28) and 1),
       Reserved_29 => B3 (Shift_Right (V, 29))));
   function Enabled_Banks (V : Fuse_Control) return Natural
     with Post => Enabled_Banks'Result <= 8;
   -- Fixed four-bank ADL-N probe only. Caller establishes identity, stable
   -- valid reads, forcewake and ownership, with this allocation programmed.
   -- Other bank/hash configurations need separate admission, not guessing.
   -- Reserved bits are deliberately NOT compared to determine capacity.
   function Probe_URB_KiB
     (Owned : Boolean; Fuse : Fuse_Control; Info : Parameters;
      Observed : Allocation) return Natural
     with Post => Probe_URB_KiB'Result in 0 | 512;
   -- 4 KiB/way/bank: 128 KiB URB, 352 KiB combined tagged clients.
   -- Matches Mesa tgl_l3_configs[0]. This constant grants no capacity:
   -- caller must establish SKU/bank inventory, synchronization, allocation
   -- programming and absence of allocation error before using that storage.
   Render_Allocation : constant Allocation :=
     (URB_Ways => 32, All_Client_Ways => 88, others => <>);
   Default_Allocation : constant Allocation :=
     (URB_Ways => 16, All_Client_Ways => 104, others => <>);
end Intel_GPU_ADLN_L3;
