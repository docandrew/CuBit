------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages;
with CuBit.Monotonic;

package body RTW89_MMIO is

   Base : Unsigned_64 := 0;

   procedure Attach (Virtual : Unsigned_64) is
   begin
      Base := Virtual;
   end Attach;

   function Attached return Boolean is (Base /= 0);

   function Address_Of (Offset : Register) return System.Address is
     (To_Address (Integer_Address (Base + Unsigned_64 (Offset))));

   function Raw32 (Offset : Register) return Unsigned_32 is
      Cell : Unsigned_32 with Import, Volatile, Address => Address_Of (Offset);
   begin
      return Cell;
   end Raw32;

   function Read32 (Offset : Register) return Unsigned_32 is
      Value : Unsigned_32 := Raw32 (Offset);
   begin
      if Offset in CMAC_REG_START .. CMAC_REG_END then
         for Attempt in 1 .. CMAC_DEAD_RETRIES loop
            exit when Value /= Dead;
            Write32 (CK_EN, CMAC_ALLCKEN);
            Value := Raw32 (Offset);
         end loop;
      end if;
      return Value;
   end Read32;

   Byte_Lane_Mask : constant Register := 16#0003#;
   Bits_Per_Byte  : constant := 8;

   function Read16 (Offset : Register) return Unsigned_16 is
   begin
      if Offset in CMAC_REG_START .. CMAC_REG_END then
         return Unsigned_16 (Shift_Right (Read32 (Offset and not Byte_Lane_Mask),
                             Natural (Offset and Byte_Lane_Mask) * Bits_Per_Byte)
                             and 16#FFFF#);
      end if;
      declare
         Cell : Unsigned_16 with Import, Volatile, Address => Address_Of (Offset);
      begin
         return Cell;
      end;
   end Read16;

   function Read8 (Offset : Register) return Unsigned_8 is
   begin
      if Offset in CMAC_REG_START .. CMAC_REG_END then
         return Unsigned_8 (Shift_Right (Read32 (Offset and not Byte_Lane_Mask),
                            Natural (Offset and Byte_Lane_Mask) * Bits_Per_Byte)
                            and 16#FF#);
      end if;
      declare
         Cell : Unsigned_8 with Import, Volatile, Address => Address_Of (Offset);
      begin
         return Cell;
      end;
   end Read8;

   procedure Write32 (Offset : Register; Value : Unsigned_32) is
      Cell : Unsigned_32 with Import, Volatile, Address => Address_Of (Offset);
   begin
      Cell := Value;
   end Write32;

   procedure Write16 (Offset : Register; Value : Unsigned_16) is
      Cell : Unsigned_16 with Import, Volatile, Address => Address_Of (Offset);
   begin
      Cell := Value;
   end Write16;

   procedure Write8 (Offset : Register; Value : Unsigned_8) is
      Cell : Unsigned_8 with Import, Volatile, Address => Address_Of (Offset);
   begin
      Cell := Value;
   end Write8;

   procedure Set32 (Offset : Register; Bits : Unsigned_32) is
   begin
      Write32 (Offset, Read32 (Offset) or Bits);
   end Set32;

   procedure Clear32 (Offset : Register; Bits : Unsigned_32) is
   begin
      Write32 (Offset, Read32 (Offset) and not Bits);
   end Clear32;

   procedure Set16 (Offset : Register; Bits : Unsigned_16) is
   begin
      Write16 (Offset, Read16 (Offset) or Bits);
   end Set16;

   procedure Clear16 (Offset : Register; Bits : Unsigned_16) is
   begin
      Write16 (Offset, Read16 (Offset) and not Bits);
   end Clear16;

   procedure Set8 (Offset : Register; Bits : Unsigned_8) is
   begin
      Write8 (Offset, Read8 (Offset) or Bits);
   end Set8;

   procedure Clear8 (Offset : Register; Bits : Unsigned_8) is
   begin
      Write8 (Offset, Read8 (Offset) and not Bits);
   end Clear8;

   procedure Field32 (Offset : Register; Mask : Unsigned_32; Shift : Natural;
                      Value : Unsigned_32) is
   begin
      Write32 (Offset, (Read32 (Offset) and not Mask)
                       or (Shift_Left (Value, Shift) and Mask));
   end Field32;

   ---------------------------------------------------------------------------
   --  Bounded waiting
   ---------------------------------------------------------------------------

   --  A register read over PCIe takes well over 10 ns; this caps the loop
   --  even when the monotonic clock is unavailable or stopped.
   Iterations_Per_Microsecond_Cap : constant := 100;

   type Waiter is record
      Start     : Unsigned_64;
      Budget    : Microseconds;
      Clocked   : Boolean;
      Remaining : Unsigned_64;
   end record;

   function Begin_Wait (Budget : Microseconds) return Waiter is
      Now : constant CuBit.Monotonic.Reading := CuBit.Monotonic.Read;
   begin
      return (Start     => (if Now.Available then Now.Microseconds else 0),
              Budget    => Budget,
              Clocked   => Now.Available,
              Remaining => Budget * Iterations_Per_Microsecond_Cap);
   end Begin_Wait;

   --  False once the budget is spent.
   function Keep_Waiting (W : in out Waiter) return Boolean is
   begin
      if W.Remaining = 0 then
         return False;
      end if;
      W.Remaining := W.Remaining - 1;
      if W.Clocked then
         declare
            Now : constant CuBit.Monotonic.Reading := CuBit.Monotonic.Read;
         begin
            if Now.Available and then Now.Microseconds - W.Start > W.Budget then
               return False;
            end if;
         end;
      end if;
      return True;
   end Keep_Waiting;

   function Poll32 (Offset : Register; Mask, Expected : Unsigned_32;
                    Budget : Microseconds) return Boolean is
      W : Waiter := Begin_Wait (Budget);
   begin
      loop
         if (Read32 (Offset) and Mask) = Expected then
            return True;
         end if;
         exit when not Keep_Waiting (W);
      end loop;
      --  One last look: the deadline may have passed while we were preempted.
      return (Read32 (Offset) and Mask) = Expected;
   end Poll32;

   function Poll16 (Offset : Register; Mask, Expected : Unsigned_16;
                    Budget : Microseconds) return Boolean is
      W : Waiter := Begin_Wait (Budget);
   begin
      loop
         if (Read16 (Offset) and Mask) = Expected then
            return True;
         end if;
         exit when not Keep_Waiting (W);
      end loop;
      return (Read16 (Offset) and Mask) = Expected;
   end Poll16;

   function Poll8 (Offset : Register; Mask, Expected : Unsigned_8;
                   Budget : Microseconds) return Boolean is
      W : Waiter := Begin_Wait (Budget);
   begin
      loop
         if (Read8 (Offset) and Mask) = Expected then
            return True;
         end if;
         exit when not Keep_Waiting (W);
      end loop;
      return (Read8 (Offset) and Mask) = Expected;
   end Poll8;

   procedure Delay_For (Budget : Microseconds) is
      W : Waiter := Begin_Wait (Budget);
   begin
      while Keep_Waiting (W) loop
         null;
      end loop;
   end Delay_For;

   procedure Sleep (Duration : Milliseconds) is
      Ignore : Unsigned_64;
   begin
      --  The kernel's tick may end a sleep up to one tick early: ask for one
      --  more than the minimum.
      Ignore := CuBit.Messages.syscall (CuBit.Messages.SYSCALL_SLEEP, Duration + 1);
   end Sleep;

   ---------------------------------------------------------------------------
   --  Crystal serial interface
   ---------------------------------------------------------------------------
   XTAL_Budget : constant Microseconds := 50_000;

   procedure XTAL_Command (Command : Unsigned_32; Result : out Unsigned_32;
                           OK : out Boolean) is
   begin
      Write32 (WLAN_XTAL_SI_CTRL, Command);
      OK := Poll32 (WLAN_XTAL_SI_CTRL, XTAL_SI_CMD_POLL, 0, XTAL_Budget);
      Result := Read32 (WLAN_XTAL_SI_CTRL);
   end XTAL_Command;

   procedure XTAL_Write (Offset : XTAL_Offset; Value, Mask : Unsigned_8;
                         OK : out Boolean) is
      Result : Unsigned_32;
   begin
      XTAL_Command
        (Shift_Left (Unsigned_32 (Offset), XTAL_SI_ADDR_SHIFT)
         or Shift_Left (Unsigned_32 (Value), XTAL_SI_DATA_SHIFT)
         or Shift_Left (Unsigned_32 (Mask), XTAL_SI_BITMASK_SHIFT)
         or Shift_Left (XTAL_SI_NORMAL_WRITE, XTAL_SI_MODE_SHIFT)
         or XTAL_SI_CMD_POLL, Result, OK);
   end XTAL_Write;

   procedure XTAL_Read (Offset : XTAL_Offset; Value : out Unsigned_8;
                        OK : out Boolean) is
      Result : Unsigned_32;
   begin
      XTAL_Command
        (Shift_Left (Unsigned_32 (Offset), XTAL_SI_ADDR_SHIFT)
         or Shift_Left (XTAL_SI_NORMAL_READ, XTAL_SI_MODE_SHIFT)
         or XTAL_SI_CMD_POLL, Result, OK);
      Value := Unsigned_8 (Shift_Right (Result, XTAL_SI_DATA_SHIFT) and XTAL_SI_BYTE_MASK);
   end XTAL_Read;

   ---------------------------------------------------------------------------
   --  MDIO
   ---------------------------------------------------------------------------
   MDIO_Budget : constant Microseconds := 2_000;

   procedure MDIO_Start (Address : Unsigned_8; Speed : PHY_Speed;
                         Flag : Unsigned_16; OK : out Boolean) is
      Page  : Unsigned_16;
      Value : Unsigned_16;
   begin
      Page := (case Speed is when Gen1 => 0, when Gen2 => 2)
        + (if Address >= MDIO_HIGH_PAGE_FIRST then 1 else 0);
      Write8 (MDIO_CFG, Address and Unsigned_8 (MDIO_ADDR_MASK));
      Value := Read16 (MDIO_CFG);
      Value := (Value and not MDIO_PAGE_MASK) or Shift_Left (Page, MDIO_PAGE_SHIFT);
      Write16 (MDIO_CFG, Value);
      Set16 (MDIO_CFG, Flag);
      OK := Poll16 (MDIO_CFG, Flag, 0, MDIO_Budget);
   end MDIO_Start;

   procedure MDIO_Read (Address : Unsigned_8; Speed : PHY_Speed;
                        Value : out Unsigned_16; OK : out Boolean) is
   begin
      MDIO_Start (Address, Speed, MDIO_RFLAG, OK);
      Value := Read16 (MDIO_RDATA);
   end MDIO_Read;

   procedure MDIO_Write (Address : Unsigned_8; Speed : PHY_Speed;
                         Value : Unsigned_16; OK : out Boolean) is
   begin
      Write16 (MDIO_WDATA, Value);
      MDIO_Start (Address, Speed, MDIO_WFLAG, OK);
   end MDIO_Write;

   ---------------------------------------------------------------------------
   --  DBI
   ---------------------------------------------------------------------------
   DBI_Budget : constant Microseconds := 200;

   procedure DBI_Read (Address : Config_Offset; Value : out Unsigned_8;
                       OK : out Boolean) is
      Lane : constant Register := Register (Address) and Byte_Lane_Mask;
   begin
      Write16 (DBI_FLAG, Address and DBI_ADDR_MASK);
      Write8 (DBI_FLAG_COMMAND, DBI_READ_FLAG);
      OK := Poll8 (DBI_FLAG_COMMAND, 16#FF#, 0, DBI_Budget);
      Value := Read8 (DBI_RDATA + Lane);
   end DBI_Read;

   procedure DBI_Write (Address : Config_Offset; Value : Unsigned_8;
                        OK : out Boolean) is
      Lane : constant Register := Register (Address) and Byte_Lane_Mask;
   begin
      Write8 (DBI_WDATA + Lane, Value);
      Write16 (DBI_FLAG, (Address and DBI_ADDR_MASK)
                         or Shift_Left (Shift_Left (Unsigned_16'(1), Natural (Lane)),
                                        DBI_WREN_SHIFT));
      Write8 (DBI_FLAG_COMMAND, DBI_WRITE_FLAG);
      OK := Poll8 (DBI_FLAG_COMMAND, 16#FF#, 0, DBI_Budget);
   end DBI_Write;

end RTW89_MMIO;
