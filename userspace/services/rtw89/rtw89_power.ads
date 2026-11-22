------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  RTL8852B chip identity, MAC power on/off and the physical efuse read
--  (docs/wifi.md, "Power-on", "Chip identity", "Efuse").
--
--  @description
--  Sequences from rtw89 rtw8852b.c (rtw8852b_pwr_on_func,
--  rtw8852b_pwr_off_func), mac.c and efuse.c, resolved for the first
--  power-on of a probe: the efuse has not been read yet, so rtw89's
--  efuse-dependent voltage trims and RFE-type branch take their
--  "not valid" paths. Nothing here enables RF or transmits.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with RTW89_Efuse;
with RTW89_Outcomes; use RTW89_Outcomes;
with RTW89_MMIO;

package RTW89_Power is

   --  rtw89 enum rtw89_cv.
   type Cut_Version is (CAV, CBV, CCV, CDV, CEV, CFV, Unknown);
   function Name (Cut : Cut_Version) return String is
     (case Cut is
        when CAV => "CAV", when CBV => "CBV", when CCV => "CCV",
        when CDV => "CDV", when CEV => "CEV", when CFV => "CFV",
        when Unknown => "unknown");

   type Identity is record
      Cut          : Cut_Version := Unknown;
      Raw_Cut      : Unsigned_8 := 0;
      Analog_Cut   : Unsigned_8 := 0;   --  ACV, from XTAL_SI_CV
   end record;

   --  MAC power state before we touch anything: True when a previous owner
   --  (firmware, a crashed driver) left the MAC on.
   function MAC_Was_On return Boolean with Pre => RTW89_MMIO.Attached;

   procedure Read_Identity (Chip : out Identity; Result : out Outcome)
     with Pre => RTW89_MMIO.Attached;

   --  Trim: apply the efuse-gated regulator trims (rtw8852b_pwr_on_func
   --  with a valid, autoloaded efuse whose power calibration byte is not
   --  set; core start). The probe-time power-on passes False, as rtw89's
   --  does before the efuse is parsed.
   procedure Power_On (Trim : Boolean; Result : out Outcome) with Pre => RTW89_MMIO.Attached;
   procedure Power_Off (Result : out Outcome) with Pre => RTW89_MMIO.Attached;

   --  Whether the hardware autoloaded the efuse (R_AX_SYS_WL_EFUSE_CTRL).
   function Efuse_Autoloaded return Boolean with Pre => RTW89_MMIO.Attached;

   --  Read the whole physical efuse (wifi bank) byte by byte.
   procedure Read_Efuse (Map : out RTW89_Efuse.Physical_Map; Result : out Outcome)
     with Pre => RTW89_MMIO.Attached;

   --  The two efuse bytes rtw89 reads for secure-boot information
   --  (EFUSE_EXTERNALPN_ADDR_AX, EFUSE_CUSTOMER_ADDR_AX). Both 0xFF: a
   --  retail part without secure boot, the only kind the pinned firmware
   --  supports (its security sections carry no signatures).
   Secure_Boot_Info_Address : constant := 16#5EC#;
   Secure_Boot_Info_Bytes   : constant := 2;
   type Secure_Boot_Info is array (1 .. Secure_Boot_Info_Bytes) of Unsigned_8;
   procedure Read_Secure_Boot_Info (Info : out Secure_Boot_Info; Result : out Outcome)
     with Pre => RTW89_MMIO.Attached;

   --  One byte of the physical efuse at any address the controller decodes
   --  (the phycap area 0x580 .. 0x5FF lies past the wifi bank Read_Efuse
   --  maps), with the efuse power cut around it.
   subtype Efuse_Address is Unsigned_32 range 0 .. 16#7FF#;
   procedure Read_Physical_Byte (Address : Efuse_Address; Value : out Unsigned_8;
                                 Result : out Outcome)
     with Pre => RTW89_MMIO.Attached;

end RTW89_Power;
