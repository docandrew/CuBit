------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Realtek's conditional BB/RF register tables (docs/wifi.md D7): choosing
--  the table's configuration for this board, and which entries apply.
--
--  @description
--  Semantics from Linux rtw89 phy.c (rtw89_phy_sel_headline,
--  rtw89_phy_init_reg) and phy.h, GPL-2.0 OR BSD-3-Clause, taken under
--  BSD-3-Clause. A table is (address, data) pairs. It may begin with
--  headlines (address bits 31..28 = 16#F#), each naming a configuration
--  by RFE type (bits 23..16) and chip cut (bits 7..0). One headline is
--  chosen, in rtw89's order: RFE and cut match; RFE match with cut "don't
--  care" (16#FF#); RFE match with the largest cut listed; RFE "don't care"
--  with the largest cut listed. After the headlines, entries whose bits
--  31..28 are 8 (IF), 9 (ELSE IF), 16#A# (ELSE), 16#B# (END) or 4 (CHECK)
--  steer which plain entries are applied: an IF/ELSE IF names a target
--  (bits 27..0) and the CHECK after it applies the branch when the target
--  is the chosen headline's; ELSE applies when no branch matched, but if
--  none did, rtw89 stops loading the table (and so do we: Aborted).
--
--  Proved (tests/wifi-rtw89, level 2): no run-time errors; the walk is a
--  pure function of the entries seen. Tested: on the three 8852B tables
--  with conditions, the entries applied for RFE 1 / cut CBV equal those of
--  an independent Python reading of rtw89's code (differential test).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package RTW89_PHY_Tables with Pure, SPARK_Mode is

   type Table_Entry is record
      Address : Unsigned_32;
      Data    : Unsigned_32;
   end record;

   Maximum_Entries : constant := 65_536;
   subtype Entry_Index is Positive range 1 .. Maximum_Entries;
   type Table is array (Entry_Index range <>) of Table_Entry;

   --  Address fields (phy.h get_phy_headline / get_phy_cond / ...).
   Kind_Shift      : constant := 28;
   Target_Mask     : constant Unsigned_32 := 16#0FFF_FFFF#;
   RFE_Shift       : constant := 16;
   Field_Mask      : constant Unsigned_32 := 16#FF#;

   Headline_Kind   : constant Unsigned_32 := 16#F#;   --  PHY_HEADLINE_VALID
   Branch_If       : constant Unsigned_32 := 16#8#;   --  PHY_COND_BRANCH_IF
   Branch_Else_If  : constant Unsigned_32 := 16#9#;   --  PHY_COND_BRANCH_ELIF
   Branch_Else     : constant Unsigned_32 := 16#A#;   --  PHY_COND_BRANCH_ELSE
   Branch_End      : constant Unsigned_32 := 16#B#;   --  PHY_COND_BRANCH_END
   Branch_Check    : constant Unsigned_32 := 16#4#;   --  PHY_COND_CHECK
   Dont_Care       : constant Unsigned_8  := 16#FF#;  --  PHY_COND_DONT_CARE

   function Kind (Address : Unsigned_32) return Unsigned_32 is (Shift_Right (Address, Kind_Shift));
   function Target (Address : Unsigned_32) return Unsigned_32 is (Address and Target_Mask);
   function RFE_Of (Address : Unsigned_32) return Unsigned_8 is
     (Unsigned_8 (Shift_Right (Address, RFE_Shift) and Field_Mask));
   function Cut_Of (Address : Unsigned_32) return Unsigned_8 is
     (Unsigned_8 (Address and Field_Mask));
   function Compare (RFE, Cut : Unsigned_8) return Unsigned_32 is
     (Shift_Left (Unsigned_32 (RFE), RFE_Shift) or Unsigned_32 (Cut));

   --  The chosen configuration: how many headlines lead the table and the
   --  target the branches are checked against.
   type Selection (Found : Boolean := False) is record
      case Found is
         when True =>
            Headlines  : Natural;
            Cfg_Target : Unsigned_32;
         when False => null;
      end case;
   end record;

   function Select_Headline (T : Table; RFE, Cut : Unsigned_8) return Selection
   with Pre  => T'Length > 0,
        Post => (if Select_Headline'Result.Found then Select_Headline'Result.Headlines < T'Length
                   or else Select_Headline'Result.Headlines = T'Length);

   --  The walk over the entries after the headlines.
   type Walk is record
      Matched : Boolean := True;
      Found   : Boolean := False;
      Branch  : Unsigned_32 := 0;
   end record;

   type Action is (Apply, Skip, Stop);

   --  One entry: what to do with it and the walk's next state.
   procedure Step (W : in out Walk; Cfg_Target : Unsigned_32; E : Table_Entry; Do_It : out Action);

   type Load_Status is (Loaded, No_Configuration, Aborted);

   function Name (S : Load_Status) return String is
     (case S is
        when Loaded           => "loaded",
        when No_Configuration => "no-configuration",
        when Aborted          => "aborted");

   --  Apply every entry the conditions select, in order (Write does the
   --  register write or the delay an entry encodes).
   generic
      with procedure Write (E : Table_Entry);
   procedure Load (T : Table; RFE, Cut : Unsigned_8; Applied : out Natural; Status : out Load_Status)
   with Pre => T'Length > 0;

end RTW89_PHY_Tables;
