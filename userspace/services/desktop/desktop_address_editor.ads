------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The Deskbar's per-interface addressing editor (UI-027): DHCP, static
--  (address/prefix, gateway) or off, and the DNS servers, for one
--  interface by its stable name. It checks what was typed before anything
--  is applied, shows the CCL the change is, and keeps what was in force
--  before the last Apply so the row can revert it in place.
--
--  @description
--  Applying is netmgr's (CuBit.Net_Manager_Control Apply, then Set_DNS
--  when the DNS text changed); the editor only says what to submit
--  (Plan) and takes the outcome (Applied, Refused). netstack has one DNS
--  pair for every interface: the DNS field sets that pair.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Net_Link;
with CuBit.Net_Manager_Report;
with Desktop_Net_Address;

package Desktop_Address_Editor with SPARK_Mode is
   package NL renames CuBit.Net_Link;
   package NMR renames CuBit.Net_Manager_Report;
   package DNA renames Desktop_Net_Address;
   use type NMR.Policy, DNA.Problem;

   type Method is (Automatic, Static, Disabled);
   type Field_Name is (Address_Field, Gateway_Field, DNS_Field);
   --  A field's text: the model's value, not an editing widget (the
   --  panel edits it in a toolkit field, CuBit.UI.Editor, and hands the
   --  result here with Set_Field).
   subtype Field_Length is Natural range 0 .. DNA.Maximum_Field;
   type Field_Text is record
      Data   : String (1 .. DNA.Maximum_Field) := [others => ' '];
      Length : Field_Length := 0;
   end record;
   function Text (F : Field_Text) return String is (F.Data (1 .. F.Length));
   type Field_Set is array (Field_Name) of Field_Text;

   --  Characters the fields take: digits, dots, the prefix slash and the
   --  DNS separators.
   function Accepts (Ch : Character) return Boolean is
     (Ch in '0' .. '9' | '.' | '/' | ',' | ' ');

   --  The longest setting source the editor writes (a Static one is about
   --  140 characters; a named ceiling, checked).
   Maximum_Source : constant := 192;
   subtype Source_Length is Natural range 0 .. Maximum_Source;
   type Source_Text is record
      Data   : String (1 .. Maximum_Source) := [others => ' '];
      Length : Source_Length := 0;
   end record;
   function Image (S : Source_Text) return String is (S.Data (1 .. S.Length));

   type Outcome is (Editing, Applying, Applied, Refused, Reverting, Reverted);

   type Editor is record
      Open      : Boolean := False;
      Name      : NL.Interface_Name;
      Chosen    : Method := Automatic;
      Fields    : Field_Set;
      Focus     : Field_Name := Address_Field;
      --  What is in force (as the editor filled it on opening, or as last
      --  applied), and what was before the last Apply: Revert's.
      In_Force_Method : Method := Automatic;
      In_Force  : Field_Set;
      Previous_Method : Method := Automatic;
      Previous  : Field_Set;
      Can_Revert : Boolean := False;
      Result    : Outcome := Editing;
      Problem   : DNA.Problem := DNA.None;
   end record;

   function Method_Of (P : NMR.Policy) return Method is
     (case P is when NMR.Automatic => Automatic, when NMR.Static => Static,
                when NMR.Disabled => Disabled);

   --  Open for S, its fields filled from what is in force (and the DNS
   --  servers DNS_1, DNS_2 netmgr reports).
   procedure Open_For (E : out Editor; S : NMR.Interface_Status; DNS_1, DNS_2 : Unsigned_32)
   with Pre => NMR.Normal (S), Post => E.Open and then E.Result = Editing;

   procedure Close (E : in out Editor) with Post => not E.Open;

   --  A field takes typing: static addressing and DNS only, and not
   --  while a change is being applied.
   function Editable (E : Editor; F : Field_Name) return Boolean is
     (E.Result not in Applying | Reverting and then (E.Chosen = Static or else F = DNS_Field));

   --  The panel's field F now holds Text (ignored when not Editable or
   --  Text has a character the field does not take).
   procedure Set_Field (E : in out Editor; F : Field_Name; Text : String)
   with Pre => Text'Length <= DNA.Maximum_Field;
   procedure Choose (E : in out Editor; M : Method) with Post => E.Chosen = M;

   --  What the fields say, checked: the setting's source (when Problem is
   --  None), and the DNS servers.
   procedure Plan (E : Editor; Source : out Source_Text; DNS_1, DNS_2 : out Unsigned_32;
                   DNS_Changed : out Boolean; Problem : out DNA.Problem);

   --  The CCL the panel shows for the fields (empty while they have a
   --  problem).
   function Shown_CCL (E : Editor) return String
   with Post => Shown_CCL'Result'Length <= 2 * Maximum_Source;

   --  Apply pressed: Problem set (nothing to submit) or Applying.
   procedure Start_Apply (E : in out Editor; Submit : out Boolean)
   with Post => (if Submit then E.Result = Applying and then E.Problem = DNA.None);

   --  Revert pressed: the fields go back to Previous and are submitted.
   procedure Start_Revert (E : in out Editor; Submit : out Boolean)
   with Post => (if Submit then E.Result = Reverting);

   --  netmgr's answer to the Apply (or the revert's).
   procedure Finished (E : in out Editor; OK : Boolean);

   function Result_Text (E : Editor) return String is
     (if E.Problem /= DNA.None then DNA.Problem_Text (E.Problem)
      else (case E.Result is
               when Editing   => "",
               when Applying  => "Applying",
               when Applied   => "Until restart",
               when Refused   => "netmgr refused it",
               when Reverting => "Reverting",
               when Reverted  => "Reverted"));
end Desktop_Address_Editor;
