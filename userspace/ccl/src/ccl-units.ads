--  Values as people read them, chosen by their type (docs/ccl-places.md):
--  a Bytes as "1.2 KiB", a Timestamp as "2026-10-02 18:01",
--  UNIX_File_Permissions as "drwxr-xr-x", Milliseconds as "1.4 s". Front ends format cells
--  through this one table; the Observatory's units.js matches it
--  (golden vectors in tests/ccl-console). The value itself is unchanged:
--  sorting and arithmetic use the number.
package CCL.Units with SPARK_Mode is
   type Unit is (No_Unit, Bytes, Timestamp, UNIX_File_Permissions, Milliseconds);

   --  The unit a type's name carries (No_Unit for any other type).
   function Unit_Of (Type_Name : String) return Unit;

   --  Text: a value as its literal prints it (decimal). The human form,
   --  or Text itself when it is not a decimal number or has no unit.
   function Humanize (Of_Unit : Unit; Text : String) return String
     with Post => (if Of_Unit = No_Unit then Humanize'Result = Text);
end CCL.Units;
