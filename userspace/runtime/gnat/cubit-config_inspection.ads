pragma Ada_2022;
with Interfaces;

--  Read-only inspection of non-secret Config data. Names are not authority.
--  This initial protocol addresses the machine context only. Other context
--  identifiers are rejected, never silently redirected to machine settings.
package CuBit.Config_Inspection with Pure, SPARK_Mode is
   type Operation is (Read_Value, List_Keys, Probe);
   for Operation use
     (Read_Value => 16#0606#, List_Keys => 16#0607#, Probe => 16#0608#);
   type Status is
     (OK, Denied, Missing, Too_Large, Invalid_Request, Unavailable);
   for Status use
     (OK => 16#F000#, Denied => 16#F007#, Missing => 16#F060#,
      Too_Large => 16#F061#, Invalid_Request => 16#F062#,
      Unavailable => 16#F063#);
   type Context_ID is new Interfaces.Unsigned_32;
   Machine_Context : constant Context_ID := 0;
   Maximum_Text : constant := 1_024;
   subtype Text_Length is Natural range 0 .. Maximum_Text;
   type Text is record
      Length : Text_Length := 0;
      Data : String (1 .. Maximum_Text) := [others => ' '];
   end record;

   --  Empty scope denotes an explicitly granted wildcard, not a default.
   --  Trailing dots in existing manifest scopes are accepted. A scope never
   --  matches a partial component ("app" does not match "apple").
   function Contains (Scope, Key : String) return Boolean;
end CuBit.Config_Inspection;
