------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A trace of Initialize/Adjust/Finalize calls for ada-runtime-check.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Finalization;

package Events with Elaborate_Body is

   Max_Events : constant := 64;
   subtype Event_Count is Natural range 0 .. Max_Events;

   --  One letter per call: I(nitialize), A(djust), F(inalize), followed by
   --  the object's Id digit. Read with Trace; emptied with Clear.
   procedure Clear;
   function Trace return String;
   function Count (Kind : Character) return Natural;

   subtype Object_Id is Natural range 0 .. 9;

   type Tracked is new Ada.Finalization.Controlled with record
      Id : Object_Id := 0;
   end record;
   overriding procedure Initialize (T : in out Tracked);
   overriding procedure Adjust (T : in out Tracked);
   overriding procedure Finalize (T : in out Tracked);

   type Resource is new Ada.Finalization.Limited_Controlled with record
      Id : Object_Id := 0;
   end record;
   overriding procedure Initialize (R : in out Resource);
   overriding procedure Finalize (R : in out Resource);

end Events;
