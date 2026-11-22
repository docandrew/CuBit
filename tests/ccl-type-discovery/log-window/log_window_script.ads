with Interfaces; use Interfaces;
with CCL.Interfaces.Logs;

--  The scripted services the hosted logstore stand-in replays
--  (log-window/ccl_log_io.adb). An instance's records are at First_Time,
--  First_Time + Spacing, ...; "name: step K"; Gap_Count records not
--  retained just before record Gap_Before (0: none).
package Log_Window_Script is
   package Logs renames CCL.Interfaces.Logs;

   MAXIMUM_TEXT : constant := 32;
   subtype Text_Length is Natural range 0 .. MAXIMUM_TEXT;
   type Scripted_Record is record
      Level : Logs.Severity := Logs.Information;
      Text : String (1 .. MAXIMUM_TEXT) := [others => ' '];
      Length : Text_Length := 0;
   end record;

   type Scripted_Step is record
      Time : Logs.Boot_Milliseconds := 0;
      Not_Retained : Unsigned_64 := 0;
      Value : Scripted_Record;
   end record;

   MAXIMUM_NAME : constant := 8;
   subtype Name_Length is Natural range 0 .. MAXIMUM_NAME;
   type Scripted_Instance is record
      Source : Unsigned_64 := 0;
      Name : String (1 .. MAXIMUM_NAME) := [others => ' '];
      Length : Name_Length := 0;
      Records : Natural := 0;
      First_Time, Spacing : Logs.Boot_Milliseconds := 0;
      Gap_Before : Natural := 0;
      Gap_Count : Unsigned_64 := 0;
   end record;
   type Instance_List is array (Positive range <>) of Scripted_Instance;

   function Known (Service : String) return Boolean;
   --  Service's instances, newest first.
   function Instances_Of (Service : String) return Instance_List;
   function Step_Of (Instance : Scripted_Instance; Index : Positive) return Scripted_Step
     with Pre => Index <= Instance.Records;

   --  The records "flood" has past what one result shows.
   FLOOD_OVER : constant := 100;
   --  How many times the stand-in was asked (a refused window never is).
   Calls : Natural := 0;
end Log_Window_Script;
