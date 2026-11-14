with Interfaces; use Interfaces;
with CCL_Log_Selection;
with Log_Window_Script; use Log_Window_Script;

--  A Linux-hosted stand-in for logstore (not CuBit): each scripted service's
--  instances are replayed, newest first, record by record with their gaps
--  and then their tally, into the same CCL_Log_Selection CuBit's
--  native/ccl_log_io.adb drives from its logstore subscriptions.
package body CCL_Log_IO is
   package Logs renames CCL.Interfaces.Logs;
   package Failures renames CuBit.Failures;

   function Level_Of (Item : Scripted_Record) return Logs.Severity is (Item.Level);
   function Text_Of (Item : Scripted_Record) return String is (Item.Text (1 .. Item.Length));
   package Selections is new CCL_Log_Selection (Scripted_Record, Level_Of, Text_Of);

   function Available return Boolean is (True);
   procedure Announce (Text : String) is null;
   procedure Note (Text : String) is null;

   procedure Between
     (Service : String; Within : Logs.Log_Window; Contract : CCL.Objects.Binding;
      Image : out CCL.Objects.Holders.Holder; Success : out Boolean;
      Why : out CuBit.Failures.Failure)
   is
      Chosen : Selections.Selection;
   begin
      Calls := Calls + 1;
      Why := (others => <>);
      Logs.Start (Contract, Image);
      Success := Known (Service);
      if not Success then
         Why := Failures.Failed (Failures.Not_Found, "no service named """ & Service & """ is scripted");
         return;
      end if;
      Selections.Start (Chosen, Within);
      for Instance of Instances_Of (Service) loop
         Selections.Begin_Instance (Chosen, Instance.Source);
         for Index in 1 .. Instance.Records loop
            declare
               Step : constant Scripted_Step := Step_Of (Instance, Index);
            begin
               if Step.Not_Retained > 0 then
                  Selections.Omitted (Chosen, Step.Not_Retained, 0, Next => Step.Time);
               end if;
               Selections.Logged (Chosen, Step.Time, Step.Value);
            end;
         end loop;
         Selections.Tallied
           (Chosen, (Published => Unsigned_64 (Instance.Records) + Instance.Gap_Count,
                     Retained => Unsigned_64 (Instance.Records), Not_Retained => Instance.Gap_Count,
                     Over_Budget => 0, Publisher_Full => 0));
         Selections.End_Instance (Chosen);
      end loop;
      Selections.Build (Chosen, Contract, Image);
   end Between;

   procedure Recent
     (Service : String; Contract : CCL.Objects.Binding;
      Image : out CCL.Objects.Holders.Holder; Success : out Boolean;
      Why : out CuBit.Failures.Failure) is
   begin
      Between (Service, Logs.WHOLE_HISTORY, Contract, Image, Success, Why);
   end Recent;

   procedure Minimum
     (Level : out Logs.Severity; Success : out Boolean; Why : out CuBit.Failures.Failure) is
   begin
      Level := Logs.Information;
      Success := True;
      Why := (others => <>);
   end Minimum;

   procedure Set_Minimum
     (Level : Logs.Severity; Previous : out Logs.Severity;
      Success : out Boolean; Why : out CuBit.Failures.Failure) is
   begin
      Previous := Level;
      Success := True;
      Why := (others => <>);
   end Set_Minimum;
end CCL_Log_IO;
