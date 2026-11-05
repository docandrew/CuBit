pragma Ada_2022;
package body ACPI_Field_Transactions with SPARK_Mode is
   function Observe (Owner : Context) return Statistics is
      ((Owner.Depth,Owner.Working,Owner.Stopped,Owner.Data_Due,Owner.Physical_Due));
   function Pending (Owner : Context) return Boolean is
      (Owner.Working or else Owner.Stopped or else Owner.Data_Due or else Owner.Physical_Due);
   procedure Begin_Field (Owner : in out Context; IO : in out IO_Context;
      Requires_Lock, For_Write : Boolean; Accepted : in out Boolean) is
      Physical_OK, Data_OK : Boolean := False;
   begin
      Accepted := False;
      if Owner.Working or else Owner.Stopped then return;
      elsif Owner.Depth = Depth_Count'Last then Owner.Stopped := True; return; end if;
      Owner.Working := True;
      Physical_Begin (IO,Requires_Lock,For_Write,Physical_OK);
      if not Physical_OK then
         Owner.Stopped := True; Owner.Physical_Due := True;
      else
         Data_Begin (IO,Requires_Lock,For_Write,Data_OK);
         if Data_OK then Owner.Depth := Owner.Depth + 1; Accepted := True;
         else
            -- Core will not call End for a rejected Begin. Undo exactly this
            -- accepted physical frame now; preserve any failed-begin duty.
            Owner.Stopped := True; Owner.Data_Due := True;
            if Owner.Depth = 0 then
               -- There is no accepted data parent to pop or stop here. Try
               -- closing an uncertain opening scope before physical release.
               Data_Cleanup (IO,Data_OK); Owner.Data_Due := not Data_OK;
            end if;
            Physical_OK := False; Physical_End (IO,Physical_OK);
            if not Physical_OK then Owner.Physical_Due := True; end if;
         end if;
      end if;
      Owner.Working := False;
   end Begin_Field;
   procedure End_Field (Owner : in out Context; IO : in out IO_Context;
      Accepted : in out Boolean) is
      Data_OK, Physical_OK : Boolean := False;
   begin
      Accepted := False;
      if Owner.Working then return;
      elsif Owner.Depth = 0 then Owner.Stopped := True; return; end if;
      Owner.Working := True;
      Data_End (IO,Data_OK);
      Physical_End (IO,Physical_OK);
      Owner.Depth := Owner.Depth - 1;
      if not Data_OK then Owner.Stopped := True; Owner.Data_Due := True; end if;
      if not Physical_OK then Owner.Stopped := True; Owner.Physical_Due := True; end if;
      Owner.Working := False;
      Accepted := Data_OK and then Physical_OK and then not Pending (Owner);
   end End_Field;
   procedure Cleanup (Owner : in out Context; IO : in out IO_Context; Accepted : out Boolean) is
      Data_OK, Physical_OK : Boolean := False;
   begin
      Accepted := False; if Owner.Working then return; end if;
      Owner.Stopped := True;
      if Owner.Depth /= 0 then return; end if;
      Owner.Working := True;
      Data_Cleanup (IO,Data_OK);
      Physical_Cleanup (IO,Physical_OK);
      Owner.Data_Due := not Data_OK; Owner.Physical_Due := not Physical_OK;
      Owner.Working := False; Accepted := Data_OK and then Physical_OK;
   end Cleanup;
end ACPI_Field_Transactions;
