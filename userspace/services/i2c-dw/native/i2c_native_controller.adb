pragma Ada_2022;
with Intel_LPSS_I2C_Profile;
package body I2C_Native_Controller is
   package P renames Intel_LPSS_I2C_Profile;
   use type Registers.Outcome;
   use type Controller.Phase;
   use type Controller.Admission;
   Attempted : Boolean := False;
   procedure Initialize
     (Authority : CuBit.Handles.Device_Memory;
      Facts : Controller.Startup; Virtual : I2C_Mapping_Policy.Address;
      State : in out Controller.State; Mapping : out Registers.Report;
      Admission : out Controller.Admission; Started : out Boolean) is
   begin
      Started := False; Admission := Controller.Busy; Mapping := (others => <>);
      if Attempted or else Controller.Observe (State).At_Phase /= Controller.Cold then return; end if;
      Attempted := True;
      if not P.Supported (Facts.Vendor, Facts.Device) then
         Admission := Controller.Invalid_Resource; return;
      end if;
      Registers.Bind (Authority, I2C_Mapping_Policy.Address (Facts.BAR_Base),
         I2C_Mapping_Policy.Address (Facts.Window_Bytes), Virtual, Mapping);
      if Mapping.Status /= Registers.Bound or else not Registers.Ready then
         Admission := Controller.Invalid_Resource; return;
      end if;
      Controller.Start (State, Facts, Admission);
      Started := Admission = Controller.Accepted;
   end Initialize;
end I2C_Native_Controller;
