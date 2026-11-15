pragma Ada_2022;
with ACPI_Inventory_Protocol;
package body ACPI_Local_PCI_Load is
   use type G.Outcome,B.Tick,P.Word;
   function Pending (IO : Context) return Boolean is (IO.Working or else IO.Halted);
   function Is_Stopped (IO : Context) return Boolean is (IO.Halted);
   procedure Read (IO : in out Context; Item : Held_Window; Region : P.Region;
      Offset : P.Word; Width : P.T.Access_Width; Deadline_Microseconds : B.Tick;
      Guard : not null access function return Boolean; Result : out R.Result) is
      Plan : G.View;
      Origin,Relative,Bytes,Value : P.Word;
      Started,Now : B.Tick;
      Good,Completed : Boolean;
   begin
      Result := (others => <>);
      if IO.Working then Result.Status := R.Reentered; return; end if;
      if IO.Halted then Result.Status := R.Stopped; return; end if;
      IO.Working := True;
      Plan := G.Plan_Config (Geometry (Item),Region,Offset,Width);
      if Plan.Status /= G.Read_Ready then Result.Status := R.Invalid_Geometry; IO.Working := False; return; end if;
      Origin := Virtual_Origin (Item); Relative := P.Word (Plan.First); Bytes := P.Word (Plan.Bytes);
      if Origin = 0 or else Origin >= B.User_Virtual_Limit
        or else Relative >= B.User_Virtual_Limit-Origin
        or else Bytes > B.User_Virtual_Limit-Origin-Relative then
         Result.Status := R.Invalid_Mapping; IO.Working := False; return;
      end if;
      Read_Clock (Started,Good);
      if not Good then Result.Status := R.Clock_Unavailable; IO.Working := False; return; end if;
      if Deadline_Microseconds in 0 | B.No_Finite_Deadline or else Started >= Deadline_Microseconds then
         Result.Status := R.Deadline_Expired; IO.Working := False; return;
      end if;
      if not Current (Item) then
         Result.Status := R.Backing_Unavailable; IO.Halted := True; IO.Working := False; return;
      end if;
      Read_Clock (Now,Good);
      if not Good then Result.Status := R.Clock_Unavailable; IO.Working := False; return; end if;
      if Now < Started then Result.Status := R.Clock_Reversed; IO.Working := False; return; end if;
      if Now >= Deadline_Microseconds then Result.Status := R.Deadline_Expired; IO.Working := False; return; end if;
      if not Guard.all then Result.Status := R.Guard_Refused; IO.Working := False; return; end if;
      Transfer (Origin+Relative,Width,Value,Completed);
      if Completed and then Value <= ACPI_Inventory_Protocol.Width_Mask (Width) then
         Result := (R.Read_Complete,Value);
      else Result.Status := R.Completion_Unknown; IO.Halted := True;
      end if;
      IO.Working := False;
   end Read;
end ACPI_Local_PCI_Load;
