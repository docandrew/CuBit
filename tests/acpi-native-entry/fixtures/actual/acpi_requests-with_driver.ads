pragma Ada_2022;
with ACPI_Device_Initialization;
generic
   with package Device_Driver is new ACPI_Device_Initialization
     (Core => ACPI_Service, others => <>);
      with procedure Execute_Module_Startup
        (Service : in out ACPI_Service.State; Budget : Natural;
         Report : out ACPI_Service.Execution_Report;
         Status : out ACPI_Service.Values.Access_Status) is ACPI_Service.Execute_Frozen;
package ACPI_Requests.With_Driver with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   -- Service-owner selected driver and process-lifetime Run. No wire-selected
   -- evaluator or authority. Actual register access must go down to its backend;
   -- never synchronously call a peer blocked in this snapshot-completion request.
   -- All non-finish requests retain the existing request handler and protocol.
   procedure Handle (Server : in out State; Run : in out Device_Driver.Session; Origin : Authority;
                     Request : Packet; Reply : out Response) with
     Pre => Valid (Server), Post => Valid (Server) and then (if Origin = No_Authority then Reply.Status = Denied and then Reply.Data = [0, 0, 0, 0]
              else Reply.Data (0) = Revision (Server))
       and then Revision (Server) >= Revision (Server)'Old
       and then (if Origin = Snapshot_Provider
         and then Request.Label in Start_Snapshot .. Finish_Snapshot
         and then Reply.Status in OK | Table_Rejected | Incomplete
         then Revision (Server)'Old < Max_Revision
           and then Revision (Server) = Revision (Server)'Old + 1)
       and then (for all Word of Reply.Data => Word <= Max_Revision)
       and then (if Origin /= Snapshot_Provider or else Request.Label in Read_Metrics | Read_Table_Info | Read_Table_Chunk | Read_MCFG_Info .. Read_DMAR_Path | Read_Notification | Read_Notification_State
         or else Reply.Status in Denied | Malformed | Stale | Wrong_Order | Resource_Limit
         then Model (Server) = Model (Server)'Old);
end ACPI_Requests.With_Driver;
