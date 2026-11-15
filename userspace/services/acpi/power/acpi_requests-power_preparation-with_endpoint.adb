pragma Ada_2022;
with CuBit.Power_Event_Exchange;
package body ACPI_Requests.Power_Preparation.With_Endpoint with SPARK_Mode is
   package E renames CuBit.Power_Event_Exchange;
   use type Objects.Register_Model, Bridge.Outcome, Driver.Outcome;
   Window_Microseconds : constant Unsigned_64 := Unsigned_64 (E.Prepare_Budget_Milliseconds) * 1_000;
   function Method_Error (Value : Methods.Preparation_Report) return Startup_Error_Code is
     (case Value.Status is
        when Methods.Not_Declared | Methods.Lookup_Failed => Error_Unknown_Name,
        when Methods.Wrong_Object_Type => Error_Invalid_Method,
        when Methods.Budget_Exhausted => Error_Budget_Exceeded,
        when Methods.Cleanup_Failed | Methods.Provider_Contract_Error => Error_Field_Indeterminate,
        when Methods.Identity_Changed => Error_Unsupported_Value,
        when others => Error_Code (Value.Execution));
   function Data_Error (Value : Objects.Query_Report) return Startup_Error_Code is
     (case Value.Outcome is
        when Objects.Not_Declared | Objects.Lookup_Failed => Error_Unknown_Name,
        when Objects.Wrong_Result_Type | Objects.Wrong_Element_Type | Objects.Identity_Changed => Error_Unsupported_Value,
        when Objects.Invalid_Type_Value => Error_Numeric_Overflow,
        when Objects.Budget_Exhausted => Error_Budget_Exceeded,
        when Objects.Cleanup_Failed | Objects.Provider_Contract_Error => Error_Field_Indeterminate,
        when Objects.Evaluation_Failed => Error_Code (Value.Execution),
        when others => Error_Bad_Package);
   procedure Dispatch (Server : in out State; Item : in out Run;
      Stamp : Interfaces.Unsigned_64; Request : Packet;
      Reply : out Packet; Handled : out Boolean) is
      Now : Unsigned_64; Clock_Available : Boolean;
      Registers : Objects.Register_Model;
      Result : Report; Copy : Driver.Copied_Types;
      procedure Status (Value : E.Prepare_Status; Detail : Startup_Error_Code := Error_Returned;
         Has_Detail : Boolean := False) is
      begin
         Reply.Data (E.Prepare_Status_Word) := Unsigned_64 (E.Prepare_Status'Enum_Rep (Value));
         Reply.Data (E.Detail_Word) := (if Has_Detail then Unsigned_64 (Startup_Error_Code'Enum_Rep (Detail)) else 0);
      end Status;
   begin
      Reply := (others => <>); Handled := Request.Label = E.Prepare_To_Sleep;
      if not Handled then return; end if;
      Reply.Label := E.Prepare_To_Sleep; Reply.Length := E.Prepare_Reply_Words;
      Reply.Data (E.Reply_Request_Word) := Request.Data (E.Request_Word);
      if Stamp /= E.Authority_Tag then Status (E.Not_Approved); return; end if;
      if Request.Flags /= 0 or else Request.Reserved /= 0 or else Request.Data (3) /= 0
        or else not E.Sleep_Request_Valid (Unsigned_64 (Request.Length), Request.Data (E.Request_Word),
          Request.Data (E.State_Word), Request.Data (E.Confirm_Word))
      then Status (E.Malformed); return; end if;
      if Request.Data (E.State_Word) /= Unsigned_64 (E.Sleep_State'Enum_Rep (E.S5)) then
         Status (E.Unsupported_State); return;
      end if;
      -- A valid new S5 attempt invalidates the old copy even when admission
      -- or clock fails. Denied/malformed/unsupported requests cannot do so.
      Invalidate (Item);
      if not Owner_Ready (Server) then Status (E.Not_Ready); return; end if;
      Registers := Register_Model;
      if Registers = Objects.No_Sleep_Registers then Status (E.Not_Ready); return; end if;
      Read_Clock (Now, Clock_Available);
      if not Clock_Available or else Now >= Unsigned_64'Last - Window_Microseconds then
         Status (E.Not_Ready); return;
      end if;
      Prepare (Server, Item, Registers, Fuel, Now + Window_Microseconds, Result);
      Copy := Current_Types (Server, Item);
      if Result.Status in Bridge.Prepared | Bridge.Prepared_With_Method_Failure and then Copy.Available then
         Reply.Data (E.Sleep_Types_Word) := E.Sleep_Types_Of (Unsigned_16 (Copy.Type_A), Unsigned_16 (Copy.Type_B));
         if Result.Status = Bridge.Prepared then Status (E.Prepared);
         else Status (E.Method_Failed, Method_Error
           (Result.Preparation.Method_Results (Result.Preparation.First_Failure)), True); end if;
      elsif Result.Status = Bridge.Preparation_Failed and then Result.Closed
        and then Result.Preparation.Status = Driver.No_Sleep_Data
      then Status (E.No_Sleep_Package, Data_Error (Result.Preparation.Data_Result), True);
      else Status (E.Not_Ready);
      end if;
   end Dispatch;
end ACPI_Requests.Power_Preparation.With_Endpoint;
