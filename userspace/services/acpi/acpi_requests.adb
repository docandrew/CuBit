pragma Ada_2022;
with Firmware_Tables.DMAR;
with Firmware_Tables.MCFG;
with Firmware_Tables.MADT;
with Firmware_Tables.SRAT;
with Firmware_Tables.SLIT;
package body ACPI_Requests with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   function Valid (Server : State) return Boolean is (ACPI_Bootstrap.Valid (Server.Boot));
   function Model (Server : State) return State_Model is
     (Table_Capacity => Server.Table_Capacity, Byte_Capacity => Server.Byte_Capacity,
      Table_Byte_Limit => Server.Table_Byte_Limit, Boot => ACPI_Bootstrap.Model (Server.Boot),
      Initial_Revision => Server.Initial_Revision,
      Version => Server.Version, Open => Server.Open, ID => Server.ID,
      Kind => Server.Kind, Extent => Server.Extent, Used => Server.Used,
      Buffer_Data => Server.Buffer_Data);
   use type ACPI_Service.Install_Status;
   use type ACPI_Service.State_Model;
   function Revision (Server : State) return Revision_Number is (Server.Version);
   function Table_Open (Server : State) return Boolean is (Server.Open);
   function Received (Server : State) return Natural is (Server.Used);
   function Observe (Server : State) return ACPI_Service.Metrics is (ACPI_Bootstrap.Observe (Server.Boot));
   procedure Import_Block
     (Server : in out State; Origin : Authority; Token : Unsigned_64;
      ID : Positive; Kind : ACPI_Service.Table_Kind;
      Data : Firmware_Tables.Bytes; Reply : out Response) is
      Result : ACPI_Service.Install_Status;
   begin
      Reply := (Status => Denied, Data => [others => 0]);
      if Origin = No_Authority then return; end if;
      Reply.Data (0) := Server.Version;
      if Origin /= Snapshot_Provider then return; end if;
      Reply.Status := Stale;
      if Token /= Server.Version then return; end if;
      Reply.Status := Resource_Limit;
      if Server.Version = Max_Revision then return; end if;
      Reply.Status := Malformed;
      if Data'Length < Firmware_Tables.Table_Header_Size
        or else Data'Length > Server.Table_Byte_Limit
      then return; end if;
      Reply.Status := Wrong_Order;
      if Current (Server) /= Receiving or else Server.Open or else
        ACPI_Bootstrap.Installed (Server.Boot) = ACPI_Bootstrap.Expected (Server.Boot)
      then return; end if;
      ACPI_Bootstrap.Import_Table (Server.Boot, ID, Kind, Data, Result);
      Server.Version := Server.Version + 1;
      Reply.Data (0) := Server.Version;
      Reply.Status := (if Result = ACPI_Service.Installed then OK else Table_Rejected);
      if Reply.Status = Table_Rejected then
         Reply.Data (1) := Unsigned_64 (ACPI_Service.Install_Status'Pos (Result));
      end if;
   end Import_Block;
   -- Read requests cannot modify the snapshot or its upload state.
   procedure Read_Request (Server : State; Request : Packet; Reply : out Response)
     with Pre => Consistent (Server) and then Valid (Server) and then
       Request.Label in Read_Metrics | Read_Table_Info | Read_Table_Chunk | Read_MCFG_Info .. Read_DMAR_Path,
       Post => Reply.Data (0) = Revision (Server)
         and then (for all Word of Reply.Data => Word <= Max_Revision)
   is
      Amount : Natural range 0 .. 16;
   begin
      Reply := (Status => Malformed, Data => [0 => Server.Version, others => 0]);
      if Request.Label = Read_Metrics then
         if Request.Data (0) > 8 or else Request.Data (1 .. 3) /= [0, 0, 0] then return; end if;
         declare
            Stats : constant ACPI_Service.Metrics := Observe (Server);
         begin
            Reply.Status := OK;
            case Request.Data (0) is
               when 0 => Reply.Data (1 .. 3) :=
                 [Unsigned_64 (Phase'Enum_Rep (Current (Server))),
                  Unsigned_64 (ACPI_Bootstrap.Expected (Server.Boot)), Unsigned_64 (Stats.Tables)];
               when 1 => Reply.Data (1 .. 3) :=
                 [Unsigned_64 (Stats.Bytes), Unsigned_64 (Stats.Objects), Unsigned_64 (Stats.Value_Objects)];
               when 2 => Reply.Data (1 .. 3) :=
                 [Unsigned_64 (Stats.Value_Bytes), Unsigned_64 (Stats.Package_Elements), Unsigned_64 (Stats.Rejections)];
               when 3 => Reply.Data (1 .. 3) :=
                 [Boolean'Pos (Stats.Counter_Saturated), Boolean'Pos (Server.Open), Unsigned_64 (Server.Used)];
               when 4 => Reply.Data (1 .. 3) :=
                 [Unsigned_64 (Server.Table_Byte_Limit), Unsigned_64 (Server.Byte_Capacity),
                  Unsigned_64 (Server.Table_Capacity)];
               when 5 => Reply.Data (1 .. 3) :=
                 [Unsigned_64 (Stats.Method_Bytes),
                  Unsigned_64 (ACPI_Service.Method_Storage_Capacity),
                  Unsigned_64 (ACPI_Service.Method_Storage_Capacity - Stats.Method_Bytes)];
               when 6 => Reply.Data (1 .. 3) :=
                 [Unsigned_64 (Stats.Last_Load_Code), Unsigned_64 (ACPI_Service.Max_Namespace_Nodes),
                  Unsigned_64 (ACPI_Service.Max_Namespace_Nodes - Stats.Objects)];
               when 7 =>
                  declare
                     Report : constant ACPI_Service.Namespace.Initialization_Report :=
                       ACPI_Bootstrap.Member_Initialization (Server.Boot);
                  begin
                     Reply.Data (1 .. 3) := [Unsigned_64 (Report.Bound),
                       Unsigned_64 (Report.Missing), Unsigned_64 (Report.Unsupported)];
                  end;
               when others => Reply.Data (1 .. 3) :=
                 [Boolean'Pos (ACPI_Bootstrap.Members_Initialized (Server.Boot)),
                  Unsigned_64 (ACPI_Bootstrap.Pending_Members (Server.Boot)), 0];
            end case;
         end;
         return;
      end if;
      if Request.Label in Read_MADT_Info .. Read_MADT_Fields then
         if Request.Label = Read_MADT_Info then
            if Request.Data (2) > MADT_Address_Page or else Request.Data (3) /= 0 then return; end if;
         elsif Request.Label = Read_MADT_Record then
            if Request.Data (3) /= 0 then return; end if;
         elsif Request.Data (3) > MADT_Override_Flags_Page then return;
         end if;
         if Request.Data (0) /= Server.Version then Reply.Status := Stale; return; end if;
         if Current (Server) /= Complete then Reply.Status := Wrong_Order; return; end if;
         Reply.Status := Not_Found;
         if Request.Data (1) = 0 or else
           Request.Data (1) > Unsigned_64 (ACPI_Bootstrap.Installed (Server.Boot))
         then return; end if;
         declare
            Index : constant Positive := Positive (Request.Data (1));
            Low_Mask : constant Unsigned_64 := 16#FFFF_FFFF#;
            Info : Firmware_Tables.MADT.Table_Metadata;
         begin
            Reply.Status := Wrong_Table_Kind;
            if ACPI_Bootstrap.Table_Info (Server.Boot, Index).Signature /= "APIC" then return; end if;
            Reply.Status := Table_Rejected;
            Info := ACPI_Bootstrap.MADT_Info (Server.Boot, Index);
            if not Info.Valid then return; end if;
            if Request.Label = Read_MADT_Info then
               if Request.Data (2) = MADT_Header_Page then
                  Reply.Data (1 .. 3) := [Unsigned_64 (Info.Revision), Unsigned_64 (Info.Count), Unsigned_64 (Info.Flags)];
               else
                  Reply.Data (1 .. 3) := [Unsigned_64 (Info.Local_Address) and Low_Mask,
                    Shift_Right (Unsigned_64 (Info.Local_Address), 32), 0];
               end if;
            else
               Reply.Status := Index_Out_Of_Range;
               if Request.Data (2) > Unsigned_64 (Natural'Last) then return; end if;
               declare
                  Item : constant Firmware_Tables.MADT.Record_Result :=
                    ACPI_Bootstrap.MADT_Record (Server.Boot, Index, Natural (Request.Data (2)));
                  use Firmware_Tables.MADT;
               begin
                  if not Item.Valid then return; end if;
                  if Request.Label = Read_MADT_Record then
                     Reply.Data (1 .. 3) := [Unsigned_64 (Item.Value.Wire_Type),
                       Unsigned_64 (Item.Value.Offset), Unsigned_64 (Item.Value.Length)];
                  else
                     Reply.Status := Unsupported_Record_Kind;
                     if Item.Value.Kind = Unknown then return; end if;
                     Reply.Status := Malformed;
                     if Request.Data (3) = MADT_Override_Flags_Page then
                        if Item.Value.Kind /= Source_Override then return; end if;
                        Reply.Data (1) := Unsigned_64 (Item.Value.Override_Flags);
                     else
                        case Item.Value.Kind is
                           when Local_APIC | Local_X2APIC =>
                              Reply.Data (1 .. 3) := [Unsigned_64 (Item.Value.UID),
                                Unsigned_64 (Item.Value.Controller), Unsigned_64 (Item.Value.CPU_Flags)];
                           when IO_APIC =>
                              Reply.Data (1 .. 3) := [Unsigned_64 (Item.Value.IO_ID),
                                Unsigned_64 (Item.Value.IO_Address), Unsigned_64 (Item.Value.Interrupt_Base)];
                           when Source_Override =>
                              Reply.Data (1 .. 3) := [Unsigned_64 (Item.Value.Bus),
                                Unsigned_64 (Item.Value.Source), Unsigned_64 (Item.Value.Global_Interrupt)];
                           when NMI_Source =>
                              Reply.Data (1 .. 3) := [Unsigned_64 (Item.Value.NMI_Interrupt), Unsigned_64 (Item.Value.NMI_Flags), 0];
                           when Local_NMI | X2APIC_NMI =>
                              Reply.Data (1 .. 3) := [Unsigned_64 (Item.Value.NMI_UID),
                                Unsigned_64 (Item.Value.LINT), Unsigned_64 (Item.Value.Local_Flags)];
                           when Address_Override =>
                              Reply.Data (1 .. 3) := [Unsigned_64 (Item.Value.Address) and Low_Mask,
                                Shift_Right (Unsigned_64 (Item.Value.Address), 32), 0];
                           when Unknown => return;
                        end case;
                     end if;
                  end if;
               end;
            end if;
            Reply.Status := OK;
         end;
         return;
      end if;
      if Request.Label in Read_DMAR_Info .. Read_DMAR_Path then
         declare
            use Firmware_Tables.DMAR;
            Max_Scope : constant Unsigned_64 := Unsigned_64 (Scope_Count'Last);
            Selector : constant Unsigned_64 := Request.Data (3);
            Scope_Index : Scope_Count := 0;
            Path_Index : Path_Count := 0;
            Page : Unsigned_64 := Selector;
            Low_Mask : constant Unsigned_64 := 16#FFFF_FFFF#;
         begin
            case Request.Label is
               when Read_DMAR_Info =>
                  if Request.Data (2) not in DMAR_Header_Page or else Selector /= 0 then return; end if;
               when Read_DMAR_Record =>
                  if Selector not in DMAR_Record_Page then return; end if;
               when Read_DMAR_Fields =>
                  if Selector not in DMAR_Field_Page then return; end if;
               when Read_DMAR_Scope =>
                  if Selector > Max_Scope * DMAR_Scope_Page_Radix + DMAR_Scope_Page'Last then return; end if;
                  Page := Selector mod DMAR_Scope_Page_Radix;
                  if Page not in DMAR_Scope_Page or else Selector / DMAR_Scope_Page_Radix = 0 then return; end if;
                  Scope_Index := Scope_Count (Selector / DMAR_Scope_Page_Radix);
               when Read_DMAR_Path =>
                  if Selector > Max_Scope * DMAR_Path_Index_Radix + Unsigned_64 (Path_Count'Last) then return; end if;
                  if Selector / DMAR_Path_Index_Radix = 0 or else
                    Selector mod DMAR_Path_Index_Radix not in 1 .. Unsigned_64 (Path_Count'Last)
                  then return; end if;
                  Scope_Index := Scope_Count (Selector / DMAR_Path_Index_Radix);
                  Path_Index := Path_Count (Selector mod DMAR_Path_Index_Radix);
               when others => return;
            end case;
            if Request.Data (0) /= Server.Version then Reply.Status := Stale; return; end if;
            if Current (Server) /= Complete then Reply.Status := Wrong_Order; return; end if;
            Reply.Status := Not_Found;
            if Request.Data (1) = 0 or else Request.Data (1) > Unsigned_64 (ACPI_Bootstrap.Installed (Server.Boot)) then return; end if;
            declare
               Index : constant Positive := Positive (Request.Data (1));
               Info : Table_Metadata;
            begin
               Reply.Status := Wrong_Table_Kind;
               if ACPI_Bootstrap.Table_Info (Server.Boot, Index).Signature /= "DMAR" then return; end if;
               Reply.Status := Table_Rejected;
               Info := ACPI_Bootstrap.DMAR_Info (Server.Boot, Index);
               if not Info.Valid then return; end if;
               if Request.Label = Read_DMAR_Info then
                  if Request.Data (2) = 0 then
                     Reply.Data (1 .. 3) := [Unsigned_64 (Info.Revision), Unsigned_64 (Info.Host_Width), Unsigned_64 (Info.Flags)];
                  else Reply.Data (1) := Unsigned_64 (Info.Count);
                  end if;
               else
                  Reply.Status := Index_Out_Of_Range;
                  if Request.Data (2) = 0 or else Request.Data (2) > Unsigned_64 (Natural'Last) then return; end if;
                  declare
                     Rec : constant Natural := Natural (Request.Data (2));
                     Item : constant Record_Result := ACPI_Bootstrap.DMAR_Record (Server.Boot, Index, Rec);
                  begin
                     if not Item.Valid then return; end if;
                     if Request.Label = Read_DMAR_Record then
                        if Page = 0 then
                           Reply.Data (1 .. 3) := [Unsigned_64 (Item.Value.Wire_Type), Unsigned_64 (Item.Value.Offset), Unsigned_64 (Item.Value.Length)];
                        else Reply.Data (1) := Unsigned_64 (Item.Value.Scopes);
                        end if;
                     elsif Request.Label = Read_DMAR_Fields then
                        Reply.Status := Unsupported_Record_Kind;
                        if Item.Value.Kind = Unknown then return; end if;
                        Reply.Status := Malformed;
                        declare
                           V : Record_Data renames Item.Value;
                           Address : Firmware_Tables.Address_Value := 0;
                        begin
                           if Page = 0 then
                              case V.Kind is
                                 when Hardware_Unit => Reply.Data (1 .. 3) := [Unsigned_64 (V.Unit_Flags), Unsigned_64 (V.Register_Size), Unsigned_64 (V.Unit_Segment)];
                                 when Reserved_Memory => Reply.Data (1) := Unsigned_64 (V.Memory_Segment);
                                 when ATS_Root | SATC => Reply.Data (1 .. 2) := [Unsigned_64 (V.Cache_Flags), Unsigned_64 (V.Cache_Segment)];
                                 when Hardware_Affinity => Reply.Data (1) := Unsigned_64 (V.Domain);
                                 when Namespace_Device => Reply.Data (1 .. 3) := [Unsigned_64 (V.Device_Number), Unsigned_64 (V.Name_Offset), Unsigned_64 (V.Name_Length)];
                                 when SIDP => Reply.Data (1) := Unsigned_64 (V.Device_Segment);
                                 when Unknown => return;
                              end case;
                           else
                              case V.Kind is
                                 when Hardware_Unit => if Page /= 1 then return; end if; Address := V.Register_Base;
                                 when Reserved_Memory => Address := (if Page = 1 then V.Base_Address else V.Inclusive_Limit);
                                 when Hardware_Affinity => if Page /= 1 then return; end if; Address := V.Affinity_Base;
                                 when others => return;
                              end case;
                              Reply.Data (1 .. 2) := [Address and Low_Mask, Shift_Right (Address, 32)];
                           end if;
                        end;
                     else
                        if Item.Value.Kind = Unknown then Reply.Status := Unsupported_Record_Kind; return; end if;
                        declare
                           Scope : constant Scope_Result := ACPI_Bootstrap.DMAR_Scope (Server.Boot, Index, Rec, Scope_Index);
                        begin
                           if not Scope.Valid then return; end if;
                           if Request.Label = Read_DMAR_Scope then
                              case DMAR_Scope_Page (Page) is
                                 when 0 => Reply.Data (1 .. 3) := [Unsigned_64 (Scope.Wire_Type), Unsigned_64 (Scope.Offset), Unsigned_64 (Scope.Length)];
                                 when 1 => Reply.Data (1 .. 3) := [Unsigned_64 (Scope.Flags), Unsigned_64 (Scope.Reserved), Unsigned_64 (Scope.Enumeration_ID)];
                                 when 2 => Reply.Data (1 .. 3) := [Unsigned_64 (Scope.Start_Bus), Unsigned_64 (Scope.Paths), Boolean'Pos (Scope.Known_Path)];
                              end case;
                           else
                              if not Scope.Known_Path then Reply.Status := Unsupported_Record_Kind; return; end if;
                              declare
                                 Path : constant Path_Result := ACPI_Bootstrap.DMAR_Path (Server.Boot, Index, Rec, Scope_Index, Path_Index);
                              begin
                                 if not Path.Valid then return; end if;
                                 Reply.Data (1 .. 2) := [Unsigned_64 (Path.Device), Unsigned_64 (Path.Func)];
                              end;
                           end if;
                        end;
                     end if;
                  end;
               end if;
               Reply.Status := OK;
            end;
         end;
         return;
      end if;
      if Request.Label in Read_SRAT_Info .. Read_SRAT_Fields then
         if Request.Label = Read_SRAT_Info then
            if Request.Data (2) > SRAT_Reserved_Page or else Request.Data (3) /= 0 then return; end if;
         elsif Request.Label = Read_SRAT_Record then
            if Request.Data (3) /= 0 then return; end if;
         elsif Request.Data (3) > SRAT_Handle_Last_Page then return;
         end if;
         if Request.Data (0) /= Server.Version then Reply.Status := Stale; return; end if;
         if Current (Server) /= Complete then Reply.Status := Wrong_Order; return; end if;
         Reply.Status := Not_Found;
         if Request.Data (1) = 0 or else
           Request.Data (1) > Unsigned_64 (ACPI_Bootstrap.Installed (Server.Boot))
         then return; end if;
         declare
            Index : constant Positive := Positive (Request.Data (1));
            Low_Mask : constant Unsigned_64 := 16#FFFF_FFFF#;
            Info : Firmware_Tables.SRAT.Table_Metadata;
         begin
            Reply.Status := Wrong_Table_Kind;
            if ACPI_Bootstrap.Table_Info (Server.Boot, Index).Signature /= "SRAT" then return; end if;
            Reply.Status := Table_Rejected;
            Info := ACPI_Bootstrap.SRAT_Info (Server.Boot, Index);
            if not Info.Valid then return; end if;
            if Request.Label = Read_SRAT_Info then
               if Request.Data (2) = SRAT_Header_Page then
                  Reply.Data (1 .. 3) := [Unsigned_64 (Info.Revision), Unsigned_64 (Info.Count), Unsigned_64 (Info.Table_Revision)];
               else
                  Reply.Data (1 .. 3) := [Info.Reserved and Low_Mask,
                    Shift_Right (Info.Reserved, 32), 0];
               end if;
            else
               Reply.Status := Index_Out_Of_Range;
               if Request.Data (2) > Unsigned_64 (Natural'Last) then return; end if;
               declare
                  Item : constant Firmware_Tables.SRAT.Record_Result :=
                    ACPI_Bootstrap.SRAT_Record (Server.Boot, Index, Natural (Request.Data (2)));
                  use Firmware_Tables.SRAT;
               begin
                  if not Item.Valid then return; end if;
                  if Request.Label = Read_SRAT_Record then
                     Reply.Data (1 .. 3) := [Unsigned_64 (Item.Value.Wire_Type),
                       Unsigned_64 (Item.Value.Offset), Unsigned_64 (Item.Value.Length)];
                  else
                     Reply.Status := Unsupported_Record_Kind;
                     if Item.Value.Kind = Unknown then return; end if;
                     Reply.Status := Malformed;
                     declare
                        Page : constant Unsigned_64 := Request.Data (3);
                        V : Record_Data renames Item.Value;
                     begin
                        case V.Kind is
                           when Local_APIC =>
                              if Page = SRAT_Common_Page then
                                 Reply.Data (1 .. 3) := [Unsigned_64 (V.Domain_Low), Unsigned_64 (V.Domain_High), Unsigned_64 (V.APIC_ID)];
                              elsif Page = SRAT_Detail_Page then
                                 Reply.Data (1 .. 3) := [Unsigned_64 (V.APIC_Flags), Unsigned_64 (V.SAPIC_EID), Unsigned_64 (V.APIC_Clock)];
                              else return; end if;
                           when Memory_Affinity =>
                              if Page = SRAT_Common_Page then
                                 Reply.Data (1 .. 3) := [Unsigned_64 (V.Memory_Domain), Unsigned_64 (V.Memory_Flags), 0];
                              elsif Page = SRAT_Base_Page then
                                 Reply.Data (1 .. 3) := [V.Base_Address and Low_Mask, Shift_Right (V.Base_Address, 32), 0];
                              elsif Page = SRAT_Length_Page then
                                 Reply.Data (1 .. 3) := [V.Address_Length and Low_Mask, Shift_Right (V.Address_Length, 32), 0];
                              else return; end if;
                           when X2APIC =>
                              if Page = SRAT_Common_Page then
                                 Reply.Data (1 .. 3) := [Unsigned_64 (V.X2_Domain), Unsigned_64 (V.X2_ID), Unsigned_64 (V.X2_Flags)];
                              elsif Page = SRAT_Detail_Page then Reply.Data (1) := Unsigned_64 (V.X2_Clock);
                              else return; end if;
                           when GICC =>
                              if Page = SRAT_Common_Page then
                                 Reply.Data (1 .. 3) := [Unsigned_64 (V.GICC_Domain), Unsigned_64 (V.GICC_UID), Unsigned_64 (V.GICC_Flags)];
                              elsif Page = SRAT_Detail_Page then Reply.Data (1) := Unsigned_64 (V.GICC_Clock);
                              else return; end if;
                           when GIC_ITS =>
                              if Page /= SRAT_Common_Page then return; end if;
                              Reply.Data (1 .. 3) := [Unsigned_64 (V.ITS_Domain), Unsigned_64 (V.ITS_ID), 0];
                           when Generic_Initiator | Generic_Port =>
                              if Page = SRAT_Common_Page then
                                 Reply.Data (1 .. 3) := [Unsigned_64 (V.Generic_Domain), Unsigned_64 (V.Handle_Type), Unsigned_64 (V.Generic_Flags)];
                              elsif Page in SRAT_Handle_First_Page .. SRAT_Handle_Last_Page then
                                 declare First : constant Natural := Natural (Page - SRAT_Handle_First_Page) * SRAT_Handle_Bytes_Per_Page; begin
                                    for I in 0 .. SRAT_Handle_Bytes_Per_Page - 1 loop
                                       if First + I < Handle_Size then
                                          Reply.Data (1 + I) := Unsigned_64 (V.Handle (First + I));
                                       end if;
                                    end loop;
                                 end;
                              else return; end if;
                           when RINTC =>
                              if Page = SRAT_Common_Page then
                                 Reply.Data (1 .. 3) := [Unsigned_64 (V.RINTC_Domain), Unsigned_64 (V.RINTC_UID), Unsigned_64 (V.RINTC_Flags)];
                              elsif Page = SRAT_Detail_Page then Reply.Data (1) := Unsigned_64 (V.RINTC_Clock);
                              else return; end if;
                           when Unknown => return;
                        end case;
                     end;
                  end if;
               end;
            end if;
            Reply.Status := OK;
         end;
         return;
      end if;
      if Request.Label in Read_MCFG_Info .. Read_SLIT_Distance then
         if Request.Label = Read_SLIT_Info then
            if Request.Data (2 .. 3) /= [0, 0] then return; end if;
         elsif Request.Label = Read_MCFG_Info then
            if Request.Data (2) > MCFG_Reserved_Page or else Request.Data (3) /= 0 then return; end if;
         elsif Request.Label = Read_MCFG_Allocation then
            if Request.Data (3) > MCFG_Bus_Page then return; end if;
         end if;
         if Request.Data (0) /= Server.Version then Reply.Status := Stale; return; end if;
         if Current (Server) /= Complete then Reply.Status := Wrong_Order; return; end if;
         Reply.Status := Not_Found;
         if Request.Data (1) = 0 or else
           Request.Data (1) > Unsigned_64 (ACPI_Bootstrap.Installed (Server.Boot))
         then return; end if;
         declare
            Index : constant Positive := Positive (Request.Data (1));
            Low_Mask : constant Unsigned_64 := 16#FFFF_FFFF#;
         begin
            Reply.Status := Wrong_Table_Kind;
            if ACPI_Bootstrap.Table_Info (Server.Boot, Index).Signature /=
              (if Request.Label in Read_MCFG_Info | Read_MCFG_Allocation then "MCFG" else "SLIT")
            then return; end if;
            Reply.Status := Table_Rejected;
            case Request.Label is
               when Read_MCFG_Info =>
                  declare
                     Info : constant Firmware_Tables.MCFG.Table_Metadata :=
                       ACPI_Bootstrap.MCFG_Info (Server.Boot, Index);
                  begin
                     if not Info.Valid then return; end if;
                     if Request.Data (2) = MCFG_Header_Page then
                        Reply.Data (1 .. 3) := [Unsigned_64 (Info.Revision), Unsigned_64 (Info.Count), 0];
                     else
                        Reply.Data (1 .. 3) := [Info.Reserved and Low_Mask, Shift_Right (Info.Reserved, 32), 0];
                     end if;
                  end;
               when Read_MCFG_Allocation =>
                  if not ACPI_Bootstrap.MCFG_Info (Server.Boot, Index).Valid then return; end if;
                  Reply.Status := Index_Out_Of_Range;
                  if Request.Data (2) > Unsigned_64 (Natural'Last) then return; end if;
                  declare
                     Item : constant Firmware_Tables.MCFG.Allocation_Result :=
                       ACPI_Bootstrap.MCFG_Allocation (Server.Boot, Index, Natural (Request.Data (2)));
                  begin
                     if not Item.Valid then return; end if;
                     if Request.Data (3) = MCFG_Address_Page then
                        Reply.Data (1 .. 3) := [Unsigned_64 (Item.Value.Base) and Low_Mask,
                          Shift_Right (Unsigned_64 (Item.Value.Base), 32), Unsigned_64 (Item.Value.Segment)];
                     else
                        Reply.Data (1 .. 3) := [Unsigned_64 (Item.Value.First_Bus),
                          Unsigned_64 (Item.Value.Last_Bus), Unsigned_64 (Item.Value.Reserved)];
                     end if;
                  end;
               when Read_SLIT_Info =>
                  declare
                     Info : constant Firmware_Tables.SLIT.Table_Metadata :=
                       ACPI_Bootstrap.SLIT_Info (Server.Boot, Index);
                  begin
                     if not Info.Valid then return; end if;
                     Reply.Data (1 .. 3) := [Unsigned_64 (Info.Revision), Unsigned_64 (Info.Count), 0];
                  end;
               when Read_SLIT_Distance =>
                  if not ACPI_Bootstrap.SLIT_Info (Server.Boot, Index).Valid then return; end if;
                  Reply.Status := Index_Out_Of_Range;
                  if Request.Data (2) > Unsigned_64 (Natural'Last) or else
                    Request.Data (3) > Unsigned_64 (Natural'Last) then return; end if;
                  declare
                     Item : constant Firmware_Tables.SLIT.Distance_Result :=
                       ACPI_Bootstrap.SLIT_Distance (Server.Boot, Index,
                         Natural (Request.Data (2)), Natural (Request.Data (3)));
                  begin
                     if not Item.Valid then return; end if;
                     Reply.Data (1) := Unsigned_64 (Item.Value);
                  end;
               when others => return;
            end case;
            Reply.Status := OK;
         end;
         return;
      end if;
      if Request.Label in Read_Table_Info | Read_Table_Chunk then
         if Request.Data (3) /= 0 or else
           (Request.Label = Read_Table_Info and then Request.Data (2) /= 0)
         then return; end if;
         if Request.Data (0) /= Server.Version then Reply.Status := Stale; return; end if;
         if Current (Server) /= Complete then Reply.Status := Wrong_Order; return; end if;
         if Request.Data (1) = 0 or else
           Request.Data (1) > Unsigned_64 (Server.Table_Capacity)
         then Reply.Status := Not_Found; return; end if;
         declare
            Index : constant Positive := Positive (Request.Data (1));
            Info : ACPI_Service.Table_Metadata;
            Low, High : Unsigned_32 := 0;
            Offset : Natural;
         begin
            if Index > ACPI_Bootstrap.Installed (Server.Boot) then
               Reply.Status := Not_Found; return;
            end if;
            Info := ACPI_Bootstrap.Table_Info (Server.Boot, Index);
            if Request.Label = Read_Table_Info then
               for I in Info.Signature'Range loop
                  Low := Low or Shift_Left (Unsigned_32 (Character'Pos (Info.Signature (I))), (I - 1) * 8);
               end loop;
               Reply.Data (1 .. 3) := [Unsigned_64 (Info.ID), Unsigned_64 (Info.Extent), Unsigned_64 (Low)];
            else
               if Request.Data (2) > Unsigned_64 (Server.Table_Byte_Limit) then return; end if;
               Offset := Natural (Request.Data (2));
               if Offset > Info.Extent then return; end if;
               Amount := Natural'Min (8, Info.Extent - Offset);
               for I in 0 .. Amount - 1 loop
                  declare
                     B : constant Unsigned_32 := Unsigned_32 (ACPI_Bootstrap.Table_Byte (Server.Boot, Index, Offset + I));
                  begin
                     if I < 4 then Low := Low or Shift_Left (B, I * 8);
                     else High := High or Shift_Left (B, (I - 4) * 8); end if;
                  end;
               end loop;
               Reply.Data (1 .. 3) := [Unsigned_64 (Amount), Unsigned_64 (Low), Unsigned_64 (High)];
            end if;
            Reply.Status := OK;
         end;
         return;
      end if;
   end Read_Request;
   -- The packet handler has already rejected repeated startup and bad counts.
   procedure Start_Validated (Session : in out ACPI_Bootstrap.State; Count : Positive)
     with Pre => ACPI_Bootstrap.Valid (Session)
       and then not ACPI_Bootstrap.Started (Session)
       and then Count <= Session.Table_Capacity,
       Post => ACPI_Bootstrap.Valid (Session)
         and then ACPI_Bootstrap.Started (Session)
         and then ACPI_Bootstrap.Current (Session) = ACPI_Bootstrap.Receiving
         and then ACPI_Bootstrap.Expected (Session) = Count
         and then ACPI_Bootstrap.Core_Model (Session) =
           ACPI_Bootstrap.Core_Model (Session)'Old
   is
      Accepted : Boolean;
   begin
      ACPI_Bootstrap.Start (Session, Count, Accepted);
      pragma Assert (Accepted);
   end Start_Validated;
   procedure Handle (Server : in out State; Origin : Authority;
                     Request : Packet; Reply : out Response) is
      Amount : Natural range 0 .. 16;
      Initial_Version : constant Revision_Number := Server.Version;
      Result : ACPI_Service.Install_Status;
      function Octet (Index : Natural) return Firmware_Tables.Byte is
        (Firmware_Tables.Byte (Shift_Right (Request.Data (2 + Index / 8), (Index mod 8) * 8) and 255))
        with Pre => Index < 16;
   begin
      Reply := (Status => Malformed, Data => [0 => Server.Version, others => 0]);
      if Origin = No_Authority then
         Reply := (Status => Denied, Data => [others => 0]);
         return;
      end if;
      if Request.Length /= 4 or Request.Flags /= 0 or Request.Reserved /= 0 then return; end if;
      if Request.Label in Read_Metrics | Read_Table_Info | Read_Table_Chunk | Read_MCFG_Info .. Read_DMAR_Path then
         Read_Request (Server, Request, Reply);
         return;
      end if;
      if Origin /= Snapshot_Provider then Reply.Status := Denied; return; end if;
      if Request.Label not in Start_Snapshot .. Finish_Snapshot then return; end if;
      if Request.Data (0) /= Server.Version then Reply.Status := Stale; return; end if;
      if Initial_Version = Max_Revision then Reply.Status := Resource_Limit; return; end if;
      case Request.Label is
         when Start_Snapshot =>
            if Request.Data (1) not in 1 .. Unsigned_64 (Positive'Last)
              or else Request.Data (2 .. 3) /= [0, 0] then return; end if;
            declare
               Count : constant Positive := Positive (Request.Data (1));
            begin
               if Count > Server.Boot.Table_Capacity then return; end if;
               if ACPI_Bootstrap.Started (Server.Boot) then Reply.Status := Wrong_Order; return; end if;
               Start_Validated (Server.Boot, Count);
            end;
            pragma Assert (Consistent (Server));
         when Begin_Table =>
            if Request.Data (1) not in 1 .. Unsigned_64 (Positive'Last)
              or else Request.Data (2) > 2
              or else Request.Data (3) < Unsigned_64 (Firmware_Tables.Table_Header_Size)
              or else Request.Data (3) > Unsigned_64 (Natural'Last)
            then return; end if;
            -- Check representation before conversion, then compare the quota
            -- in the same integer domain as the retained buffer extent.
            if Natural (Request.Data (3)) > Server.Table_Byte_Limit then return; end if;
            if Current (Server) /= Receiving or else Server.Open or else
              ACPI_Bootstrap.Installed (Server.Boot) = ACPI_Bootstrap.Expected (Server.Boot)
            then Reply.Status := Wrong_Order; return; end if;
            Server.ID := Positive (Request.Data (1));
            Server.Kind := ACPI_Service.Table_Kind'Val (Request.Data (2));
            Server.Extent := Natural (Request.Data (3));
            pragma Assert (ACPI_Bootstrap.Current (Server.Boot) = ACPI_Bootstrap.Receiving);
            pragma Assert (Server.Extent >= Firmware_Tables.Table_Header_Size);
            Server.Open := True;
            pragma Assert (Consistent (Server));
         when Write_Chunk =>
            if not Server.Open or else Server.Used = Server.Extent then Reply.Status := Wrong_Order; return; end if;
            if Request.Data (1) /= Unsigned_64 (Server.Used) then Reply.Status := Wrong_Order; return; end if;
            Amount := Natural'Min (16, Server.Extent - Server.Used);
            -- Reject noncanonical padding before changing any buffered byte.
            for I in Amount .. 15 loop
               if Octet (I) /= 0 then return; end if;
            end loop;
            declare
               Chunk_Length : constant Natural := Amount;
               Chunk : Firmware_Tables.Bytes (1 .. Chunk_Length);
            begin
               for I in Chunk'Range loop Chunk (I) := Octet (I - 1); end loop;
               Server.Buffer_Data (Server.Used + 1 .. Server.Used + Amount) := Chunk;
            end;
            Server.Used := Server.Used + Amount;
            pragma Assert (ACPI_Bootstrap.Current (Server.Boot) = ACPI_Bootstrap.Receiving);
            pragma Assert (Server.Extent >= Firmware_Tables.Table_Header_Size);

            pragma Assert (Consistent (Server));
         when Commit_Table =>
            if Request.Data (1 .. 3) /= [0, 0, 0] then return; end if;
            if not Server.Open or else Server.Used /= Server.Extent then Reply.Status := Wrong_Order; return; end if;
            declare
               Count : constant Natural := Server.Extent;
            begin
               Server.Open := False;
               Server.Used := 0;
               Server.Extent := 0;
               ACPI_Bootstrap.Import_Table (Server.Boot, Server.ID, Server.Kind, Server.Buffer_Data (1 .. Count), Result);
            end;
            if Result /= ACPI_Service.Installed then
               Reply.Status := Table_Rejected;
               Reply.Data (1) := Unsigned_64 (ACPI_Service.Install_Status'Pos (Result));
            end if;
            pragma Assert (Consistent (Server));
         when Finish_Snapshot =>
            if Request.Data (1 .. 3) /= [0, 0, 0] then return; end if;
            if Current (Server) /= Receiving or else Server.Open then Reply.Status := Wrong_Order; return; end if;
            ACPI_Bootstrap.Finish (Server.Boot);
            if Current (Server) /= Complete then Reply.Status := Incomplete; end if;
            pragma Assert (Consistent (Server));
         when others => return;
      end case;
      Server.Version := Initial_Version + 1;
      Reply.Data (0) := Server.Version;
      if Reply.Status = Malformed then Reply.Status := OK; end if;
   end Handle;
end ACPI_Requests;
