pragma Ada_2022;
with Firmware_Tables;
with ACPI_FACS_Header;
package body ACPI_Backend_FACS_Binding with SPARK_Mode is
   use Interfaces;
   package W renames Bindings.W;
   package P renames Bindings.P;
   package T renames Bindings.T;
   package F renames ACPI_FACS_Header;
   use type P.Address_Space, F.Header_Status;
   FACS_Alignment : constant Unsigned_64 := 64;
   function Observe (State : Context) return Report is (State.Info);
   function Contains (Window : W.Granted_Window; Address, Length : Unsigned_64) return Boolean is
     (W.Valid (Window) and then Address >= Window.Base
      and then Address - Window.Base < Window.Length
      and then Length /= 0 and then Length <= Window.Length - (Address - Window.Base));
   function Word_View
     (State : Context; Hardware : Hardware_State; For_Write : Boolean) return Bindings.Access_View;
   procedure Prepare
     (State : in out Context; Hardware : Hardware_State; FADT : ACPI_FADT.Description;
      Window : W.Window_ID; Result : out Report)
   is
      procedure Run is
         Held : constant W.Window_Result := Describe_Window (Hardware, Window);
         Data : Firmware_Tables.Bytes (1 .. F.Minimum_Length) := [others => 0];
         Header : F.Description;
         Completed : Boolean;
      begin
         if ACPI_FADT.Hardware_Reduced (FADT) then State.Info.Status := Hardware_Reduced; return; end if;
         State.Physical := (if FADT.X_FACS.Present and then FADT.X_FACS.Value /= 0
           then FADT.X_FACS.Value else Unsigned_64 (FADT.FACS));
         if State.Physical = 0 then State.Info.Status := No_FACS; return; end if;
         if State.Physical mod FACS_Alignment /= 0 then State.Info.Status := Misaligned_FACS; return; end if;
         if Unsigned_64 (F.Minimum_Length - 1) > Unsigned_64'Last - State.Physical then
            State.Info.Status := Address_Overflow; return;
         end if;
         if not Held.Allowed then State.Info.Status := Window_Unavailable; return; end if;
         if Held.Value.Space /= P.Memory_Space then State.Info.Status := Wrong_Space; return; end if;
         if not Held.Value.Readable or else not Held.Value.Writable then
            State.Info.Status := Rights_Insufficient; return;
         end if;
         if not Contains (Held.Value, State.Physical, Unsigned_64 (F.Minimum_Length)) then
            State.Info.Status := Prefix_Outside_Window; return;
         end if;
         if not Coherent_RAM (Window) then State.Info.Status := Incoherent_Memory; return; end if;
         for I in Offset loop
            declare
               View : constant Bindings.Access_View := Select_Access
                 (Hardware, Window, P.Memory_Space, State.Physical + Unsigned_64 (I), T.Byte_Access, False);
            begin
               if not View.Allowed or else View.Space /= P.Memory_Space then
                  State.Info.Status := Read_Refused; State.Info.Failed_Offset := I; return;
               end if;
               Read_Byte (View.Virtual_Address, Data (Data'First + I), Completed);
               if not Completed then
                  State.Info.Status := Read_Refused; State.Info.Failed_Offset := I; return;
               end if;
               State.Info.Header_Bytes := State.Info.Header_Bytes + 1;
            end;
         end loop;
         Header := F.Decode_Prefix (Data);
         if Header.Status /= F.Recognized_Header then State.Info.Status := Invalid_Header; return; end if;
         if not Contains (Held.Value, State.Physical, Unsigned_64 (Header.Declared_Length)) then
            State.Info.Status := Declared_Outside_Window; return;
         end if;
         State.Window := Window;
         State.Info.Declared_Length := Header.Declared_Length; State.Info.Version := Header.Version;
         -- Recheck live backing after the final callback as well.
         State.Info.Status := Ready;
         declare View : constant Bindings.Access_View := Word_View (State, Hardware, True); begin
            if not View.Allowed then State.Info.Status := Window_Unavailable; end if;
         end;
      end Run;
   begin
      if State.Stage = Preparing then Result := (Status => Reentered, others => <>); return;
      elsif State.Stage /= Fresh then Result := (Status => Already_Consumed, others => <>); return;
      end if;
      State.Stage := Preparing; Run; State.Stage := Finished; Result := State.Info;
   end Prepare;
   function Word_View
     (State : Context; Hardware : Hardware_State; For_Write : Boolean) return Bindings.Access_View
   is
      Held : constant W.Window_Result := Describe_Window (Hardware, State.Window);
   begin
      if State.Info.Status /= Ready or else not Held.Allowed then
         return (Allowed => False, Space => P.Memory_Space);
      end if;
      if Held.Value.Space /= P.Memory_Space or else not Held.Value.Readable or else not Held.Value.Writable
        or else not Contains (Held.Value, State.Physical, Unsigned_64 (State.Info.Declared_Length))
        or else not Coherent_RAM (State.Window)
      then return (Allowed => False, Space => P.Memory_Space); end if;
      return Select_Access
        (Hardware, State.Window, P.Memory_Space, State.Physical + Unsigned_64 (F.Global_Lock_Offset),
         T.Dword_Access, For_Write);
   end Word_View;
   function Select_Word
     (State : Context; Hardware : Hardware_State; For_Write : Boolean) return Bindings.Access_View is
   begin
      if State.Stage /= Finished then return (Allowed => False, Space => P.Memory_Space); end if;
      return Word_View (State, Hardware, For_Write);
   end Select_Word;
end ACPI_Backend_FACS_Binding;
