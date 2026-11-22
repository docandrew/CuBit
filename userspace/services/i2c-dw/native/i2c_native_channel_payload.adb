pragma Ada_2022;
with System; with System.Storage_Elements;
package body I2C_Native_Channel_Payload is
   use System.Storage_Elements;
   use type System.Address;
   use type T.Count;
   Payload_Bytes : constant := I2C_Channel.Payload_Pages * RC.Page_Bytes;
   function Valid_View (View : RC.Payload_View) return Boolean is
     (View.Base /= System.Null_Address and then View.Bytes = Payload_Bytes
      and then To_Integer (View.Base) mod RC.Page_Bytes = 0
      and then To_Integer (View.Base) <= Integer_Address'Last - (Payload_Bytes - 1));
   function Disjoint (A, B : RC.Payload_View) return Boolean is
     (To_Integer (A.Base) + Payload_Bytes - 1 < To_Integer (B.Base)
      or else To_Integer (B.Base) + Payload_Bytes - 1 < To_Integer (A.Base));
   procedure Bind
     (S : in out State; Input, Output : RC.Payload_View;
      Write_Count, Read_Count : T.Count; Result : out Binding) is
   begin
      if S.Active then Result := Already_Bound; return; end if;
      if Write_Count = 0 and then Read_Count = 0 then Result := Invalid_Counts; return; end if;
      if Read_Count > T.Count'Last - Write_Count then Result := Invalid_Counts; return; end if;
      if not Valid_View (Input) or else not Valid_View (Output)
        or else Input.Writable or else not Output.Writable
        or else not Disjoint (Input, Output)
      then Result := Invalid_Views; return; end if;
      S.Input := Input; S.Output := Output;
      S.Writes := Write_Count; S.Reads := Read_Count;
      S.Input_Position := 0; S.Output_Position := 0;
      S.Active := True; Result := Bound;
   end Bind;
   function Is_Bound (S : State) return Boolean is (S.Active);
   procedure Read_Byte
     (S : in out State; Index : T.Count; Value : out T.Byte; Accepted : out Boolean) is
   begin
      Value := 0; Accepted := False;
      if not S.Active or else S.Input_Position >= S.Writes
        or else Index /= S.Input_Position + 1
      then return; end if;
      declare
         Item : T.Byte with Import, Volatile,
           Address => S.Input.Base + Storage_Offset (Index - 1);
      begin Value := Item; end;
      S.Input_Position := Index; Accepted := True;
   end Read_Byte;
   procedure Write_Byte
     (S : in out State; Index : T.Count; Value : T.Byte; Accepted : out Boolean) is
   begin
      Accepted := False;
      if not S.Active or else S.Output_Position >= S.Reads
        or else Index /= S.Output_Position + 1
      then return; end if;
      declare
         Item : T.Byte with Import, Volatile,
           Address => S.Output.Base + Storage_Offset (Index - 1);
      begin Item := Value; end;
      S.Output_Position := Index; Accepted := True;
   end Write_Byte;
   procedure Release (S : in out State; Result : out Releasing) is
   begin
      if not S.Active then Result := Not_Bound; return; end if;
      if not May_Release then Result := Not_Quiescent; return; end if;
      S.Active := False; S.Input := RC.Empty_Payload; S.Output := RC.Empty_Payload;
      S.Writes := 0; S.Reads := 0; S.Input_Position := 0; S.Output_Position := 0;
      Result := Released;
   end Release;
end I2C_Native_Channel_Payload;
