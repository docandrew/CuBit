pragma Ada_2022;
with AML_Identity.Issuer;
with AML_Register_Views;
with AML_Decode;
with AML_Field_Datums;
package body ACPI_Local_NVS_Provider is
   use type A.Endpoint_ID,A.Target,A.Counter,AML_Identity.Identity,Records.Outcome,Records.Phase;
   use type Records.Word,Records.Epoch_Number,Records.Record_ID,Store.Outcome;
   use type L.Register_Geometry,AML_Decode.Byte,ACPI_NVS_Writes.Completion;
   function Pending (IO : Context) return Boolean is
     (IO.Working or else IO.Captured.Present or else Records.Observe (Backing).Working);
   function Is_Ready (IO : Context) return Boolean is
     (IO.Stage = Ready and then not Pending (IO) and then Records.Observe (Backing).Stage = Records.Serving);
   function Identity (IO : Context) return AML_Identity.Identity is (IO.Issuer);
   function Register_Count (IO : Context) return Natural is
     (if Is_Ready (IO) then Store.Length (IO.Targets) else 0);
   function Uncertain (IO : Context) return A.Observation is (IO.Captured);
   function Completed_Fault (IO : Context) return A.Observation is (IO.Fault);
   function Target_At (IO : Context; Slot : Positive) return A.Target is
     ((IO.Issuer,IO.Symbol,A.Counter (IO.Epoch),A.Register_ID (Slot)));
   function Matches (IO : Context; Target : A.Target) return Boolean is
     (Is_Ready (IO) and then Target.Provider = IO.Issuer and then IO.Issuer /= AML_Identity.No_Identity
      and then Target.Endpoint = IO.Symbol and then Target.Epoch = A.Counter (IO.Epoch)
      and then Target.Register in 1 .. A.Register_ID (Store.Length (IO.Targets)));
   procedure Initialize (IO : in out Context; Symbol : A.Endpoint_ID; Accepted : out Boolean) is
      Header : Records.Header_Report;
   begin
      Accepted := False;
      if IO.Stage /= Fresh or else Pending (IO) then return; end if;
      IO.Stage := Initializing; IO.Working := True;
      if Scope = 0 or else Symbol = A.No_Endpoint then IO.Stage := Failed; IO.Working := False; return; end if;
      Records.Header (Backing,Scope,Header);
      if Header.Status /= Records.Ready or else Header.Epoch = Records.No_Epoch then
         IO.Stage := Failed; IO.Working := False; return;
      end if;
      AML_Identity.Issuer.Issue (IO.Issuer,Accepted);
      if not Accepted then IO.Stage := Failed; IO.Working := False; return; end if;
      IO.Symbol := Symbol; IO.Epoch := Header.Epoch; IO.Count := Header.Count;
      IO.Stage := Ready; IO.Working := False;
   end Initialize;
   procedure Export_Target (IO : Context; Ordinal : Positive; Target : out A.Target; Accepted : out Boolean) is
   begin
      Target := A.No_Target; Accepted := Is_Ready (IO) and then Ordinal <= Store.Length (IO.Targets);
      if Accepted then Target := Target_At (IO,Ordinal); end if;
   end Export_Target;
   procedure Resolve (IO : in out Context; Geometry : L.Register_Geometry;
      Target : out A.Target; Match : out Match_Kind; Accepted : out Boolean) is
      Header : Records.Header_Report;
      Item : Records.Description_Report;
      Selected : Records.Record_ID := Records.No_Record;
      Offset : Records.Word := 0;
      Last : Records.Word;
      Slot : Store.Count;
      Stored : Store.Outcome;
      View : Records.Write_View;
      Result : Records.Outcome;
      Write_Status : ACPI_NVS_Writes.Completion := ACPI_NVS_Writes.Unavailable;
      Writable : Boolean := False;
   begin
      Target := A.No_Target; Match := Unavailable; Accepted := False;
      if not Is_Ready (IO) or else not AML_Register_Views.Valid_Register (Geometry)
        or else Geometry.Region.Space /= L.Memory_Space or else Geometry.Region.PCI.Known then return; end if;
      IO.Working := True;
      Records.Header (Backing,Scope,Header);
      if Header.Status /= Records.Ready or else Header.Epoch /= IO.Epoch or else Header.Count /= IO.Count then
         IO.Stage := Failed; IO.Working := False; return;
      end if;
      Match := Absent; Last := Geometry.Region.Base+(Geometry.Region.Length-1);
      for I in 1 .. IO.Count loop
         Records.Describe (Backing,Scope,IO.Epoch,I,Item);
         if Item.Status /= Records.Ready or else Item.ID = Records.No_Record or else Item.Length = 0
           or else Item.Length-1 > Records.Word'Last-Item.Base then
            IO.Stage := Failed; IO.Working := False; Match := Unavailable; return;
         end if;
         if Geometry.Region.Base <= Item.Base+(Item.Length-1) and then Item.Base <= Last then
            if Match /= Absent or else Geometry.Region.Base < Item.Base
              or else Geometry.Region.Length > Item.Length-(Geometry.Region.Base-Item.Base)
            then Match := Reserved; IO.Working := False; return; end if;
            Match := Whole_Record; Selected := Item.ID;
            Offset := Geometry.Region.Base-Item.Base+Geometry.Byte_Offset;
         end if;
      end loop;
      if Match /= Whole_Record then IO.Working := False; return; end if;
      for I in 1 .. Store.Length (IO.Targets) loop
         if Store.Read (IO.Targets,I).Geometry = Geometry then
            Target := Target_At (IO,I); Accepted := True; IO.Working := False; return;
         end if;
      end loop;
      Records.Select_Write (Backing,Scope,IO.Epoch,Selected,Offset,Geometry.Width,View,Result);
      if Result = Records.Ready then
         Write_Access (AML_Register_Views.First_Address (Geometry),View.Virtual_Address,
            Geometry.Width,0,False,Write_Status);
         Writable := Write_Status = ACPI_NVS_Writes.Ready;
      elsif Result /= Records.Denied then
         IO.Stage := Failed; IO.Working := False; Match := Unavailable; return;
      end if;
      Store.Append (IO.Targets,(Geometry,Selected,Offset,Writable),Slot,Stored);
      if Stored = Store.Stored then Target := Target_At (IO,Slot); Accepted := True; end if;
      IO.Working := False;
   end Resolve;
   procedure Select_Target (IO : in out Context; Geometry : L.Register_Geometry;
      For_Write : Boolean; Target : out A.Target; Accepted : out Boolean) is
   begin
      Target := A.No_Target; Accepted := False;
      if not Is_Ready (IO) then return; end if;
      for I in 1 .. Store.Length (IO.Targets) loop
         if Store.Read (IO.Targets,I).Geometry = Geometry
           and then (not For_Write or else Store.Read (IO.Targets,I).Writable) then
            Target := Target_At (IO,I); Accepted := True; return;
         end if;
      end loop;
   end Select_Target;
   procedure Describe (IO : Context; Target : A.Target; Geometry : out L.Register_Geometry; Accepted : out Boolean) is
   begin
      Geometry := (others => <>); Accepted := Matches (IO,Target);
      if Accepted then Geometry := Store.Read (IO.Targets,Positive (Target.Register)).Geometry; end if;
   end Describe;
   procedure Authorize (IO : Context; Target : A.Target; For_Write : Boolean; Accepted : out Boolean) is
   begin Accepted := Matches (IO,Target) and then IO.Sequence < A.Counter'Last
      and then (not For_Write or else Store.Read (IO.Targets,Positive (Target.Register)).Writable); end Authorize;
   procedure Dispatch (IO : in out Context; Item : A.Request; Reply : out A.Response) is
      Good : Boolean;
      Value : Records.Word := 0;
      Result : Records.Outcome;
      View : Records.Write_View;
      Write_Status : ACPI_NVS_Writes.Completion := ACPI_NVS_Writes.Unavailable;
   begin
      Reply := (others => <>); Authorize (IO,Item.Destination,Item.For_Write,Good);
      if not Good or else (not Item.For_Write and then Item.Value /= 0) then return; end if;
      if Item.Value > AML_Field_Datums.Width_Mask
         (Store.Read (IO.Targets,Positive (Item.Destination.Register)).Geometry.Width) then return; end if;
      IO.Working := True;
      if not Access_Allowed (Item.For_Write) then IO.Working := False; return; end if;
      IO.Sequence := IO.Sequence+1;
      declare Row : constant Association := Store.Read (IO.Targets,Positive (Item.Destination.Register)); begin
         if Item.For_Write then
            Records.Select_Write (Backing,Scope,IO.Epoch,Row.ID,Row.Offset,Row.Geometry.Width,View,Result);
            if Result = Records.Ready then
               Write_Access (AML_Register_Views.First_Address (Row.Geometry),View.Virtual_Address,
                  Row.Geometry.Width,Item.Value,True,Write_Status);
               case Write_Status is
                  when ACPI_NVS_Writes.Completed => Value := 0; Result := Records.Done;
                  when ACPI_NVS_Writes.Unavailable => Result := Records.Denied;
                  when ACPI_NVS_Writes.Fault =>
                     Records.Stop (Backing); IO.Working := False; IO.Stage := Failed;
                     IO.Fault := (True,IO.Issuer,Item,A.No_Operation);
                     Reply := (A.Failed_Completed,IO.Issuer,0,A.No_Operation); return;
                  when others => Records.Stop (Backing); Result := Records.Read_Failed;
               end case;
            end if;
         else Records.Read (Backing,Scope,IO.Epoch,Row.ID,Row.Offset,Row.Geometry.Width,Value,Result);
         end if;
      end;
      IO.Working := False;
      case Result is
         when Records.Done => Reply := (A.Completed,IO.Issuer,Value,A.No_Operation);
         when Records.Read_Failed =>
            IO.Stage := Failed; IO.Captured := (True,IO.Issuer,Item,A.Operation_ID (IO.Sequence));
            Reply := (A.Indeterminate,IO.Issuer,0,IO.Captured.Operation);
         when Records.Denied => null;
         when others =>
            IO.Stage := Failed; IO.Fault := (True,IO.Issuer,Item,A.No_Operation);
            Reply := (A.Failed_Completed,IO.Issuer,0,A.No_Operation);
      end case;
   end Dispatch;
   procedure Reconciled (IO : Context; Expected : A.Observation; Accepted : out Boolean) is
      pragma Unreferenced (IO,Expected);
   begin Accepted := False; end Reconciled;
   procedure Close (IO : in out Context) is
   begin if not IO.Working then IO.Stage := Closed; end if; end Close;
end ACPI_Local_NVS_Provider;
