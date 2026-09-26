with Config_Collections;
with Config_Objects;

package body Config_Object_Dispatch with SPARK_Mode is
   package Wire renames Config_Object_Messages;
   package Catalog renames Config_Collections;
   package Values renames Config_Objects;
   package Typed renames Config_Typed_Store;
   use type Interfaces.Unsigned_64;
   use type Interfaces.Unsigned_32;
   use type Catalog.Result;
   use type Values.Outcome;
   function Waiting (Object : State) return Boolean is (Object.Awaiting);

   procedure Handle
     (Object : in out State; Store : in out Typed.State;
      Authority : Config_Authority.Authority_State; Sender : CuBit.Messages.ProcessID;
      Action : Wire.Operation; Request : CuBit.Messages.Message;
      Input : Wire.Frame; Storage_Token : Interfaces.Unsigned_64;
      Reply_Reserved : Boolean; Reply : out CuBit.Messages.Message;
      Value : out CCL.Objects.Image; Next : out Disposition)
   is
      Handle, Revision : Interfaces.Unsigned_64;
      Access_Result : Catalog.Result;
      Read_Result : Values.Read_Result;
      Result : Values.Outcome;
      Authorized : Boolean;
   begin
      Reply := Wire.Reply (Wire.Invalid_Request); Value := (others => <>); Next := Reply_Now;
      if not Wire.Valid_Request (Request, Action) then return; end if;
      case Action is
         when Wire.Create_Collection => null; -- Creation has its own deferred service path.
         when Wire.Open_Collection =>
            if not Wire.Valid_Descriptor (Input.Control) then return; end if;
            Typed.Open (Store, Authority, Sender,
              Input.Control.Name (1 .. Natural (Input.Control.Name_Length)), Input.Control.Context,
              (Config_Authority.Read_Config => Input.Control.Access_Rights mod 2 = 1,
               Config_Authority.Write_Config => Input.Control.Access_Rights >= 2),
              Input.Control.Schema, Handle, Access_Result);
            case Access_Result is
               when Catalog.Opened => Reply := Wire.Reply (Wire.Success, Handle);
               when Catalog.Denied => Reply := Wire.Reply (Wire.Denied);
               when Catalog.Missing => Reply := Wire.Reply (Wire.Missing);
               when Catalog.Schema_Conflict => Reply := Wire.Reply (Wire.Schema_Mismatch);
               when Catalog.Capacity_Exceeded | Catalog.Identity_Exhausted => Reply := Wire.Reply (Wire.Capacity_Exceeded);
               when others => null;
            end case;
         when Wire.Get_Object =>
            Typed.Get (Store, Authority, Sender, Request.words (0), Value, Revision, Authorized, Read_Result);
            if not Authorized then Reply := Wire.Reply (Wire.Denied); return; end if;
            case Read_Result is
               when Values.Found => Reply := Wire.Reply (Wire.Success, Revision);
               when Values.Stale => Reply := Wire.Reply (Wire.Stale, Revision);
               when Values.Missing => Reply := Wire.Reply (Wire.Missing);
               when Values.Unavailable => Reply := Wire.Reply (Wire.Unavailable);
               when Values.Schema_Mismatch => Reply := Wire.Reply (Wire.Schema_Mismatch);
            end case;
         when Wire.Set_Object =>
            if not Typed.Check_Access (Store, Authority, Sender, Request.words (0), Config_Authority.Write_Config) then
               Reply := Wire.Reply (Wire.Denied); return;
            end if;
            if Object.Awaiting then Reply := Wire.Reply (Wire.Busy); return; end if;
            if not Reply_Reserved then Reply := Wire.Reply (Wire.Unavailable); return; end if;
            Typed.Set (Store, Authority, Sender, Request.words (0), Input.Value,
                       Request.words (2), Storage_Token, Authorized, Result);
            if not Authorized then Reply := Wire.Reply (Wire.Denied); return; end if;
            case Result is
               when Values.Accepted =>
                  Object.Awaiting := True;
                  Object.Session := Typed.Pending_Session (Store);
                  Object.Token := Typed.Pending_Token (Store);
                  Next := Await_Storage;
                  Reply := CuBit.Messages.NULL_MESSAGE;
               when Values.Busy => Reply := Wire.Reply (Wire.Busy);
               when Values.Revision_Conflict => Reply := Wire.Reply (Wire.Conflict);
               when Values.Invalid_Value | Values.Invalid_Request => null;
               when Values.Rejected | Values.Revision_Exhausted => Reply := Wire.Reply (Wire.Rejected);
               when others => Reply := Wire.Reply (Wire.Unavailable);
            end case;
         when Wire.Close_Collection =>
            Typed.Close (Store, Sender, Request.words (0), Access_Result);
            Reply := Wire.Reply (if Access_Result = Catalog.Closed then Wire.Success else Wire.Denied);
      end case;
   end Handle;

   procedure Finish
     (Object : in out State; Store : in out Typed.State;
      Response : Config_Worker_Protocol.Frame; Reply : out CuBit.Messages.Message; Ready : out Boolean)
   is
      Owned : constant Config_Worker_Protocol.Frame := Response;
      Result : Values.Outcome;
   begin
      Ready := False; Reply := CuBit.Messages.NULL_MESSAGE;
      Typed.Complete (Store, Owned, Result);
      if Object.Awaiting and then Object.Session = Owned.Session and then Object.Token = Owned.Token
        and then Result /= Values.Ignored
      then
         Ready := True; Object.Awaiting := False;
         Object.Session := 0; Object.Token := 0;
         case Result is
            when Values.Published => Reply := Wire.Reply (Wire.Success, Owned.Revision);
            when Values.Rejected => Reply := Wire.Reply (Wire.Rejected);
            -- The request crossed the durable-write boundary. A failed or
            -- invalid receipt cannot establish that it had no effect.
            when others => Reply := Wire.Reply (Wire.Uncertain);
         end case;
      end if;
   end Finish;

   procedure Lost
     (Object : in out State; Store : in out Typed.State;
      Session : Interfaces.Unsigned_64; Reply : out CuBit.Messages.Message; Ready : out Boolean) is
   begin
      Typed.Worker_Lost (Store, Session);
      Ready := Object.Awaiting and then Object.Session = Session;
      Reply := CuBit.Messages.NULL_MESSAGE;
      if Ready then
         Object.Awaiting := False; Object.Session := 0; Object.Token := 0;
         Reply := Wire.Reply (Wire.Uncertain);
      end if;
   end Lost;
end Config_Object_Dispatch;
