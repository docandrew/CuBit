with CuBit.Filesystems;
with CuBit.Memory_Grants;
with Compositor_Requests;
with Observatory_Query_Lifetime;
with Observatory_Archive_Stream;
package body Observatory_Archive_Reader with SPARK_Mode => Off is
   use Interfaces;
   use CuBit.Messages;
   package FS renames CuBit.Filesystems;
   package G renames CuBit.Memory_Grants;
   package L renames Observatory_Query_Lifetime;
   package S renames Observatory_Archive_Stream;
   use type FS.File_Handle, L.Phase;
   type Byte_Page is array (Natural range 0 .. 4095) of Unsigned_8;
   Buffer : Byte_Page := [others => 0] with Alignment => 4096, Volatile;
   Grant : G.Grant_Reference;
   Has_Grant, Revoked : Boolean := False;
   Life : L.State;
   Stream : S.State;
   type Operation is (Opening, Reading, Closing);
   Pending : Operation := Opening;
   Result : Result_Kind := Idle;
   Handle : FS.File_Handle := FS.INVALID_FILE_HANDLE;
   Offset, Requested, Reply_Value : Unsigned_64 := 0;
   Path : constant String := "@nvme:0/work/desktop-trace.cubittrace";
   function Status return Result_Kind is (Result);
   function Cleanup_Pending return Boolean is (Has_Grant);
   function Matches (Token : Unsigned_64) return Boolean is
     (Token /= 0 and then Token = L.Token (Life));
   procedure Close is
   begin
      Result := Unavailable; L.Fail (Life);
      -- A failed or timed-out operation is not cancelled. Keep Buffer alive;
      -- retirement may complete later, but this instance is never reused.
   end Close;
   procedure Start (Page : Observatory_Trace_View.Page_Number; Accepted : out Boolean) is
   begin
      Accepted := Result in Idle | Complete | Incomplete and not Has_Grant and
        Handle = FS.INVALID_FILE_HANDLE and L.Status (Life) = L.Idle;
      if not Accepted then return; end if;
      S.Start (Stream, Page); Offset := 0; Pending := Opening; Result := Loading;
   end Start;
   procedure Submit (Sequence : in out Unsigned_64; Now_Us : Unsigned_64) is
      Token : Unsigned_64;
      Accepted : Boolean;
      Msg : Message;
   begin
      if Has_Grant or L.Status (Life) /= L.Idle then Close; return; end if;
      Compositor_Requests.Allocate (Sequence, Token);
      L.Start (Life, Token, Now_Us, Accepted, Budget_Us => Timeout_Us);
      if not Accepted then Close; return; end if;
      if Pending /= Closing then
         Buffer := [others => 0];
         if Pending = Opening then
            for I in Path'Range loop Buffer (I - Path'First) := Character'Pos (Path (I)); end loop;
         end if;
         G.Create_Via_Capability (Capability, Buffer'Address, 1, True, Grant, Has_Grant);
         if not Has_Grant then Close; return; end if;
         Revoked := False;
      end if;
      case Pending is
         when Opening => Msg := FS.Open_Request (Grant, Path'Length, FS.OPEN_READ_ONLY);
         when Reading =>
            Requested := Unsigned_64'Min (4096, Unsigned_64 (S.Maximum_Bytes) - Offset + 1);
            Msg := FS.Read_At_Request (Handle, Grant, Requested, Offset);
         when Closing => Msg := FS.Close_Request (Handle);
      end case;
      if not capSubmit (Capability, Msg, Token) then Close; end if;
   end Submit;
   procedure Collect (Value : CompletionEntry) is
      Admitted : Boolean;
   begin
      if L.Status (Life) /= L.Waiting or else not Matches (Value.token) then return; end if;
      Admitted := Value.valid and then Value.status = 0 and then
        Value.msg.tag.label = FS.REPLY_OK and then
        Value.msg.tag.length = (if Pending = Opening then 2 else 1) and then
        Value.msg.tag.flags = 0 and then Value.msg.tag.reserved = 0 and then
        (for all I in 2 .. 3 => Value.msg.words (I) = 0) and then
        (if Pending /= Opening then Value.msg.words (1) = 0) and then
        (case Pending is
           when Opening => Value.msg.words (0) /= 0,
           when Reading => Value.msg.words (0) <= Requested,
           when Closing => Value.msg.words (0) = 0);
      Reply_Value := Value.msg.words (0);
      L.Receive (Life, Value.token, Admitted);
   end Collect;
   procedure Tick (Sequence : in out Unsigned_64; Now_Us : Unsigned_64) is
   begin
      L.Expire (Life, Now_Us);
      if L.Status (Life) = L.Failed then Result := Unavailable; end if;
      if Has_Grant and then L.Status (Life) in L.Retiring | L.Failed then
         if not Revoked then G.Revoke (Grant, Revoked); end if;
         if Revoked and then G.Retirement_Confirmed (Grant) then Has_Grant := False; end if;
      end if;
      if L.Status (Life) = L.Retiring and not Has_Grant then L.Retired (Life, True); end if;
      if L.Status (Life) = L.Ready then
         case Pending is
            when Opening => Handle := FS.File_Handle (Reply_Value); Pending := Reading;
            when Reading =>
               if Reply_Value = 0 then S.Finish (Stream); Pending := Closing;
               elsif Reply_Value > Unsigned_64 (S.Maximum_Bytes) - Offset then
                  -- One byte beyond the format limit is sufficient to reject.
                  S.Start (Stream, Observatory_Trace_View.Page (S.Context (Stream)));
                  Pending := Closing;
               else
                  -- The matching reply and independent retirement have both
                  -- completed. Only now may foreign-written bytes be consumed.
                  for I in 0 .. Natural (Reply_Value) - 1 loop S.Append (Stream, Buffer (I)); end loop;
                  Offset := Offset + Reply_Value;
               end if;
            when Closing =>
               Handle := FS.INVALID_FILE_HANDLE;
               Result := (if S.Ready (Stream) then Complete else Incomplete);
         end case;
         L.Consume (Life);
      end if;
      if Result = Loading and then L.Status (Life) = L.Idle then Submit (Sequence, Now_Us); end if;
   end Tick;
   procedure Take (View : out Observatory_Trace_View.State; Success : out Boolean) is
   begin
      Success := Result = Complete and not Has_Grant and Handle = FS.INVALID_FILE_HANDLE;
      if Success then View := S.Context (Stream);
      else Observatory_Trace_View.Start (View, [others => 0], 0); end if;
   end Take;
end Observatory_Archive_Reader;
