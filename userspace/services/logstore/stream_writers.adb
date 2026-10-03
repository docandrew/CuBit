with System.Storage_Elements; use System.Storage_Elements;
with System.Machine_Code;
with CuBit.Messages;
with CuBit.Datagram_Rings;
with CuBit.Log_Protocol;
with CuBit.Log_Streams;

package body Stream_Writers is
   package Grants renames CuBit.Memory_Grants;
   package Rings renames CuBit.Channel_Rings;
   package Streams renames CuBit.Log_Streams;
   use type System.Address;
   use type CuBit.Log_Protocol.Status;
   use type CuBit.Datagram_Rings.Put_Result;
   use type Grants.Grant_Reference;

   procedure Release_Fence;
   function Shared_Index (Base : System.Address; Offset : Natural) return Rings.Index;
   procedure Set_Shared_Index (Base : System.Address; Offset : Natural; Value : Rings.Index);
   procedure Release (S : in out Stream);

   --  Ring bytes before the index that publishes them: no reordering past it.
   procedure Release_Fence is
   begin
      System.Machine_Code.Asm ("", Clobber => "memory", Volatile => True);
   end Release_Fence;

   function Shared_Index (Base : System.Address; Offset : Natural) return Rings.Index is
      Value : Rings.Index with Import, Volatile, Address => Base + Storage_Offset (Offset);
   begin
      return Value;
   end Shared_Index;
   procedure Set_Shared_Index (Base : System.Address; Offset : Natural; Value : Rings.Index) is
      Target : Rings.Index with Import, Volatile, Address => Base + Storage_Offset (Offset);
   begin
      Target := Value;
   end Set_Shared_Index;

   procedure Release (S : in out Stream) is
      Returned : Boolean;
   begin
      if S.Base /= System.Null_Address then
         Grants.Return_Acquisition (S.Ref, Returned);
      end if;
      S := (others => <>);
   end Release;

   procedure Attach
     (Item : in out Table; Handle, Owner, Authority : Unsigned_64;
      Ref : Grants.Grant_Reference; Attached : out Boolean)
   is
      Free : Natural := 0;
      Base : System.Address;
   begin
      Attached := False;
      for I in Item'Range loop
         if Item (I).Handle = Handle and then Item (I).Base /= System.Null_Address then
            if Item (I).Ref = Ref then
               Attached := True;
               return;
            end if;
            Release (Item (I));
         end if;
         if Free = 0 and then Item (I).Base = System.Null_Address then
            Free := I;
         end if;
      end loop;
      if Free = 0 then
         return;
      end if;
      Grants.Acquire
        (Ref, CuBit.Messages.ProcessID (Owner), 0, Streams.STREAM_BYTES, Grants.Write_Access, Base, Attached);
      if Attached then
         Item (Free) :=
           (Handle => Handle, Owner => Owner, Authority => Authority, Ref => Ref, Base => Base,
            Producer => Rings.New_Producer (Streams.RING_BYTES));
         --  logstore's index is its own; the reader starts from zero too.
         Set_Shared_Index (Base, Streams.PRODUCED_OFFSET, 0);
      end if;
   end Attach;

   procedure Detach (Item : in out Table; Handle : Unsigned_64) is
   begin
      for S of Item loop
         if S.Handle = Handle and then S.Base /= System.Null_Address then
            Release (S);
         end if;
      end loop;
   end Detach;

   procedure Drain (Item : in out Table; Store : in out Log_Fanout.Broker; Backlog : out Boolean) is
   begin
      Backlog := False;
      for S of Item loop
         if S.Base /= System.Null_Address then
            if not Log_Fanout.Active (Store, S.Handle) then
               --  Closed or expired: the region goes back to its owner.
               Release (S);
            else
               declare
                  Ring : Rings.Bytes (0 .. Streams.RING_BYTES - 1)
                    with Import, Address => S.Base + Storage_Offset (Streams.CONTROL_BYTES);
                  Accepted : Boolean;
                  Wrote : Boolean := False;
                  Value : CuBit.Log_Protocol.Event;
                  Lost : Unsigned_64;
                  Result : CuBit.Log_Protocol.Status;
                  Entry_Bytes : Streams.Entry_Buffer;
                  Length : Streams.Entry_Length;
                  Put : CuBit.Datagram_Rings.Put_Result;
               begin
                  --  The reader's index is untrusted: an index that does not
                  --  move forward within the ring is ignored.
                  Rings.Accept_Consumed (S.Producer, Shared_Index (S.Base, Streams.CONSUMED_OFFSET), Accepted);
                  loop
                     if Rings.Space (S.Producer) < Streams.Room_Needed then
                        Backlog := True;
                        exit;
                     end if;
                     Log_Fanout.Read_Next
                       (Store, S.Owner, S.Authority, S.Handle, Value, Lost, Result, Renew => False);
                     exit when Result not in CuBit.Log_Protocol.OK | CuBit.Log_Protocol.Gap;
                     if Result = CuBit.Log_Protocol.Gap then
                        Streams.Encode_Gap (Lost, Entry_Bytes, Length);
                     else
                        Streams.Encode_Event (Value, Entry_Bytes, Length);
                     end if;
                     CuBit.Datagram_Rings.Put (S.Producer, Ring, Entry_Bytes (0 .. Length - 1), Put);
                     Wrote := Wrote or else Put = CuBit.Datagram_Rings.Put;
                  end loop;
                  if Wrote then
                     Release_Fence;
                     Set_Shared_Index (S.Base, Streams.PRODUCED_OFFSET, S.Producer.Produced);
                  end if;
               end;
            end if;
         end if;
      end loop;
   end Drain;
end Stream_Writers;
