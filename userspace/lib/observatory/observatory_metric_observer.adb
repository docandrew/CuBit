with CuBit.Memory_Grants;
with CuBit.Metric_Records;
with Compositor_Requests;
with Observatory_Query_Lifetime;
package body Observatory_Metric_Observer with SPARK_Mode => Off is
   use Interfaces;
   use CuBit.Messages;
   package G renames CuBit.Memory_Grants;
   package P renames CuBit.Metric_Protocol;
   package Q renames Observatory_Metric_Queries;
   package L renames Observatory_Query_Lifetime;
   use type L.Phase;
   type Aligned_Page is new P.Summary_Page with Alignment => 4096;
   -- Written by a foreign process while granted; compiler-visible storage
   -- effects remain explicit even though reads wait for confirmed retirement.
   Page : Aligned_Page := [others => [others => 0]] with Volatile;
   Grant : G.Grant_Reference;
   Has_Grant, Revoked : Boolean := False;
   Life : L.State;
   Requested : Q.Cursor := 0;
   Reply : Q.Reply;
   function Disabled return Boolean is (L.Status (Life) = L.Failed);
   function Cleanup_Pending return Boolean is (Has_Grant);
   function Ready return Boolean is (L.Status (Life) = L.Ready);
   function Matches (Token : Unsigned_64) return Boolean is
     (Token /= 0 and then Token = L.Token (Life));
   procedure Close is
   begin
      L.Fail (Life);
      -- Page remains alive. Tick may finish retirement but never revives Life.
   end Close;
   procedure Begin_Query (Cursor : Q.Cursor; Sequence : in out Unsigned_64;
      Now : Unsigned_64; Submitted : out Boolean) is
      Token : Unsigned_64;
      Accepted : Boolean;
      Msg : Message := NULL_MESSAGE;
   begin
      Submitted := False;
      if L.Status (Life) /= L.Idle or else Has_Grant then return; end if;
      Compositor_Requests.Allocate (Sequence, Token);
      L.Start (Life, Token, Now, Accepted);
      if not Accepted then Close; return; end if;
      Requested := Cursor;
      -- No grant exists here. Unwritten rows in a later reply must be invalid,
      -- rather than retaining apparently valid samples from the last query.
      Page := [others => [others => 0]];
      G.Create_Via_Capability (Capability, Page'Address, 1, True, Grant, Has_Grant);
      if not Has_Grant then Close; return; end if;
      Revoked := False;
      Msg.tag := (P.Operation'Enum_Rep (P.Query_Summaries), P.Message_Words, 0, 0);
      Msg.words := [Cursor, Grant.slot, Grant.generation, CuBit.Metric_Records.Page_Bytes];
      Submitted := capSubmit (Capability, Msg, Token);
      if not Submitted then Close; end if;
   end Begin_Query;
   procedure Collect (Value : CompletionEntry) is
   begin
      if L.Status (Life) /= L.Waiting or else not Matches (Value.token) then return; end if;
      Reply := (Value.valid, Value.status, Value.msg.tag.label, Value.msg.tag.length,
                Value.msg.tag.flags, Value.msg.tag.reserved,
                [for I in Q.Words'Range => Value.msg.words (I)]);
      L.Receive (Life, Value.token, Q.Admitted (Reply, Requested));
      -- Do not read the writable page on completion alone. Independently
      -- revoke the grant and establish retirement in Tick before decoding.
   end Collect;
   procedure Tick (Now : Unsigned_64) is
   begin
      L.Expire (Life, Now);
      if Has_Grant and then L.Status (Life) in L.Retiring | L.Failed then
         if not Revoked then G.Revoke (Grant, Revoked); end if;
         if Revoked and then G.Retirement_Confirmed (Grant) then
            Has_Grant := False;
            if L.Status (Life) = L.Retiring then
               if Q.Valid_Page (Reply, Requested, P.Summary_Page (Page)) then
                  L.Retired (Life, True);
               else Close; end if;
            end if;
         end if;
      end if;
   end Tick;
   procedure Take (Rows : out P.Summary_Page; Written : out P.Row_Count;
      Next : out Q.Cursor; Success : out Boolean) is
   begin
      Rows := [others => [others => 0]]; Written := 0; Next := Requested;
      Success := Ready;
      if not Success then return; end if;
      Rows := P.Summary_Page (Page);
      Written := P.Row_Count (Reply.Payload (0)); Next := Q.Cursor (Reply.Payload (1));
      L.Consume (Life);
   end Take;
end Observatory_Metric_Observer;
