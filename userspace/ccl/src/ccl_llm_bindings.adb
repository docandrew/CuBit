with CCL.Interfaces.Llm;
with CCL.Objects;
with CCL.Objects.Views;
with CCL.Streams;
with CCL_Llm;
with CuBit.Failures;
with CuBit.Llm_Protocol;
with Agent_Blocks;

package body CCL_Llm_Bindings is
   use Interfaces;
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Catalog.Grant_Result;
   use type CCL.Objects.Build_Result;
   use type CCL.Host_Values.Value_Kind;
   package I renames CCL.Interfaces.Llm;
   use type I.Operation;
   package Views renames CCL.Objects.Views;

   Contracts : I.Contracts;

   --  The exchanges this session follows: their text stream and reply
   --  task, pinned while the exchange runs and after, until the slot is
   --  needed for a newer one (the oldest finished goes first).
   MAXIMUM_FOLLOWED : constant := 16;
   type Followed is record
      Used : Boolean := False;
      Exchange : Unsigned_64 := 0;
      Text, Reply : CCL.Streams.Handle := CCL.Streams.No_Handle;
      Finished : Boolean := False;
      Started : Unsigned_64 := 0;   --  start order, for eviction
   end record;
   Table : array (1 .. MAXIMUM_FOLLOWED) of Followed;
   Starts : Unsigned_64 := 0;
   Started_Items : Started_Array := [others => 0];
   Started_Count : Natural := 0;

   function Handles (Binding : Unsigned_32) return Boolean is
     (for some Op in I.Operation => Binding = I.Binding_Of (Op));

   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean)
   is
      Error : CCL.Catalog.Catalog_Error;
      Grant : CCL.Catalog.Grant_Result;
      Resolved : CCL.Catalog.Resolved_Operation;
      Found : Boolean;
   begin
      I.Publish (Catalog, Error);
      Success := Error = CCL.Catalog.Catalog_Valid;
      if not Success then return; end if;
      CCL.Catalog.Resolve_Schema (Catalog, I.REQUEST_KEY, Contracts.Request);
      CCL.Catalog.Resolve_Schema (Catalog, I.EXCHANGE_KEY, Contracts.Exchange);
      CCL.Catalog.Resolve_Schema (Catalog, I.REPLY_KEY, Contracts.Reply);
      Success := CCL.Objects.Is_Bound (Contracts.Request) and then CCL.Objects.Is_Bound (Contracts.Exchange)
        and then CCL.Objects.Is_Bound (Contracts.Reply);
      for Op in I.Operation loop
         exit when not Success;
         CCL.Catalog.Resolve (Catalog, "llm." & I.Name (Op), Resolved, Found);
         Success := Found;
         if Success then
            CCL.Catalog.Install (Grants, Resolved, I.Binding_Of (Op), Grant);
            Success := Grant = CCL.Catalog.Grant_Added;
         end if;
      end loop;
   end Install;

   function Find (Exchange : Unsigned_64) return Natural is
   begin
      for K in Table'Range loop
         if Table (K).Used and then Table (K).Exchange = Exchange then
            return K;
         end if;
      end loop;
      return 0;
   end Find;

   --  A free row, else the oldest finished one (its streams unpinned).
   function Free_Row (Streams : in out CCL_Stream_Table.Table) return Natural is
      Oldest : Natural := 0;
   begin
      for K in Table'Range loop
         if not Table (K).Used then
            return K;
         elsif Table (K).Finished and then
           (Oldest = 0 or else Table (K).Started < Table (Oldest).Started)
         then
            Oldest := K;
         end if;
      end loop;
      if Oldest /= 0 then
         CCL_Stream_Table.Unpin (Streams, Table (Oldest).Text);
         CCL_Stream_Table.Unpin (Streams, Table (Oldest).Reply);
         Table (Oldest) := (others => <>);
      end if;
      return Oldest;
   end Free_Row;

   function Exchange_Image (Exchange : Unsigned_64) return CCL.Host_Values.Value is
      Image : CCL.Objects.Image := CCL.Objects.Empty (Contracts.Exchange);
      Step : CCL.Objects.Build_Result := CCL.Objects.Added;
   begin
      CCL.Objects.Append (Image, CCL.Objects.Product_Cell (I.EXCHANGE_FIELDS), Step);
      if Step = CCL.Objects.Added then
         CCL.Objects.Append (Image, CCL.Objects.Integer_Cell (Integer_64 (Exchange and 16#7FFF_FFFF_FFFF_FFFF#)), Step);
      end if;
      return CCL.Host_Values.Object_Constant (Image);
   end Exchange_Image;

   procedure Run_Ask
     (Argument : CCL.Host_Values.Value; Streams : in out CCL_Stream_Table.Table;
      Reply : in out CCL.Host_Values.Call_Result)
   is
      Object : Views.Snapshot;
      Captured : Boolean := False;
   begin
      if Argument.Kind = CCL.Host_Values.Object_Value then
         Views.Capture (Object, Contracts.Request, Argument.Object, Captured);
      end if;
      if not Captured then
         Reply.Why := CuBit.Failures.Failed (CuBit.Failures.Invalid_Argument, "llm.ask takes an Llm_Request");
         return;
      end if;
      declare
         Root : constant Views.Cursor := Views.Root (Object);
         function Field (N : Positive) return Views.Cursor is (Views.Field (Object, Root, N));
         Model_Field : constant Views.Cursor := Field (I.REQUEST_MODEL);
         Ceiling_Field : constant Views.Cursor := Field (I.REQUEST_CEILING);
         Named_Model : constant Boolean := Natural (Views.Alternative (Object, Model_Field)) = I.MODEL_NAMED;
         Model : constant String :=
           (if Named_Model then Views.Text (Object, Views.Payload (Object, Model_Field)) else "");
         Bounded : constant Boolean := Natural (Views.Alternative (Object, Ceiling_Field)) = I.CEILING_NANO_USD;
         Ceiling : constant Integer_64 :=
           (if Bounded then CCL.Objects.Integer_Of (Views.Scalar (Object, Views.Payload (Object, Ceiling_Field)))
            else 0);
         Max_Tokens : constant Integer_64 := CCL.Objects.Integer_Of (Views.Scalar (Object, Field (I.REQUEST_MAX_TOKENS)));
         Deadline : constant Integer_64 := CCL.Objects.Integer_Of (Views.Scalar (Object, Field (I.REQUEST_DEADLINE)));
         Prompt : constant String := Views.Text (Object, Field (I.REQUEST_PROMPT));
         System : constant String := Views.Text (Object, Field (I.REQUEST_SYSTEM));
         Mission : constant String := Views.Text (Object, Field (I.REQUEST_MISSION));
         Exchange : Unsigned_64;
         Result : CCL_Llm.Ask_Result;
         Why : String (1 .. 160);
         Why_Length : Natural;
         Row : Natural;
         use type CCL_Llm.Ask_Result;
      begin
         --  The CCL range types check these; the service checks them again.
         if Max_Tokens not in 1 .. I.MAXIMUM_TOKENS or else Deadline not in 1 .. I.MAXIMUM_DEADLINE_MS or else Ceiling < 0
           or else (Named_Model and then Model'Length = 0)
         then
            Reply.Why := CuBit.Failures.Failed
              (CuBit.Failures.Invalid_Argument, "an Llm_Request limit is out of range",
               "max_tokens 1-128000, deadline_ms 1-600000, a ceiling of at least 0 nano-USD, a model name");
            return;
         end if;
         Row := Free_Row (Streams);
         if Row = 0 then
            Reply.Why := CuBit.Failures.Failed
              (CuBit.Failures.Exhausted, "this session follows" & Natural'Image (MAXIMUM_FOLLOWED) &
               " exchanges that are still running");
            return;
         end if;
         --  The prompt is the conversation's one block; no tools, no cache
         --  (one question has no prefix to reuse), the text only.
         CCL_Llm.Ask (Model, Mission, System, "", Agent_Blocks.Encode (CuBit.Llm_Protocol.User,
                      CuBit.Llm_Protocol.Text_Block, Prompt), False, False,
                      Positive (Max_Tokens), Positive (Deadline),
                      (if Bounded then CCL_Llm.Nano_Usd else CCL_Llm.Unlimited), Unsigned_64 (Ceiling),
                      Exchange, Result, Why, Why_Length);
         if Result /= CCL_Llm.Started then
            Reply.Why := CuBit.Failures.Failed
              ((case Result is
                  when CCL_Llm.Not_Granted => CuBit.Failures.Not_Granted,
                  when CCL_Llm.Unavailable => CuBit.Failures.Unavailable,
                  when others => CuBit.Failures.Refused),
               "llm: " & Why (1 .. Why_Length));
            return;
         end if;
         Table (Row) := (Used => True, Exchange => Exchange, others => <>);
         Starts := Starts + 1;
         Table (Row).Started := Starts;
         CCL_Stream_Table.Open_Outlet (Streams, CCL_Stream_Table.Text_Elements, Table (Row).Text, Pinned => True);
         CCL_Stream_Table.Open_Task (Streams, Table (Row).Reply, Pinned => True);
         if Started_Count < MAXIMUM_STARTED then
            Started_Count := Started_Count + 1;
            Started_Items (Started_Count) := Exchange;
         end if;
         Reply := (Value => Exchange_Image (Exchange), Success => True, Why => <>);
      end;
   end Run_Ask;

   procedure Invoke
     (Binding : Unsigned_32; Argument : CCL.Host_Values.Value;
      Streams : in out CCL_Stream_Table.Table;
      Reply : out CCL.Host_Values.Call_Result)
   is
      Op : I.Operation;
      Object : Views.Snapshot;
      Captured : Boolean := False;
   begin
      Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False, Why => <>);
      if not Handles (Binding) then return; end if;
      Op := I.Operation'Val (Binding - I.FIRST_BINDING);
      if Op = I.Ask then
         Run_Ask (Argument, Streams, Reply);
         return;
      end if;
      if Argument.Kind = CCL.Host_Values.Object_Value then
         Views.Capture (Object, Contracts.Exchange, Argument.Object, Captured);
      end if;
      if not Captured then
         Reply.Why := CuBit.Failures.Failed (CuBit.Failures.Invalid_Argument, "llm: takes an Llm_Exchange");
         return;
      end if;
      declare
         Id : constant Integer_64 :=
           CCL.Objects.Integer_Of (Views.Scalar (Object, Views.Field (Object, Views.Root (Object), 1)));
         Row : constant Natural := (if Id < 0 then 0 else Find (Unsigned_64 (Id)));
      begin
         if Row = 0 then
            Reply.Why := CuBit.Failures.Failed
              (CuBit.Failures.Invalid_Argument, "llm: this session does not follow that exchange");
            return;
         end if;
         case Op is
            when I.Text =>
               Reply := (Value => CCL.Host_Values.Integer_Constant (Integer_64 (Table (Row).Text)),
                         Success => True, Why => <>);
            when I.Reply =>
               Reply := (Value => CCL.Host_Values.Integer_Constant (Integer_64 (Table (Row).Reply)),
                         Success => True, Why => <>);
            when I.Cancel =>
               declare
                  Done : Boolean;
               begin
                  if not Table (Row).Finished then
                     CCL_Llm.Cancel (Table (Row).Exchange, Done);
                  end if;
                  Reply := (Value => Exchange_Image (Table (Row).Exchange), Success => True, Why => <>);
               end;
            when I.Ask =>
               null;
         end case;
      end;
   end Invoke;

   procedure Reply_Image (S : CCL_Llm.Summary; Image : out CCL.Objects.Image; Built : out Boolean) is
      Step : CCL.Objects.Build_Result := CCL.Objects.Added;
      procedure Put (Cell : CCL.Objects.Cell) is
      begin
         if Step = CCL.Objects.Added then CCL.Objects.Append (Image, Cell, Step); end if;
      end Put;
      procedure Text (Value : CCL_Llm.Short_Text) is
      begin
         if Step = CCL.Objects.Added then
            CCL.Objects.Append_Text (Image, Value.Data (1 .. Value.Length), Step);
         end if;
      end Text;
      function Count (Value : Unsigned_64) return CCL.Objects.Cell is
        (CCL.Objects.Integer_Cell (Integer_64 (Unsigned_64'Min (Value, Unsigned_64 (Integer_64'Last)))));
   begin
      Image := CCL.Objects.Empty (Contracts.Reply);
      Put (CCL.Objects.Product_Cell (I.REPLY_FIELDS));
      Put (CCL.Objects.Variant_Cell (CCL_Llm.Stop'Pos (S.Ended_As) + 1));
      Put (CCL.Objects.Unit_Cell);
      Text (S.Error);
      Text (S.Problem);
      Put (Count (Unsigned_64 (S.Status)));
      Text (S.Model);
      Put (Count (S.Input));
      Put (Count (S.Output));
      Put (Count (S.Cache_Read));
      Put (Count (S.Cache_Write));
      if S.Priced then
         Put (CCL.Objects.Variant_Cell (I.COST_PRICED));
         Put (Count (S.Cost_Nano_Usd));
      else
         Put (CCL.Objects.Variant_Cell (I.COST_UNPRICED));
         Put (CCL.Objects.Unit_Cell);
      end if;
      Put (Count (S.Duration_Ms));
      Put (Count (S.Text_Bytes));
      Text (S.Detail);
      Built := Step = CCL.Objects.Added and then CCL.Objects.Validate (Image, Contracts.Reply);
   end Reply_Image;

   procedure Pump (Streams : in out CCL_Stream_Table.Table; Arrived : out Boolean) is
      Chunk : String (1 .. 4_096);
      Last : Natural;
      Pushed : Boolean;
   begin
      Arrived := False;
      for K in Table'Range loop
         if Table (K).Used and then not Table (K).Finished then
            loop
               CCL_Llm.Take_Text (Table (K).Exchange, Chunk, Last);
               exit when Last < Chunk'First;
               CCL_Stream_Table.Append_Text (Streams, Table (K).Text, Chunk (Chunk'First .. Last), Pushed);
               Arrived := True;
            end loop;
            declare
               S : CCL_Llm.Summary;
               No_Content : CCL_Llm.Text_Access;
               Ended, Built, Completed : Boolean;
               Image : CCL.Objects.Image;
            begin
               CCL_Llm.Take_Summary (Table (K).Exchange, S, No_Content, Ended);
               CCL_Llm.Free (No_Content);
               if Ended or else not CCL_Llm.Following (Table (K).Exchange) then
                  Table (K).Finished := True;
                  CCL_Stream_Table.End_Stream (Streams, Table (K).Text);
                  if Ended then
                     Reply_Image (S, Image, Built);
                     if Built then
                        CCL_Stream_Table.Complete_Task (Streams, Table (K).Reply, Image, Completed);
                     end if;
                  end if;
                  Arrived := True;
               end if;
            end;
         end if;
      end loop;
   end Pump;

   function Running return Boolean is
     (for some F of Table => F.Used and then not F.Finished);

   procedure Take_Started (Items : out Started_Array; Count : out Natural) is
   begin
      Items := Started_Items;
      Count := Started_Count;
      Started_Items := [others => 0];
      Started_Count := 0;
   end Take_Started;
end CCL_Llm_Bindings;
