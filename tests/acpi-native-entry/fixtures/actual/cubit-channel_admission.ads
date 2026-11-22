------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A server's channel limits (CAP-002 step 3, docs/capability-handles.md
--  section 4): per offered contract, a per-client limit and a total, each
--  either At_Most N or Server_Limit (the server's own capacity, spelled out
--  in the manifest like Kernel_Limit for handles; user, 2026-10-17).
--
--  @description
--  A client is one process life (the kernel-reported sender identity's
--  word). Admit counts a new channel or says which declared limit refused
--  it; Release uncounts a channel that ended. Under Server_Limit nothing is
--  capped here: the server's real exhaustion refuses (Channel_Servers), and
--  a client is bounded by its memory quota, which its grants are charged
--  to. The table of clients grows with the server's connection table
--  (Grow).
--
--  Proved (level 2): each offer's total is always the sum of its clients'
--  counts (ghost); Admit never passes a capped limit and changes exactly the
--  counts it names, and only when it admits; Release lowers exactly those;
--  Grow keeps every count and total. Tested against a model
--  (tests/channel-core).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Channel_Limits;

generic
   Offer_Count : Positive;   --  contracts the server offers
package CuBit.Channel_Admission with Pure, SPARK_Mode is

   --  The structural bound on channels per offer (and on the client table).
   Maximum_Total : constant := CuBit.Channel_Limits.Maximum_Total;
   subtype Offer_Index is Positive range 1 .. Offer_Count;
   subtype Channel_Count is Natural range 0 .. Maximum_Total;
   subtype Table_Size is Positive range 1 .. Maximum_Total;

   --  A client: one process life's identity word. Zero is no client.
   type Client_Id is new Unsigned_64;
   No_Client : constant Client_Id := 0;

   subtype Channel_Limit is CuBit.Channel_Limits.Channel_Limit;
   function Within (Limit : Channel_Limit; Count : Natural) return Boolean
     renames CuBit.Channel_Limits.Within;

   type Limits is record
      Per_Client : Channel_Limit;
      Total      : Channel_Limit;
   end record;
   type Limit_Table is array (Offer_Index) of Limits;

   type Count_Table is array (Offer_Index) of Channel_Count;
   No_Counts : constant Count_Table := [others => 0];

   type Client_Record is record
      Id     : Client_Id := No_Client;
      Counts : Count_Table := No_Counts;
   end record;
   No_Record : constant Client_Record := (Id => No_Client, Counts => No_Counts);
   type Client_Table is array (Table_Size range <>) of Client_Record;

   type State (Size : Table_Size) is record
      Declared : Limit_Table;
      Totals   : Count_Table := No_Counts;
      Clients  : Client_Table (1 .. Size) := [others => No_Record];
   end record;

   --  The channels counted on Offer over clients 1 .. Last (ghost: the
   --  total is always the sum, which is what makes Release exact).
   subtype Partial_Sum is Long_Long_Integer range 0 .. Long_Long_Integer (Maximum_Total) * Maximum_Total;
   function Sum (Clients : Client_Table; Offer : Offer_Index; Last : Natural) return Partial_Sum is
     (if Last = 0 then 0
      else Sum (Clients, Offer, Last - 1) + Long_Long_Integer (Clients (Last).Counts (Offer)))
   with Ghost, Pre => Clients'First = 1 and then Last <= Clients'Last,
        Post => Sum'Result <= Long_Long_Integer (Last) * Maximum_Total,
        Subprogram_Variant => (Decreases => Last);

   function Distinct_Clients (S : State) return Boolean is
     (for all I in 1 .. S.Size =>
        (for all J in 1 .. S.Size =>
           (if I /= J and then S.Clients (I).Id /= No_Client
            then S.Clients (I).Id /= S.Clients (J).Id)));

   function Within_Limits (S : State) return Boolean with Ghost;
   function Within_Limits (S : State) return Boolean is
     ((for all O in Offer_Index =>
         Within (S.Declared (O).Total, S.Totals (O))
         and then Long_Long_Integer (S.Totals (O)) = Sum (S.Clients, O, S.Size)
         and then (for all C in 1 .. S.Size =>
                     Within (S.Declared (O).Per_Client, S.Clients (C).Counts (O))))
      --  An empty record counts nothing.
      and then (for all C in 1 .. S.Size =>
                  (if S.Clients (C).Id = No_Client
                   then (for all O in Offer_Index => S.Clients (C).Counts (O) = 0))));

   function Valid (S : State) return Boolean is (Distinct_Clients (S) and then Within_Limits (S))
   with Ghost;

   procedure Start (S : out State; Declared : Limit_Table)
   with Post => Valid (S) and then S.Declared = Declared and then S.Totals = No_Counts;

   function Has (S : State; Client : Client_Id) return Boolean is
     (for some C in 1 .. S.Size => S.Clients (C).Id = Client);

   function Index_Of (S : State; Client : Client_Id) return Table_Size
   with Pre  => Has (S, Client),
        Post => Index_Of'Result <= S.Size and then S.Clients (Index_Of'Result).Id = Client;

   --  The count of Client's channels on Offer.
   function Count_Of (S : State; Offer : Offer_Index; Client : Client_Id) return Channel_Count is
     (if Has (S, Client) then S.Clients (Index_Of (S, Client)).Counts (Offer) else 0)
   with Pre => Client /= No_Client;

   --  Changing one client's count on Offer by one changes the sum by one.
   procedure Lemma_Sum_Step
     (Before, After : Client_Table; Offer : Offer_Index; K : Table_Size; Up : Boolean)
   with Ghost,
        Pre  => Before'First = 1 and then After'First = 1 and then Before'Last = After'Last
                and then Before'Last >= 1 and then K <= Before'Last
                and then (for all C in Before'Range =>
                            (if C /= K then Before (C).Counts (Offer) = After (C).Counts (Offer)))
                and then (if Up then After (K).Counts (Offer) = Before (K).Counts (Offer) + 1
                          else After (K).Counts (Offer) + 1 = Before (K).Counts (Offer)),
        Post => (if Up then Sum (After, Offer, After'Last) = Sum (Before, Offer, Before'Last) + 1
                 else Sum (After, Offer, After'Last) + 1 = Sum (Before, Offer, Before'Last));

   --  Tables equal on Offer's counts have equal sums.
   procedure Lemma_Sum_Same (Before, After : Client_Table; Offer : Offer_Index)
   with Ghost,
        Pre  => Before'First = 1 and then After'First = 1 and then Before'Last = After'Last
                and then Before'Last >= 1
                and then (for all C in Before'Range => Before (C).Counts (Offer) = After (C).Counts (Offer)),
        Post => Sum (After, Offer, After'Last) = Sum (Before, Offer, Before'Last);

   --  A table extended with empty records keeps its sums.
   procedure Lemma_Sum_Extended (Before, After : Client_Table; Offer : Offer_Index)
   with Ghost,
        Pre  => Before'First = 1 and then After'First = 1 and then Before'Last <= After'Last
                and then Before'Last >= 1
                and then (for all C in Before'Range => Before (C).Counts (Offer) = After (C).Counts (Offer))
                and then (for all C in Before'Last + 1 .. After'Last => After (C).Counts (Offer) = 0),
        Post => Sum (After, Offer, After'Last) = Sum (Before, Offer, Before'Last);

   type Admission is (Admitted, Per_Client_Limit, Total_Limit, No_Client_Room);

   procedure Admit
     (S : in out State; Offer : Offer_Index; Client : Client_Id; Result : out Admission)
   with Pre  => Valid (S) and then Client /= No_Client,
        Post => Valid (S) and then S.Declared = S'Old.Declared
                and then (if Result = Admitted then
                            S.Totals (Offer) = S'Old.Totals (Offer) + 1
                            and then Count_Of (S, Offer, Client) = Count_Of (S'Old, Offer, Client) + 1
                            and then (for all O in Offer_Index =>
                                        (if O /= Offer then S.Totals (O) = S'Old.Totals (O)))
                          else S = S'Old)
                and then (if not Within (S'Old.Declared (Offer).Total, S'Old.Totals (Offer) + 1)
                          then Result /= Admitted)
                and then (if not Within (S'Old.Declared (Offer).Per_Client,
                                         Count_Of (S'Old, Offer, Client) + 1)
                          then Result /= Admitted);

   --  A channel of Client's on Offer ended (no effect if none was counted).
   procedure Release (S : in out State; Offer : Offer_Index; Client : Client_Id)
   with Pre  => Valid (S) and then Client /= No_Client,
        Post => Valid (S) and then S.Declared = S'Old.Declared
                and then (if Count_Of (S'Old, Offer, Client) > 0 then
                            S.Totals (Offer) = S'Old.Totals (Offer) - 1
                            and then Count_Of (S, Offer, Client) = Count_Of (S'Old, Offer, Client) - 1
                          else S = S'Old)
                and then (for all O in Offer_Index =>
                            (if O /= Offer then S.Totals (O) = S'Old.Totals (O)));

   --  The same counts in a larger table (the server's connections grew).
   procedure Grow (From : State; Into : out State)
   with Pre  => Valid (From) and then Into.Size >= From.Size,
        Post => Valid (Into) and then Into.Declared = From.Declared
                and then Into.Totals = From.Totals
                and then (for all C in 1 .. From.Size => Into.Clients (C) = From.Clients (C));

end CuBit.Channel_Admission;
