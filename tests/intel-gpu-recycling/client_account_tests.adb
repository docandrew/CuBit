with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_Client_Budgets; use Intel_GPU_Client_Budgets;
-- Per-session client budget accounts with recycled sessions (Linux-hosted,
-- the production ledger against a model): accounts of finished sessions are
-- forgotten only when closed and charged nothing, every other account stays
-- exact through the tree surgery, and the ledger holds only live accounts.
procedure Client_Account_Tests is
   type Ledger_Access is access Ledger;
   Object : Ledger renames Ledger_Access'(new Ledger).all;
   Page : constant := 4096;
   Storage_Bytes : constant := 16 * Page;
   type Memory is array (1 .. Storage_Bytes + Page) of Unsigned_8;
   type Memory_Access is access Memory;
   Arena : constant Memory_Access := new Memory'(others => 0);
   Start : constant Unsigned_64 := Unsigned_64 (To_Integer (Arena.all'Address));
   Base : constant Unsigned_64 := Start + (Page - Start mod Page) mod Page;
   Max_Accounts : constant := 300;
   type Model_Account is record
      Session, Limit, Charged : Unsigned_64 := 0;
      Closed : Boolean := False;
   end record;
   Model : array (1 .. Max_Accounts) of Model_Account;
   Used : Natural := 0;
   Next_Session : Unsigned_64 := 16#4750_0000_0001_0001#;
   Seed : Unsigned_64 := 16#2545_F491_4F6C_DD1D#;
   function Random (Below : Positive) return Natural is
   begin
      Seed := Seed xor Shift_Left (Seed, 13);
      Seed := Seed xor Shift_Right (Seed, 7);
      Seed := Seed xor Shift_Left (Seed, 17);
      return Natural (Seed mod Unsigned_64 (Below));
   end Random;
   Checks, Failures, Forgotten : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         if Failures <= 20 then Put_Line ("FAIL: " & What); end if;
      end if;
   end Check;
   procedure Verify is
      U : Usage;
   begin
      Check (Intel_GPU_Client_Budgets.Count (Object) = Used, "ledger holds exactly the live accounts");
      for I in 1 .. Used loop
         U := Snapshot (Object, Model (I).Session);
         Check (U.Known and U.Limit = Model (I).Limit and U.Charged = Model (I).Charged and
                U.Closed = Model (I).Closed, "every live account stays exact");
      end loop;
      Check (not Snapshot (Object, Next_Session + 1).Known, "an unopened account is unknown");
   end Verify;
   OK : Boolean;
   I : Positive;
begin
   if Ada.Command_Line.Argument_Count = 1 then
      Seed := Unsigned_64'Value (Ada.Command_Line.Argument (1));
   end if;
   Extend (Object, Base, Storage_Bytes, OK);
   Check (OK and Capacity (Object) >= Max_Accounts, "storage for the accounts");
   for Step in 1 .. 200_000 loop
      case Random (6) is
         when 0 =>
            if Used < Max_Accounts then
               -- Monotonic tags with gaps, as session tags are.
               Next_Session := Next_Session + Unsigned_64 (Random (5)) * 16#1_0000# + 1;
               Open (Object, Next_Session, 16 * Page, OK);
               Check (OK, "open");
               Used := Used + 1;
               Model (Used) := (Next_Session, 16 * Page, 0, False);
            end if;
         when 1 =>
            if Used > 0 then
               I := Random (Used) + 1;
               Reserve (Object, Model (I).Session, Page, OK);
               Check (OK = (not Model (I).Closed and Model (I).Charged + Page <= Model (I).Limit),
                      "reserve within the limit of an open account");
               if OK then Model (I).Charged := Model (I).Charged + Page; end if;
            end if;
         when 2 =>
            if Used > 0 then
               I := Random (Used) + 1;
               Release_Confirmed (Object, Model (I).Session, Page, True, OK);
               Check (OK = (Model (I).Charged >= Page), "release what was charged");
               if OK then Model (I).Charged := Model (I).Charged - Page; end if;
            end if;
         when 3 =>
            if Used > 0 then
               I := Random (Used) + 1;
               Close (Object, Model (I).Session); Model (I).Closed := True;
            end if;
         when others =>
            if Used > 0 then
               I := Random (Used) + 1;
               Forget (Object, Model (I).Session, OK);
               Check (OK = (Model (I).Closed and Model (I).Charged = 0),
                      "forget only a closed account that is charged nothing");
               if OK then
                  Check (not Snapshot (Object, Model (I).Session).Known, "a forgotten account is gone");
                  Model (I) := Model (Used); Used := Used - 1;
                  Forgotten := Forgotten + 1;
               end if;
            end if;
      end case;
      if Step mod 97 = 0 then Verify; end if;
   end loop;
   Verify;
   Put_Line ("forgotten" & Natural'Image (Forgotten) & ", live" & Natural'Image (Used) &
             ", checks" & Natural'Image (Checks));
   if Failures = 0 and then Forgotten > 1_000 then
      Put_Line ("Client accounts PASS: forget of finished sessions, exact accounts through tree surgery");
   else
      Put_Line ("Client accounts FAIL:" & Natural'Image (Failures));
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Client_Account_Tests;
