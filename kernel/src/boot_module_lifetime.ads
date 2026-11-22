pragma Ada_2022;
with Boot_Modules;

-- Who still reads a loader-placed boot module, and when its pages may go
-- back to the buddy allocator. Pure lifetime bookkeeping: locks, page
-- tables and physical memory belong to the kernel adapter (Modules).
--
-- A module is Retained from boot: its exact pages are reserved and only the
-- kernel reads them. The kernel then either releases it whole (nobody
-- consumes it) or Shares it: from then on every page carries the number of
-- user mappings of it, and a page whose count reaches zero is released at
-- once, never to be mapped again. A Shared module is Released when its last
-- page is.
package Boot_Module_Lifetime with SPARK_Mode, Pure is
   type Phase is (Retained, Shared, Released);

   function Allowed (From, To : Phase) return Boolean is
     ((From = Retained and then To in Shared | Released) or else
      (From = Shared and then To = Released));

   procedure Advance (State : in out Phase; To : Phase) with
     Global => null,
     Pre => Allowed (State, To),
     Post => State = To;

   -- Pages of one module, from its first page. Modules lie in the bootstrap
   -- window, so a module never has more pages than the window.
   Window_Pages : constant := Boot_Modules.Window_End / Boot_Modules.Page_Bytes;
   subtype Page_Count is Natural range 0 .. Window_Pages;
   subtype Page_Offset is Natural range 0 .. Window_Pages - 1;

   -- Named ceiling on simultaneous user mappings of one page (every
   -- process together): a mapping beyond it is refused, never wrapped.
   Maximum_References : constant := 2 ** 16 - 1;
   type Reference_Count is range 0 .. Maximum_References with Size => 16;
   -- Zero, once Shared, means released: the page is no longer the module's.
   type Reference_Table is array (Page_Offset range <>) of Reference_Count
     with Component_Size => 16;

   function Within (Table : Reference_Table; First : Page_Offset;
                    Count : Page_Count) return Boolean is
     (Count > 0 and then First >= Table'First and then First <= Table'Last
      and then Count - 1 <= Table'Last - First);

   function Live (Table : Reference_Table; First : Page_Offset;
                  Count : Page_Count) return Boolean is
     (for all P in First .. First + (Count - 1) => Table (P) > 0)
   with Pre => Within (Table, First, Count);

   -- Sharing begins with exactly one mapping of every page: the root
   -- mapping the kernel installs for the module's first consumer.
   procedure Share (Table : out Reference_Table) with
     Global => null,
     Post => (for all P in Table'Range => Table (P) = 1);

   -- One more mapping of each page in the span: all or nothing. A released
   -- page (count zero) is never revived, and no count passes the ceiling.
   procedure Acquire (Table : in out Reference_Table; First : Page_Offset;
                      Count : Page_Count; Granted : out Boolean) with
     Global => null,
     Pre => Within (Table, First, Count),
     Post =>
       Granted = (for all P in First .. First + (Count - 1) =>
                    Table'Old (P) in 1 .. Maximum_References - 1)
       and then
       (for all P in Table'Range =>
          (if Granted and then P in First .. First + (Count - 1)
           then Table (P) = Table'Old (P) + 1
           else Table (P) = Table'Old (P)));

   -- One mapping of Page is gone. Released when it was the last one.
   procedure Drop (Table : in out Reference_Table; Page : Page_Offset;
                   Released : out Boolean) with
     Global => null,
     Pre => Page in Table'Range and then Table (Page) > 0,
     Post => Table (Page) = Table'Old (Page) - 1 and then
       Released = (Table (Page) = 0) and then
       (for all P in Table'Range =>
          (if P /= Page then Table (P) = Table'Old (P)));
end Boot_Module_Lifetime;
