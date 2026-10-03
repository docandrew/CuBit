with Ada.Directories;
with Ada.Text_IO;

--  The Linux preview's stand-in for proc.list: the host's /proc. A
--  Linux-hosted demonstration, not CuBit's process table.
package body CCL_Processes is
   use Interfaces;
   PROC : constant String := "/proc";
   PAGE_BYTES : constant := 4_096;

   function State_Of (Code : Character) return Processes.Run_State is
     (case Code is
         when 'R' => Processes.Running,
         when 'S' | 'I' => Processes.Sleeping,
         when 'D' => Processes.Waiting,
         when 'T' | 't' => Processes.Suspended,
         when others => Processes.Ready);

   --  Clock ticks per second for /proc start times (Linux USER_HZ).
   TICKS_PER_SECOND : constant := 100;
   MS_PER_SECOND : constant := 1_000;

   --  Milliseconds since boot, from /proc/uptime.
   function Uptime_Ms return Unsigned_64 is
      File : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Open (File, Ada.Text_IO.In_File, PROC & "/uptime");
      declare
         Line : constant String := Ada.Text_IO.Get_Line (File);
         Dot : Natural := Line'First;
      begin
         Ada.Text_IO.Close (File);
         while Dot <= Line'Last and then Line (Dot) /= '.' loop Dot := Dot + 1; end loop;
         return Unsigned_64'Value (Line (Line'First .. Dot - 1)) * MS_PER_SECOND;
      end;
   exception
      when others => return 0;
   end Uptime_Ms;

   procedure List
     (Entries : out Listing; Count : out Listed_Count; Total : out Natural; Result : out Result_Kind)
   is
      Search : Ada.Directories.Search_Type;
      Item : Ada.Directories.Directory_Entry_Type;
      Now : Unsigned_64;
   begin
      Entries := [others => (others => <>)];
      Count := 0;
      Total := 0;
      Result := (if Ada.Directories.Exists (PROC) then Listed_All else Unavailable);
      if Result /= Listed_All then return; end if;
      Now := Uptime_Ms;
      Ada.Directories.Start_Search (Search, PROC, "", (Ada.Directories.Directory => True, others => False));
      while Ada.Directories.More_Entries (Search) loop
         Ada.Directories.Get_Next_Entry (Search, Item);
         declare
            Pid_Text : constant String := Ada.Directories.Simple_Name (Item);
         begin
            if Pid_Text'Length in 1 .. 9 and then (for all C of Pid_Text => C in '0' .. '9') then
               declare
                  File : Ada.Text_IO.File_Type;
               begin
                  Ada.Text_IO.Open (File, Ada.Text_IO.In_File, PROC & "/" & Pid_Text & "/stat");
                  declare
                     --  pid (comm) state ppid ... : fields after ")" count from 3.
                     Line : constant String := Ada.Text_IO.Get_Line (File);
                     Open : Natural := 0;
                     Close : Natural := 0;
                     Field : Natural := 2;
                     Start : Natural;
                     Got : Listed;
                  begin
                     for I in Line'Range loop
                        if Line (I) = '(' and then Open = 0 then Open := I; end if;
                        if Line (I) = ')' then Close := I; end if;
                     end loop;
                     if Open > 0 and then Close > Open and then Close + 2 <= Line'Last then
                        Got.Pid := Natural'Value (Pid_Text);
                        Got.Name_Length := Natural'Min (Close - Open - 1, MAXIMUM_NAME);
                        Got.Name (1 .. Got.Name_Length) := Line (Open + 1 .. Open + Got.Name_Length);
                        Got.State := State_Of (Line (Close + 2));
                        Start := Close + 2;
                        for I in Close + 2 .. Line'Last + 1 loop
                           if I > Line'Last or else Line (I) = ' ' then
                              Field := Field + 1;
                              if Field = 4 then Got.Launcher := Natural'Value (Line (Start .. I - 1)); end if;
                              if Field = 22 then
                                 declare
                                    Started : constant Unsigned_64 :=
                                      Unsigned_64'Value (Line (Start .. I - 1)) * MS_PER_SECOND / TICKS_PER_SECOND;
                                 begin
                                    Got.Age_Ms := (if Started <= Now then Now - Started else 0);
                                 end;
                              end if;
                              if Field = 24 then Got.Memory := Unsigned_64'Value (Line (Start .. I - 1)) * PAGE_BYTES; end if;
                              Start := I + 1;
                           end if;
                        end loop;
                        Total := Total + 1;
                        if Count < Processes.MAXIMUM_LISTED then
                           Count := Count + 1;
                           Entries (Count) := Got;
                        end if;
                     end if;
                  end;
                  Ada.Text_IO.Close (File);
               exception
                  --  A process may exit while it is read.
                  when others =>
                     if Ada.Text_IO.Is_Open (File) then Ada.Text_IO.Close (File); end if;
               end;
            end if;
         end;
      end loop;
      Ada.Directories.End_Search (Search);
   end List;
end CCL_Processes;
