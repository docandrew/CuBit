with Servo_Session;
package body Servo_Shell is
   package S1 is new Servo_Session;
   package S2 is new Servo_Session;
   package S3 is new Servo_Session;
   package S4 is new Servo_Session;
   subtype Slot is Positive range 1 .. 4;
   Selected : Slot := 1;
   Lease : Natural range 0 .. 4 := 0;
   Used, Retiring : array (Slot) of Boolean := [others => False];
   function Select_Window (ID : Unsigned_32) return Unsigned_32 is
   begin
      if ID not in 1 .. 4 or else not Used (Slot (ID)) or else
        Retiring (Slot (ID)) or else (Lease /= 0 and then Lease /= Natural (ID))
      then return 0; end if;
      Selected := Slot (ID); return 1;
   end Select_Window;

   procedure Retry_Close (I : Slot) is
   begin
      case I is
         when 1 => S1.Close; Used (I) := S1.Is_Open;
         when 2 => S2.Close; Used (I) := S2.Is_Open;
         when 3 => S3.Close; Used (I) := S3.Is_Open;
         when 4 => S4.Close; Used (I) := S4.Is_Open;
      end case;
      Retiring (I) := Used (I);
   end Retry_Close;
   function Open return Unsigned_32 is
      Opened : Unsigned_32;
   begin
      if Lease /= 0 then return 0; end if;
      for I in Slot loop
         if Retiring (I) then Retry_Close (I); end if;
         if not Used (I) then
            case I is
               when 1 => Opened := S1.Open;
               when 2 => Opened := S2.Open;
               when 3 => Opened := S3.Open;
               when 4 => Opened := S4.Open;
            end case;
            if Opened /= 0 then
               Used (I) := True; Selected := I; return Unsigned_32 (I);
            end if;
         end if;
      end loop;
      return 0;
   end Open;
   procedure Metrics (Result : access Viewport) is
   begin
      case Selected is
         when 1 => S1.Metrics (Result);
         when 2 => S2.Metrics (Result);
         when 3 => S3.Metrics (Result);
         when 4 => S4.Metrics (Result);
      end case;
   end Metrics;
   procedure Begin_Input is
   begin
      case Selected is
         when 1 => S1.Begin_Input;
         when 2 => S2.Begin_Input;
         when 3 => S3.Begin_Input;
         when 4 => S4.Begin_Input;
      end case;
   end Begin_Input;
   function Poll (Result : access Event) return Unsigned_32 is
   begin
      case Selected is
         when 1 => return S1.Poll (Result);
         when 2 => return S2.Poll (Result);
         when 3 => return S3.Poll (Result);
         when 4 => return S4.Poll (Result);
      end case;
   end Poll;
   function Location (Text : System.Address; Capacity : Unsigned_32) return Unsigned_32 is
   begin
      case Selected is
         when 1 => return S1.Location (Text, Capacity);
         when 2 => return S2.Location (Text, Capacity);
         when 3 => return S3.Location (Text, Capacity);
         when 4 => return S4.Location (Text, Capacity);
      end case;
   end Location;
   procedure State
     (URL : System.Address; URL_Length : Unsigned_32;
      Title : System.Address; Title_Length : Unsigned_32;
      Flags : Unsigned_32) is
   begin
      case Selected is
         when 1 => S1.State (URL, URL_Length, Title, Title_Length, Flags);
         when 2 => S2.State (URL, URL_Length, Title, Title_Length, Flags);
         when 3 => S3.State (URL, URL_Length, Title, Title_Length, Flags);
         when 4 => S4.State (URL, URL_Length, Title, Title_Length, Flags);
      end case;
   end State;
   procedure Tab_Title (Index : Unsigned_32; Text : System.Address; Length : Unsigned_32) is
   begin
      case Selected is
         when 1 => S1.Tab_Title (Index, Text, Length);
         when 2 => S2.Tab_Title (Index, Text, Length);
         when 3 => S3.Tab_Title (Index, Text, Length);
         when 4 => S4.Tab_Title (Index, Text, Length);
      end case;
   end Tab_Title;
   procedure Tab_Parked (Index : Unsigned_32) is
   begin
      case Selected is
         when 1 => S1.Tab_Parked (Index);
         when 2 => S2.Tab_Parked (Index);
         when 3 => S3.Tab_Parked (Index);
         when 4 => S4.Tab_Parked (Index);
      end case;
   end Tab_Parked;
   procedure Navigation_Error is
   begin
      case Selected is
         when 1 => S1.Navigation_Error;
         when 2 => S2.Navigation_Error;
         when 3 => S3.Navigation_Error;
         when 4 => S4.Navigation_Error;
      end case;
   end Navigation_Error;
   function Prepare return Unsigned_32 is
      Ready : Unsigned_32;
   begin
      if Lease /= 0 then return 0; end if;
      case Selected is
         when 1 => Ready := S1.Prepare;
         when 2 => Ready := S2.Prepare;
         when 3 => Ready := S3.Prepare;
         when 4 => Ready := S4.Prepare;
      end case;
      if Ready /= 0 then Lease := Selected; end if; return Ready;
   end Prepare;
   procedure Cancel is
   begin
      case Selected is
         when 1 => S1.Cancel;
         when 2 => S2.Cancel;
         when 3 => S3.Cancel;
         when 4 => S4.Cancel;
      end case;
      Lease := 0;
   end Cancel;
   function Present
     (RGBA : System.Address; Length : Unsigned_64;
      Width, Height : Unsigned_32) return Unsigned_32 is
      Result : Unsigned_32;
   begin
      if Lease /= Selected then return 0; end if;
      case Selected is
         when 1 => Result := S1.Present (RGBA, Length, Width, Height);
         when 2 => Result := S2.Present (RGBA, Length, Width, Height);
         when 3 => Result := S3.Present (RGBA, Length, Width, Height);
         when 4 => Result := S4.Present (RGBA, Length, Width, Height);
      end case;
      Lease := 0; return Result;
   end Present;
   function Pending return Unsigned_32 is
   begin
      case Selected is
         when 1 => return S1.Pending;
         when 2 => return S2.Pending;
         when 3 => return S3.Pending;
         when 4 => return S4.Pending;
      end case;
   end Pending;
   procedure Window_Error is
   begin
      case Selected is
         when 1 => S1.Window_Error;
         when 2 => S2.Window_Error;
         when 3 => S3.Window_Error;
         when 4 => S4.Window_Error;
      end case;
   end Window_Error;
   procedure Close is
   begin
      Cancel; Retiring (Selected) := True; Retry_Close (Selected);
   end Close;
end Servo_Shell;
