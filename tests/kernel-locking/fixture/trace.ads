with Interfaces;
package Trace is
    subtype EventKind is Natural;
    EVENT_LOCK_WAIT : constant EventKind := 7;
    EVENT_LOCK_HOLD : constant EventKind := 8;
    function IsEnabled return Boolean is (False);
    procedure ObserveDuration (Event : EventKind; Ticks : Interfaces.Unsigned_64)
      is null;
end Trace;
