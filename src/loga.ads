with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Loga is
   type Logger is tagged private;
   type Colors is (Normal, Red, Green, Yellow, Blue, Magenta, Cyan);

   function New_Logger (Name  : String;
                        Color : Colors := Normal) return Logger;

   procedure Log (Self : Logger; Message : String);

   function Get_Color (Self : Logger) return Colors;
private
   Colors_As_Integer : array (Colors) of Natural := (Normal  => 0,
                                                     Red     => 31,
                                                     Green   => 32,
                                                     Yellow  => 33,
                                                     Blue    => 34,
                                                     Magenta => 35,
                                                     Cyan    => 36);
   type Logger is tagged record
      Name     : Unbounded_String;
      Color    : Colors;
      Disabled : Boolean := True;
   end record;
end Loga;
