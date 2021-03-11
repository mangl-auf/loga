with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Loga is
   type Logger is tagged private;

   procedure New_Logger (Self : in out Logger; Name : String);
   procedure Log (Self : Logger; Message : String);
private
   type Colors is (Normal, Red, Green, Yellow, Blue, Magenta, Cyan);

   Colors_As_Integer : array (Colors) of Natural := (Normal  => 0,
                                                     Red => 31,
                                                     Green => 32,
                                                     Yellow => 33,
                                                     Blue => 34,
                                                     Magenta => 35,
                                                     Cyan => 36);
   type Logger is tagged record
      Name     : Unbounded_String;
      Color    : Colors;
      Disabled : Boolean := True;
   end record;

   Current_Color : Natural range Colors_As_Integer (Colors'Val (1))
      .. Colors_As_Integer (Colors'Last) := Colors_As_Integer (Red);

   procedure Colorize_Output (Color : Colors);
   function Start_Bold return String;
   function End_Bold return String;
end Loga;
