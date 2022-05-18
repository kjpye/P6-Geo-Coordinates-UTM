use v6;
use Test;

plan 3;

# use "../lib";
use Geo::Coordinates::UTM;

ok True, "Module loaded";

my (Str $zone, Real $east, Real $north);

ok ($zone,$east,$north)=|latlon-to-utm('WGS-84',57.833055556,-2.788951667), "latlon-to-utm available";

ok utm-to-latlon('WGS-84',$zone,$east,$north), "utm-to-latlon available";
